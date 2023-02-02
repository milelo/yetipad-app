(ns lib.db
  (:refer-clojure :exclude [atom])
  (:require
   [lib.log :as log :refer-macros [stack trace debug info warn fatal] :refer [pprintl trace-diff]]
   [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go-loop go]]
   [lib.asyncutils :as au :refer [put-last! chan?] :refer-macros [<? go? go-try goc go-let]]
   [lib.utils :as utils]
   ;https://cljdoc.org/d/andare/andare/1.0.0/api/cljs.core.async.interop
   [cljs.core.async.interop :refer [p->c] :refer-macros [<p!]]
   [promesa.core :as p]
   [reagent.core :as r]
   [clojure.core :as core]
   [lib.debug :as debug :refer [we wd]]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [clojure.pprint :refer [pprint]])
  (:require-macros
   [taoensso.truss :as truss :refer [have have! have? have!?]]
   [lib.assertion :as assert]))

(def log (log/logger 'lib.db))

(defonce db* (r/atom {::db? true}))

;provide db binding to enable update-db to guard against db changes.
;todo implement and provide 'ignore' key paths.
(def ^:dynamic *context* {})

;DEPRECATED Eliminate ASAP
(def after-db-change* (core/atom nil))

;====================================task queue=================================================


(defonce <task-queue (chan 20))
(def <inject-task (chan 4))

(defn task-runner []
  ;execute queued go-block functions
  (trace log 'task-runner-started)
  (go-loop [i 0]
    (when-let [[<fn <c {:keys [label timeout timer]}] (<! <task-queue)]
      (binding [*context* (assoc *context* :db @db* :label label :do-sync? true)]
        (let [<response (try (<fn (:db *context*)) (catch :default e (go e)))
              <response (if (chan? <response) <response (go <response))
              <timeout (if timeout (async/timeout timeout) (chan))
              id (when timer (js/setInterval #(warn log 'task-runner "no response from task: " label i) timer))]
          (trace log 'task-started label i)
          (when-let [[v port] (async/alts! [<response <inject-task <timeout])]
            (when timer (js/clearInterval id))
            (put! <c (condp = port
                       <response (do
                                   (trace log 'task-complete label i)
                                   (if (nil? v) ::nil v))
                       <timeout (js/Error. (str "task timeout: " label " " i " " timeout "ms"))
                       <inject-task (js/Error. (str "aborting task: " label " " i)))))))
      (close! <c)
      (recur (inc i)))
    (log/error log 'task-runner-stopped)))

(defn- start-task-runner []
  (go
    (while (async/poll! <inject-task))
    (task-runner)))

(defn- stop-task-runner []
  (put! <task-queue false))

(defn- abort-tasks []
  (go
    ;(while (async/offer! <inject-task false))
    (while (async/poll! <task-queue))
    (put! <inject-task false)
    (trace log 'abort-tasks "tasks aborted")))

(defn- reset-task-runner []
  (go
    (<! (abort-tasks))
    (stop-task-runner)
    (<! (start-task-runner))))

(comment
  (reset-task-runner)
  (abort-tasks)
  (stop-task-runner)
  (start-task-runner))

(defonce _ (task-runner))


(defn <do-sync
  "Adds a function returning a channel yielding a single result (usually a go block) to a task queue.
  Sequences calls to go-blocks submitted from concurrent processes.
  Required to support external async calls.
  <fn is a function that returns a channel yielding a single value.
  Listeners: on-success & on-error notify of completion or error respectively.
  "
  ([<fn]
   (<do-sync nil <fn))
  ([label-or-props <fn]
   (assert (not (:do-sync? *context*)))
   (go-let [props (if (map? label-or-props)
                    label-or-props
                    {:label label-or-props})
            props (merge-with #(or %1 %2) props {:timer 10000 :label (:label *context*)}); provide defaults 
            <do (fn [<fn]
                  (let [<c (chan)]
                    (trace log "task-queue put" (:label props))
                    (put! <task-queue [<fn <c props])
                    <c))]
           (let [r (<! (<do <fn))] (if (= r ::nil) nil r))))
  ([label-or-props <fn {:keys [on-success on-error]}]
   (go-let [v (<! (<do-sync label-or-props <fn))]
           (if (utils/error? v)
             (if on-error (on-error v) (warn log '<do-sync v))
             (when on-success (on-success v))))))

(defn $do-sync
  ([$fn]
   ($do-sync nil $fn))
  ([label-or-props $fn {:keys [on-success on-error]}]
   (assert (not (:do-sync? *context*)))
   (-> (let [props (if (map? label-or-props)
                     label-or-props
                     {:label label-or-props})
             props (merge-with #(or %1 %2) props {:label (:label *context*)})
             d (p/deferred)]
         (go (p/resolve! d (<? (<do-sync props
                                         (fn [db]
                                           (p->c (p/do ($fn db)))))
                               #(p/reject! d %))))
         d)
       (p/then #(and on-success (on-success)))
       (p/catch #(if on-error
                   (on-error)
                   (fn [e]
                     (log/error log 'do-sync "unhandled task error: " e)
                     e)))))
  ([label-or-props $f]
   ($do-sync label-or-props $f nil)))

(defn do-async [label-or-props f]
  (try
    (let [{:keys [label]} (if (map? label-or-props)
                            label-or-props
                            {:label label-or-props})]
      (binding [*context* (assoc *context* :db @db* :label label)]
        (trace log 'start label)
        (let [r (f (:db *context*))]
          (trace log 'end label)
          r)))
    (catch :default e (log/error log 'do-async "unhandled task error: " e))))

(defn- <delay [ms v]
  (let [<c (chan)]
    (js/setTimeout #(put! <c (or v false)) ms)
    <c))

(comment
  (reset-task-runner)
  (abort-tasks)
  (stop-task-runner)
  (start-task-runner)

  (go (prn (<! (<do-sync :a (fn [_db] (<delay 5000 :a))))))
  (go (prn (<! (<do-sync {:label 'timeout :timeout 4000} (fn [_db] (<delay 5000 :timeout))))))
  (go (prn (<! (<do-sync :go-nil (fn [_db] (go nil))))))
  (go (prn (<! (<do-sync :nil (fn [_db] nil)))))
  (go
    (prn :start)
    (go (prn (<! (<do-sync :a (fn [_db] (<delay 5000 :a))))))
    (go (prn (<! (<do-sync 'error-test (fn [_db] (go? (throw (ex-info "error" {:error :an-error}))))))))
    (go (prn (<! (<do-sync :b (fn [_db] (go :b))))))
    (go (prn (<! (<do-sync {:label 'timer :timer 4000} (fn [_db] (<delay 5000 :timer))))))
    (go (prn (<! (<do-sync {:label 'timeout :timeout 4000} (fn [_db] (<delay 5000 :timeout))))))
    (go (prn (<! (<do-sync :go-nil (fn [_db] (go nil))))))
    (go (prn (<! (<do-sync :nil (fn [_db] nil)))))
    (go (prn (<! (<do-sync 'db-keys (fn [db] (go (keys db)))))))))

(defn- $delay [ms & [v]]
  (let [p (p/deferred)]
    (js/setTimeout #(p/resolve! p v) ms)
    p))

(comment
  (p/then ($do-sync :a (fn [_db] ($delay 5000 :a))) prn)
  (p/then ($do-sync {:label 'timeout :timeout 4000} (fn [_db] ($delay 5000 :timeout))) prn)
  (p/then ($do-sync :b (fn [_db] :b)) prn)
  (p/then ($do-sync :nil (fn [_db] nil)) prn)

  (let []
    (prn :start)
    (p/then ($do-sync :a (fn [_db] ($delay 5000 :a))) prn)
    (p/catch ($do-sync 'error (fn [_db] (throw (ex-info "error" {:error :an-error})))) prn)
    (p/then ($do-sync :b (fn [_db] :b)) prn)
    (p/catch ($do-sync {:label 'timeout :timeout 4000} (fn [_db] ($delay 5000 :timeout))) prn)
    (p/then ($do-sync :nil (fn [_db] nil)) prn)
    (p/then ($do-sync 'db-keys (fn [db] (keys db))) prn)))



;====================================task queue=================================================

(defn update-db!
  ([label-or-props f]
   (assert (fn? f))
   (let [{:keys [label]} (if (map? label-or-props)
                           label-or-props
                           {:label label-or-props})
         label (or label (:label *context*))
         _  (if label
              (trace log 'update-db! label)
              (stack log 'update-db!))
         old-db @db*
         new-db (swap! db* (fn [db]
                             (when (and (:db *context*) (not= db (:db *context*)))
                               (warn log 'update-db! "undeclared db async change:\n"
                                     (trace-diff 'do-sync-db (:db *context*) 'update-db!-db db)))
                             (let [new-db (f db)]
                               (cond
                                 (nil? new-db) db
                                 (and (map? new-db) (::db? new-db)) new-db
                                 :else (do
                                         (log/error log 'update-db! "Attempted db overwrite.")
                                         db)))))]
     (when-let [after! @after-db-change*]
       (after! old-db new-db))
     new-db))
  ([f] (update-db! nil f)))

(defn atom
  "Returns: a potentially cacheable deref-able var that behaves as reagent atom.
   ipfn: fn [db] Produces argument ipfn-op for cachefn. Executes on a deref.
   cachefn: fn [ipfn-op] Generates cached cachefn-op, updated on ipfn-op change. Caches are memoized against args."
  ([ipfn cachefn]
   ;cache is map of instance-args vs cache-entry - {:input cache-inputs :cached cached-value}
   (let [cache* (clojure.core/atom {})]
     (r/track
      #(let [updater (fn [cache-entry];{:input input :cached cached-value}
                       (let [cache-fn-args (ipfn @db*)]
                         (if (and cache-entry (= (:input cache-entry) cache-fn-args))
                           cache-entry
                           (assoc cache-entry :cached (cachefn cache-fn-args) :input cache-fn-args))))]
         (get (swap! cache* updater) :cached)))))
  ([ipfn]
   (r/track
    #(ipfn @db*))))

(defn atomfn
  "Returns: fn [& instance-args] that generates a potentially cacheable deref-able var that behaves as reagent atom.
   ipfn: fn [db & instance-args] Produces argument ipfn-op for cachefn. Executes on a deref.
   cachefn: fn [ipfn-op] Generates cached cachefn-op, updated on ipfn-op change. Caches are memoized against args."
  ([ipfn cachefn]
   ;cache is map of instance-args vs cache-entry - {:input cache-inputs :cached cached-value}
   (let [cache* (clojure.core/atom {})]
     (partial r/track
              (fn [& instance-args]
                (let [updater (fn [cache-entry];{:input input :cached cached-value}
                                (let [cache-fn-args (apply ipfn @db* instance-args)]
                                  (if (and cache-entry (= (:input cache-entry) cache-fn-args))
                                    cache-entry
                                    (assoc cache-entry :cached (apply cachefn cache-fn-args instance-args) :input cache-fn-args))))]
                  (get-in (swap! cache* update instance-args updater) [instance-args :cached]))))))
  ([ipfn]
   (partial r/track
            (fn [& instance-args]
              (apply ipfn @db* instance-args)))))

(comment
  (pprint @db*)
  (keys @db*)
  (pprint (dissoc @db* :doc :logger-config :doc-file-index :platform)))