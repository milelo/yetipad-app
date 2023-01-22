(ns lib.db
  (:refer-clojure :exclude [atom])
  (:require
   [lib.log :as log :refer [trace debug info warn fatal pprintl trace-diff]]
   [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go-loop go]]
   [lib.asyncutils :as au :refer [put-last!] :refer-macros [<? go? go-try goc go-let]]
   [lib.utils :as utils]
   ;https://cljdoc.org/d/andare/andare/1.0.0/api/cljs.core.async.interop
   [cljs.core.async.interop :refer [p->c] :refer-macros [<p!]]
   [promesa.core :as p]
   [reagent.core :as r]
   [clojure.core :as core]
   [lib.debug :as debug :refer [we wd]]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [clojure.pprint :refer [pprint]]))

(def log (log/logger 'lib.db))

(defonce db* (r/atom {::db? true}))

;provide db binding to enable update-db to guard against db changes.
;todo implement and provide 'ignore' key paths.
(def ^:dynamic *db*)

;DEPRECATED Eliminate ASAP
(def after-db-change* (core/atom nil))

;====================================task queue=================================================


(defonce <task-queue (chan 20))                             ;go-block queue

(defn task-runner []
  ;execute queued go-block functions
  (trace log 'task-runner-started)
  (go-loop [i 0]
    (when-let [[<fn <c label] (<! <task-queue)]
      (when-let [[v _port] (try (binding [*db* @db*]
                                  (trace log 'task-runner 'task-started label i)
                                  (async/alts! [(<fn *db*) (async/timeout 10000)]))
                                (catch :default e [e nil]))]
        (trace log 'task-runner 'task-complete label i)
        (put! <c (or v (js/Error. (str "task timeout: " i)))))
      (close! <c)
      (recur (inc i)))
    (log/error log 'task-runner-stopped)))

(defn- start-task-runner []
  (task-runner))

(defn- stop-task-runner []
  (put! <task-queue false))

(comment
  (stop-task-runner)
  (start-task-runner)

  (go (let [<c (chan)
            _ (put! <c :a)
            [v _p] (async/alts! [<c (async/timeout 1000)])]
        (prn (or v :b)))))

(defonce _ (task-runner))


(defn <do-sync
  "Adds a function returning a channel yielding a single result (usually a go block) to a task queue.
  Sequences calls to go-blocks submitted from concurrent processes.
  Required to support external async calls.
  <fn is a function that returns a channel yielding a single value.
  Listeners: on-success & on-error notify of completion or error respectively.
  "
  ([label <fn]
   (go-let [<do (fn [<fn]
                  (let [<c (chan)]
                    (put! <task-queue [<fn <c label])
                    <c))]
           (<! (<do <fn))))
  ([label <fn {:keys [on-success on-error]}]
   (go-let [v (<! (<do-sync label <fn))]
           (if (utils/error? v)
             (if on-error (on-error v) (warn log '<do-sync v))
             (when on-success (on-success v))))))

(defn do-sync
  ([label $fn]
  ;adaptor for channel to promise based do-sync
   (let [d (p/deferred)]
     (go (p/resolve! d (<? (<do-sync label
                                     (fn [db]
                                       (go? (<p! (p/do ($fn db))))))
                           #(p/reject! d %))))
     d))
  ([label $f {:keys [on-success on-error]}]
   (let [p (do-sync label $f)]
     (-> p
         (p/then #(and on-success (on-success)))
         (p/catch #(if on-error
                     (on-error)
                     (fn [e]
                       (log/error log 'do-sync "unhandled task error: " e)
                       e))))
     p)))

(defn do-async [f]
  (try
    (binding [*db* @db*]
      (f *db*))
    (catch :default e (log/error log 'do-async "unhandled task error: " e))))

(defn- <delay [ms v]
  (let [<c (chan)]
    (js/setTimeout #(put! <c (or v false)) ms)
    <c))

(comment
  (stop-task-runner)
  (start-task-runner)

  (go (prn (<! (<do-sync :a (fn [_db] (<delay 5000 :a))))))
  (go
    (prn :start)
    (go (prn (<! (<do-sync :a (fn [_db] (<delay 5000 :a))))))
    (go (prn (<! (<do-sync 'error-test (fn [_db] (go? (throw (ex-info "error" {:error :an-error}))))))))
    (go (prn (<! (<do-sync :b (fn [_db] (go :b))))))
    (go (prn (<! (<do-sync 'db-keys (fn [db] (go (keys db)))))))))

(defn- $delay [ms & [v]]
  (let [p (p/deferred)]
    (js/setTimeout #(p/resolve! p v) ms)
    p))

(comment
  (p/then (do-sync :a (fn [_db] ($delay 5000 :a))) prn)

  (p/then (do-sync :b (fn [_db] :b)) prn)

  (let []
    (prn :start)
    (p/then (do-sync :a (fn [_db] ($delay 5000 :a))) prn)
    (p/catch (do-sync 'error (fn [_db] (throw (ex-info "error" {:error :an-error})))) prn)
    (p/then (do-sync :b (fn [_db] :b)) prn)
    (p/then (do-sync 'db-keys (fn [db] (keys db))) prn)))



;====================================task queue=================================================

(defn update-db!
  ([{:keys [label]} f]
   (assert (fn? f))
   (trace log 'update-db label)
   (let [old-db @db*
         new-db (swap! db* (fn [db]
                             (when (and *db* (not= db *db*))
                               (warn log 'update-db! "undeclared db async change:\n"
                                     (trace-diff 'do-sync-db *db* 'update-db!-db db)))
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

(defn firex
  "^:deprecated - Update the db, optionally with a promise."
  ([f {:keys [label before! after!]}]
   (let [old-db @db*
         _ (and before! (before! old-db))
         updates (f old-db)
         {:keys [db on-updated]} (cond
                                   (-> updates map? not) nil
                                   (::db? updates) {:db updates}
                                   (get-in updates [:db ::db?]) updates
                                   :else nil)
         set-db! (fn [db]
                   (trace log 'fire label)
                   (if db
                     (let [old-db- @db*]
                       (when-not (= old-db old-db-)
                         (log/error log 'fire 'overwritten-db-change (trace-diff 'old-db old-db 'old-db- old-db-))
                         #_(throw (js/Error (str "Out of sync db changes"))))
                       (reset! db* db)
                       (when on-updated (on-updated))
                       (and after! (after! old-db db))
                       (trace log 'fire-end label)
                       db)
                     (when updates (throw (js/Error "Attempted db overwrite.")))))]
     (if (p/promise? db)
       (-> db
           (p/then (set-db! db))
           (p/catch (fn [e] (-> e str println)
                 ;(-> e .-name println)
                 ;(-> e .-message println)
                      )))
       (set-db! db))))
  ([f] (firex f nil)))

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