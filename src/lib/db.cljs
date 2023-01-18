(ns lib.db
  (:refer-clojure :exclude [atom])
  (:require
   [lib.log :as log :refer [trace debug info warn fatal pprintl trace-diff]]
   [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go-loop go]]
   [lib.asyncutils :as au :refer [put-last!] :refer-macros [<? go? go-try goc go-let]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [promesa.core :as p]
   [reagent.core :as r]
   [clojure.core :as core]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [clojure.pprint :refer [pprint]]))

(def log (log/logger 'lib.db))

(defonce db* (r/atom {::db? true}))

;====================================task queue=================================================

(defonce <task-queue (chan 20))                             ;go-block queue

(defn defer
  "?f can return a value or promise
   p will be resolved after the return value of ?f resolved.
   ?f will be called after f is called also returning p"
  [?f]
  (let [p (p/deferred)
        f (fn []
            (-> (p/do (?f @db*));use 'do' to convert all return types and exceptions to a promise
                (p/then (partial p/resolve! p))
                (p/catch (partial p/reject! p)))
            p)]
    [p f]))

(def task-runner-ctrl* (core/atom {}))

(defn start-task-runner []
  ;execute queued go-block functions
  (trace log 'task-runner-started)
  (go (loop []
        (try (<p! ((<! <task-queue)))
             ;ignore exception; value is also returned in promise
             (catch :default _e))
        (when-not (:stop @task-runner-ctrl*) 
          (recur)))
      (warn log 'task-runner-stopped)))

(comment
  (swap! task-runner-ctrl* assoc :stop true)
  (swap! task-runner-ctrl* dissoc :stop)
  (start-task-runner)
  )

(defonce _ (start-task-runner))

(defn  do-sync
  ([?f]
   (let [[p f] (defer ?f)]
     (put! <task-queue f)
     p))
  ([?f {:keys [on-success on-error]}]
   (-> (do-sync ?f)
       (p/then on-success)
       (p/catch on-error))))

(comment 
  (let [[p f] (defer (fn [_db] :x))]
    (-> p (p/then (partial prn :then)))
    (js/setTimeout #(-> (f) (p/then (partial prn :timeout))) 5000))

  (let [d (p/deferred)]
    (prn :start)
    (p/then (do-sync (fn [_db] d)) prn) 
    (p/catch (do-sync (fn [_db] (throw :error))) prn)
    (p/then (do-sync (fn [_db] :b)) prn)
    (p/then (do-sync (fn [db] (keys db))) prn)
    (js/setTimeout #(p/resolve! d :a) 5000)))


;====================================task queue=================================================


(defn fire [fsync f])

(defn firex
  "Update the db, optionally with a promise."
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