(ns lib.db
  (:refer-clojure :exclude [atom])
  (:require
   [lib.log :as log :refer-macros [stack trace debug info warn error fatal] :refer [pprintl trace-diff]]
   [lib.operation-queue :as operation-queue]
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

;====================================operation queue============================================

(defonce operation-tail* (operation-queue/create))

(defn $enqueue!
  "Runs f after every previously enqueued operation has settled. f receives the
  latest db value when it starts and may return a value or a Promise. A failure
  rejects only the returned operation Promise; the shared queue remains usable."
  [label f]
  (assert (fn? f))
  (-> (operation-queue/enqueue!
       operation-tail*
       (fn []
         (trace log 'operation-started label)
         (p/let [value (f @db*)]
           (trace log 'operation-complete label)
           value)))
      (p/catch (fn [e]
                 (error log '$enqueue! label e)
                 (p/rejected e)))))

(defn $queue-idle []
  (operation-queue/idle operation-tail*))

;====================================operation queue============================================

(defn update-db!
  ([label-or-props f]
   (assert (fn? f))
   (let [{:keys [label]} (if (map? label-or-props)
                           label-or-props
                           {:label label-or-props})
         _  (if label
              (trace log 'update-db! label)
              (stack log 'update-db!))
         old-db @db*
         new-db (swap! db* (fn [db]
                             (let [new-db (f db)]
                               (cond
                                 (nil? new-db) db
                                 (and (map? new-db) (::db? new-db)) new-db
                                 :else (do
                                         (log/error log 'update-db! "Attempted db overwrite.")
                                         db)))))]
     ;(debug log label 'db-change (trace-diff :old old-db :new new-db))
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
