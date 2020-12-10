(ns app.localstore
  (:require
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.local-db :as ldb :refer [<put-data <get-data]]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go go-loop]]
    [lib.asyncutils :refer-macros [<? go?]]
    [lib.utils :as utils]
    [cljs.pprint :refer [pprint]]
    ))

(def log (log/logger 'app.localstore))

(defn doc-meta [doc]
  {:id       (get doc :doc-id)
   :title    (get-in doc [:options :doc-title])
   :subtitle (get-in doc [:options :doc-subtitle])
   :change   (get doc :change)
   })

(def index-key :yetipad)
(def index-format :object)

(defn <read-index []
  (<get-data index-key {:format index-format :default {}}))

#_(defn <read-doc [{doc-key :id doc-format :format :as doc-meta}]
  (<get-data doc-key {:format (or doc-format :object)}))

(defn <read-doc [doc-id]
  (go
    (let [{doc-format :format :as doc-meta} (get (<? (<read-index)) doc-id)]
      (<! (<get-data doc-id {:format (or doc-format :object) :default {}}))
      )))

(defn <write-doc [doc]
  (go?
    (let [{doc-key :id doc-format :format :as doc-meta} (doc-meta doc)
          index (<! (<read-index))
          index (dissoc (assoc index doc-key doc-meta) nil)
          ]
      (<? (<put-data index-key index {:format index-format}))
      (<? (<put-data doc-key (dissoc doc nil) {:format (or doc-format :object)}))
      ;(println "index:")
      ;(pprint index)
      index
      )))


