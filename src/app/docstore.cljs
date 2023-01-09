(ns app.docstore
  (:require
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.localstore :as ls :refer [put-data get-data]]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go go-loop]]
    [cljs.core.async.interop :refer-macros [<p!]]
    [lib.asyncutils :refer-macros [<? go?]]
    [lib.utils :as utils]
    [cljs.pprint :refer [pprint]]
    ))

(def log (log/logger 'app.localstore))

(defn doc-meta [doc]
  {:id       (get doc :doc-id)
   :title    (get-in doc [:options :doc-title])
   :subtitle (get-in doc [:options :doc-subtitle])
   :change   (get doc :change)})

(def index-key :yetipad)

(defn <read-index []
  (go? (<p! (get-data index-key)))
  )

(defn <read-doc [doc-id]
  (go? (<p! (get-data doc-id {}))))

(defn <write-doc [doc]
  (go?
   (let [{doc-key :id doc-format :format :as doc-meta} (doc-meta doc)
         index (<! (<read-index))
         index (dissoc (assoc index doc-key doc-meta) nil)]
     (<p! (put-data index-key index))
     (<p! (put-data doc-key (dissoc doc nil)))
      ;(println "index:")
      ;(pprint index)
     index)))

