(ns app.ui.registry
  (:require
    ["@material-ui/icons/HelpTwoTone" :default no-kind-icon]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [cljs.pprint :refer [pprint]]
    ))

(def log (log/logger 'app.ui.registry))

(def registry* (atom {}))

(def defaults {:icon             no-kind-icon
               :index-sort-order 100
               })

(defn register [{:keys [kind id] :as data}]
  (assert (or id kind))
  (swap! registry* assoc (or id kind) data))

(defn rget
  ([kind]
   (merge defaults (get @registry* kind)))
  ([kind k & [default]]
   (get-in @registry* [kind k] (or default (get defaults k)))))

(defn singleton-ids []
  (keep :id (vals @registry*)))
