(ns app.ui.registry
  (:require
    ["@material-ui/icons/HelpTwoTone" :default no-kind-icon]
    ))

(def registry* (atom {}))

(def defaults {:icon             no-kind-icon
               :index-sort-order 100
               })

(defn register [{:keys [kind] :as data}]
  (assert kind)
  (swap! registry* assoc kind data))

(defn rget
  ([kind]
   (merge defaults (get @registry* kind)))
  ([kind k & [default]]
   (get-in @registry* [kind k] (or default (get defaults k)))))
