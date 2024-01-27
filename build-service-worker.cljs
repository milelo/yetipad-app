;Invoke with nbb build-service-worker.cljs
(ns build-service-worker
  (:require 
   ["workbox-build" :as workbox]
   [clojure.pprint :refer [pprint]]
   ))

(defn generate-sw []
  (let [sw-dest "docs/service-worker.js"]
    (-> (workbox/generateSW #js {:swDest "docs/service-worker.js"
                                 :globDirectory "docs"
                                 :maximumFileSizeToCacheInBytes (* 50 1024 1024)
                               ;; Other configuration options can be added here
                                 })
        (.then (fn [result]
                 (let [{:keys [count size warnings] :as r} (js->clj result :keywordize-keys true)]
                   ;(pprint r)
                   (println)
                   (doseq [w warnings]
                     (println w))
                   (println (str "Generated " sw-dest ", which will precache " count " files, totaling " size " bytes."))
                   ))))))

(generate-sw)