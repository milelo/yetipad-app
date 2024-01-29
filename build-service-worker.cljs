;Invoke with nbb build-service-worker.cljs
(ns build-service-worker
  (:require 
   ["workbox-build" :as workbox]
   [clojure.pprint :refer [pprint]]
   ["glob" :refer [glob]]
   [promesa.core :as p]
   [clojure.set :refer [difference]]
   ))

(def target-dir "docs")
(def cache-patterns ["**/*.{html,js,css,png,gif,ico,jpg,svg,json}"])
(def external [{:url "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap" :revision "1"}
               {:url "https://unpkg.com/react-quill@1.3.3/dist/quill.snow.css" :revision "1.3.3"}])

(defn $glob [& args]
  (apply glob (map clj->js args)))

(defn list-files []
  (p/let [all ($glob ["**/*.*"] {:cwd target-dir})
          all (concat all external)
          cache ($glob cache-patterns {:cwd target-dir 
                                       :ignore ["service-worker.js" "workbox-*.js"] ;handled by default
                                       })
          cache (concat cache external)
          not-cached (difference (-> all set) cache)]
    (println "\n-- all:")
    (doseq [f all]
      (println f))
    (println "\n-- not-cached:")
    (doseq [f not-cached]
      (println f))
    (println "-- All:" (count all) " Caching:" (count cache))))

(defn generate-sw []
  (println)
  (p/let [sw-dest "docs/service-worker.js"
          $r (workbox/generateSW (clj->js
                                  {:swDest sw-dest 
                                   :globDirectory target-dir
                                   :globPatterns cache-patterns
                                   :maximumFileSizeToCacheInBytes (* 50 1024 1024)
                                   :additionalManifestEntries external
                                   :runtimeCaching [{:urlPattern #".*"
                                                     :handler "CacheFirst"
                                                     }]
                                   }))
          {:keys [count size warnings] :as r} (js->clj $r :keywordize-keys true)]
    ;(pprint r)
    (doseq [w warnings]
      (println w))
    (println (str "Generated " sw-dest ", which will pre-cache " count " files, totaling " size " bytes."))))

(p/do 
  (list-files)
  (generate-sw))