(ns lib.local-db
  (:require
    ["localforage" :as local-forage]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go go-loop]]
    [lib.asyncutils :refer-macros [<? go?]]
    [lib.debug :as debug :refer [wd] :refer-macros [we w wl]]
    [cljs.reader :refer [read-string]]
    [cognitect.transit :as t]
    ))

(def writer (t/writer :json))
(def reader (t/reader :json))

(defn <put-data [k v & [{:keys [format]}]]
  (assert k)
  (let [c (chan)
        data (case format
               :transit (t/write writer v)
               :object (pr-str v)
               v)
        ]
    (.setItem local-forage (name k) data (fn [err]
                                           (put! c (or err v false))
                                           (close! c)))
    c))

(defn <get-data [k & [{:keys [format default]}]]
  (assert k)
  (let [c (chan)]
    (.getItem local-forage (name k) (fn [err v]
                                      (if err
                                        (put! c err)
                                        (let [data (case format
                                                     :transit (t/read reader v)
                                                     :object (read-string v)
                                                     v)
                                              ]
                                          (put! c (or data default false))
                                          ))
                                      (close! c)))
    c))

(defn <remove-item [k]
  (assert k)
  (let [c (chan)]
    (.removeItem local-forage (name k) (fn [err]
                                         (put! c (or err true))
                                         (close! c)))
    c))

#_(defn t1 []
    (go?
      (<? (<put-data :hello :world))
      (println (<? (<get-data :hello)))
      ))