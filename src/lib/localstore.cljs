(ns lib.localstore
  (:refer-clojure :exclude [keys key])
  (:require
   ["localforage" :as local-forage]
   [cljs.reader :refer [read-string]]
   [promesa.core :as p]))

;https://localforage.github.io/localForage/

(defn $get-item
  "Gets an item from the storage library and supplies the result to a callback. 
   If the key does not exist, getItem() will return null"
  [k]
  (assert k)
  (.getItem local-forage (name k)))

(defn $remove-item
  "Removes the value of a key from the offline store"
  [k]
  (assert k)
  (.removeItem local-forage (name k)))

(defn $set-item
  "Saves data to an offline store. You can store the following types of JavaScript objects:
   Array, ArrayBuffer, Blob,
   Float32Array, Float64Array, Int8Array, Int16Array, Int32Array, 
   Number, Object, Uint8Array, Uint8ClampedArray, Uint16Array, Uint32Array, String"
  [k v]
  (assert k)
  (.setItem local-forage (name k) v))

(defn $keys
  "Get the list of all keys in the datastore."
  []
  (.keys local-forage))

(defn $clear
  "Removes every key from the database, returning it to a blank slate."
  []
  (.clear local-forage))

(defn $length
  "Gets the number of keys in the offline store."
  []
  (.length local-forage))

(defn $key
  "keyname at index"
  [idx]
  (.key local-forage idx))

(defn $put-data [k cljs]
  ($set-item k (pr-str cljs)))

(defn $get-data
  ([k default]
   (p/let [v ($get-item k)]
     (or (read-string v) default)))
  ;default to false to support async channels
  ([k] ($get-data k false)))




