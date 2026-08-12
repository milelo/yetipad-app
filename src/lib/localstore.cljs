(ns lib.localstore
  (:refer-clojure :exclude [keys key])
  (:require
   ["localforage" :as local-forage]
   [lib.log :as log :refer-macros [trace stack debug info warn error fatal] :refer [pprintl trace-diff]]
   [cljs.reader :refer [read-string]]
   [promesa.core :as p]))

(def log (log/logger 'lib.localstore))

;https://localforage.github.io/localForage/

(defn $get-item
  "Gets an item from the storage library and supplies the result to a callback. 
   If the key does not exist, getItem() will return null"
  [k]
  (assert (or (string? k) (keyword? k)))
  (.getItem local-forage (name k)))

(defn $remove-item
  "Removes the value of a key from the offline store"
  [k]
  (assert (or (string? k) (keyword? k)))
  (.removeItem local-forage (name k)))

(defn $set-item
  "Saves data to an offline store. You can store the following types of JavaScript objects:
   Array, ArrayBuffer, Blob,
   Float32Array, Float64Array, Int8Array, Int16Array, Int32Array, 
   Number, Object, Uint8Array, Uint8ClampedArray, Uint16Array, Uint32Array, String"
  [k v]
  (trace log :k k)
  (assert (or (string? k) (keyword? k)))
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

(defn put-data-sync!
  "Synchronously persist small lifecycle-critical data in Web Storage.
   Returns true when the write succeeds and false when Web Storage is
   unavailable (for example in a restricted browser context)."
  [k cljs]
  (assert (or (string? k) (keyword? k)))
  (try
    (if-let [storage (.-localStorage js/globalThis)]
      (do
        (.setItem storage (name k) (pr-str cljs))
        true)
      false)
    (catch :default e
      (warn log 'put-data-sync!-unavailable e)
      false)))

(defn get-data-sync
  "Synchronously read Clojure data from Web Storage, or return nil when the
   key or Web Storage is unavailable. Intended only for small startup data."
  [k]
  (assert (or (string? k) (keyword? k)))
  (try
    (when-let [storage (.-localStorage js/globalThis)]
      (some-> (.getItem storage (name k)) read-string))
    (catch :default e
      (warn log 'get-data-sync-unavailable e)
      nil)))

(defn $get-data
  ([k default]
   (p/let [v ($get-item k)]
     (or (read-string v) default)))
  ;default to false to support async channels
  ([k] ($get-data k nil)))
