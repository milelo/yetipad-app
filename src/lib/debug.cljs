(ns lib.debug
  (:refer-clojure :exclude [println])
  (:require
    [goog.debug :as gdebug]
    [cljs.pprint :refer [pprint]]
    [lib.log :as log]
    ))

(def expose gdebug/expose)

(def println cljs.core.println)

(def log (log/logger 'lib.debug))

(defn we
  "Watch - enabled"
  ([in] (.debug js/console " #>" (pr-str in) "<# ") in)
  ([id in] (we id nil in))
  ([id f in] (.debug js/console "\n" id ">" ((or f pr-str) in) "<" id "\n") in)
  )

#_(defn watch [id a]
  (add-watch a id (fn [k v old new]
                    (we id new))))
  
(defn wd
  "Watch - disabled"
  ([in] in)
  ([id in] in)
  ([id f in] in))

(defn wee
  [id in]
  (expose (str id ">"))
  (expose in)
  (expose (str "<" id))
  in)

(defn debug-get-in
  "Debug version of get-in"
  {:static true}
  ([m ks]
   (debug-get-in m ks nil))
  ([m ks not-found]
   (loop [sentinel lookup-sentinel
          m m
          ks (seq ks)]
     (if ks
       (if (not (satisfies? ILookup m))
         not-found
         (let [k (first ks)
               m (get m k sentinel)
               ]
           (if (identical? sentinel m)
             not-found
             (do
               (log/debug log "km" [k (boolean m)])
               (recur sentinel m (next ks))))))
       m))))

(defn uid [id a]
  (.debug js/console (str id ">" (goog/getUid a) ":" @a "<" id))
  a
  )

;http://stackoverflow.com/questions/20352348/is-it-possible-to-get-the-file-name-and-line-number-of-the-clojurescript-file-us
(let [d* (atom {})]
  (defn track [id item]
    (swap! d* update-in [id :items] conj item)
    ;(swap! d* update-in [id :stack] conj (js/Error "stack"))
    )
  (defn pr-freq [id]
    (let [items (get-in @d* [id :items])]
      (-> items frequencies pprint)
      (println "count: " (count items))))
  (defn stack-freq [id]
    (let [items (get-in @d* [id :stack])]
      ;(-> items frequencies pprint)
      (println "count: " (count items))))
  (defn track-reset [id]
    (swap! d* dissoc id)
    )

  )

