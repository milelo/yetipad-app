(ns lib.utils
  (:refer-clojure :exclude [flatten])
  (:require
    [goog.date :as date]
    [goog.string :as gstring]
    )
  (:import
    ;see http://clojurescriptmadeeasy.com/blog/when-do-i-use-require-vs-import.html
    [goog.i18n DateTimeFormat]
    [goog date]
    [goog.date Interval Date DateTime UtcDateTime]
    [goog.text LoremIpsum]                                  ;from org.clojure/google-closure-library-third-party
    ))

(def lorem-ipsum (LoremIpsum.))

(defn sentence [& [text]]
  (or text (.generateSentence lorem-ipsum)))

(defn paragraph [& [text]]
  (or text (.generateParagraph lorem-ipsum)))

(defn error? [v]
  (or (instance? js/Error v) (instance? ExceptionInfo v)))

(defn throw-error [& s]
  (throw (js/Error. (apply str s))))

(defn format
  ;see also cljs.pprint/cl-format
  [fmt & args]
  (apply gstring/format fmt args))

(defn time-formatter [format-str]
  (DateTimeFormat. format-str))

#_(def time-formatter DateTimeFormat.)

(defn is-same-day?
  "true if dt1 & dt2 are same day. Year and month must also match."
  [dt1 dt2]
  (and dt1 dt2 (date/isSameDay dt1 dt2)))

(defn format-date
  "http://userguide.icu-project.org/formatparse/datetime"
  ([date format]
   (if (string? format)
     (.format (time-formatter format) date)
     (.format format date)
     )))

(def time-format-full "dd-MMM-yy HH:mm:ss.SS")
(def time-format-time "HH:mm:ss.SS")

(defn to-date [time]
  (Date. (.getYear time) (.getMonth time) (.getDate time)))

(def time-now-ms js/Date.now)

;;(def time-now DateTime.);;runtime cljs bug ??
(defn time-now []
  (DateTime.))

(defn current-time []
  (.getTime (DateTime.))
  )

(defn ms->date-time [ms]
  (-> ms js/Date. DateTime.))

(defn iso-time->date-time
  "return DateTime or nil if parsing fails"
  [iso-str]
  (DateTime.fromIsoString iso-str))

(defn date-time->iso-time
  "Returns compact UTC iso time. Will be loaded with timezone offset"
  ;https://google.github.io/closure-library/api/goog.date.DateTime.html
  [^js/goog.date.DateTime date-time]
  (.toUTCIsoString date-time false true))

(defn iso-time-now []
  (date-time->iso-time (time-now)))

(defn date-time->ms [^js/goog.date.DateTime date-time]
  (.valueOf date-time))

(defn iso-time->ms [iso-str]
  (if-let [d (DateTime.fromIsoString iso-str)]
    (.valueOf d)
    (throw-error "Require yyyymmddThhmmss not: " iso-str)))

(defn ms->iso-time [ms]
  (-> ms js/Date. DateTime. (.toUTCIsoString false true)))

(defn format-iso-time [iso-str format]
  (-> iso-str DateTime.fromIsoString DateTime. (format-date format)))

(defn day-of-year [^js/goog.date.DateTime date-time]
  (.getDayOfYear date-time))

(defn format-ms
  ([ms] (format-ms ms time-format-full))                    ;changeable: use for debug only
  ([ms format] (when ms (format-date (ms->date-time ms) format))))

(defn js->cljs [js]
  (js->clj js :keywordize-keys true))

(defn only
  "(only vector? 1) -> nil"
  [f x]
  (when (f x) x))

(defn prn-stack-trace
  "Prints a stack-trace to the console"
  []
  (.trace js/console))

;(group [:a :b :c] (range))
;=> ([:a 0] [:b 1] [:c 2])
(defn group [& c]
  (apply map vector c))

(defn run-later [x & [ms]]
  (.setTimeout js/window x (or ms 1)))

(defn parse-36 [str]
  (js/parseInt str 36))

(defn to-str-36 [^number num]
  (.toString num 36))

(def simple-uuid
  (let [allocated (atom 0)]
    (fn []
      ;ensure sequentially allocated values are unique
      (let [id (current-time)
            id (if (>= @allocated id) (inc @allocated) id)
            ]
        (reset! allocated id)
        (to-str-36 id)))))

(defn new-item-num [docs]
  ;tags may contain a deleted item id that can't be reused.
  (let [new-item-num (fn [doc]
                       (inc (reduce-kv (fn [i id {:keys [tags]}]
                                         (if (string? id)
                                           (reduce (fn [i id]
                                                     (max i (parse-36 id))
                                                     ) i (cons id tags))
                                           i)
                                         ) 0 doc)))
        ]
    (reduce (fn [m doc] (max m (new-item-num doc))) 0 docs)))

(defn new-item-ids [& docs]
  (map to-str-36 (iterate inc (new-item-num docs))))

(defn new-item-id [& docs]
  (to-str-36 (new-item-num docs)))

(defn map-remove [pred m]
  "Remove (pred v) = true entries from a map"
  (reduce-kv (fn [m k v]
               (if (pred v) (dissoc m k) m)
               ) m m))

(defn flatten
  "As core flatten but flatten? selects subject"
  ([x] (cljs.core/flatten x))
  ([x flatten?]
   (if (flatten? x)
     (filter (complement flatten?)
             (rest (tree-seq flatten? identity x)))
     x)))

(comment
  *clojurescript-version*
  (let [d (DateTime.)
        utc-iso (.toUTCIsoString d false true)
        iso (.toIsoString d)
        iso->format (fn [iso] (-> iso DateTime.fromIsoString (format-date time-format-time)))
        ]
    (println :utc-iso [utc-iso (iso->format utc-iso)])
    (println :iso [iso (iso->format iso)])
    )
  )