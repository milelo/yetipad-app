(ns app.ui.utils
  (:require
    [lib.utils :as utils]
    [goog.string :as gstr]
    [clojure.string :as str]
    [clojure.walk :refer [postwalk]]
    [goog.object :as gobject]
    [lib.debug :as debug :refer [we wd wee expose]]
    [clojure.string :as str]
    [cljs.pprint :refer [pprint]]
    ["@material-ui/core/styles" :refer [createTheme]]
    ["@material-ui/core/colors" :as mui-colors]
    ["react" :as react]
    ))

(defn show-empty [v message]
  (if (empty? v) [:span {:style {:font-style :italic}} message] v))

(def no-title "no title")

(defn show-empty-title [title]
  (show-empty title no-title))

(def date-format (utils/time-formatter "d MMM yyyy"))

(def iso-time->formatted-date
  (memoize (fn [iso-str]
             (-> iso-str
                 utils/iso-time->date-time
                 (utils/format-date date-format)
                 ))))

(def time-format-full "d-MMM-yy HH:mm:ss")

(defn iso-time->time-format-full [iso-str]
  (some-> iso-str
          utils/iso-time->date-time
          (utils/format-date time-format-full)
          ))

(defn str-contains [match content]
  ;see also clojure.string/includes?
  (when (gstr/contains (str/lower-case content) match)
    content))

(defn normalize [form]
  (when (and (vector? form) (-> form first keyword?))
    (if (-> form second map?)
      (let [[tag args & content] form]
        [tag (not-empty args) content])
      (let [[tag & content] form]
        [tag nil content]))))

(defn contains [match form]
  (if-let [[_tag _attrs content] (normalize form)]
    (contains match content)
    (cond
      (string? form) (str-contains match form)
      (coll? form) (some (partial contains match) form)
      form (println "unknown form?" form)
      )))

(defn find-content [match {:keys [content] :as pane}]
  (contains match content))

(defn search [match pane]
  (if match
    (let [{:keys [title]} pane]
      (not-empty
        (filter boolean
                [(let [title (show-empty-title title)]
                   (when (contains match title)
                     ["in-title" title]))
                 (when-let [found (when find-content (find-content match pane))]
                   ["in-content" found]
                   )
                 ])))
    :no-search
    ))

(defn camel-case
  "Returns camel case version of the key, e.g. :http-equiv becomes :httpEquiv."
  [k]
  (if (or (keyword? k)
          (string? k)
          (symbol? k))
    (let [[first-word & words] (.split (name k) "-")]
      (if (or (empty? words)
              (= "aria" first-word)
              (= "data" first-word))
        k
        (-> (map str/capitalize words)
            (conj first-word)
            str/join
            keyword)))
    k))

(defn camel-case-keys
  "Recursively transforms all map keys into camel case."
  [m]
  (if (map? m)
    (let [m (into {}
                  (map (fn [[k v]] [(camel-case k) v]))
                  m)]
      (cond-> m
              (map? (:style m))
              (update :style camel-case-keys)))
    m))


(defn transform-keys [t coll]
  "Recursively transforms all map keys in coll with t."
  (letfn [(transform [[k v]] [(t k) v])]
    (postwalk (fn [x] (if (map? x) (into {} (map transform x)) x)) coll)))

(def props-kebab->camel->js (comp clj->js camel-case-keys))

#_(defn create-mui-cmp
    ([react-class args]
     (let [first-arg (first args)
           args (if (or (map? first-arg) (nil? first-arg)) args (cons {} args))]
       (apply react/createElement react-class
              (props-kebab->camel->js (first args)) (rest args))))
    ([root-obj type args]
     (create-mui-cmp (gobject/getValueByKeys root-obj type) args)))

(defn create-mui-theme
  ([] (create-mui-theme nil))
  ([raw-theme] (->> raw-theme
                    (transform-keys camel-case)
                    clj->js
                    createTheme)))

(defn create-mui-transition [theme properties transitionParams]
  (.create (gobject/get theme "transitions") (clj->js properties) (clj->js transitionParams)))

(defn color
  "Takes the color as a symbol for the first argument and then either
  a number or a symbol for the second arg to specify the shade"
  [color-name color-key]
  (let [key-string (if (integer? color-key)
                     (str color-key)
                     (name (camel-case color-key)))]
    (gobject/getValueByKeys mui-colors #_js/MaterialUIColors (name (camel-case color-name)) key-string)))