(ns lib.html-parse
  (:require
    [lib.utils :as u]
    [cljs.reader :as reader]
    [clojure.string :as str]
    [goog.string :as gstr]
    [lib.debug :refer [we wd wee]]
    )
  (:import
    [goog.string.html HtmlParser HtmlSaxHandler])
  (:require-macros
    [lib.utils :refer [to-property goog-extend]]
    [lib.debug :refer [w wl]]
    ))

(defn read-string [s]
  (try
    (reader/read-string s)
    (catch js/Object e (str e "->" s))))

(def no-content-tags #{"br" "hr" "base" "basefont" "img" "col" "frame" "input" "isindex" "link" "meta" "param"})

(defn parse-attrs [s]
  (let [entries (map #(str/split % #":") (str/split s #";"))]
    (into {}
          (for [[k v] entries]
            [(keyword (str/trim k)) (str/trim v)]))))

;see http://www.50ply.com/blog/2012/07/08/extending-closure-from-clojurescript/
;http://stackoverflow.com/questions/9018326/how-do-i-create-an-js-object-with-methods-and-constructor-in-clojurescript
;(goog-extend (deftype HtmlHandler [output])  goog/string.html.HtmlSaxHandler
(goog-extend ^js HtmlHandler HtmlSaxHandler
             #_([args]
                (this-as this (set! (.-args this) args))
                ;(goog.base (js-this));call the base constructor (js* "this")
                (goog.base js/this)                         ;call the base constructor (js* "this")

                )
             ([])
             (init [args] (this-as ^js this (set! (.-args this) args)))
             (startDoc []
                       ;            (goog/base (js* "this") "startDoc")
                       ;           (we :startDoc))
                       (this-as ^js this ((.-args this) "("))
                       )
             (cdata [text]
                    (when text (this-as ^js this ((.-args this) (str "\"" (-> text gstr/stripNewlines gstr/trim) "\""))))
                    )
             (endDoc []
                     ;          (we :endDoc :no-args)
                     (this-as ^js this ((.-args this) ")"))
                     )
             (endTag [name]
                     ;          (we :endTag name)
                     (this-as ^js this ((.-args this) (when-not (no-content-tags name) "]")))
                     )
             (pcdata [text]
                     ;          (we :pcdata text)
                     (let [
                           text (gstr/stripNewlines (gstr/unescapeEntities text))
                           ;                text (.replace (/(\r\n|\r|\n)+/g, ""))
                           ]
                       (this-as ^js this ((.-args this) (pr-str text)))
                       ))
             (rcdata [text]
                     (we :rcdata text)
                     )
             (startTag [name attributes]
                       (let [attrs (when-not (empty? attributes)
                                     (pr-str (into {}
                                                   (for [[k v] (partition 2 attributes)]
                                                     (let [k (keyword k)
                                                           v (if (and (= k :style) (string? v))
                                                               (parse-attrs v)
                                                               v)
                                                           ]
                                                       [k v]
                                                       )))))
                             end (when (no-content-tags name) "]")
                             ]
                         (this-as ^js this ((.-args this) "[:" name attrs end))
                         )))

(defn html->clj
  "Converts HTML to data structure compatible with crate."
  [html]
  (let [
        out (atom nil)
        ;handler (HtmlHandler. (fn [& c] (swap! out concat c)))
        handler (HtmlHandler.)
        ;handler (html-handler (fn [& c] (swap! out concat c)))
        parser (HtmlParser.)
        ]
    (.init handler (fn [& c] (swap! out concat c)))
    (.parse parser handler html)
    (let [clj-str (apply str @out)]
      (read-string clj-str)
      )))

#_(defn pretty [s & [{:keys [number?]}]]
    (str/join
      (get (reduce (fn [[level a] c]
                     (case c
                       "[" (let [l (inc level)] [l (conj a "\n" (su/indent l number?) c)]) ;("[" "(")
                       "]" [(dec level) (conj a c)]         ;("]" ")")
                       [level (conj a c)]
                       )) [0 []] s) 1)))

#_(defn dump [html]
    (-> html html->clj str pretty println)
    html)
