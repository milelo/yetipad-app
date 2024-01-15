(ns app.ui.note-editor-quill
  (:require
   [lib.log :as log :refer-macros [trace debug info warn error fatal]]
   [react :refer [createElement useEffect useMemo useState]]
   [reagent.core :as r :refer [as-element reactify-component current-component
                               props children create-class adapt-react-class]]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [clojure.pprint :refer [pprint]]
   [reagent.dom :as rdom :refer [dom-node]]
   [reagent.dom.server :refer [render-to-string]]
   ["react-quill" :as ReactQuill :refer [Quill]]
   [lib.html-parse :as hp]
   [app.events :as evts]
   [app.ui.ui :as ui]
   ["@mui/icons-material/Undo" :default undo-icon]
   ["@mui/icons-material/Redo" :default redo-icon]
   ;["quill/assets/icons/undo.svg" :as undo-icon]
  ; ["quill-mobile-view" :as mobile-view]
   [goog.debug :as gdebug]))

(def log (log/logger 'app.ui.note-editor-quill))

(def expose gdebug/expose)

(defn modules [^Object this id]
  (->js
   {:toolbar
    {:handlers {:undo #(let [c this]
                         (prn :this (-> this type)))
                :redo #(.quill.history.redo ^Object (current-component))}
     :container
     [[{:font []} {:size [] #_[:small false :large :huge]} {:header [] #_[1 2 false]}]
                         ;[{:direction :rtl}] 
      [{:color []} {:background []}]
      [:bold :italic :underline :strike :blockquote :code-block]
      [{:align []}]
      [{:list :ordered} {:list :bullet} {:indent "-1"} {:indent "+1"}]
      [{:script :sub}, {:script :super}]
      [:link :image :video :formula]
      [:undo :redo]
      [:clean]]}
    :history {:delay 2000 :maxStack 500 :userOnly true}}))

(let [icons (.import Quill "ui/icons")]
  (set! (.-undo icons) "<strong>↶</strong>")
  ;(set! (.-undo icons) (reactify-component (fn [] [:icon undo-icon])))
  (set! (.-redo icons) "<strong>↷</strong>"))

#_(defn not-empty-content [content]
    (when (some #(if (vector? %)
                   (let [[kind] %] (when (not= kind :br) %))
                   %) content)
      content))

(defn not-empty-content [content]
  (when-not (= '([:p [:br]]) content)
    content))

(defn content-editor [{:keys [id content]}]
  ;https://cljdoc.org/d/reagent/reagent/1.2.0/doc/tutorials/creating-reagent-components
  (let [value* (atom (render-to-string content))]
    [ui/error-boundary ::quill-editor 
     [:div.content-editor {}
      [(with-meta (fn [] ^{:key (str :editor id)} [:> ReactQuill {:theme :snow
                                                                  :id id
                                                                  :value @value*
                                                                  :modules (modules (current-component) id)
                                                         ;:formats formats ;enable / disable:
                                                                  :on-change (fn [v]
                                                                               (reset! value* v))}])
         {:component-did-mount
          (fn [this]
            (trace log :this (-> this))
            (trace log :props (-> this r/props))
            (trace log :children (-> this r/children))
            (trace log :argv (-> this r/argv first expose))
            ;(trace log :dom-node (-> this r/dom-node))
            )
          :component-will-unmount
          (fn [_this]
            (evts/new-content! id (-> @value* hp/html->clj not-empty-content)))})]]]))

