(ns app.ui.note-editor-quill
  (:require
   [lib.log :as log :refer-macros [trace debug info warn error fatal] :refer [pprintl]]
   [reagent.core :as r]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [reagent.dom.server :as rdoms]
   ["react-quill" :as ReactQuill :refer [Quill]]
   [lib.html-parse :as hp]
   [app.events :as evts]
   [app.ui.ui :as ui]
   ["@mui/icons-material/Undo" :default undo-icon]
   ["@mui/icons-material/Redo" :default redo-icon]
   ;["quill/assets/icons/undo.svg" :as undo-icon]
  ; ["quill-mobile-view" :as mobile-view]
   [goog.debug :as gdebug]))

;Note this doesn't support true nested lists it just indents with {:class "ql-indent-1"}
;maybe this will be fixed in quill release 2.0
;https://github.com/quilljs/quill/issues/979

(def log (log/logger 'app.ui.note-editor-quill))

(def expose gdebug/expose)

(defn modules [quill* _id]
  (->js
   {:toolbar
    {:handlers {:undo #(-> @quill* .getEditor .-history .undo)
                :redo #(-> @quill* .getEditor .-history .redo)}
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
  ;(set! (.-undo icons) "<strong>↶</strong>")
  ;(set! (.-redo icons) "<strong>↷</strong>")
  (set! (.-undo icons) (-> [:> undo-icon] rdoms/render-to-string))
  (set! (.-redo icons) (-> [:> redo-icon] rdoms/render-to-string))
  )

(defn not-empty-content [content]
  (when-not (= '([:p [:br]]) content)
    content))

(defn quill-editor [id content]
  ;https://cljdoc.org/d/reagent/reagent/1.2.0/doc/tutorials/creating-reagent-components
  (let [value* (atom (rdoms/render-to-string content))
        quill* (atom nil)]
    (r/create-class
     {:display-name "quill-editor" 
      
      :component-will-unmount
      (fn [_this]
        (evts/new-content! id (-> @value* hp/html->clj not-empty-content)))
      
      :reagent-render
      (fn [id content]
        ^{:key (str :editor id)} [:> ReactQuill {:theme :snow
                                                 :id id
                                                 :value @value*
                                                 :ref #(reset! quill* %)
                                                 :modules (modules quill* id)
                                                  ;:formats formats ;enable / disable:
                                                 :on-change (fn [v]
                                                              (reset! value* v))}])})))

(defn content-editor [{:keys [id content]}] 
  [ui/error-boundary ::quill-editor
   [:div.content-editor {}
    [quill-editor id content]]])

