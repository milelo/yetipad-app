(ns app.ui.note-pane
  (:require
    [reagent.core :as r]
    [lib.log :as log :refer-macros [trace debug info warn error fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [app.subs :as subs]
    [cljs.pprint :refer [pprint]]
    [app.events :as events]
    [app.ui.ui :as ui :refer [error-boundary]]
    [app.ui.registry :as reg]
    [app.ui.tagchips :refer [tag-editor tag-viewer]]
    [app.ui.note-editor-goog :as note-editor-goog]
    [app.ui.note-editor-quill :as note-editor-quill]
    [app.ui.note-editor-ck :as note-editor-ck]
    ["@mui/material" :refer [TextField]]
    ["@mui/icons-material/NotesTwoTone" :default note-icon]
    ))

(def log (log/logger 'app.ui.note-pane))

(defn content-pane [{:keys [content]}]
  ;break-word is required to avoid horizontal scroll bar on long words ie url's
  [:div {:style {:word-wrap :break-word}} content]
  )

(defn note-viewer [{:keys [id] :as item}]
  [ui/viewer-pane item
   {:body   ^{:key :cont} [content-pane item]
    :footer ^{:key :tags} [tag-viewer id]
    }])

(defn title-editor [{:keys [id title]}]
  (let [new-title* (atom title)]
    ;Doesn't re-render when title is edited so inner render function not required.
    [(with-meta (fn [] [:> TextField {:variant       :outlined
                                      ;:input-props   {:style {:padding 5}}
                                      :size          :small
                                      :margin        :dense
                                      :label         :title
                                      ;:InputLabelProps {}
                                      :default-value title
                                      :on-change     (fn [e]
                                                       (reset! new-title* (.-target.value ^js e)))
                                      }])
                {:component-will-unmount
                 (fn [_this]
                   ;(debug log :new-title @new-title*)
                   (events/new-title! id @new-title*)
                   )})]))

(defn note-editor [{{:keys [id] :as item} :source} & [options]]
  [ui/editor-pane item (merge {:body [:<>
                                      ^{:key :title-e} [title-editor item]
                                      ^{:key :cont-e} [error-boundary ::note-editor
                                                       [(case @subs/content-editor*
                                                          :goog-editor note-editor-goog/content-editor
                                                          :quill-editor note-editor-quill/content-editor
                                                          :ck-editor note-editor-ck/content-editor
                                                          ) item]]
                                      ^{:key :tags-e} [:div {:style {:margin-top 5}} [tag-editor id]]
                                      ]
                               } options)])

(defn note-pane [{:keys [item]}]
  (if-let [edit-item @(subs/edit-item (:id item))]
    [note-editor edit-item]
    [note-viewer item]
    ))

(reg/register {:kind             :note
               :icon             note-icon
               :pane             note-pane
               :index-sort-order 10
               })
