(ns app.ui.tag-pane
  (:require
    [reagent.core :as r]
    [lib.log :as log :refer-macros [trace debug info warn error fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [app.subs :as subs]
    [cljs.pprint :refer [pprint]]
    [app.events :as events]
    [app.ui.ui :as ui :refer [error-boundary]]
    [app.ui.utils :as ui-utils :refer [no-title]]
    [app.ui.registry :as reg]
    [app.ui.tagchips :refer [tag-editor tag-viewer]]
    [app.ui.note-editor :refer [content-editor]]
    [app.ui.note-pane :refer [note-editor]]
    ["@mui/material" :refer [TextField List ListItem ListItemText ListItemIcon]]
    ["@mui/icons-material/LabelTwoTone" :default tag-icon]
    ["@mui/icons-material/OpenInNewTwoTone" :default open-all-icon]
    ))

(def log (log/logger 'app.ui.tag-pane))

(defn index-list-item [icon title on-click]
  (let [font-size 12]
    [:> ListItem {:button   true
                  :style    {:padding "0 16px"}
                  ;:align-items :flex-start
                  :on-click (fn []
                              (on-click)
                              )
                  }
     (when icon [:> ListItemIcon {:style {:min-width 0}}
                 [:> icon {:style {:font-size font-size}}]
                 ])
     [:> ListItemText {:primary                  (or title no-title)
                       :primary-typography-props {:style (if title
                                                           {:font-size font-size}
                                                           {:font-size  font-size
                                                            :font-style :italic
                                                            })
                                                  }
                       :style                    {:min-height 0
                                                  :margin     "0 4px"
                                                  }
                       }]]))

(defn tag-list [parent-id]
        (let [[tags other] @(subs/child-data-by-tag-id parent-id)]
    [:> List (for-all [{:keys [id kind title]} (concat other tags)]
               ^{:key id} [index-list-item (reg/rget kind :icon) title #(events/open-item! id)])]))

(defn content-pane [{:keys [id content]}]
  ;break-word is required to avoid horizontal scroll bar on long words ie url's
  [:<>
   [:div {:style {:word-wrap :break-word}} content]
   [:div {:style {:margin "1em 1em"}} [tag-list id]]]
  )

(defn tag-view [{:keys [id tags] :as item} & [options]]
  [ui/viewer-pane item (merge {:body   ^{:key :cont} [content-pane item]
                               :footer ^{:key :tags} [tag-viewer id]
                               } options)])

(defn open-all-button [id]
  [ui/item-button open-all-icon "open all children" #(events/open-tag-children! id)])

(defn tag-pane [{:keys [item]}]
  (if-let [edit-item @(subs/edit-item (:id item))]
    [note-editor edit-item]
    [tag-view item {:buttons (cons open-all-button ui/standard-viewer-buttons)}]
    ))

(reg/register {:kind             :tag
               :icon             tag-icon
               :pane             tag-pane
               :index-sort-order 30
               })