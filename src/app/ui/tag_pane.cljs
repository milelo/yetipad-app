(ns app.ui.tag-pane
  (:require
    [reagent.core :as r]
    [re-frame.core :as re-frame]
    [lib.log :as log :refer [trace debug info warn fatal]]
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
    ["@material-ui/core" :refer [TextField List ListItem ListItemText ListItemIcon]]
    ["@material-ui/icons/LabelTwoTone" :default tag-icon]
    ))

(def log (log/logger 'app.ui.tag-pane))

(def rsubs (comp deref re-frame/subscribe))
(def dispatch! re-frame/dispatch)

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
  (let [[tags other] (rsubs [::subs/child-data-by-tag-id parent-id])]
    [:> List (for-all [{:keys [id kind title]} (concat other tags)]
               ^{:key id} [index-list-item (reg/rget kind :icon) title #(dispatch! [::events/open-item id])])]))

(defn content-pane [{:keys [id content]}]
  ;break-word is required to avoid horizontal scroll bar on long words ie url's
  [:<>
   [:div {:style {:word-wrap :break-word}} content]
   [:div {:style {:margin "1em 1em"}} [tag-list id]]]
  )

(defn tag-view [{:keys [id tags] :as item}]
  [ui/viewer-pane item {:body   ^{:key :cont} [content-pane item]
                        :footer ^{:key :tags} [tag-viewer id]
                        }])

(defn tag-pane [{:keys [item]}]
  (if-let [edit-item (rsubs [::subs/edit-item (:id item)])]
    [note-editor edit-item]
    [tag-view item]
    ))

(reg/register {:kind             :tag
               :icon             tag-icon
               :pane             tag-pane
               :index-sort-order 30
               })