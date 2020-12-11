(ns app.ui.trash-pane
  (:require
    [reagent.core :as r]
    [re-frame.core :as re-frame]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [app.subs :as subs]
    [cljs.pprint :refer [pprint]]
    [app.events :as events]
    [app.ui.ui :as ui]
    [app.ui.utils :as ui-utils :refer [no-title]]
    [app.ui.registry :as reg]
    ["@material-ui/core" :refer [Tooltip Typography
                                 TextField List ListItem ListItemText ListItemIcon
                                 ]]
    ["@material-ui/icons/DeleteOutline" :default trash-icon]
    ))

(def log (log/logger 'app.ui.trash-pane))

(def rsubs (comp deref re-frame/subscribe))
(def dispatch! re-frame/dispatch)

(defn index-list-item [icon title on-click]
  (let [font-size 14]
    [:> ListItem {:button   true
                  :style    {:padding "0 16px"}
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

(defn trash-list []
  (let [trashed (rsubs [::subs/deleted-items])]
    [:<>
     [:> Typography {:variant :body2} (if trashed "Select to view:" "Empty")]
     [:> List
      (for-all [{:keys [id kind title]} trashed]
        (let [icon (reg/rget kind :icon)]
          ^{:key id} [index-list-item icon title #(dispatch! [::events/open-item id])]))]]
    ))



(defn trash-pane [_context]
  (let [item {:id    :trash
              :kind  :trash
              :title "Trash"
              }
        ]
    [ui/viewer-pane item {:body    [trash-list]
                          :buttons [ui/empty-trash-button ui/fullscreen-button]
                          }]
    ))

(reg/register {:kind  :trash
               :title "Trash"
               :icon  trash-icon
               :pane  trash-pane
               })
