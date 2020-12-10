(ns app.ui.tagmenu
  (:require
    [reagent.core :as r]
    [breaking-point.core :as bp]
    [re-frame.core :as re-frame]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [app.subs :as subs]
    [app.events :as events]
    [cljs.pprint :refer [pprint]]
    ["@material-ui/core" :refer [MenuList MenuItem Popover Typography]]
    ["@material-ui/icons/ArrowForwardIos" :default more-icon]
    ))

(def log (log/logger 'app.ui.tagmenu))

(def rsubs (comp deref re-frame/subscribe))
(def dispatch! re-frame/dispatch)

(defonce selected-elements* (r/atom {}))

(defn tag-submenu [tid tag-items other-items]
  (let [screen (rsubs [::bp/screen])]
    [:> MenuList
     (for-all [{:keys [id title kind]} (concat tag-items other-items)]
       ^{:key (str tid id)} [:> MenuItem {
                                          :style    {:padding    "0 4px"
                                                     :min-height (case screen :mobile 30 :auto)
                                                     }
                                          :on-click (fn [e]
                                                      (.stopPropagation e)
                                                      (if (= kind :tag)
                                                        (let [element (-> e.currentTarget.childNodes first)]
                                                          (swap! selected-elements* update id #(if % false element)))
                                                        (do
                                                          (reset! selected-elements* {})
                                                          (dispatch! [::events/open-tag-drawer false])
                                                          (dispatch! [::events/open-item id {:disable-toggle true}])
                                                          )))
                                          }
                             [:<>
                              [:div {:style {:display        :flex
                                             :alignItems     :center
                                             :flex-direction :horizontal
                                             }}
                               [:> Typography title] (when (= kind :tag)
                                                       [:> more-icon {:style {:font-size   14
                                                                              :margin-left 4
                                                                              }}])]
                              (when-let [selected-el (get @selected-elements* id)]
                                [:> Popover {:open          true
                                             :anchor-origin {:horizontal :right :vertical :center}
                                             :anchorEl      selected-el
                                             }
                                 (let [[tag-items other-items] (rsubs [::subs/child-data-by-tag-id id])]
                                   [tag-submenu id tag-items other-items])])
                              ]])]))

(defn tag-menu []
  (let [root-tag-items (rsubs [::subs/root-tag-data])]
    (if root-tag-items
      [tag-submenu nil root-tag-items nil]
      [:> Typography {:align   :center
                      :style   {:padding "2em 1em"
                                :color   :lightgray
                                }}
       "To create a tag menu here," [:br]
       "add tags to items or other tags."]
      )))