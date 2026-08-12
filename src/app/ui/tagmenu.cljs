(ns app.ui.tagmenu
  (:require
    [reagent.core :as r]
    [lib.log :as log :refer-macros [trace debug info warn error fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [app.subs :as subs]
    [app.events :as events]
    [cljs.pprint :refer [pprint]]
    ["@mui/material" :refer [Menu MenuList MenuItem Popover Typography]]
    ["@mui/icons-material/ArrowForwardIos" :default more-icon]
    ))

(def log (log/logger 'app.ui.tagmenu))

(defonce selected-elements* (r/atom {}))
(defonce context-menu* (r/atom nil))
(defonce long-press-timer* (atom nil))

(defn close-context-menu! []
  (reset! context-menu* nil))

(defn open-tag! [id]
  (reset! selected-elements* {})
  (close-context-menu!)
  (events/open-tag-drawer! false)
  (events/open-item! id {:disable-toggle true}))

(defn open-context-menu! [id e]
  (.preventDefault e)
  (reset! context-menu* {:id       id
                         :position {:top    (.-clientY e)
                                    :left   (.-clientX e)}}))

(defn cancel-long-press! []
  (when-let [timer @long-press-timer*]
    (js/clearTimeout timer)
    (reset! long-press-timer* nil)))

(defn start-long-press! [id e]
  (cancel-long-press!)
  (let [touch (aget (.-touches e) 0)]
    (reset! long-press-timer*
            (js/setTimeout
              (fn []
                (reset! long-press-timer* nil)
                (reset! context-menu* {:id       id
                                       :position {:top  (.-clientY touch)
                                                  :left (.-clientX touch)}}))
              500))))

(defn tag-submenu [tid tag-items other-items]
  [:> MenuList
   (for-all [{:keys [id title kind]} (concat tag-items other-items)]
            ^{:key (str tid id)} [:> MenuItem {
                                               :style    {:padding    "0 4px"
                                                          :min-height 30
                                                          }
                                               :on-click (fn [e]
                                                           (.stopPropagation e)
                                                           (if (= kind :tag)
                                                             (let [element (-> e.currentTarget.childNodes first)]
                                                               (swap! selected-elements* update id #(if % false element)))
                                                             (open-tag! id)))
                                               :on-double-click (when (= kind :tag)
                                                                  #(open-tag! id))
                                               :on-context-menu (when (= kind :tag)
                                                                  #(open-context-menu! id %))
                                               :on-touch-start (when (= kind :tag)
                                                                 #(start-long-press! id %))
                                               :on-touch-end (when (= kind :tag)
                                                               (fn [_]
                                                                 (cancel-long-press!)))
                                               :on-touch-move (when (= kind :tag)
                                                               (fn [_]
                                                                 (cancel-long-press!)))
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
                                      (let [[tag-items other-items] @(subs/child-data-by-tag-id id)]
                                        [tag-submenu id tag-items other-items])])
                                    ]])
   (when-let [{:keys [id position]} @context-menu*]
     [:> Menu {:open             true
               :on-close         close-context-menu!
               :anchor-reference :anchorPosition
               :anchor-position  position}
       [:> MenuItem {:on-click #(open-tag! id)} "Open tag"]])
    ])

(defn tag-menu []
  (let [root-tag-items @subs/root-tag-data*]
    (if root-tag-items
      [tag-submenu nil root-tag-items nil]
      [:> Typography {:align   :center
                      :style   {:padding "2em 1em"
                                :color   :lightgray
                                }}
       "To create a tag menu here," [:br]
       "add tags to items or other tags."]
      )))
