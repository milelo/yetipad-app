(ns app.ui.ui
  (:require
    [clojure.string :as str]
    [cljs.pprint :refer [pprint]]
    [reagent.core :as r]
    [app.events :as events]
    [lib.utils :as utils :refer-macros [for-all]]
    [app.ui.theme :as theme :refer [theme]]
    [app.config :as config]
    [lib.debug :as debug :refer [we wd wee expose]]
    [app.ui.utils :as ui-utils :refer [show-empty-title no-title]]
    ["@material-ui/core" :refer [Icon IconButton Tooltip Typography Paper Dialog
                                 Toolbar AppBar
                                 ]]
    ;-------------Item control icons----------
    ["@material-ui/icons/EditTwoTone" :default edit-icon]
    ;["@material-ui/icons/PageviewTwoTone" :default accept-edit-icon]
    ["@material-ui/icons/CheckTwoTone" :default accept-edit-icon]
    ;["@material-ui/icons/CancelTwoTone" :default cancel-edit-icon]
    ["@material-ui/icons/CloseTwoTone" :default cancel-edit-icon]
    ["@material-ui/icons/DeleteTwoTone" :default delete-icon]
    ["@material-ui/icons/RemoveFromQueueTwoTone" :default close-icon]
    ["@material-ui/icons/CenterFocusStrongTwoTone" :default close-other-icon]
    ["@material-ui/icons/BugReportTwoTone" :default inspect-content-icon]
    ["@material-ui/icons/FullscreenTwoTone" :default fullscreen-icon]
    ["@material-ui/icons/FullscreenExitTwoTone" :default fullscreen-exit-icon]
    ["@material-ui/icons/RestoreFromTrash" :default restore-icon]
    ["@material-ui/icons/DeleteForever" :default delete-permanent-icon]
    [app.ui.registry :as reg]
    ))

(defonce item-state* (r/atom {}))

(defn error-boundary
  [disp-name & children]
  (let [err-state (r/atom nil)]
    (r/create-class
      {:display-name        (name disp-name)
       :component-did-catch (fn [err info]
                              (reset! err-state [err info]))
       :reagent-render      (fn [disp-name & children]
                              (if (nil? @err-state)
                                (into [:<>] children)
                                (let [[_err info] @err-state]
                                  [:div {:style {:display :flex :flex-direction :column}}
                                   [:span {:style {:color :red}
                                           } (first (str/split-lines info))]
                                   [:pre [:code (with-out-str (pprint children))]]
                                   ])))
       })))

(defn item-button [icon text on-click]
  [:> Tooltip {:title text}
   [:> IconButton {:on-click on-click} [:> icon]]
   ])

(defn edit-button [item-id]
  [item-button edit-icon "edit" #(events/start-edit! item-id)])

(defn fullscreen-button [item-id]
  [item-button fullscreen-icon "full-screen" #(swap! item-state* update item-id assoc :open true)])

(defn inspect-button [item-id]
  (when config/debug?
    [item-button inspect-content-icon "inspect content" events/dump-item-content item-id]))

(defn close-button [item-id]
  [item-button close-icon "close" #(events/close-item! item-id)])

(defn close-other-button [item-id]
  [item-button close-other-icon "close other items" #(events/close-other-items! item-id)])

(defn trash-item-button [item-id]
  (when (string? item-id) [item-button delete-icon "delete" #(events/trash-item! item-id)]))

(defn restore-button [item-id]
  [item-button restore-icon "restore from trash" #(events/restore-item! item-id)])

(defn delete-permanent-button [item-id]
  [item-button delete-permanent-icon "delete permanent" #(events/delete-item-permanent!! item-id)])

(defn empty-trash-button []
  [item-button delete-permanent-icon "empty trash" events/empty-trash!!])

(defn fullscreen-exit-button [item-id]
  [item-button fullscreen-exit-icon "exit full-screen" #(swap! item-state* update item-id assoc :open false)])

(defn title-bar [{:keys [id title kind]}]
  [:div {:style {:display         :flex
                 :align-items     :center
                 :text-decoration :underline
                 }}
   [:> (reg/rget kind :icon) (theme ::theme/small-icon)]
   [:> Tooltip {:title id}
    [:> Typography {:variant :subtitle1
                    :style   {:margin-left 5
                              }}
     (show-empty-title title)]]])

(defn- viewer-fullscreen [{:keys [id] :as item} body footer]
  [:> Dialog {:full-screen        true
              :open               (boolean (get-in @item-state* [id :open]))
              :on-escape-key-down (fn [e]
                                    (.stopPropagation e)
                                    (swap! item-state* update id assoc :open false)
                                    nil)
              }
   [:div {:style {:display        :flex
                  :flex-direction :column
                  :overflow       :hidden
                  :margin         "2px 10px"
                  }}
    [:> AppBar {:color    :inherit
                :position :sticky
                }
     [:div {:style {:display     :flex
                    :align-items :center
                    :padding     "0 20px"
                    }}
      [title-bar item]
      [:div {:style {:display         :flex
                     :flex-grow       1
                     :justify-content :flex-end
                     }}
       [fullscreen-exit-button id]]
      ]]
    [:div {:style {:overflow-y :scroll
                   }}
     [:> Paper {:style {:padding 10}}

      [error-boundary ::viewer-content body]
      ]]
    [:div {:style {:padding    10
                   :border-top "solid 1px LightGrey"
                   }}
     [error-boundary ::viewer-footer footer]
     ]]])

(defn- viewer-inline [{:keys [id] :as item} body footer buttons]
  [:div (theme ::theme/pane)
   [:div (theme ::theme/pane-buttons)
    (for-all [button buttons]
      ^{:key button} [button id])
    ]
   [:div
    [:> Paper {:style {:padding "0 10px 10px 10px"}}
     [title-bar item]
     [error-boundary ::viewer-content body]
     (when footer [:div {:style {:margin-top 4}} footer])
     ]]])

(def standard-viewer-buttons [edit-button fullscreen-button])

(defn viewer-pane
  ([{:keys [trashed] :as item} {:keys [body footer buttons]}]
   [:<>
    [viewer-inline item body footer (concat (if trashed
                                              [delete-permanent-button restore-button]
                                              (or buttons standard-viewer-buttons)
                                              ) [inspect-button close-button close-other-button])]
    [viewer-fullscreen item body footer]
    ]))

(defn accept-edit-button [id]
  [item-button accept-edit-icon "end edit" #(events/accept-edit! id)])

(defn cancel-edit-button [id]
  [item-button cancel-edit-icon "cancel edit" #(events/cancel-edit! id)])

(defn editor-pane [{:keys [id] :as item} {:keys [body buttons]}]
  [:div (theme ::theme/pane)
   [:div (theme ::theme/pane-buttons)
    (for-all [button (conj (vec (concat [accept-edit-button cancel-edit-button] buttons))
                           inspect-button trash-item-button close-other-button)]
      ^{:key button} [button id])
    ]
   [:> Paper {:style {:padding "0 10px 10px 10px"}}
    [title-bar item]
    [error-boundary ::editor-content body]
    ]])

