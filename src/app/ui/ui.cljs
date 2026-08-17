(ns app.ui.ui
  (:require
   [clojure.string :as str]
   [cljs.pprint :refer [pprint]]
   [goog.object :as gobject]
   [reagent.core :as r]
   [app.events :as events]
   [app.subs :as subs]
   [lib.utils :as utils :refer-macros [for-all]]
   [app.ui.theme :as theme :refer [theme]]
   [app.config :as config]
   [lib.debug :as debug :refer [we wd wee expose]]
   [app.ui.utils :as ui-utils :refer [show-empty-title no-title]]
   ["@mui/material" :refer [Icon IconButton Tooltip Typography Paper Dialog
                                DialogTitle DialogContent DialogContentText DialogActions
                                Button Toolbar AppBar]]
    ;-------------Item control icons----------
   ["@mui/icons-material/EditTwoTone" :default edit-icon]
   ["@mui/icons-material/Edit" :default slate-edit-icon]
    ;["@mui/icons-material/PageviewTwoTone" :default accept-edit-icon]
   ["@mui/icons-material/CheckTwoTone" :default accept-edit-icon]
    ;["@mui/icons-material/CancelTwoTone" :default cancel-edit-icon]
   ["@mui/icons-material/CloseTwoTone" :default cancel-edit-icon]
   ["@mui/icons-material/DeleteTwoTone" :default delete-icon]
   ["@mui/icons-material/RemoveFromQueueTwoTone" :default close-icon]
   ["@mui/icons-material/CenterFocusStrongTwoTone" :default close-other-icon]
   ["@mui/icons-material/BugReportTwoTone" :default inspect-content-icon]
   ["@mui/icons-material/FullscreenTwoTone" :default fullscreen-icon]
   ["@mui/icons-material/FullscreenExitTwoTone" :default fullscreen-exit-icon]
   ["@mui/icons-material/RestoreFromTrash" :default restore-icon]
   ["@mui/icons-material/DeleteForever" :default delete-permanent-icon]
   [app.ui.registry :as reg]))

(defonce item-state* (r/atom {}))
(defonce trash-confirmation* (r/atom nil))
(defonce trash-confirmation-in-flight?* (r/atom false))
(defonce cancel-edit-confirmation* (r/atom nil))

(defn request-permanent-delete! [item-id]
  (when (and (string? item-id) (not @trash-confirmation-in-flight?*))
    (reset! trash-confirmation* {:action :delete-item-permanent
                                 :item-id item-id})))

(defn request-empty-trash! []
  (let [item-count (count @subs/deleted-items*)]
    (when (and (pos? item-count) (not @trash-confirmation-in-flight?*))
      (reset! trash-confirmation* {:action :empty-trash
                                   :item-count item-count}))))

(defn- close-trash-confirmation! []
  (when-not @trash-confirmation-in-flight?*
    (reset! trash-confirmation* nil)))

(defn- confirm-trash-action! []
  (when-let [{:keys [action item-id]} @trash-confirmation*]
    (when-not @trash-confirmation-in-flight?*
      (reset! trash-confirmation-in-flight?* true)
      (let [operation (case action
                        :delete-item-permanent (events/delete-item-permanent!! item-id)
                        :empty-trash (events/empty-trash!!))]
        (-> operation
            (.then (fn [_]
                     (reset! trash-confirmation-in-flight?* false)
                     (reset! trash-confirmation* nil)))
            (.catch (fn [_]
                      (reset! trash-confirmation-in-flight?* false))))))))

(defn trash-confirmation-dialog []
  (let [{:keys [action item-id item-count]} @trash-confirmation*
        item-title (when item-id (:title @(subs/doc-item item-id)))
        empty-trash? (= action :empty-trash)]
    (when action
      [:> Dialog {:open true
                  :on-close close-trash-confirmation!
                  :aria-labelledby "trash-confirmation-title"
                  :aria-describedby "trash-confirmation-description"}
       [:> DialogTitle {:id "trash-confirmation-title"}
        (if empty-trash? "Empty trash?" "Permanently delete item?")]
       [:> DialogContent
        [:> DialogContentText {:id "trash-confirmation-description"}
         (if empty-trash?
           (str "Permanently delete " item-count " trashed "
                (if (= item-count 1) "item" "items") "? This cannot be undone.")
           (str "Permanently delete “" (or item-title item-id) "”? This cannot be undone."))]]
       [:> DialogActions
        [:> Button {:on-click close-trash-confirmation!
                   :disabled @trash-confirmation-in-flight?*} "Cancel"]
        [:> Button {:color :error
                    :auto-focus true
                    :disabled @trash-confirmation-in-flight?*
                    :on-click confirm-trash-action!}
         (if empty-trash? "Empty trash" "Delete permanently")]]])))

(defn- close-cancel-edit-confirmation! []
  (reset! cancel-edit-confirmation* nil))

(defn- confirm-cancel-edit! []
  (when-let [item-id @cancel-edit-confirmation*]
    (close-cancel-edit-confirmation!)
    (events/cancel-edit! item-id)))

(defn cancel-edit-confirmation-dialog []
  (when @cancel-edit-confirmation*
    [:> Dialog {:open true
                :on-close close-cancel-edit-confirmation!
                :aria-labelledby "cancel-edit-title"
                :aria-describedby "cancel-edit-description"}
     [:> DialogTitle {:id "cancel-edit-title"} "Discard changes?"]
     [:> DialogContent
      [:> DialogContentText {:id "cancel-edit-description"}
       "Your changes will be lost."]]
     [:> DialogActions
      [:> Button {:on-click close-cancel-edit-confirmation!} "Keep editing"]
      [:> Button {:color :error
                  :auto-focus true
                  :on-click confirm-cancel-edit!}
       "Discard"]]]))

(defn- set-pane-toolbar-height! [pane]
  (when pane
    (when-let [toolbar (.querySelector pane ".pane-toolbar")]
      (let [height (.-height (.getBoundingClientRect toolbar))]
        (.setProperty (.-style pane) "--pane-toolbar-height" (str height "px"))))))

(defn- pane-ref [pane]
  (when pane
    (set-pane-toolbar-height! pane)
    (when (.-ResizeObserver js/window)
      (let [observer (js/ResizeObserver.
                      (fn [_entries]
                        (set-pane-toolbar-height! pane)))]
        (.observe observer (.querySelector pane ".pane-toolbar"))
        (gobject/set pane "paneToolbarResizeObserver" observer)))))

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
                                 [:Card {:variant :outlined :style {:display :flex :flex-direction :column}}
                                  (for [l (str/split-lines info)]
                                    [:span {:style {:color :red}} l])
                                  [:pre [:code (with-out-str (pprint children))]]])))})))

(defn item-button [icon text on-click]
  [:> Tooltip {:title text}
   [:> IconButton {:on-click on-click} [:> icon]]])

(defn edit-button [item-id]
  [item-button edit-icon "edit" #(events/start-edit! item-id)])

(defn fullscreen-button [item-id]
  [item-button fullscreen-icon "full-screen" #(swap! item-state* update item-id assoc :open true)])

(defn inspect-button [item-id]
  (when config/debug?
    [item-button inspect-content-icon "inspect content" #(events/dump-item-content item-id)]))

(defn close-button [item-id]
  [item-button close-icon "close" #(events/close-item! item-id)])

(defn close-other-button [item-id]
  [item-button close-other-icon "close other items" #(events/close-other-items! item-id)])

(defn trash-item-button [item-id]
  (when (string? item-id) [item-button delete-icon "delete" #(events/trash-item! item-id)]))

(defn restore-button [item-id]
  [item-button restore-icon "restore from trash" #(events/restore-item! item-id)])

(defn delete-permanent-button [item-id]
  [item-button delete-permanent-icon "delete permanent" #(request-permanent-delete! item-id)])

(defn empty-trash-button []
  [item-button delete-permanent-icon "empty trash" request-empty-trash!])

(defn fullscreen-exit-button [item-id]
  [item-button fullscreen-exit-icon "exit full-screen" #(swap! item-state* update item-id assoc :open false)])

(defn title-bar [{:keys [id title kind]} & [{:keys [toolbar?]}]]
  [:div {:style (merge {:display         :flex
                        :align-items     :center
                        :text-decoration :underline}
                       (when toolbar? {:flex-grow 1
                                       :padding   "0 10px"}))}
   [:> (reg/rget kind :icon) (theme ::theme/small-icon)]
   [:> Tooltip {:title id}
    [:> Typography {:variant :subtitle1
                    :style   {:margin-left 5}}
     (show-empty-title title)]]])

(defn- viewer-fullscreen [{:keys [id] :as item} body footer]
  [:> Dialog {:full-screen        true
              :open               (boolean (get-in @item-state* [id :open]))
              :on-escape-key-down (fn [e]
                                    (.stopPropagation e)
                                    (swap! item-state* update id assoc :open false)
                                    nil)}
   [:div {:style {:display        :flex
                  :flex-direction :column
                  :overflow       :hidden
                  :margin         "2px 10px"}}
    [:> AppBar {:color    :inherit
                :position :sticky}
     [:div {:style {:display     :flex
                    :align-items :center
                    :padding     "0 20px"}}
      [title-bar item]
      [:div {:style {:display         :flex
                     :flex-grow       1
                     :justify-content :flex-end}}
       [fullscreen-exit-button id]]]]
    [:div {:style {:overflow-y :scroll}}
     [:> Paper {:style {:padding 10}}

      [error-boundary ::viewer-content body]]]
    [:div {:style {:padding    10
                   :border-top "solid 1px LightGrey"}}
     [error-boundary ::viewer-footer footer]]]])

(defn pane-tags [content]
  [:div (theme ::theme/pane-tags) content])

(defn- viewer-inline [{:keys [id] :as item} body footer buttons]
  [:div (theme ::theme/pane)
   [:div (theme ::theme/pane-buttons)
    [title-bar item {:toolbar? true}]
    (for-all [button buttons]
             ^{:key button} [button id])]
   [:div
     [:> Paper {:style {:padding "0 10px 10px 10px"}}
     [error-boundary ::viewer-content body]
     (when footer [pane-tags footer])]]])

(def standard-viewer-buttons [edit-button fullscreen-button])

(defn viewer-pane
  ([{:keys [trashed] :as item} {:keys [body footer buttons]}]
   [:<>
    [viewer-inline item body footer (concat (if trashed
                                              [delete-permanent-button restore-button]
                                              (or buttons standard-viewer-buttons)) [inspect-button close-button close-other-button])]
    [viewer-fullscreen item body footer]]))

(defn accept-edit-button [id]
  [item-button accept-edit-icon "accept edit" #(events/accept-edit! id)])

(defn cancel-edit-button [id]
  [item-button cancel-edit-icon "cancel edit"
   #(reset! cancel-edit-confirmation* id)])

(defn editor-pane [{:keys [id] :as item} {:keys [body buttons]}]
  [:div (assoc (theme ::theme/pane) :ref pane-ref)
   [:div (theme ::theme/pane-buttons)
    [title-bar item {:toolbar? true}]
    (for-all [button (conj (vec (concat [accept-edit-button cancel-edit-button] buttons))
                           inspect-button trash-item-button close-other-button)]
             ^{:key button} [button id])]
   [:> Paper {:style {:padding "0 10px 10px 10px"}}
    [error-boundary ::editor-content body]]])

