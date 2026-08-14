(ns app.ui.views
  ;See https://github.com/reagent-project/reagent/blob/master/doc/InteropWithReact.md
  (:require
   [cljs.reader :refer [read-string]]
   [clojure.string :as str]
   [app.config :as config]
   [reagent.core :as r]
   ;[breaking-point.core :as bp]
   [lib.log :as log :refer-macros [trace debug info warn fatal]]
   [lib.debug :as debug :refer [we wd wee expose]]
   [lib.utils :as utils :refer [iso-time->date-time format-date time-formatter] :refer-macros [for-all]]
   [lib.debug :as debug]
   [cljs.pprint :refer [pprint]]
   [app.subs :as subs]
   [app.events :as events]
   [app.ui.registry :as reg]
   [app.ui.tagmenu :refer [tag-menu]]
   [app.ui.theme :as theme :refer [theme]]
   [app.ui.utils :as ui-utils :refer [create-mui-theme color
                                      show-empty-title no-title]]
   [app.ui.ui :as ui]
   ["@mui/material" :refer [AppBar Drawer Divider Toolbar Paper Typography Avatar Icon
                            Button IconButton Badge Tooltip
                            List ListItem ListItemIcon ListItemText ListItemSecondaryAction
                            Grid GridList GridListTile GridListTileBar
                            Card CardMedia CardContent CardActions
                            BottomNavigation BottomNavigationAction
                            Tabs Tab
                            Dialog DialogTitle DialogContent DialogContentText DialogActions
                            TextField InputBase
                            CssBaseline MuiThemeProvider
                            MobileStepper]]
   ["@mui/icons-material/MenuRounded" :default menu-icon]
   ["@mui/icons-material/AccountTreeOutlined" :default tree-menu-icon]
   ["@mui/icons-material/KeyboardBackspace" :default keyboard-backspace]
   ["@mui/icons-material/ChevronLeft" :default chevron-left-icon]
   ["@mui/icons-material/ChevronRight" :default chevron-right-icon]

   ["@mui/icons-material/AccountCircle" :default account-icon]
   ["@mui/icons-material/SearchOutlined" :default search-icon]
   ["@mui/icons-material/ClearOutlined" :default clear-search-icon]
    ;-------------toolbar status--------------------
   ["@mui/icons-material/CloudDoneOutlined" :default synced-icon]
   ["@mui/icons-material/CloudOffOutlined" :default signed-out-icon]
   ["@mui/icons-material/CloudOutlined" :default signed-in-icon]
   ["@mui/icons-material/CloudUploadOutlined" :default uploading-icon]
   ["@mui/icons-material/CloudDownloadOutlined" :default downloading-icon]
   ["@mui/icons-material/SyncAltOutlined" :default syncing-icon]
   ["@mui/icons-material/ErrorOutlineOutlined" :default online-error-icon]
    ;---------------main-menu--------------------------
   ["@mui/icons-material/CloseOutlined" :default close-all-icon]
   ["@mui/icons-material/NoteAddOutlined" :default new-note-icon]
   ["@mui/icons-material/FolderOpenOutlined" :default open-file-icon]
   ["@mui/icons-material/DeleteOutlineOutlined" :default delete-document-icon]
   ["@mui/icons-material/ArchiveOutlined" :default move-items-icon]
    ;-------------
   )
  (:import
   ["@fontsource/roboto/300.css"]))

(def log (log/logger 'app.ui.views))

(defn on-before-unload [e]
  (when-not @subs/can-reload?*
    ;modern browsers ignore message
    (let [m "Allow reload? Changes will be lost."]
      (set! (.-returnValue e) m)
      m)))

(defonce _runonce (js/window.addEventListener js/goog.events.EventType.BEFOREUNLOAD on-before-unload))

(def file-reader (js/FileReader.))

(defn got-file [^js e]
  (when-let [file (-> e .-target.files first)]
    (set! (.-onload file-reader) (fn [^js e]
                                   (let [doc (-> e .-target.result read-string)]
                                     (events/open-doc-file!! doc))))
    (.readAsText file-reader file)))

(def mui-theme
  (create-mui-theme
   {#_:root       #_{:display        :flex
                     :justifyContent :center
                     :flexWrap       :wrap}
    :palette {;:type :light
               ;:type :dark
              :primary    {:main (color :blue 500)}
              :secondary  {:main (color :pink :A400)}
              :text-color (color :common :white)}
    :chip    {:margin 10}
     ;:typography {:use-next-variants true}
    }))

(def dark-theme
  (create-mui-theme
   {:palette {:type :dark}                                 ; select dark theme
    }))

;(def theme dark-theme)

;(pprint (-> theme (js->clj :keywordize-keys true) :palette))

(defn index-list-item [icon title on-click]
  (let [font-size 12]
    [:> ListItem {:button   true
                  :style    {:padding "0 16px"}
                  ;:align-items :flex-start
                  :on-click on-click}
     (when icon [:> ListItemIcon {:style {:min-width 0}}
                 [:> icon {:style {:font-size font-size}}]])
     [:> ListItemText {:primary                  (or title no-title)
                       :slot-props {:primary {:style (if title
                                                        {:font-size font-size}
                                                        {:font-size  font-size
                                                         :font-style :italic})}}
                       :style                    {:min-height 0
                                                  :margin     "0 4px"}}]]))

(defonce search-value* (r/atom ""))
(defonce selected-doc-id* (r/atom nil))
(defonce delete-confirmation* (r/atom nil))
(defonce upload-input* (r/atom nil))
;(add-watch search-value* :search-watch (fn [_ v] (println v)))

(defn history-index-pane []
  (let [items-by-history @(subs/items-by-history-filtered @search-value*)
        date-item (fn [text]
                     [:> ListItem {:style {:padding "0 4px"}}
                     [:> ListItemText {:primary                  text
                                       :slot-props {:primary {:style {:font-size 13
                                                                      ;:font-weight :bold
                                                                      }}}
                                       :style                    {:min-height 0
                                                                  :margin     0}}]])]
    [:> List (theme ::theme/index-list)
     (for-all [{:keys [id title kind create change head__]} items-by-history]
              ^{:key id} [:<>
                          (when head__ [date-item (ui-utils/iso-time->formatted-date (or change create))])
                          [index-list-item (reg/rget kind :icon) title #(events/open-item! id)]])]))

(defn title-index-pane []
  (let [items-filtered @(subs/items-by-title-filtered @search-value*)]
    [:> List (theme ::theme/index-list)
     (for-all [{:keys [id title kind]} items-filtered]
              ^{:key id} [index-list-item (reg/rget kind :icon) title #(events/open-item! id)])]))

(defn doc-list-pane []
  (let [docs @subs/doc-list*
        selected-doc-id (or @selected-doc-id* @subs/doc-id*)]
    [:> List (theme ::theme/index-list)
     (for-all [{:keys [doc-id title subtitle file-id file-name file-description status]} docs]
              ^{:key doc-id} [:> ListItem {:style    {:padding "0 4px"}
                                           :button   true
                                           :selected (= selected-doc-id doc-id)
                                           :on-click (fn []
                                                       (reset! selected-doc-id* doc-id)
                                                       (if @subs/moving-items?*
                                                  ;source and target must be different
                                                         (when (not= selected-doc-id doc-id)
                                                           (events/move-items!! doc-id))
                                                         (do
                                                           (events/read-doc-by-id!! doc-id)
                                                           (events/select-index-view! :index-history))))}
                              [:> Tooltip {:title (or subtitle file-description doc-id "")}
                               [:> ListItemText {:primary (str (or title file-name doc-id) " (" (name status) \))}]]])]))

(defn doc-index-tool [icon title action & [{:keys [selected]}]]
  [:> IconButton {:title    title
                  :on-click action
                  :color    (if selected :secondary :inherit)}
   [:> icon (theme ::theme/small-icon)]])

(defn doc-index-toolbar []
  (let [docs @subs/doc-list*
        selected-doc-id (or @selected-doc-id* @subs/doc-id*)
        selected-doc (some #(when (= selected-doc-id (:doc-id %)) %) docs)]
    [:<>
     [:div {:style {:display        :flex
                    :flex-direction :row}}
      [doc-index-tool new-note-icon "New document" events/new-document!]
      [doc-index-tool delete-document-icon "Delete document"
       #(when selected-doc (reset! delete-confirmation* selected-doc))]
      [:input {:accept ".edn,.odn" :style {:display :none} :multiple false :type "file"
               :ref #(reset! upload-input* %) :on-change got-file}]
      [doc-index-tool open-file-icon "Open file"
       #(when-let [input @upload-input*] (.click input))]
      [doc-index-tool move-items-icon "start move open items..." events/toggle-start-move-items!
       {:selected @subs/moving-items?*}]]
     (when-let [{:keys [doc-id title file-name]} @delete-confirmation*]
       [:> Dialog {:open true :on-close #(reset! delete-confirmation* nil)
                   :aria-labelledby "delete-document-title"
                   :aria-describedby "delete-document-description"}
        [:> DialogTitle {:id "delete-document-title"} "Delete document?"]
        [:> DialogContent
         [:> DialogContentText {:id "delete-document-description"}
          (str "Delete “" (or title file-name doc-id) "”? The associated file will be moved to trash.")]]
        [:> DialogActions
         [:> Button {:on-click #(reset! delete-confirmation* nil)} "Cancel"]
         [:> Button {:color :error :auto-focus true
                     :on-click (fn []
                                 (reset! delete-confirmation* nil)
                                 (reset! selected-doc-id* nil)
                                 (events/delete-doc!! {:doc-id doc-id}))}
          "Delete"]]])]))

(defn doc-index-pane []
  [:<>
   [doc-index-toolbar]
   [doc-list-pane]])

(defn search-input []
  [:<>
   [:> InputBase {:placeholder "search..."
                  :value       @search-value*
                  :on-change   (fn [e]
                                 (reset! search-value*
                                         (str/lower-case (-> e .-target .-value))))}]
   [:> IconButton {:title    "clear-search"
                   :on-click #(reset! search-value* "")}
    [:> clear-search-icon (theme ::theme/small-icon)]]])

(defn menu-list-item
  ([icon text on-click]
   (menu-list-item icon text on-click false))
  ([icon text on-click disabled?]
   [:> ListItem {:button   true
                 :disabled disabled?
                 :on-click (when-not disabled?
                             (fn []
                               (events/open-tag-drawer! false)
                               (on-click)))}
   (when icon [:> ListItemIcon [:> icon]])
   [:> ListItemText {:primary text}]]))

(defn index-pane []
  (let [index-view @subs/index-view*
        tab (fn [id label]
              [:> Tab {:label    label
                       :on-click #(events/select-index-view! id)
                       :style    {:text-transform :none
                                  :min-width      50
                                  :min-height     0         ;override and let line-height determine
                                  }}])]
    [:div {:style {:display        :flex
                   :flex-direction :column
                   :padding        "0 8px"}}
     [:> List
      [menu-list-item close-all-icon "Close all" events/close-all-items!]
      [menu-list-item new-note-icon "New note" events/start-edit-new-note!]]
     [:> Paper {:style {:width "100%"}}
      [:> Tabs {:value    (case index-view :index-history 0 :index-title 1 :index-docs 2)
                :variant  :fullWidth
                :centered true
                :style    {:min-height 0}}
       [tab :index-history "History"]
       [tab :index-title "All"]
       [tab :index-docs "Documents"]]]
     (when (#{:index-history :index-title} index-view)
       [:> Toolbar {:style {:min-height 0}}
        [:> search-icon (theme ::theme/small-icon)]
        [search-input]])
     (case index-view
       :index-history [history-index-pane]
       :index-title [title-index-pane]
       :index-docs [doc-index-pane])]))

(defonce item-refs* (r/atom {}))

(defn- open-item-ids [items]
  (mapv :id items))

(defn- item-to-scroll [old-ids new-ids]
  (cond
    (> (count new-ids) (count old-ids))
    (some (fn [id]
            (when-not (some #(= id %) old-ids)
              id))
          (reverse new-ids))

    (and (= (count old-ids) (count new-ids))
         (not= (last old-ids) (last new-ids)))
    (last new-ids)))

(defn- scroll-item-into-view! [item-id]
  (when item-id
    (js/requestAnimationFrame
     (fn []
       (when-let [item (get @item-refs* item-id)]
         (.scrollIntoView item #js {:behavior "smooth"
                                     :block "nearest"}))))))

(defn items-pane []
  (let [previous-item-ids* (r/atom nil)]
    (r/create-class
     {:display-name "items-pane"
      :component-did-mount
      (fn [_this]
        (reset! previous-item-ids* (open-item-ids @subs/open-items-with-trash*)))
      :component-did-update
      (fn [_this _old-argv]
        (let [new-items @subs/open-items-with-trash*
              new-ids (open-item-ids new-items)
              target-id (item-to-scroll (or @previous-item-ids* []) new-ids)]
          (reset! previous-item-ids* new-ids)
          (scroll-item-into-view! target-id)))
      :reagent-render
      (fn []
        [:<>
         [:> Paper (theme ::theme/items)
          (for [{:keys [id kind] :as item} @subs/open-items-with-trash*]
            ^{:key id}
            [:div {:ref (fn [node]
                          (if node
                            (swap! item-refs* assoc id node)
                            (swap! item-refs* dissoc id)))}
             [(reg/rget kind :pane) {:item item}]])]
         [ui/trash-confirmation-dialog]])})))

(defn static-pane-list-item [kind]
  (let [{:keys [title icon]} (reg/rget kind)]
    [menu-list-item icon title #(events/open-item! kind #{:disable-toggle})]))

(defn tag-drawer []
  ;left side drawer
  (let [sync-status @subs/sync-status*
        signed-in? @subs/signed-in?*
        show-close-main-menu false]
    [:> Drawer
     {:open     @subs/main-menu-open*
      :on-close #(events/open-tag-drawer! false)}
     (when show-close-main-menu
       [:div {:style {:display        :flex
                      :alignItems     :center
                      :justifyContent :flex-end
                      :padding        "0 8px"}}
        [:> chevron-left-icon {:on-click #(events/open-tag-drawer! false)}]])
     (when show-close-main-menu
       [:> Divider])
     [:div {:style {:flex-grow 2
                    :width     240}}]
     [tag-menu]
     [:div {:style {:flex-grow 3}}]
     [:> List
      [:> Divider]
      [static-pane-list-item :trash]
      [static-pane-list-item :options]
      [menu-list-item account-icon "Sign-out"
       #(when (js/confirm "Are you sure you want to sign out?")
          (events/sign-out!))
       (not signed-in?)]
      [static-pane-list-item :about]
      [static-pane-list-item :log]
      ;[menu-list-item refresh-icon "Reload" #(js/window.location.reload true)]
      [:> ListItem [:> ListItemText {:primary (str "Version: " @subs/app-version*)}]]
      (when config/debug?
        [:<>
         [:> Divider]
         [menu-list-item nil "check-doc" events/check-doc]
         [menu-list-item nil "fix-doc" events/fix-doc]
         [menu-list-item nil "restore-all-trashed" events/restore-all-trashed!]
         [menu-list-item nil "Dump document meta" events/dump-doc-meta]
         [menu-list-item nil "Dump document" events/dump-doc]
         [menu-list-item nil "List app drive files meta" events/debug-list-app-drive-files]
         [menu-list-item nil "Dump this file meta" events/dump-file-meta]
         [menu-list-item nil "Update doc index pane" events/sync-doc-index!!]
         [menu-list-item nil "Dump index" events/dump-index]
         [menu-list-item nil "Delete doc - keep file" #(events/delete-doc!! {:keep-file true})]
         [menu-list-item nil "Dump tag-map" #(pprint @subs/tag-map*)]
         [menu-list-item nil "rename-file" events/debug-rename-file!!]
         [menu-list-item nil "find-file" events/debug-find-file]
         [menu-list-item nil "Trash file" events/debug-trash-file]
         [menu-list-item nil "FIle content" events/debug-file-content]
         [menu-list-item nil "Add property" events/debug-add-properties]
         [:> Divider]
         ;[:> ListItem [:> ListItemText {:primary (str "can reload?: " (rsubs [::subs/can-reload?]))}]]
         ;[:> ListItem [:> ListItemText {:primary (str "screen-width: " (rsubs [::bp/screen-width]))}]]
         ;[:> ListItem [:> ListItemText {:primary (str "screen: " (rsubs [::bp/screen]))}]]
         ])]]))

(defn index-drawer []
  (let [show-close-index-menu false]
    [:> Drawer
     {:open     @subs/index-menu-open*
      :anchor   :right
      :on-close #(events/open-index-drawer! false)}
     [:div {:style {:position :relative
                    :width    240
                    :anchor   :right}}
      (when show-close-index-menu
        [:div {:style {:display    :flex
                       :alignItems :center
                       ;:justifyContent :flex-end
                       :padding    "0 8px"
                       ;; ...theme.mixins.toolbar,
                       }}
         [:> chevron-right-icon {:on-click #(events/open-index-drawer! false)}]])
      (when show-close-index-menu
        [:> Divider])
      [index-pane]]]))

(defn set-tab-title []
  (let [doc-title @subs/doc-title*]
    (set! (.-title js/document) doc-title)
    nil))

(defn local-file-dialog-pane []
  [:> Dialog {:open (boolean @subs/local-file-dialog*)
                ;:on-close #()
              }
   [:> DialogTitle "Open file dialog"]
   [:> List
    [:> ListItem
     [:> ListItemText {:primary "List item"}]]]])

(defn app-root []
  (let [page-item false
        app-status @subs/app-status*
        sync-status @subs/sync-status*]
    ;(debug log (->  base-theme js->clj :root))
    [ui/error-boundary ::app-route
     [:link {:rel "stylesheet" :href "/goog.css"}]
     [:base {:target "_blank"}]
     [:div ;:> MuiThemeProvider {:theme mui-theme}
      ;{:theme (aget js/MaterialUIStyles "DarkRawTheme")}
      [:> CssBaseline]
      [:style ".ql-toolbar, .ck-toolbar {position: sticky; top: var(--pane-toolbar-height, 48px); background-color: white; z-index: 2;} "]
      [set-tab-title]
      [:> AppBar
         {:position :sticky
          :color    :inherit
          :style    {:flex-direction :row}}
         (let [menu-btn-style {}]
           [:> Toolbar {:style {:flex 1}}
            (if page-item
              [:> IconButton {:title    "back"
                              :color    :inherit
                              :style    menu-btn-style
                              :on-click #(js/window.history.back)} [:> keyboard-backspace]]
              [:> IconButton {:title    "Open drawer"
                              :color    :inherit
                              :style    menu-btn-style
                              :on-click #(events/open-tag-drawer! true)} [:> tree-menu-icon]])
            [:> Tooltip {:title (or @subs/doc-subtitle* "")}
             [:> Typography {:variant :h6
                             :color   :inherit
                             :style   {:flex        1
                                       :line-height :normal
                                     ;;:font-weight :bold
                                       }}@subs/doc-title*]]
            [:> Tooltip
             {:title (case sync-status
                       :offline "Offline — Drive will reconnect when the network returns"
                       :connecting "Connecting to Drive…"
                       :authorization-required "Click to connect to Drive"
                       :syncing "Synchronizing with Drive…"
                       :uploading "Uploading to Drive…"
                       :downloading "Downloading from Drive…"
                       :synced "Synchronized with Drive"
                       :online "Connected to Drive"
                       :error "Drive connection failed — click to retry"
                       false "Offline — click to reconnect"
                       "Drive status")}
             [:> IconButton {:aria-label "Drive sync status"
                             :color      :inherit
                             :style      {:flex 0}
                             :on-click   events/sync-status-clicked!}
              (case sync-status
                :offline [:> signed-out-icon]
                :connecting [:> syncing-icon]
                :authorization-required [:> signed-out-icon]
                :syncing [:> syncing-icon]
                :uploading [:> uploading-icon]
                :downloading [:> downloading-icon]
                :synced [:> synced-icon]
                :online [:> signed-in-icon]
                :error [:> online-error-icon]
                false [:> signed-out-icon]    ;offline
                (warn log ::sync-status-unknown sync-status))]]
          ;variant ["h1","h2","h3","h4","h5","h6","subtitle1","subtitle2","body1","body2","caption","button","overline","srOnly","inherit"]
          ;valid colours: ["initial","inherit","primary","secondary","textPrimary","textSecondary","error"]
            [:> Typography {:variant :subtitle2
                            :color   (case (:type app-status)
                                       :info :primary
                                       :warn :error
                                       :error :error
                                       :error)
                            :style   {:flex        1
                                      :line-height :normal
                                    ;;:font-weight :bold
                                      }}(:text app-status)]
            [:> IconButton {:title    "index menu"
                            :color    :inherit
                            :style    menu-btn-style
                            :on-click #(events/open-index-drawer! true)} [:> menu-icon]]])]

      [ui/error-boundary ::tag-drawer [tag-drawer]]
      [ui/error-boundary ::index-drawer [index-drawer]]
      [ui/error-boundary ::dialog [local-file-dialog-pane]]
      [ui/error-boundary ::items-pane [items-pane]]]]))

(defn main-panel []
  [app-root])
