(ns app.ui.views
  ;See https://github.com/reagent-project/reagent/blob/master/doc/InteropWithReact.md
  (:require
    [clojure.string :as str]
    [app.config :as config]
    [reagent.core :as r :refer [create-class as-element]]
    [reagent.dom :as reagent-dom :refer [dom-node]]
    ;[reagent-catch.core :as rc]
    ;[dommy.core :as dom]
    [goog.dom :refer [appendChild]]
    [re-frame.core :as re-frame]
    [breaking-point.core :as bp]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as utils :refer [iso-time->date-time format-date time-formatter] :refer-macros [for-all]]
    [lib.debug :as debug]
    [cljs.pprint :refer [pprint]]
    [app.subs :as subs]
    [app.events :as events]
    [cljs.reader :refer [read-string]]
    [app.ui.registry :as reg]
    [app.ui.tagmenu :refer [tag-menu]]
    [app.ui.tagchips :refer [tag-editor tag-viewer]]
    [app.ui.theme :as theme :refer [theme]]
    [app.ui.utils :as ui-utils :refer [create-mui-theme color
                                       show-empty-title no-title]]
    [app.ui.ui :as ui]
    ["@material-ui/core" :refer [AppBar Drawer Divider Toolbar Paper Typography Avatar Icon
                                 Button IconButton Badge Tooltip
                                 List ListItem ListItemIcon ListItemText ListItemSecondaryAction
                                 Grid GridList GridListTile GridListTileBar
                                 Card CardMedia CardContent CardActions
                                 BottomNavigation BottomNavigationAction
                                 Tabs Tab
                                 TextField InputBase
                                 CssBaseline MuiThemeProvider
                                 MobileStepper
                                 ]]
    ["@material-ui/icons/MenuRounded" :default menu-icon]
    ["@material-ui/icons/ListRounded" :default list-icon]
    ["@material-ui/icons/AccountTreeOutlined" :default tree-menu-icon]
    ["@material-ui/icons/Search" :default search-icon]
    ["@material-ui/icons/KeyboardBackspace" :default keyboard-backspace]
    ["@material-ui/icons/ChevronLeft" :default chevron-left-icon]
    ["@material-ui/icons/ChevronRight" :default chevron-right-icon]
    ["@material-ui/icons/Settings" :default settings-icon]
    ["@material-ui/icons/Refresh" :default refresh-icon]
    ["@material-ui/icons/Delete" :default trash-icon]
    ["@material-ui/icons/AccountCircle" :default account-icon]
    ["@material-ui/icons/SearchOutlined" :default search-icon]
    ["@material-ui/icons/ClearOutlined" :default clear-search-icon]
    ;-------------toolbar status--------------------
    ["@material-ui/icons/CloudDoneOutlined" :default synced-icon]
    ["@material-ui/icons/CloudOffOutlined" :default signed-out-icon]
    ["@material-ui/icons/CloudOutlined" :default signed-in-icon]
    ["@material-ui/icons/CloudUploadOutlined" :default uploading-icon]
    ["@material-ui/icons/CloudDownloadOutlined" :default downloading-icon]
    ["@material-ui/icons/SyncAltOutlined" :default syncing-icon]
    ;---------------main-menu--------------------------
    ["@material-ui/icons/SettingsOutlined" :default preferences-icon]
    ["@material-ui/icons/CloseOutlined" :default close-all-icon]
    ["@material-ui/icons/AddOutlined" :default new-note-icon]
    ["@material-ui/icons/FolderOpenOutlined" :default open-file-icon]
    ["@material-ui/icons/DeleteOutlineOutlined" :default delete-document-icon]
    ;-------------
    )
  (:import
    [fontsource-roboto]
    ))

(def log (log/logger 'app.ui.views))

(def app-version "v 0.0.20")
(info log app-version)

(def rsubs (comp deref re-frame/subscribe))
(def dispatch! re-frame/dispatch)

(def file-reader (js/FileReader.))

(defn got-file [e]
  (when-let [file (-> e .-target.files first)]
    (set! (.-onload file-reader) (fn [e]
                                   (let [doc (-> e .-target.result read-string)]
                                     (dispatch! [::events/open-doc-file (dissoc doc nil)])
                                     )))
    (.readAsText file-reader file)
    ))

(def mui-theme
  (create-mui-theme
    {#_:root       #_{:display        :flex
                      :justifyContent :center
                      :flexWrap       :wrap
                      }
     :palette {
               ;:type :light
               ;:type :dark
               :primary    {:main (color :blue 500)}
               :secondary  {:main (color :pink :A400)}
               :text-color (color :common :white)
               }
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

(defonce search-value* (r/atom ""))
;(add-watch search-value* :search-watch (fn [_ v] (println v)))

(defn history-index-pane []
  (let [items-by-history (rsubs [::subs/items-by-history-filtered @search-value*])
        date-item (fn [text]
                    [:> ListItem {:style {:padding "0 4px"}}
                     [:> ListItemText {:primary                  text
                                       :primary-typography-props {:style {:font-size 13
                                                                          ;:font-weight :bold
                                                                          }}
                                       :style                    {:min-height 0
                                                                  :margin     0
                                                                  }
                                       }
                      ]])]
    [:> List (theme ::theme/index-list)
     (for-all [{:keys [id title kind create change head__]} items-by-history]
       ^{:key id} [:<>
                   (when head__ [date-item (ui-utils/iso-time->formatted-date (or change create))])
                   [index-list-item (reg/rget kind :icon) title #(dispatch! [::events/open-item id])]
                   ])]))

(defn title-index-pane []
  (let [items-filtered (rsubs [::subs/items-by-title-filtered @search-value*])]
    [:> List (theme ::theme/index-list)
     (for-all [{:keys [id title kind]} items-filtered]
       ^{:key id} [index-list-item (reg/rget kind :icon) title #(dispatch! [::events/open-item id])]
       )]))

(defn doc-list-pane []
  (let [docs (rsubs [::subs/doc-list])
        selected-doc-id (rsubs [::subs/doc-id])
        ]
    [:> List (theme ::theme/index-list)
     (for-all [{:keys [doc-id title subtitle status]} docs]
       ^{:key doc-id} [:> ListItem {:style    {:padding "0 4px"}
                                    :button   true
                                    :selected (= selected-doc-id doc-id)
                                    :on-click (fn []
                                                (dispatch! [::events/read-doc-by-id- doc-id])
                                                (dispatch! [::events/select-index-view :index-history])
                                                )
                                    }
                       [:> ListItemText {:primary (str (or title subtitle doc-id) " (" (name status) \))}]]
       )]))

(defn doc-index-tool [icon title action]
  [:> IconButton {:title    title
                  :on-click action
                  }
   [:> icon (theme ::theme/small-icon)]])

(defn doc-index-toolbar []
  [:div {:style {:display        :flex
                 :flex-direction :row
                 }}
   [doc-index-tool delete-document-icon "Delete current document" #(dispatch! [::events/delete-doc])]
   [:input {:accept    ".edn,.odn"
            :style     {:display :none}
            :multiple  false
            :type      "file"
            :ref       #(set! (.-upload (js-this)) %)
            :on-change got-file
            }]
   [doc-index-tool open-file-icon "Open file" #(-> (js-this) .-upload .click)]
   ])

(defn doc-index-pane []
  [:<>
   [doc-index-toolbar]
   [doc-list-pane]
   ])

(defn search-input []
  [:<>
   [:> InputBase {:placeholder "search..."
                  :value       @search-value*
                  :on-change   (fn [e]
                                 (reset! search-value*
                                         (str/lower-case (-> e .-target .-value))))
                  }]
   [:> IconButton {:title    "clear-search"
                   :on-click #(reset! search-value* "")
                   }
    [:> clear-search-icon (theme ::theme/small-icon)]]
   ])

(defn menu-list-item [icon text on-click]
  [:> ListItem {:button   true
                :on-click (fn []
                            (dispatch! [::events/open-tag-drawer false])
                            (on-click)
                            )
                }
   (when icon [:> ListItemIcon [:> icon]])
   [:> ListItemText {:primary text}]
   ])

(defn index-pane []
  (let [index-view (rsubs [::subs/index-view])
        tab (fn [id label]
              [:> Tab {:label    label
                       :on-click #(dispatch! [::events/select-index-view id])
                       :style    {
                                  :text-transform :none
                                  :min-width      50
                                  :min-height     0         ;override and let line-height determine
                                  }
                       }])
        ]
    [:div {:style {:display        :flex
                   :flex-direction :column
                   :padding        "0 8px"
                   }}
     [:> List
      [menu-list-item close-all-icon "Close all" #(dispatch! [::events/close-all-items])]
      [menu-list-item new-note-icon "New note" #(dispatch! [::events/start-edit-new-note])]
      ]
     [:> Paper {:style {:width "100%"
                        }}
      [:> Tabs {:value    (case index-view :index-history 0 :index-title 1 :index-docs 2)
                :variant  :fullWidth
                :centered true
                :style    {:min-height 0
                           }}
       [tab :index-history "History"]
       [tab :index-title "All"]
       [tab :index-docs "Documents"]
       ]]
     (when (#{:index-history :index-title} index-view)
       [:> Toolbar {:style {:min-height 0}}
        [:> search-icon (theme ::theme/small-icon)]
        [search-input]
        ])
     (case index-view
       :index-history [history-index-pane]
       :index-title [title-index-pane]
       :index-docs [doc-index-pane]
       )
     ]))

(defn items-pane []
  [:> Paper
   (for-all [{:keys [id kind] :as item} (rsubs [::subs/open-items-with-trash])]
     ^{:key id} [(reg/rget kind :pane) {:item item}]
     )])

(defn static-pane-list-item [kind]
  (let [{:keys [title icon]} (reg/rget kind)]
    [menu-list-item icon title #(dispatch! [::events/open-item kind #{:disable-toggle}])]))

(defn tag-drawer []
  ;left side drawer
  (let [main-menu-open (rsubs [::subs/tag-drawer-open])
        online-status (rsubs [::subs/online-status])
        show-close-main-menu false
        ]
    [:> Drawer
     {:open     main-menu-open
      :on-close #(dispatch! [::events/open-tag-drawer false])
      }
     (when show-close-main-menu
       [:div {:style {:display        :flex
                      :alignItems     :center
                      :justifyContent :flex-end
                      :padding        "0 8px"
                      }}
        [:> chevron-left-icon {:on-click #(dispatch! [::events/open-tag-drawer false])}]])
     (when show-close-main-menu
       [:> Divider])
     [:div {:style {:flex-grow 2
                    :width     240
                    }}]
     [tag-menu]
     [:div {:style {:flex-grow 3}}]
     [:> List
      [:> Divider]
      [static-pane-list-item :trash]
      [static-pane-list-item :options]
      [static-pane-list-item :log]
      (if online-status
        [menu-list-item account-icon "Sign-out" #(dispatch! [::events/sign-out])]
        [menu-list-item account-icon "Sign-in" #(dispatch! [::events/sign-in])])
      [static-pane-list-item :about]
      ;[menu-list-item refresh-icon "Reload" #(js/window.location.reload true)]
      [:> ListItem [:> ListItemText {:primary app-version}]]
      (when config/debug?
        [:<>
         [:> Divider]
         [menu-list-item nil "fix-doc" #(dispatch! [::events/fix-doc])]
         [menu-list-item nil "Dump tag-map" #(pprint (rsubs [::subs/tag-map]))]
         [menu-list-item nil "List app drive files" #(dispatch! [::events/debug-list-app-drive-files])]
         [menu-list-item nil "find-file" #(dispatch! [::events/debug-find-file])]
         [menu-list-item nil "Trash file" #(dispatch! [::events/debug-trash-file])]
         [menu-list-item nil "FIle content" #(dispatch! [::events/debug-file-content])]
         [menu-list-item nil "Add property" #(dispatch! [::events/debug-add-properties])]
         [menu-list-item nil "Update doc index pane" #(dispatch! [::events/sync-doc-index])]
         [menu-list-item nil "Dump index" #(dispatch! [::events/dump-index])]
         [menu-list-item nil "Dump document meta" #(dispatch! [::events/dump-doc])]
         [menu-list-item nil "Dump file meta" #(dispatch! [::events/dump-file-meta])]
         [:> Divider]
         [:> ListItem [:> ListItemText {:primary (str "screen-width: " (rsubs [::bp/screen-width]))}]]
         [:> ListItem [:> ListItemText {:primary (str "screen: " (rsubs [::bp/screen]))}]]])
      ]]))

(defn index-drawer []
  (let [index-menu-open (rsubs [::subs/index-drawer-open])
        show-close-index-menu false
        ]
    [:> Drawer
     {:open     index-menu-open
      :anchor   :right
      :on-close #(dispatch! [::events/open-index-drawer false])
      }
     [:div {:style {:position :relative
                    :width    240
                    :anchor   :right
                    }}
      (when show-close-index-menu
        [:div {:style {:display    :flex
                       :alignItems :center
                       ;:justifyContent :flex-end
                       :padding    "0 8px"
                       ;; ...theme.mixins.toolbar,
                       }}
         [:> chevron-right-icon {:on-click #(dispatch! [::events/open-index-drawer false])}]])
      (when show-close-index-menu
        [:> Divider])
      [index-pane]
      ]]
    ))

(defn set-tab-title []
  (let [doc-title (rsubs [::subs/doc-title])]
    (set! (.-title js/document) doc-title)
    nil))

(defn app-root []
  (let [
        page-item false
        doc-title (rsubs [::subs/doc-title])
        app-status (rsubs [::subs/app-status])
        online-status (rsubs [::subs/online-status])
        ]
    ;(debug log (->  base-theme js->clj :root))
    [ui/error-boundary ::app-route
     [:link {:rel "stylesheet" :href "/goog.css"}]
     [:base {:target "_blank"}]
     [:> MuiThemeProvider {:theme mui-theme}
      ;{:theme (aget js/MaterialUIStyles "DarkRawTheme")}
      [:> CssBaseline]
      [set-tab-title]
      [:> AppBar
       {:position :sticky
        :color    :inherit
        :style    {:flex-direction :row
                   :padding        "0 10px"
                   }}
       (let [menu-btn-style {:margin-left  "-10px"
                             :margin-right "20px"
                             }]
         [:> Toolbar {:style {:flex 1}}
          (if page-item
            [:> IconButton {:title    "back"
                            :color    :inherit
                            :style    menu-btn-style
                            :on-click #(js/window.history.back)
                            } [:> keyboard-backspace]
             ]
            [:> IconButton {:title    "Open drawer"
                            :color    :inherit
                            :style    menu-btn-style
                            :on-click #(dispatch! [::events/open-tag-drawer true])
                            } [:> tree-menu-icon]
             ])
          [:> Typography {:variant :h6
                          :color   :inherit
                          :style   {:flex        1
                                    :line-height :normal
                                    ;;:font-weight :bold
                                    }
                          } doc-title
           ]
          [:> IconButton {:title    "online status"
                          :color    :inherit
                          :style    {:flex 0}
                          :on-click #(dispatch! [::events/window-focused])
                          } (case online-status
                              :syncing [:> syncing-icon]
                              :uploading [:> uploading-icon]
                              :downloading [:> downloading-icon]
                              :synced [:> synced-icon]
                              :online [:> signed-in-icon]
                              false [:> signed-out-icon]    ;offline
                              (warn log ::online-status-unknown online-status)
                              )
           ]
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
                                    }
                          } (:text app-status)
           ]
          [:> IconButton {:title    "index menu"
                          :color    :inherit
                          :style    menu-btn-style
                          :on-click #(dispatch! [::events/open-index-drawer true])
                          } [:> menu-icon]
           ]])
       ]
      [ui/error-boundary ::tag-drawer [tag-drawer]]
      [ui/error-boundary ::index-drawer [index-drawer]]
      [ui/error-boundary ::items-pane [items-pane]]
      ]]))

(defn main-panel []
  [app-root])
