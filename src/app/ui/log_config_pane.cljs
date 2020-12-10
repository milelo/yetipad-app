(ns app.ui.log-config-pane
  (:require
    [reagent.core :as r]
    [re-frame.core :as re-frame]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go-loop go]]
    ;exceptions are reported by handlers
    [lib.asyncutils :refer [put-last!] :refer-macros [<? go-try]]
    [app.subs :as subs]
    [cljs.pprint :refer [pprint]]
    [app.events :as events]
    [app.ui.ui :as ui :refer [error-boundary]]
    [app.ui.registry :as reg]
    [app.ui.options-pane :as op]
    ["@material-ui/core" :refer [Icon IconButton Tooltip Typography Paper Dialog
                                 Toolbar AppBar TextareaAutosize
                                 ]]
    ["@material-ui/icons/SettingsTwoTone" :default debug-settings-icon]
    ["@material-ui/icons/SettingsBackupRestore" :default restore-defaults-icon]
    ))

(def log (log/logger 'app.ui.log-config-pane))

(def rsubs (comp deref re-frame/subscribe))
(def dispatch! re-frame/dispatch)

(def values* (r/atom {}))

(defn options-table [title options editing?]
  (r/create-class
    {:reagent-render         (fn [title options editing?]
                               (trace log :reagent-render #(with-out-str (pprint @values*)))
                               [op/table title options editing? values*]
                               )
     :component-will-unmount (fn [_this]
                               (trace log :changes #(with-out-str (pprint @values*)))
                               (when (rsubs [::subs/accept-edit? :log-config])
                                 (trace log :accept-edit)
                                 (log/set-config! @values*)
                                 ))
     }))

(defn content [editing?]
  (let [packages (rsubs [::subs/logger-packages])]
    [:<>
     [options-table "Logging level" (for-all [package (cons :default-level (sort-by name packages))]
                                      {
                                       :id     package
                                       :name   package
                                       :value  (rsubs [::subs/log-level package])
                                       :editor (partial op/combo-editor log/log-levels :default)
                                       }) editing?]
     ]))


(defn restore-defaults-button []
  [ui/item-button restore-defaults-icon "restore defaults" #(reset! values* (log/default-config))])

(defn log-config-pane [_context]
  (let [item {:id    :log-config
              :kind  :log-config
              :title "Log config"
              }
        editing? (rsubs [::subs/editing? :log-config])
        ]
    (if editing?
      [ui/editor-pane item ^{:key :edit} [content editing?] {:buttons [restore-defaults-button]}]
      [ui/viewer-pane item ^{:key :view} [content editing?] nil])))

(reg/register {:kind    :log-config
               :title   "Log configuration"
               :icon    debug-settings-icon
               :pane    log-config-pane
               })
