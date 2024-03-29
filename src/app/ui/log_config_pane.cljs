(ns app.ui.log-config-pane
  (:require
    [reagent.core :as r]
    [lib.log :as log :refer-macros [trace debug info warn error fatal] :refer [pprintl]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go-loop go]]
    ;exceptions are reported by handlers
    [lib.asyncutils :refer [put-last!] :refer-macros [<? go-try]]
    [app.subs :as subs]
    [app.events :as events]
    [app.ui.ui :as ui :refer [error-boundary]]
    [app.ui.registry :as reg]
    [app.ui.options-pane :as op]
    ["@mui/material" :refer [Icon IconButton Tooltip Typography Paper Dialog
                                 Toolbar AppBar TextareaAutosize
                                 ]]
    ["@mui/icons-material/SettingsTwoTone" :default debug-settings-icon]
    ["@mui/icons-material/SettingsBackupRestore" :default restore-defaults-icon]
    ))

(def log (log/logger 'app.ui.log-config-pane))

(def values* (r/atom {}))

(defn options-table [title options editing?]
  (r/create-class
    {:reagent-render         (fn [title options editing?]
                               [op/table title options editing? values*]
                               )
     :component-will-unmount (fn [_this]
                               (events/set-log-config! @values*))
     }))

(defn content [editing?]
  (let [packages @subs/logger-packages*]
    [options-table "Logging level" (for-all [package (cons :default-level (sort-by name packages))]
                                     {
                                      :id     package
                                      :name   package
                                      :label  :log-level
                                      :value  @(subs/log-level package)
                                      :editor (partial op/combo-editor log/log-levels :default)
                                      }) editing?]
    ))


(defn restore-defaults-button []
  [ui/item-button restore-defaults-icon "restore defaults" #(reset! values* (log/default-config))])

(defn log-config-pane [_context]
  (let [item {:id    :log-config
              :kind  :log-config
              :title "Log config"
              }
        ]
    (if @(subs/edit-item :log-config)
      [ui/editor-pane item {:body ^{:key :edit} [content true] :buttons [restore-defaults-button]}]
      [ui/viewer-pane item {:body ^{:key :view} [content false]}])))

(reg/register {:id    :log-config
               :title "Log configuration"
               :icon  debug-settings-icon
               :pane  log-config-pane
               })
