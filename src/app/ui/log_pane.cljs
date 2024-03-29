(ns app.ui.log-pane
  (:require
    [reagent.core :as r]
    [lib.log :as log :refer-macros [trace debug info warn error fatal]]
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
    ["@mui/material" :refer [Icon IconButton Tooltip Typography Paper Dialog
                                 Toolbar AppBar TextareaAutosize
                                 ]]
    ["@mui/icons-material/BugReport" :default log-icon]
    ["@mui/icons-material/AssignmentReturnedTwoTone" :default read-log-icon]
    [clojure.string :as str]))

(def log (log/logger 'app.ui.log-pane))

(defonce log* (r/atom nil))
(defonce entry-id* (atom 0))

(defn read-log []
  (reset! log* [])
  (go-loop []
    (when-let [v (async/poll! log/<logger)]
      (swap! log* conj v)
      (recur))
    ))

(defn read-log-button []
  [ui/item-button read-log-icon "read log" #(read-log)])

(defn debug-settings-button []
  (let [{:keys [icon title]} (reg/rget :log-config)]
    [ui/item-button icon title #(events/open-item! :log-config)]))

(defn entry [{:keys [level package time args meta ns scope]}]
  (let [s {:style {:margin-right 4}}
        p {:style {:margin-right 4 :color :green}}
        ]
    [:div
     [:span s time]
     [:span s (name level)]
     [:span p (if meta (str ns \: (:line meta) (when scope (str \. scope))) (name package))]
     [:span s (str/join \space (map log/arg-to-str args))]
     ]))

(defn log-container []
  [:> Paper {:style {:padding 5}}
   (for [e @log*]
     ^{:key (swap! entry-id* inc)} [entry e])])

(defn log-pane [_context]
  (let [item {:id    :log
              :kind  :log
              :title "Log"
              }
        ]
    [ui/viewer-pane item
     {:body    [log-container]
      :buttons [debug-settings-button read-log-button ui/fullscreen-button]
      }]))

(reg/register {:id    :log
               :title "Log"
               :icon  log-icon
               :pane  log-pane
               })
