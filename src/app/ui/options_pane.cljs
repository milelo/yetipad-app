(ns app.ui.options-pane
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
    [app.ui.registry :as reg]
    ["@material-ui/core" :refer [Tooltip TextField Typography
                                 TableContainer TableBody Table TableHead TableRow TableCell
                                 ]]
    ["@material-ui/lab" :refer [Autocomplete]]
    ["@material-ui/icons/SettingsTwoTone" :default options-icon]
    ))

(def log (log/logger 'app.ui.options-pane))

(def rsubs (comp deref re-frame/subscribe))
(def dispatch! re-frame/dispatch)

(defn string-editor [id value values*]
  [:> TextField {
                 ;:variant   :outlined
                 :size          :small
                 :margin        :dense
                 :default-value value
                 :on-change     #(swap! values* assoc id (not-empty (.-target.value %)))
                 }])

(defn combo-editor [options default id value values*]
  [:> Autocomplete {:options           (map name options)
                    :disable-clearable true
                    :style             {:width      300
                                        :margin-top 10
                                        }
                    :size              :small
                    :default-value     (name (or value default))
                    :render-input      (fn [^js params]
                                         ;; Don't call js->clj because that would recursively
                                         ;; convert all JS objects (e.g. React ref objects)
                                         ;; to Cljs maps, which breaks them, even when converted back to JS.
                                         ;; Best thing is to use r/create-element and
                                         ;; pass the JS params to it.
                                         ;; Use JS interop to modify params.
                                         ;(set! (.-variant params) "outlined")
                                         (set! (.-label params) "log-level")
                                         (r/create-element TextField params)
                                         )
                    :on-input-change   (fn [_e value reason]
                                         (trace log :on-input-change value reason)
                                         (swap! values* assoc id (keyword value)))
                    :on-change         (fn [_e value reason]
                                         (let [value (keyword value)]
                                           (trace log :combo-on-change id value reason)
                                           (when (= reason "select-option")
                                             (swap! values* assoc id value)
                                             )))
                    :input-value       (name (or (get @values* id) default))
                    }])

(defn table [title options editing? values*]
  [:<>
   [:> Typography {:variant :h6} title]
   [:> TableContainer
    [:> Table {:size :small}
     [:> TableBody
      (for-all [{:keys [id name value editor]} options]
        ^{:key id} [:> TableRow
                    [:> TableCell name]
                    [:> TableCell
                     (if-let [editor (and editing? editor)]
                       [editor id value values*]
                       value)]
                    ])]]]]
  )

(defn options-table [title options editing?]
  (let [values* (r/atom {})]
    (r/create-class
      {:reagent-render         (fn [title options editing?]
                                 [table title options editing? values*]
                                 )
       :component-will-unmount (fn [_this]
                                 (dispatch! [::events/options @values*]))
       })))

(defn content [editing? doc-options]
  [:<>
   ;[options-table "Device options" [] editing?]
   [options-table "Document options" [{:id     :doc-title
                                       :name   "Document title"
                                       :value  (:doc-title doc-options)
                                       :editor string-editor
                                       }
                                      {:id     :doc-subtitle
                                       :name   "Document subtitle"
                                       :value  (:doc-subtitle doc-options)
                                       :editor string-editor
                                       }
                                      ] editing?
    ]])

(defn options-pane [_context]
  (let [item {:id    :options
              :kind  :options
              :title "Settings"
              }
        editing? (rsubs [::subs/editing? :options])
        doc-options (rsubs [::subs/doc-options])
        ]
    (if editing?
      [ui/editor-pane item ^{:key :opts-e} [content editing? doc-options]]
      [ui/viewer-pane item ^{:key :opts-v} [content editing? doc-options] nil]
      )))

(reg/register {:kind  :options
               :title "Settings"
               :icon  options-icon
               :pane  options-pane
               })

