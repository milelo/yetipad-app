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
    ["@material-ui/core" :refer [Tooltip TextField Typography Checkbox
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
                 :on-change     #(swap! values* assoc id (not-empty (.-target.value ^js %)))
                 }])

(defn checkbox-editor [id value values*]
  [:> Checkbox {
                :color :primary
                :default-checked value
                :on-change #(swap! values* assoc id (.-target.checked ^js %))
                }])

(defn checkbox-viewer [_id value]
  (str (boolean value)))

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
      (for-all [{:keys [id name value editor viewer]} options]
        ^{:key id} [:> TableRow
                    [:> TableCell name]
                    [:> TableCell
                     (if (and editing? editor)
                       [editor id value values*]
                       (if viewer
                         [viewer id value]
                         value))]
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

(defn content [editing?]
  (let [doc-options (rsubs [::subs/doc-options])
        doc-id (rsubs [::subs/doc-id])
        {:keys [file-id]} (rsubs [::subs/file-index-entry doc-id])
        ]
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
                                        {:id     :compress-file?
                                         :name   "Compress file?"
                                         :value  (:compress-file? doc-options)
                                         :editor checkbox-editor
                                         :viewer checkbox-viewer
                                         }                      
                                        {:id    :doc-id
                                         :name  "Document ID"
                                         :value doc-id
                                         }
                                        {:id    :doc-file-id
                                         :name  "Document file ID"
                                         :value file-id
                                         }
                                        ] editing?
      ]]))

(defn options-pane [_context]
  (let [item {:id    :options
              :kind  :options
              :title "Settings"
              }
        ]
    (if (rsubs [::subs/edit-item :options])
      [ui/editor-pane item {:body ^{:key :opts-e} [content true]}]
      [ui/viewer-pane item {:body ^{:key :opts-v} [content false]}]
      )))

(reg/register {:id            :options
               :title         "Settings"
               :icon          options-icon
               :pane          options-pane
               :has-doc-entry true                          ;Items with keyword ID's don't by default
               })

