(ns app.ui.options-pane
  (:require
   [reagent.core :as r]
   [lib.log :as log :refer-macros [trace debug info warn error fatal]]
   [lib.debug :as debug :refer [we wd wee expose]]
   [lib.utils :as u :refer-macros [for-all]]
   [app.subs :as subs]
   [cljs.pprint :refer [pprint]]
   [app.events :as events]
   [app.ui.ui :as ui]
   [app.ui.registry :as reg]
   ["@mui/material" :refer [Tooltip TextField Typography Checkbox
                            TableContainer TableBody Table TableHead TableRow TableCell
                            Autocomplete]]
   ["@mui/icons-material/SettingsTwoTone" :default options-icon]))

(def log (log/logger 'app.ui.options-pane))

(defn string-editor [{:keys [id value]} values*]
  [:> TextField {;:variant   :outlined
                 :size          :small
                 :margin        :dense
                 :default-value value
                 :on-change     #(swap! values* assoc id (not-empty (.-target.value ^js %)))}])

(defn checkbox-editor [{:keys [id value]} values*]
  [:> Checkbox {:color :primary
                :default-checked value
                :on-change #(swap! values* assoc id (.-target.checked ^js %))}])

(defn checkbox-viewer [{:keys [value]}]
  (str (boolean value)))

(defn combo-editor [options default {:keys [id value label]} values*]
  [:> Autocomplete {:options           (map name options)
                    :disable-clearable true
                    :style             {:width      300
                                        :margin-top 10}
                    :size              :small
                    :default-value     (some-> (or value default) name)
                    :render-input      (fn [^js params]
                                         ;; Don't call js->clj because that would recursively
                                         ;; convert all JS objects (e.g. React ref objects)
                                         ;; to Cljs maps, which breaks them, even when converted back to JS.
                                         ;; Best thing is to use r/create-element and
                                         ;; pass the JS params to it.
                                         ;; Use JS interop to modify params.
                                         ;(set! (.-variant params) "outlined")
                                         (when label
                                           (set! (.-label params) (name label)))
                                         (r/create-element TextField params))
                    :on-change         (fn [_e value reason]
                                         (let [value (keyword value)]
                                           (trace log :combo-on-change id value reason)
                                           (when (= reason "selectOption")
                                             (swap! values* assoc id value))))
                    ;; :on-input-change   (fn [_e value reason]
                    ;;                      (trace log :on-input-change value reason)
                    ;;                     ;(swap! values* assoc id (keyword value))
                    ;;                      ) 
                    ;; :input-value       (some-> (or (get @values* id) default) name)
                    }])

(defn table [title options editing? values*]
  [:<>
   [:> Typography {:variant :h6} title]
   [:> TableContainer
    [:> Table {:size :small}
     [:> TableBody
      (for-all [{:keys [id name value editor viewer] :as options} options]
               ^{:key id} [:> TableRow
                           [:> TableCell name]
                           [:> TableCell
                            (if (and editing? editor)
                              [editor options values*]
                              (if viewer
                                [viewer options]
                                value))]])]]]])

(defn options-table [title options editing?]
  (let [values* (r/atom {})]
    (r/create-class
     {:reagent-render         (fn [title options editing?]
                                [table title options editing? values*])
      :component-will-unmount (fn [_this]
                                (let [doc-options (select-keys @values* [:doc-title :doc-subtitle :compress-file?])
                                      device-options (select-keys @values* [:sign-in-email :content-editor])]
                                  (trace log 'device-options device-options)
                                  (events/doc-options! doc-options)
                                  (events/device-options! device-options)))})))

(defn content [editing?]
  (let [doc-options @subs/doc-options*
        doc-id @subs/doc-id*
        {:keys [file-id]} @(subs/file-index-entry doc-id)
        email @subs/sign-in-email*]
    [:<>
     ;[options-table "Device options" [] editing?]
     [options-table "Document options" [{:id     :doc-title
                                         :name   "Document title"
                                         :value  (:doc-title doc-options)
                                         :editor string-editor}
                                        {:id     :doc-subtitle
                                         :name   "Document subtitle"
                                         :value  (:doc-subtitle doc-options)
                                         :editor string-editor}
                                        {:id     :compress-file?
                                         :name   "Compress file?"
                                         :value  (:compress-file? doc-options)
                                         :editor checkbox-editor
                                         :viewer checkbox-viewer}
                                        {:id    :doc-id
                                         :name  "Document ID"
                                         :value doc-id}
                                        {:id    :doc-file-id
                                         :name  "Document file ID"
                                         :value file-id}]
      editing?]
     [options-table "Device options" [{:id     :sign-in-email
                                       :name   "Sign-in email"
                                       :value  email
                                       :editor string-editor}
                                      {:id    :content-editor
                                       :name  "Content Editor"
                                       :value  @subs/content-editor*
                                       :editor (partial combo-editor [:goog-editor
                                                                      :ck-editor
                                                                      :quill-editor]
                                                        :goog-editor)}]
      editing?]]))

(defn options-pane [_context]
  (let [item {:id    :options
              :kind  :options
              :title "Settings"}]
    (if @(subs/edit-item :options)
      [ui/editor-pane item {:body ^{:key :opts-e} [content true]}]
      [ui/viewer-pane item {:body ^{:key :opts-v} [content false]}])))

(reg/register {:id            :options
               :title         "Settings"
               :icon          options-icon
               :pane          options-pane
               :has-doc-entry true                          ;Items with keyword ID's don't by default
               })

