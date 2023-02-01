(ns app.ui.tagchips
  (:require
    [reagent.core :as r]
    [clojure.string :as str]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as u :refer-macros [for-all]]
    [app.subs :as subs]
    [cljs.pprint :refer [pprint]]
    [app.events :as events]
    ["@material-ui/core" :refer [Chip Tooltip TextField]]
    ["@material-ui/lab" :refer [Autocomplete]]
    ))

(def log (log/logger 'app.ui.tagchips))

(defn- js->cljs [js]
  (js->clj js :keywordize-keys true))

(defn tag-chip-edit [tag-ids* new-tags* id {:keys [title new-tag?]}]
  [:> Tooltip {:title "remove tag"}
   [:> Chip {:label    (if new-tag? title @(subs/tag-path-str id))
             :size     :small
             :clickable false ;hack: avoid mui bug with 'true'
             :color    (if new-tag? :secondary :primary)
             :on-click #(if new-tag?
                          (swap! new-tags* dissoc id)
                          (swap! tag-ids* disj id))}]])

(defn- tag-chips [tag-ids* new-tags*]
  (let [tag-data-map @subs/tag-data-map*]
    [:div
     (for-all [[id tag] (merge (select-keys tag-data-map @tag-ids*) @new-tags*)]
       ^{:key (str id :tag)} [tag-chip-edit tag-ids* new-tags* id tag]
       )]))

(defn- tagger [id tag-ids* new-tags*]
  (let [value* (r/atom "")]
    (fn [id]
      (let [tag-data @subs/tag-data*
            tag-data-map @subs/tag-data-map*
            ]
        (when-not @tag-ids*
          (reset! tag-ids* (into #{} (filter tag-data-map (:tags @(subs/doc-item id))))))
        [:<>
         [tag-chips tag-ids* new-tags*]
         [:> Autocomplete {:options          (clj->js tag-data)
                           :free-solo        true
                           :get-option-label #(or (:path-str (js->cljs %)) "")
                           :style            {:width      300
                                              :margin-top 10
                                              }
                           :size             :small
                           :render-input     (fn [^js params]
                                               ;; Don't call js->clj because that would recursively
                                               ;; convert all JS objects (e.g. React ref objects)
                                               ;; to Cljs maps, which breaks them, even when converted back to JS.
                                               ;; Best thing is to use r/create-element and
                                               ;; pass the JS params to it.
                                               ;; Use JS interop to modify params.
                                               (set! (.-variant params) "outlined")
                                               (set! (.-label params) "Add / create tag")
                                               (r/create-element TextField params)
                                               )
                           :on-input-change  (fn [_e value reason]
                                               ;(println :on-input-change value reason)
                                               (when (and (#{"input" "clear"} reason) (-> value (str/includes? \/) not))
                                                 (reset! value* value)
                                                 ))
                           :on-change        (fn [_e value reason]
                                               ;(println :on-change (:id (js->cljs value)) reason)
                                               (when (= reason "select-option")
                                                 (swap! tag-ids* conj (-> value js->cljs :id))
                                                 (reset! value* "")
                                                 )
                                               (when (= reason "create-option")
                                                 (swap! new-tags* assoc value {:title value :new-tag? true})
                                                 (reset! value* "")
                                                 ))
                           :input-value      @value*
                           }]]))))

(defn tag-chip [id]
  [:> Tooltip {:title "open tag"}
   [:> Chip {:label    @(subs/tag-path-str id)
             :size     :small
             ;:color    :primary
             :clickable false ;hack: avoid mui bug with 'true'
             :on-click #(events/open-item! id)
             }]])

(defn tag-viewer [id]
  [:div
   (for-all [{:keys [id]} @(subs/item-tag-data id)]
     ^{:key id} [tag-chip id]
     )])

(defn tag-editor [id]
  (let [tag-ids* (r/atom nil)
        new-tags* (r/atom nil)
        ]
    (fn [id]
      ;Doesn't re-render when title is edited so inner render function not required.
      [(with-meta (fn [] [tagger id tag-ids* new-tags*])
                  {:component-will-unmount
                   (fn [_this]
                     (events/new-tags! id @tag-ids* @new-tags*)
                     (reset! tag-ids* nil)
                     (reset! new-tags* nil)
                     )})])))