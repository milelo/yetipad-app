(ns app.subs
  (:require
    [re-frame.core :as re-frame :refer [reg-sub subscribe]]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd wee expose]]
    [lib.utils :as utils :refer [iso-time->date-time only]]
    [cljs.pprint :refer [pprint]]
    [clojure.string :as str]
    [app.ui.utils :as ui-utils]
    [app.ui.registry :as reg]
    ))

(def log (log/logger 'app.subs))

;======================utils===========================

(reg-sub
  ::tag-drawer-open
  (fn [db]
    (:tag-drawer-open? db)
    ))

(reg-sub
  ::index-drawer-open
  (fn [db]
    (:index-drawer-open? db)
    ))

(reg-sub
  ::persist-doc
  (fn [db _]
    (:persist-doc db)
    ))

(reg-sub
  ::editing
  (fn [db _]
    (:editing db)
    ))

(reg-sub
  ::logger-config
  (fn [db _]
    (:logger-config db)
    ))

(reg-sub
  ::moving-items
  (fn [db _]
    (:moving-items db)
    ))

(reg-sub
  ::platform
  (fn [db _]
    (:platform db)
    ))

(reg-sub
  ::edit-item
  (fn []
    (subscribe [::editing]))
  (fn [editing [_ item-id]]
    ;a copy of the original item being edited
    (let [entry (get editing item-id)]
      (and (not (:accept-as entry)) (:source entry)))
    ))

(reg-sub
  ::can-reload?
  (fn []
    (subscribe [::editing]))
  (fn [editing _]
    (not (some #(and (not (:accept-as %)) (:source %)) (vals editing)))
    ))

(reg-sub
  ::index-view
  (fn [db]
    (:index-view db)
    ))

(reg-sub
  ::doc-with-trash
  (fn [db]
    (:doc db)
    ))

(reg-sub
  ::doc-file-index
  (fn [db]
    (:doc-file-index db)
    ))

(reg-sub
  ::file-index-entry
  (fn []
    (subscribe [::doc-file-index]))
  (fn [doc-file-index [_ doc-id]]
    (get doc-file-index doc-id)
    ))

(reg-sub
  ::doc
  (fn []
    (subscribe [::doc-with-trash]))
  (fn [doc]
    (into {} (filter (fn [[_k v]]
                       (if (map? v) (-> v :trashed not) true)
                       ) doc))
    ))

(reg-sub
  ::doc-options
  (fn []
    (subscribe [::doc]))
  (fn [doc]
    (:options doc)
    ))

(reg-sub
  ::doc-title
  (fn []
    (subscribe [::doc-options]))
  (fn [options _]
    (or (:doc-title options) "YetiPad")
    ))

(reg-sub
  ::doc-subtitle
  (fn []
    (subscribe [::doc-options]))
  (fn [options _]
    (:doc-subtitle options)
    ))

(reg-sub
  ::doc-options
  (fn []
    (subscribe [::doc]))
  (fn [doc]
    (get doc :options {})
    ))

(reg-sub
  ::doc-item
  (fn []
    (subscribe [::doc]))
  (fn [doc [_ item-id]]
    (get doc item-id)
    ))

(defn iso-date-str
  [iso-str]
  "Returns the date part of the date-time iso string"
  (subs iso-str 0 8))

(reg-sub
  ;returns reverse date sorted list of items grouped by day
  ::items-by-history
  (fn []
    (subscribe [::doc]))
  (fn [doc _]
    (partition-by (fn [{:keys [create change]}]
                    (iso-date-str (or change create)))
                  (reverse (sort-by (fn [{:keys [create change]}]
                                      (or change create))   ;sort using iso date-string
                                    (filter :create (vals doc)))))
    ))

(reg-sub
  ::items-by-history-filtered
  ;list of matching items the first matching item of each day has the key :head__ set true.
  (fn []
    (subscribe [::items-by-history]))
  (fn [items-by-history [_ search-str]]
    (for [day-group items-by-history
          :let [day-group (not-empty (if (empty? search-str)
                                       day-group
                                       (filter #(ui-utils/search search-str %) day-group)))]
          :when day-group
          day (cons (assoc (first day-group) :head__ true) (rest day-group))
          ]
      day
      )))

(reg-sub
  ;returns list of items sorted by title then item-kind.
  ::items-by-title
  (fn []
    (subscribe [::doc]))
  (fn [doc _]
    (sort-by (fn [{:keys [kind title]}]
               [(reg/rget kind :index-sort-order) (or title ui-utils/no-title)])
             (filter #(and (map? %) (string? (:id %))) (vals doc)))
    ))

(reg-sub
  ::doc-id
  (fn []
    (subscribe [::doc]))
  (fn [doc _]
    (get doc :doc-id)
    ))

(reg-sub
  ::items-by-title-filtered
  (fn []
    (subscribe [::items-by-title]))
  (fn [items-by-title [_ search-str]]
    (for [item items-by-title
          :when (or (empty? search-str) (ui-utils/search search-str item))
          ]
      item
      )))

(reg-sub
  ::app-status
  (fn [db _]
    (:status db)))

(reg-sub
  ::saving?
  (fn [db _]
    (:saving? db)))

(reg-sub
  ::online-status
  (fn [db _]
    (:online-status db)))

(reg-sub
  ;returns list of items sorted by title then item-kind.
  ::doc-list
  (fn [{:keys [doc-file-index]} _]
    ;(debug log ::local-docs doc-index)
    (sort-by :file-name (vals doc-file-index))))

(reg-sub
  ::open-ids
  (fn [db]
    (:open-items db)))

(reg-sub
  ;returns open items
  ::open-items-with-trash
  (fn []
    [(subscribe [::open-ids]) (subscribe [::doc-with-trash])])
  (fn [[open-ids doc] _]
    ;item may be being deleted and not exist
    (keep (fn [id] (if (keyword? id)
                     {:id id :kind id}
                     (get doc id))
            ) open-ids)))

(reg-sub
  ::item-tag-data
  ;Sorted list of tag-data for the tags of the item-id
  (fn [[_ item-id]]
    [(subscribe [::tag-data-map]) (subscribe [::doc-item item-id])])
  (fn [[tag-data-map doc-item]]
    (sort-by :title (keep tag-data-map (:tags doc-item)))))

(defn- tag-path [doc tag-id]
  (loop [path () tag (get doc tag-id)]
    (if-let [parent-tag (first (keep doc (:tags tag)))]
      (recur (cons tag path) parent-tag)
      (cons tag path))))

(defn- tag-path-str [tag-path]
  (str/join " / " (map :title tag-path)))

(reg-sub
  ::tag-path
  ;vector of tags from root to and including tag-id
  (fn []
    (subscribe [::doc]))
  (fn [doc [_ tag-id]]
    (tag-path doc tag-id)))

(reg-sub
  ::tag-path-str
  (fn [[_ tag-id]]
    (subscribe [::tag-path tag-id]))
  (fn [tag-path]
    (tag-path-str tag-path)))

(reg-sub
  ::tag-data-map
  ;map of [tag-id {:keys [title id]}] for all doc tags
  (fn []
    (subscribe [::doc]))
  (fn [doc]
    (into {} (for [[id {:keys [kind title]}] doc
                   :when (and (= kind :tag) title)
                   ]
               [id {:title title :path-str (tag-path-str (tag-path doc id)) :id id}]
               ))))

(reg-sub
  ::tag-data
  ;list of maps of {:keys [title id]} for all doc tags sorted by title
  (fn []
    (subscribe [::tag-data-map]))
  (fn [tag-data-map]
    (sort-by :title (vals tag-data-map))))

(reg-sub
  ;returns a map of tag-id vs its child-ids as [tag-ids other-ids] where child-ids are items tagged with tag-id.
  ::tag-map
  (fn []
    (subscribe [::doc]))
  (fn [doc]
    (reduce
      (fn [m {:keys [kind tags id]}]
        (reduce (fn [m tag-id]
                  (if tag-id
                    (update m tag-id (fn [[ts xs]]
                                       (if (= kind :tag)
                                         [(conj (or ts #{}) id) xs]
                                         [ts (conj (or xs #{}) id)]
                                         )))
                    m)
                  ) m tags)
        ) {} (vals doc))))

(reg-sub
  ::child-data-by-tag-id
  ;returns sorted item-data for items tagged with tag-id as [tag-item-data other-item-data]
  (fn []
    [(subscribe [::tag-map]) (subscribe [::doc])])
  (fn [[tag-map doc] [_ tag-id]]
    (let [[tids xids] (get tag-map tag-id)
          data (fn [tags] (sort-by :title (keep doc tags)))
          ]
      [(data tids) (data xids)]
      )))


(reg-sub
  ::deleted-items
  ;returns sorted item-data for items tagged with tag-id as [tag-item-data other-item-data]
  (fn []
    (subscribe [::doc-with-trash]))
  (fn [doc _]
    (not-empty (filter :trashed (vals doc)))
    ))

(reg-sub
  ::root-tag-data
  (fn []
    [(subscribe [::tag-map]) (subscribe [::doc])])
  (fn [[tag-map doc]]
    (not-empty (sort-by :title (remove #(->> % :tags (some doc)) (keep doc (keys tag-map)))))
    ))

(reg-sub
  ::logger-packages
  (fn []
    (subscribe [::logger-config]))
  (fn [logger-config _]
    (filter symbol? (keys logger-config))
    ))

(reg-sub
  ::log-level
  (fn []
    (subscribe [::logger-config]))
  (fn [logger-config [_ package]]
    (or (get logger-config package) :default)
    ))
