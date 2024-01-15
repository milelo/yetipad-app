(ns app.subs
  (:require
   [lib.db :as db]
   [lib.log :as log :refer-macros [trace debug info warn error fatal]]
   [lib.debug :as debug :refer [we wd wee expose]]
   [lib.utils :as utils :refer [iso-time->date-time only]]
   [cljs.pprint :refer [pprint]]
   [clojure.string :as str]
   [app.ui.utils :as ui-utils]
   [app.ui.registry :as reg]))

(def log (log/logger 'app.subs))

;======================utils===========================

(def main-menu-open* (db/atom
                      (fn [db]
                        (:tag-drawer-open? db))))

(def index-menu-open* (db/atom
                       (fn [db]
                         (:index-drawer-open? db))))

(def local-file-dialog* (db/atom
                         (fn [db _]
                           (:local-file-dialog db))))

(def moving-items?* (db/atom
                     (fn [db]
                       (:moving-items? db))))

(def platform* (db/atom
                (fn [db]
                  (:platform db))))

(def edit-item (db/atomfn
                (fn [db]
                  (:editing db))
                (fn [editing item-id]
                  ;a copy of the original item being edited
                  (let [entry (get editing item-id)]
                    (and (not (:accept-as entry)) entry)))))

(def can-reload?* (db/atom
                   (fn [db]
                     (:editing db))
                   (fn [editing]
                     (not (some #(and (not (:accept-as %)) (:source %)) (vals editing))))))

(def index-view* (db/atom
                  (fn [db]
                    (:index-view db))))

(def doc-with-trash* (db/atom
                      (fn [db]
                        (:doc db))))

(def file-index-entry (db/atomfn
                       (fn [db doc-id]
                         (get-in db [:doc-file-index doc-id]))))

(def doc* (db/atom
           (fn []
             @doc-with-trash*)
           (fn [doc]
             (into {} (filter (fn [[_k v]]
                                (if (map? v) (-> v :trashed not) true)) doc)))))

(def doc-options* (db/atom
                   (fn [db]
                     (get-in db [:doc :options]))))

(def doc-title* (db/atom
                 (fn [db]
                   (or (get-in db [:doc :options :doc-title]) "YetiPad"))))

(def doc-subtitle* (db/atom
                    (fn [db]
                      (get-in db [:doc :options :doc-subtitle]))))

(def doc-item (db/atomfn
               (fn [_db item-id]
                 (get @doc* item-id))))

(defn iso-date-str
  "Returns the date part of the date-time iso string"
  [iso-str]
  (subs iso-str 0 8))

(def items-by-history* (db/atom
                        (fn []
                          @doc*)
                        (fn [doc]
                          (partition-by (fn [{:keys [create mchange change]}]
                                          (iso-date-str (or change mchange create)))
                                        (reverse (sort-by (fn [{:keys [create mchange change]}]
                                                            (or change mchange create)) ;sort using iso date-string
                                                          (map doc (filter string? (keys doc)))))))))

(def items-by-history-filtered (db/atomfn
                                (fn [_db search-str]
                                  ;Don't use cache-fn; too many variants of search-str.
                                  (for [day-group  @items-by-history*
                                        :let [day-group (not-empty (if (empty? search-str)
                                                                     day-group
                                                                     (filter #(ui-utils/search search-str %) day-group)))]
                                        :when day-group
                                        day (cons (assoc (first day-group) :head__ true) (rest day-group))]
                                    day))))

(def items-by-title* (db/atom
                      (fn []
                        @doc*)
                      (fn [doc]
                        (sort-by (fn [{:keys [kind title]}]
                                   [(reg/rget kind :index-sort-order) (or title ui-utils/no-title)])
                                 (filter #(and (map? %) (string? (:id %))) (vals doc))))))

(def doc-id* (db/atom
              (fn [db]
                (get-in db [:doc :doc-id]))))

(def items-by-title-filtered (db/atomfn
                              (fn [_db search-str]
                                ;Don't use cache-fn; too many variants of search-str.
                                (for [item @items-by-title*
                                      :when (or (empty? search-str) (ui-utils/search search-str item))]
                                  item))))

(def app-status* (db/atom
                  (fn [db _]
                    (:status db))))

(def saving?* (db/atom
               (fn [db _]
                 (:saving? db))))

(def sync-status* (db/atom
                   (fn [db]
                     (and (:online? db) (-> db :online-status (= :authorized)) (:sync-status db)))))

(def doc-list* (db/atom
                (fn [{:keys [doc-file-index]}]
                  ;(debug log ::local-docs doc-index)
                  (sort-by :file-name (vals doc-file-index)))))

(def open-items-with-trash* (db/atom
                             (fn [db]
                               [(:open-items db) @doc-with-trash*])
                             (fn [[open-ids doc]]
                              ;item may be being deleted and not exist
                               (keep (fn [id] (if (keyword? id)
                                                {:id id :kind id}
                                                (get doc id))) open-ids))))

(defn- tag-path [doc tag-id]
  (loop [path () tag (get doc tag-id)]
    (if-let [parent-tag (first (keep doc (:tags tag)))]
      (recur (cons tag path) parent-tag)
      (cons tag path))))

(defn- tag-path-str- [tag-path]
  (str/join " / " (map :title tag-path)))

(def tag-data-map* (db/atom
                    (fn []
                      @doc*)
                    (fn [doc]
                      (into {} (for [[id {:keys [kind title]}] doc
                                     :when (and (= kind :tag) title)]
                                 [id {:title title :path-str (tag-path-str- (tag-path doc id)) :id id}])))))

(def item-tag-data (db/atomfn
                    ;Sorted list of tag-data for the tags of the item-id
                    (fn [_db item-id]
                      [@tag-data-map* @(doc-item item-id)])
                    (fn [[tag-data-map doc-item]]
                      (sort-by :title (keep tag-data-map (:tags doc-item))))))

(def tag-path-str (db/atomfn
                   (fn [_db]
                     @doc*)
                   (fn [doc tag-id]
                     (tag-path-str- (tag-path doc tag-id)))))

(def tag-data* (db/atom
                (fn []
                  @tag-data-map*)
                (fn [tag-data-map]
                  (sort-by :title (vals tag-data-map)))))

(def tag-map*
  "returns a map of tag-id vs its child-ids as [tag-ids other-ids] where child-ids are items tagged with tag-id."
  (db/atom
   (fn []
     @doc*)
   (fn [doc]
     (reduce
      (fn [m {:keys [kind tags id]}]
        (reduce (fn [m tag-id]
                  (if tag-id
                    (update m tag-id (fn [[ts xs]]
                                       (if (= kind :tag)
                                         [(conj (or ts #{}) id) xs]
                                         [ts (conj (or xs #{}) id)])))
                    m)) m tags)) {} (vals doc)))))

(def child-data-by-tag-id
  "returns sorted item-data for items tagged with tag-id as [tag-item-data other-item-data]"
  (db/atomfn
   (fn [_db]
     [@tag-map* @doc*])
   (fn [[tag-map doc] tag-id]
     (let [[tids xids] (get tag-map tag-id)
           data (fn [tags] (sort-by :title (keep doc tags)))]
       [(data tids) (data xids)]))))

(def deleted-items* (db/atom
                     (fn []
                       @doc-with-trash*)
                     (fn [doc]
                       (not-empty (filter :trashed (vals doc))))))

(def root-tag-data* (db/atom
                     (fn []
                       [@tag-map* @doc*])
                     (fn [[tag-map doc]]
                       (not-empty (sort-by :title (remove #(->> % :tags (some doc)) (keep doc (keys tag-map))))))))

(def logger-packages* (db/atom
                       (fn [db]
                         (:logger-config db))
                       (fn [logger-config]
                         (filter symbol? (keys logger-config)))))

(def sign-in-email* (db/atom
                     (fn [db]
                       (get-in db [:persist-device :sign-in-email]))))

(def log-level (db/atomfn
                (fn [db]
                  (:logger-config db))
                (fn [logger-config package]
                  (or (get logger-config package) :default))))

