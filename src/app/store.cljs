(ns app.store
  (:require
    [lib.log :as log :refer [trace debug info warn fatal error pprintl]]
    [lib.debug :as debug :refer [we wd]]
    [lib.local-db :as ldb :refer []]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go-loop go]]
    [lib.asyncutils :as au :refer [put-last!] :refer-macros [<? go? go-try goc go-let]]
    [lib.goog-drive :as drive]
    [lib.utils :as utils :refer [only] :refer-macros [for-all]]
    [cljs.pprint :refer [pprint cl-format]]
    [clojure.data :refer [diff]]
    [app.ui.utils :as ui-utils]
    [clojure.string :as str]
    [app.credentials]
    ))

(def log (log/logger 'app.store))

(def pr-t ui-utils/iso-time->time-format-full)

(def sign-in drive/sign-in!)
(def sign-out drive/sign-out!)

(defonce drive-file-id* (atom nil))
(defonce signed-in?* (atom nil))

(defn- prn-diff [ma a mb b]
  (let [[a b _both] (diff a b)]
    (println ::diff ma)
    (pprint a)
    (println ::diff mb)
    (pprint b)
    ))

(def index-key :yetipad)
(def index-format :object)

(defn- <read-local-index []
  (ldb/<get-data index-key {:format index-format :default {}}))

(defn- <write-local-index [index]
  (ldb/<put-data index-key index {:format index-format}))

(defn- index-data [doc]
  {:doc-change (get doc :change)
   :doc-id     (get doc :doc-id)
   :title      (get-in doc [:options :doc-title])
   :subtitle   (get-in doc [:options :doc-subtitle])
   })

(def index-fields [:doc-id :doc-change :doc-changes :file-id :file-change :title :subtitle])

(defn- index-merge
  "Update an index entry"
  [index {:keys [doc-id] :as doc} & [changes]]
  (update index doc-id (fn [entry]
                         (merge (select-keys entry index-fields)
                                (index-data doc)
                                (select-keys changes index-fields)
                                ))))

(defn- <index-merge [doc & [changes]]
  (go? (<! (<write-local-index (index-merge (<? (<read-local-index)) doc changes)))))

(defn <read-local-doc
  "Return the doc or false"
  [doc-id]
  (assert doc-id)
  (ldb/<get-data doc-id {:format :object}))

(defn <write-local-doc [doc]
  (assert doc)
  (ldb/<put-data (:doc-id doc) doc {:format :object}))

(defn- <write-localstore [k v]
  (ldb/<put-data k v {:format :object}))

(defn- <read-localstore [k]
  (ldb/<get-data k {:format :object}))

(defn- ldb-doc-data-key [doc-id]
  (str doc-id \*))

(defn <write-persist-doc [doc-id data]
  (trace log '<write-persist-doc)
  (if (empty? data)
    (ldb/<remove-item (ldb-doc-data-key doc-id))
    (<write-localstore (ldb-doc-data-key doc-id) data)))

(defn <read-persist-doc [doc-id]
  (trace log '<read-persist-doc)
  (<read-localstore (ldb-doc-data-key doc-id)))

(def ldb-device-key \*)

(defn <write-persist-device [data]
  (trace log '<write-persist-device)
  (if (empty? data)
    (ldb/<remove-item ldb-device-key)
    (<write-localstore ldb-device-key data)))

(defn <read-persist-device []
  (trace log '<read-persist-device)
  (<read-localstore \*))

(defn- <into
  "Same as async/into except if any of the values are Errors then
  the first Error is returned instead."
  [coll ch]
  (go
    (let [coll (<! (async/into coll ch))]
      (or (some #(when (utils/error? %) %) coll) coll))))

(defn- <async
  "Concurrent dispatch support:
  Returns a channel with a vector of the values from the supplied channels.
  Completes when all cs are closed.
  If any of the values are an Error then the first error is returned instead
  but otherwise it is allowed to complete."
  [& cs]
  (<into [] (async/merge cs)))

(defn write-local-doc!
  "Write doc to localstore and update the localstore index."
  [{:keys [doc-id] :as doc} & [{:keys [on-written]}]]
  (go
    (<? (<async
          (<write-local-doc doc)
          (<index-merge doc)
          ))
    (and on-written (on-written))
    )
  nil)

(let [queryfn (fn [qstr]
                (fn [v] (cl-format false qstr v)))
      ;~a is unquoted
      ;https://developers.google.com/drive/api/v3/ref-search-terms
      qfs {:name    (queryfn "name=~s")
           :trashed (queryfn "trashed=~s")
           :doc-id  (queryfn "(appProperties has { key='doc-id' and value=~s })")
           }]
  (defn make-query [options]
    (let [q (for [[k v] options
                  :let [qf (qfs k)]
                  :when qf
                  ]
              (qf v))
          query (->> q (interpose " and ") str/join)
          ]
      ;(debug log :query query)
      query)))

(defn <list-app-drive-files [& [{:keys [fields] :as options}]]
  ;http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000)
  ;https://developers.google.com/drive/api/v3/reference/files
  (go
    (let [fields (or fields "files(id, name, modifiedTime, trashed, appProperties)")
          q (make-query options)
          response (<? (drive/<list-app-files {:query q :fields fields}))
          ]
      (:files response)
      )))

(defn file-meta>data [{:keys [id name modifiedTime appProperties] :as meta}]
  (when meta
    {:doc-id (:doc-id appProperties) :file-name name :file-id id :file-change modifiedTime})
  )

(defn <file-data
  [file-id]
  (assert file-id)
  (go
    (file-meta>data (<? (drive/<get-file-meta file-id {:fields "id, name, modifiedTime, appProperties"})))
    ))

(defn <find-file-data
  "Find latest file metadata for doc-id. Return file data or false"
  [doc-id]
  (go
    (let [files-data (<? (<list-app-drive-files {:doc-id  doc-id
                                                 :fields  "files(id, name, modifiedTime, trashed, appProperties)"
                                                 :trashed false
                                                 }))
          ]
      (if (empty? files-data)
        false
        (file-meta>data (first (sort-by :modifiedTime files-data)))
        ))))

(defn <read-file-data-list
  "Get app-file metadata by doc-id.
  Files that aren't in the local index are allocated a new doc-id."
  []
  (go
    (let [files (<? (<list-app-drive-files {:trashed false
                                            :fields  "files(id, name, modifiedTime, appProperties)"
                                            }))]
      (into {} (for [file-meta files]
                 (let [{:keys [doc-id] :as data} (file-meta>data file-meta)]
                   [doc-id data])))
      )))

(defn- <write-app-data
  "Writes data to the hidden app Drive data-file.
  Not in use"
  [index]
  (drive/<write-file-content @drive-file-id* index {:content-type :edn}))

(defn- <write-file-content
  "Write the file content to file with file-id"
  [file-id doc]
  (drive/<write-file-content file-id doc {:content-type :edn}))

(defn signed-in? []
  (boolean @signed-in?*))

(defn- log-error [v]
  (when (utils/error? v)
    (js/console.error v)))

(defn <file-meta
  "Returns a map of file metadata. Defaults to all metadata or specify fields,
  a list of keys or strings selecting metadata eg [:modifiedTime :mimeType]."
  [file-id & [fields]]
  (drive/<get-file-meta file-id {:fields fields}))

(defn update-timestamps!
  "Registers a document item change in the index to support file syncing.
  The change must be registered before the timestamp is updated.
  the registered changes are cleared after the synched document is written to Drive.
  'changes' is a list of maps of the form: {:keys [id change]}
  :mchange must always be updated if the item map is changed.
  For back compatibility :mchange may not exist so use :change
  :change is used for sorting and display purposes it won't be updated if just metadata is changed
  "
  [{:keys [doc-id] :as doc} id-or-ids & [{:keys [add-create]}]]
  (let [ids (if (sequential? id-or-ids) id-or-ids [id-or-ids])]
    (go-let [index (<? (<read-local-index))
             doc-changes (get-in index [doc-id :doc-changes])
             doc-changes (reduce (fn [doc-changes id]
                                   (if (get doc-changes id)
                                     doc-changes
                                     (assoc doc-changes id {:source-change (let [item (get doc id)]
                                                                             (or (:mchange item) (:change item)))
                                                            }))
                                   ) doc-changes ids)
             ]
      (<? (<write-local-index (assoc-in index [doc-id :doc-changes] doc-changes)))
      (trace log 'update-timestamps! doc-changes))
    (let [iso-date-time (utils/iso-time-now)
          doc (reduce (fn [doc id]
                        (update doc id into [[:mchange iso-date-time]
                                             (when add-create
                                               [:create iso-date-time])
                                             ])
                        ) doc ids)
          doc (assoc doc :change iso-date-time)
          ]
      doc)))

(def <trash-file drive/<trash-file)

(def <add-properties drive/<add-properties)

(defn- <get-trashed []
  (ldb/<get-data :trashed {:format :object :default []}))

(defn- <put-trashed [data]
  (ldb/<put-data :trashed data {:format :object}))


(defn delete-doc! [{:keys [doc-id]} {:keys [on-deleted]}]
  ;todo test offline
  (if (signed-in?)
    (go
      (let [local-index (<? (<read-local-index))]
        (<? (<write-local-index (dissoc local-index doc-id)))
        (<? (ldb/<remove-item doc-id))
        (if (signed-in?)
          (let [file-id (get-in local-index [doc-id :file-id])]
            (<? (<trash-file file-id)))
          (<? (<put-trashed (conj (<? (<get-trashed)) doc-id))))
        (and on-deleted (on-deleted)))
      )))

(defn trash-files-pending! [{:keys [on-complete]}]
  (go
    (let [trashed (<? (<get-trashed))]
      (when (not-empty trashed)
        (let [idx (<? (<read-local-index))]
          (doseq [doc-id trashed]
            (<? (<trash-file (get-in idx [doc-id :file-id]))))
          (<? (<put-trashed nil))
          ))
      (and on-complete (on-complete)))))

(defn- doc-item-change-status [file-change change root-change]
  (cond
    ;No file entry, App entry:
    (not file-change) :overwrite-file
    ;App entry has been deleted file entry unchanged:
    (and root-change (not change) (= file-change root-change)) :trash
    ;App entry has been deleted file entry changed:
    (and root-change (not change) (not= file-change root-change)) :overwrite-from-file
    ;file entry, no app entry
    (and (not root-change) (not change)) :overwrite-from-file
    ;App changes but no drive changes:
    (and root-change (= root-change file-change)) :overwrite-file
    ;both app and Drive changes:
    (and root-change (not= root-change file-change)) :resolve-conflicts
    ;no app changes but drive changes
    (and (not root-change) (> file-change change)) :overwrite-from-file
    ;no app or drive changes
    (and (not root-change) (= change file-change)) :in-sync
    ;app hasn't changed but is older than drive
    ;Only makes sense if doc has been loaded from external source:
    (and (not root-change) (> change file-change)) :resolve-conflicts
    ))


(defn- deep-tag-ids [doc tag-ids]
  (distinct ((fn deep-tag-ids [doc tag-ids]
               (reduce (fn [tids tid]
                         (concat tids (deep-tag-ids doc (get-in doc [tid :tags])))
                         ) tag-ids tag-ids)) doc tag-ids)))

(defn- map-path
  "Creates a map of tag-paths for tag-id where a path takes the form of [<tag-title> (<parent tag-paths>)].
  The structures can be compared for equality (=).
  "
  [doc src-tag-ids]
  (let [to-path (fn to-path [tag-id]
                  (let [{:keys [title tags]} (get doc tag-id)]
                    [(not-empty (sort (map to-path tags))) title]))]
    (into {} (for [tag-id src-tag-ids]
               [tag-id (to-path tag-id)]))))

(def imported-title "imported")

(defn- map-import-child-path
  "Same as map-path except only includes paths under the 'imports' tag.
  The 'imports' tag isn't included.
  "
  [doc src-tag-ids]
  (let [imported? (partial = [nil imported-title])
        to-path (fn to-path [tag-id]
                  (let [{:keys [title tags]} (get doc tag-id)
                        paths (not-empty (keep to-path tags))
                        ]
                    (when (or paths (= title imported-title))
                      [(not-empty (sort (remove imported? paths))) title]
                      )))
        ]
    (into (array-map) (for [tag-id src-tag-ids]
                        (let [path (to-path tag-id)]
                          (when (and path (-> path imported? not))
                            [path tag-id]))))))

(defn- copy-items
  "copy items with item-ids from source-doc to target-doc. The item ids are remapped to new ids.
  References between items in item-ids like included tags are maintained.
  References to tags not include in item-ids are added as a child of the tag named 'imported' retaining only their name,
  where the tag name already exists as a child of the 'imported' tag, this will be reused as the reference target.
  "
  ;1) find all tag and tags-of-tag ids for items being copied
  ;2) get all tag-paths for 1).
  ;3) use these tag-paths to tie up with target tag-paths to get source id to target id map.
  ;4) for source tag-paths without corresponding target tags-paths, copy over the source tags and remap their ids
  ;source tags that already have a corresponding import tag-name don't need a new id.
  ;source tag paths need to be matched with target-imported tag paths.
  [source-doc target-doc item-ids]
  (let [;remove trashed entries from target-doc
        target-doc (reduce-kv (fn [target-doc k v] (if (:trashed v)
                                                     (dissoc target-doc k)
                                                     target-doc)
                                ) target-doc target-doc)
        ;remove any static items (with keyword ids)
        ;remove tags. Maybe the should be an option; moved-item tags are copied by default
        item-ids (filter #(and (string? %)
                               (-> % source-doc :kind (not= :tag))
                               ) item-ids)
        iso-time-now (utils/iso-time-now)
        ;the id of the target-doc 'import' tag if it exists
        imported-tag-id (some (fn [{:keys [kind id title]}]
                                (and (= kind :tag) (= title imported-title) id)
                                ) (vals target-doc))

        target-tag-ids (map :id (filter #(= (:kind %) :tag) (vals target-doc)))
        ;get the tag-ids and tag-ids of tags for the selected items
        deep-src-tag-ids (deep-tag-ids source-doc (reduce (fn [tags id] (concat tags (get-in source-doc [id :tags])))
                                                          () item-ids))
        ;_ (debug log 'deep-src-tag-ids deep-src-tag-ids)
        ;The tag paths for the items being copied,
        ; these will be matched with existing target imports or recreated under imports
        src-tag-path-for-id (map-path source-doc deep-src-tag-ids)
        ;_ (debug log 'source-tag-path-for-id (pprintl source-tag-path-for-id))
        ;The existing target import paths for their id
        import-target-id-for-child-path (map-import-child-path target-doc target-tag-ids)
        ;_ (debug log 'import-target-id-for-child-path (pprintl import-target-id-for-child-path))
        ;create map of source tags to existing import tags where they exist:
        id-map (into {} (for [[src-tag-id src-path] src-tag-path-for-id
                              :let [target-id (get import-target-id-for-child-path src-path)]
                              :when target-id
                              ]
                          [src-tag-id target-id]))
        ;_ (debug log 'id-map 'matched (pprintl id-map))
        ;create new ids for unmatched tags and items being copied:
        copy-ids (distinct (concat
                             (remove id-map (keys src-tag-path-for-id))
                             item-ids
                             (when (not imported-tag-id) [:imported])
                             ))
        id-map (into id-map (map vector
                                 copy-ids (utils/new-item-ids target-doc)))
        ;add an 'imported' tag to target doc if it doesn't exist.
        [target-doc imported-tag-id] (if imported-tag-id
                                       [target-doc imported-tag-id]
                                       (let [id (:imported id-map)]
                                         [(assoc target-doc id {:id     id
                                                                :title  imported-title
                                                                :kind   :tag
                                                                :create iso-time-now
                                                                }) id]))
        ;_ (debug log 'id-map 'create (pprintl id-map))
        ;add the unmatched source tags to the target and remap their ids:
        target-doc (into target-doc (for [{:keys [tags] :as src-tag} (map source-doc copy-ids)]
                                      (let [id (id-map (:id src-tag))
                                            tag (assoc src-tag
                                                  :id id
                                                  :tags (if (empty? tags)
                                                          (list imported-tag-id)
                                                          (keep id-map tags))
                                                  )]
                                        [id tag])))
        target-doc (assoc target-doc :change iso-time-now)
        ]
    target-doc))

(defn copy-items! [source-doc target-doc-or-id item-ids {:keys [on-error on-complete]}]
  (take! (go?
           (let [local-index (<? (<read-local-index))
                 [target-doc target-doc-id] (if (map? target-doc-or-id)
                                              [target-doc-or-id (:doc-id target-doc-or-id)]
                                              [(<? (<read-local-doc target-doc-or-id)) target-doc-or-id])
                 _ (assert (and (map? target-doc) target-doc-id))
                 target-doc (copy-items source-doc target-doc item-ids)
                 file-id (get-in local-index [target-doc-id :file-id])
                 {:keys [modifiedTime]} (<? (<write-file-content file-id target-doc))
                 ]
             (<? (<write-local-doc target-doc))
             (<? (<write-local-index (index-merge local-index target-doc {:file-change modifiedTime})))
             (when on-complete (on-complete target-doc))
             )) (fn [e] (when (utils/error? e)
                          (if on-error
                            (on-error e)
                            (warn log 'copy-items! e)
                            )))))

(defn- sync-doc-content
  "Merge drive doc-file and app doc changes."
  ;root-change is a local copy of the file change time created when the local change-time is updated.
  ;Assume file change doesn't match doc root-change ie file has been changed and requires synch.
  [doc-changes {file-change :change :as drive-doc} {:keys [change] :as app-doc}]
  (trace log 'sync-doc-content)
  ;(prn-diff :drive-doc-only drive-doc :app-doc-only app-doc)
  (assert (not= file-change change))
  (let [ks (distinct (concat (keys drive-doc) (keys app-doc)))
        item-num* (atom (dec (utils/new-item-num [drive-doc app-doc])))
        merged (into {} (for [k ks
                              :let [
                                    ;root-change (get-in app-doc [:changes k :root-change])
                                    source-change (get-in doc-changes [k :source-change])
                                    file-entry (get drive-doc k)
                                    entry (get app-doc k)
                                    file-change (when (map? file-entry) (or (:mchange file-entry)
                                                                            (:change file-entry)
                                                                            (:create file-entry)))
                                    change (when (map? entry) (or (:mchange entry)
                                                                  (:change entry)
                                                                  (:create entry)))
                                    status (if (keyword? k)
                                             :in-sync       ;keep local settings
                                             ;(entry-change-status file-change change root-change)
                                             (doc-item-change-status file-change change source-change)
                                             )
                                    _ (when-not (= status :in-sync) (trace log 'change-status= status k))
                                    ]
                              e (case status
                                  :in-sync [entry]
                                  :overwrite-file [entry]
                                  :overwrite-from-file [file-entry]
                                  :trash []
                                  ;file (master) keeps existing id, allocate new to local
                                  :resolve-conflicts [file-entry (assoc entry
                                                                   :id (utils/to-str-36 (swap! item-num* inc))
                                                                   :conflict-id (:id entry)
                                                                   )]
                                  )
                              ]
                          [(get e :id k) e]
                          ))
        merged (assoc merged :mchange (utils/iso-time-now))
        ]
    ;(prn-diff :drive-doc-only drive-doc :merged-doc-only merged)
    merged))

(defn- drive-change-status [file-data local-entry]
  (assert (signed-in?))
  (let [{:keys [file-change]} file-data
        {:keys [doc-changes] file-ref-change :file-change} local-entry
        status (cond
                 ;file, no local
                 (and file-data (not local-entry)) [:overwrite-from-file :file-only]
                 ;no file, no changes:
                 (and (not file-data) (not doc-changes)) [:in-sync :local-only]
                 ;no file, changes:
                 (and (not file-data) doc-changes) [:overwrite-file :local-only]
                 ;unchanged file, changes:
                 (and (= file-change file-ref-change) doc-changes) [:overwrite-file :local-changed]
                 ;unchanged file, no changes:
                 (and (= file-change file-ref-change) (not doc-changes)) [:in-sync :in-sync]
                 ;file changes, no changes:
                 (and (not= file-change file-ref-change) (not doc-changes)) [:overwrite-from-file :file-changed]
                 ;file changes, changes:
                 (and (not= file-change file-ref-change) doc-changes) [:resolve-conflicts :both-changed]
                 )
        ]
    status))

(defn doc-status-index! [{:keys [on-result]}]
  (take!
    (go
      (if (signed-in?)
        (let [local-index (<? (<read-local-index))
              files-data (<? (<read-file-data-list))
              ]
          ;(debug log 'doc-status-index! 'files-data (pprintl files-data))
          (into {} (for [doc-id (distinct (concat (keys local-index) (keys files-data)))]
                     ;:synched is the file changed date when the file was synched
                     ;for a particular doc-id there could be a missing file or localstore entry
                     (let [{:keys [title subtitle] :as local-entry} (get local-index doc-id)
                           {:keys [file-id file-name] :as file-data} (get files-data doc-id)
                           [_ status] (drive-change-status file-data local-entry)
                           ]
                       ;(debug log [file local])
                       [doc-id {:doc-id    doc-id
                                :title     title
                                :subtitle  subtitle
                                :file-name file-name
                                :status    status
                                :file-id   file-id
                                }]
                       ))))
        (let [local-index (<? (<read-local-index))]
          (into {} (for [doc-id (keys local-index)]
                     [doc-id (assoc (select-keys (get local-index doc-id) [:doc-id :title :subtitle])
                               :status :offline)]
                     )))
        )
      ) on-result)
  nil)

(defn- <create-file [file-name doc-id]
  (go?
    (let [file-name (str file-name ".ydn")
          {id :id} (<? (drive/<create-file {:file-name  file-name
                                            :properties {:doc-id doc-id}
                                            }))
          ]
      id)))

(defn- <sync-drive-file!-
  "Sync doc with its drive file and updates localstore and drive accordingly."
  [local-entry file-data {:keys [doc-id title subtitle] :as doc} {:keys [on-sync-status
                                                                         on-in-sync
                                                                         on-overwrite-from-file
                                                                         on-overwrite-file
                                                                         on-conflicts-resolved
                                                                         on-complete
                                                                         on-synced-file
                                                                         ]}]
  (go?
    (let [{:keys [file-id]} file-data
          [status :as change-status] (drive-change-status file-data local-entry)
          ]
      (info log '<sync-drive-file!- 'sync-status doc-id change-status)
      (and on-sync-status (on-sync-status status))
      (case status
        :in-sync
        (do
          (and on-in-sync (js/setTimeout on-in-sync))
          (and on-synced-file (js/setTimeout #(on-synced-file doc)))
          (and on-complete (js/setTimeout on-complete))
          false)
        :overwrite-from-file
        (let [drive-doc (<? (drive/<get-file-content file-id {:default {}}))
              <c (<async
                   (<write-local-doc drive-doc)
                   (<index-merge drive-doc file-data)
                   )
              ]
          (and on-synced-file (js/setTimeout #(on-synced-file drive-doc)))
          (take! <c on-complete)
          (and on-overwrite-from-file (on-overwrite-from-file drive-doc))
          drive-doc)
        :overwrite-file
        (let [file-id (or file-id (<? (<create-file (or title subtitle doc-id) doc-id)))
              {:keys [modifiedTime]} (<? (<write-file-content file-id doc))
              <c (go?
                   (<! (<async
                         (<write-local-doc doc)
                         (<index-merge doc {:file-id     file-id
                                            :file-change modifiedTime
                                            :doc-changes nil
                                            })
                         )))
              ]
          (and on-synced-file (js/setTimeout #(on-synced-file doc)))
          (take! <c on-complete)
          (and on-overwrite-file (on-overwrite-file))
          false)
        :resolve-conflicts
        (let [drive-doc (<? (drive/<get-file-content file-id {:default {}}))
              content-changed? (not= (get drive-doc :change) (get doc :change))
              ;verify doc content has changed rather than just file modifiedTime
              local-index (<? (<read-local-index))
              doc-changes (get-in local-index [doc-id :doc-changes])
              synched-doc (if content-changed? (sync-doc-content doc-changes drive-doc doc) doc)
              {:keys [modifiedTime]} (<? (<write-file-content file-id synched-doc))
              <c (<async
                   (<write-local-doc synched-doc)
                   (<write-local-index (index-merge local-index synched-doc {:file-change modifiedTime
                                                                             :doc-changes nil
                                                                             }))
                   )
              ]
          (and on-synced-file (js/setTimeout #(on-synced-file synched-doc)))
          (take! <c on-complete)
          (and on-conflicts-resolved (on-conflicts-resolved synched-doc))
          synched-doc)
        ))))

(defn sync-drive-file!
  "Sync local-doc with its Drive file.
  If local-doc is nil then it is read from localstore
  "
  [local-doc-or-id {:keys [on-error] :as listeners}]
  (take! (go?
           (let [[local-doc doc-id] (if (map? local-doc-or-id)
                                      [local-doc-or-id (:doc-id local-doc-or-id)]
                                      [(<? (<read-local-doc local-doc-or-id)) local-doc-or-id])
                 _ (assert (and (map? local-doc) doc-id))
                 {:keys [file-id] :as local-entry} (get (<? (<read-local-index)) doc-id)
                 file-data (or (and file-id (<? (<file-data file-id)))
                               (and doc-id (<? (<find-file-data doc-id)))
                               nil)
                 ]
             ;(debug log 'sync-drive-file! 'file-data (pprintl file-data))
             (<! (<sync-drive-file!- local-entry file-data local-doc listeners))
             ))
         (fn [e] (when (utils/error? e)
                   (if on-error
                     (on-error e)
                     (warn log 'sync-drive-file! e)
                     )))))

(defn sync-drive-docs!
  ;todo complete implementation - not currently used
  [{:keys [on-synched exclude-docs priority-docs]}]
  (assert false "fix implementation")
  (go
    (let [local-index (<? (<read-local-index))
          file-data-list (<? (<read-file-data-list))
          exclude (complement (into #{} exclude-docs))
          ]
      (<? (<into [] (async/merge
                      (for [doc-id (filter exclude (distinct (concat priority-docs (keys local-index) (keys file-data-list))))]
                        ;for body behaves as a fn so go-block needed.
                        (go-let [local-entry (get local-index doc-id)
                                 file-data (get file-data-list doc-id)
                                 local-doc (<? (ldb/<get-data doc-id {:format :object :default {:doc-id doc-id}}))
                                 ]
                          (<sync-drive-file!- local-entry file-data local-doc nil))
                        ))))
      (and on-synched (on-synched))
      )) nil)

(defn sync-localstore!
  "Checks for external localstore changes (from another browser instance).
  Reads localstore doc compares with specified doc change date.
  "
  [{:keys [doc-id change]} {:keys [on-ls-change on-in-sync on-virgin-doc]}]
  (go
    (if (not change)
      (and on-virgin-doc (on-virgin-doc))                   ;new unchanged doc (only save after change)
      (let [{doc-change :doc-change} (get (<? (<read-local-index)) doc-id)]
        (if (and doc-change (> doc-change change))
          (and on-ls-change (on-ls-change (<? (<read-local-doc doc-id))))
          (and on-in-sync (on-in-sync))
          ))))
  nil)

(defn- <drive-data-file-id
  "Searches for the apps drive-data-file and returns its id.
  Creates the file if it doesn't exist.
  "
  []
  (go?
    (let [data-file-name "yetipad-data.edn"
          {app-data-files :files} (<? (drive/<list-app-data-files {:query (str "name='" data-file-name "'")}))
          ;Assume many files with data-file-name, use latest.
          {drive-data-file-id :id} (first (sort-by :modifiedTime app-data-files))
          ]
      (or
        drive-data-file-id
        (:id (<? (drive/<create-file {:file-name data-file-name :app-data? true})))
        ))))

(defn load-client [signed-in-listener]
  (reset! signed-in?* false)
  (signed-in-listener false)
  (drive/load-client! app.credentials/yetipad-credentials
                      (fn [signed-in?]
                        (reset! signed-in?* signed-in?)
                        (info log 'signed-in? signed-in?)
                        (go
                          (when (and signed-in? (not @drive-file-id*))
                            ;Drive app data file not currently used
                            #_(reset! drive-file-id* (<? (<drive-data-file-id)
                                                         (fn [e] (js/console.error e))))
                            )
                          (when signed-in?
                            ;When signing in, synchronise loaded and drive doc.
                            ;View is always initially loaded from localstore then synched when sign-in is complete.
                            )
                          (signed-in-listener signed-in?)
                          ))))