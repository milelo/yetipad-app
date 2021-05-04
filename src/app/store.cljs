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

(def index-key :yetipad)
(def index-format :object)

(defonce <task-queue (chan 20))                             ;go-block queue

(defn start-task-runner []
  ;execute queued go-block functions
  (trace log 'task-runner-started)
  (go-loop []
    (when-let [[<fn <c] (<! <task-queue)]
      (when-let [v (try (<! (<fn)) (catch :default e e))]
        (put! <c v))
      (close! <c)
      (recur))
    (warn log 'task-runner-stopped)))

(defonce _ (start-task-runner))

(defn- <do-sync
  "Add go-block function to queue for synchronous execution.
  Returns a channel with the result.
  "
  [<fn]
  (let [<c (chan)]
    (put! <task-queue [<fn <c])
    <c))

(defn do-sync!
  "Adds a function returning a channel yielding a single result (usually a go block) to a task queue.
  Sequences calls to go-blocks submitted from concurrent processes.
  Required to support external async calls.
  <fn is a function that returns a channel yielding a single value.
  Listeners: on-success & on-error notify of completion or error respectively.
  "
  ([<fn {:keys [on-success on-error]}]
   (go-let [v (<! (<do-sync <fn))]
     (if (utils/error? v)
       (if on-error (on-error v) (warn log 'do-sync! v))
       (when on-success (on-success v)))
     ) nil)
  ([<fn]
   (do-sync! <fn nil)))

(defn- <read-local-index []
  (trace log '<read-local-index)
  (ldb/<get-data index-key {:format index-format :default {}}))

(defn- <write-local-index [index]
  (trace log '<write-local-index)
  (ldb/<put-data index-key index {:format index-format}))

(defn- <index-entry-merge [doc-id updates]
  (trace log '<index-entry-merge [doc-id updates])
  (assert (and (string? doc-id) (map? updates)) [doc-id updates])
  (go?
    (when (not-empty updates)
      (let [idx (or (<? (<read-local-index)) nil)
            entry (get idx doc-id)
            updated (utils/map-remove nil? (merge entry updates))
            ]
        (when (not= entry updated)
          (trace log '<index-entry-merge 'write-entry updated)
          (<? (<write-local-index (assoc idx doc-id updated))))
        nil))))

(defn- index-entry-merge! [doc-id updates]
  (do-sync!
    #(<index-entry-merge doc-id updates)))

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

(defn write-local-doc!
  "Write doc to localstore and update the localstore index."
  [doc & [options]]
  (do-sync!
    #(<write-local-doc doc)
    options))

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
  (go?
    (let [fields (or fields "files(id, name, description, modifiedTime, trashed, appProperties)")
          q (make-query options)
          response (<? (drive/<list-app-files {:query q :fields fields}))
          ]
      (:files response)
      )))

(defn file-meta>data [{:keys [id name modifiedTime description trashed appProperties mimeType]}]
  (into {} [(when-let [doc-id (:doc-id appProperties)] :doc-id [:doc-id doc-id])
            (when name [:file-name name])
            (when description [:file-description description])
            (when id [:file-id id])
            (when modifiedTime [:file-change modifiedTime])
            (when (-> trashed nil? not) [:file-trashed trashed])
            (when mimeType [:mime-type mimeType])
            ]))

(defn- <file-data
  [file-id]
  (assert file-id)
  (go
    (file-meta>data (<? (drive/<get-file-meta file-id {:fields "id, name, modifiedTime, trashed, appProperties"})))
    ))

(defn- <find-file-data
  "Find latest file metadata for doc-id. Return file data or false"
  [doc-id]
  (go?
    (let [files-data (<? (<list-app-drive-files {:doc-id  doc-id
                                                 :fields  "files(id, name, modifiedTime, trashed, appProperties)"
                                                 :trashed false
                                                 }))
          ]
      (if (empty? files-data)
        false
        (file-meta>data (first (sort-by :modifiedTime files-data)))
        ))))

(defn- <rename-file [doc-id {:keys [doc-title doc-subtitle] :as params}]
  (trace log '<rename-file doc-id params)
  (assert (and doc-id doc-title) [doc-id doc-title])
  (go?
    (when-let [file-id (get-in (<? (<read-local-index)) [doc-id :file-id])]
      (let [file-data (file-meta>data (<? (drive/<update-file file-id
                                                              {:name        (str doc-title ".ydn")
                                                               :description (str "YetiPad Document:"
                                                                                 ;"\nTitle: " title
                                                                                 (when doc-subtitle
                                                                                   (str "\nSubtitle: " doc-subtitle))
                                                                                 "\ndoc-id: " doc-id
                                                                                 "\nfile-id: " file-id)
                                                               ;:mime-type   "text/plain"
                                                               :fields      [:modifiedTime :name :description :mimeType :appProperties :fileExtension]
                                                               })))
            ]
        (<? (<index-entry-merge doc-id (merge {:title doc-title :subtitle doc-subtitle}
                                              (select-keys file-data [:file-change
                                                                      :file-id
                                                                      ]))))
        file-data))))


(defn rename-file! [doc-id params & [listeners]]
  (do-sync! #(<rename-file doc-id params) listeners))

(defn <read-file-data-list
  "Get app-file metadata by doc-id.
  Files that aren't in the local index are allocated a new doc-id."
  []
  (go?
    (let [files (<? (<list-app-drive-files
                      {:fields "files(id, name, description, modifiedTime, trashed, appProperties)"
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
  [file-id {:keys [doc-id] :as doc} & [{:keys [update-index]}]]
  (assert doc)
  (go?
    (let [file-data (file-meta>data (<? (drive/<write-file-content file-id doc {:content-type :edn
                                                                                :fields       "id, modifiedTime"
                                                                                })))]
      (when update-index
        ;(debug log '<write-file-content 'file-data file-data)
        (let [idx-updates (select-keys file-data [:file-change :file-id])]
          (<? (<index-entry-merge doc-id (assoc idx-updates :doc-changes nil)))))
      file-data)))

(defn- <read-file-content
  "Read the document from a file. If the document is used to overwrite the current document the
  update-index option will update the index entry for the doc-id with the files timestamp.
  "
  [file-id & [{:keys [update-index]}]]
  (go?
    ;todo can <read-file-content also read meta-data
    (let [doc (or (<? (drive/<read-file-content file-id)) {})]
      (when update-index
        (let [file-data (file-meta>data (<? (drive/<get-file-meta file-id {:fields "id, modifiedTime, appProperties"})))
              ;file-id this isn't set by create-file so include
              idx-updates (select-keys file-data [:file-change :file-id :doc-id])
              ]
          (<? (<index-entry-merge (:doc-id idx-updates) (assoc idx-updates :doc-changes nil)))))
      doc)))

(defn signed-in? []
  (boolean @signed-in?*))

(defn <file-meta
  "Returns a map of file metadata. Defaults to all metadata or specify fields,
  a list of keys or strings selecting metadata eg [:modifiedTime :mimeType]."
  [file-id & [fields]]
  (drive/<get-file-meta file-id {:fields fields}))

(defn update-timestamps!
  "Registers a document item change in the index to support file syncing.
  The items change must be registered in its entry before the timestamp is updated.
  the registered changes are cleared after the synched document is written to Drive.
  'changes' is a list of maps of the form: {:keys [id change]}
  :mchange must always be updated if the item map is changed.
  For back compatibility :mchange may not exist so use :change
  :change is used for sorting and display purposes it won't be updated if just metadata is changed
  "
  [{:keys [doc-changes doc-id] :as doc} ids]
  (if (empty? ids)
    doc
    (let [iso-date-time (utils/iso-time-now)
          doc-changes (reduce (fn [doc-changes id]
                                (if (get doc-changes id)
                                  doc-changes
                                  (assoc doc-changes id {:source-change (let [item (get doc id)]
                                                                          (or (:mchange item) (:change item)))
                                                         }))
                                ) doc-changes ids)
          doc (reduce (fn [doc id]
                        (if-let [{:keys [create] :as item} (get doc id)]
                          (assoc doc id (into item [[:mchange iso-date-time]
                                                    (when-not create
                                                      [:create iso-date-time])
                                                    ]))
                          doc)
                        ) doc ids)

          doc (assoc doc :change iso-date-time)
          ]
      (index-entry-merge! doc-id {:doc-change iso-date-time :doc-changes doc-changes})
      doc)))

(def <trash-file drive/<trash-file)

(def <add-properties drive/<add-properties)

(defn- <get-trashed []
  (ldb/<get-data :trashed {:format :object :default []}))

(defn- <put-trashed [data]
  (ldb/<put-data :trashed data {:format :object}))

(defn delete-doc! [doc-id {:keys [keep-file] :as listeners}]
  ;todo test offline
  (do-sync!
    #(go?
       (let [local-index (<? (<read-local-index))]
         (<? (<write-local-index (dissoc local-index doc-id)))
         (<? (ldb/<remove-item doc-id))
         (when-not keep-file
           (if (signed-in?)
             (when-let [file-id (get-in local-index [doc-id :file-id])]
               ;file may me local only
               (<? (<trash-file file-id)))
             (<? (<put-trashed (conj (<? (<get-trashed)) doc-id)))))
         nil)
       ) listeners))

(defn trash-files-pending! [listeners]
  (do-sync!
    #(go?
       (let [trashed (<? (<get-trashed))]
         (when (not-empty trashed)
           (let [idx (<? (<read-local-index))]
             (doseq [doc-id trashed]
               (<? (<trash-file (get-in idx [doc-id :file-id]))))
             (<? (<put-trashed nil))
             nil
             )))) listeners))

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
                    [(not-empty (sort-by str (map to-path tags))) title]))]
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
                      [(not-empty (sort-by str (remove imported? paths))) title]
                      )))
        ]
    (into (array-map) (for [tag-id src-tag-ids]
                        (let [path (to-path tag-id)]
                          (when (and path (-> path imported? not))
                            [path tag-id]))))))


(defn- copy-items-to-imported
  ;1) find all tag and tags-of-tag ids for items being copied
  ;2) get all tag-paths for 1).
  ;3) use these tag-paths to tie up with target tag-paths to get source id to target id map.
  ;4) for source tag-paths without corresponding target tags-paths, copy over the source tags and remap their ids
  ;source tags that already have a corresponding import tag-name don't need a new id.
  ;source tag paths need to be matched with target-imported tag paths.
  [source-doc target-doc item-ids]
  (let [;remove trashed entries from target-doc
        target-doc-notrash (reduce-kv (fn [target-doc k v] (if (:trashed v)
                                                             (dissoc target-doc k)
                                                             target-doc)
                                        ) target-doc target-doc)
        ;remove any static items (with keyword ids)
        ;remove tags. Maybe the should be an option; moved-item tags are copied by default
        item-ids (filter #(and (string? %)
                               (-> % source-doc :kind (not= :tag))
                               ) item-ids)
        ;the id of the target-doc 'import' tag if it exists
        imported-tag-id (some (fn [{:keys [kind id title]}]
                                (and (= kind :tag) (= title imported-title) id)
                                ) (vals target-doc-notrash))

        target-tag-ids (map :id (filter #(= (:kind %) :tag) (vals target-doc-notrash)))
        ;get the tag-ids and tag-ids of tags for the selected items
        deep-src-tag-ids (deep-tag-ids source-doc (reduce (fn [tags id] (concat tags (get-in source-doc [id :tags])))
                                                          () item-ids))
        ;_ (debug log 'deep-src-tag-ids deep-src-tag-ids)
        ;The tag paths for the items being copied,
        ; these will be matched with existing target imports or recreated under imports
        src-tag-path-for-id (map-path source-doc deep-src-tag-ids)
        ;_ (debug log 'source-tag-path-for-id (pprintl source-tag-path-for-id))
        ;The existing target import paths for their id
        import-target-id-for-child-path (map-import-child-path target-doc-notrash target-tag-ids)
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
                                         [(assoc target-doc id {:id    id
                                                                        :title imported-title
                                                                        :kind  :tag
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
        change-ids (vals id-map)
        ]
    [target-doc change-ids]))


(defn- copy-items-to-root
  ;1) find all tag and tags-of-tag ids for items being copied
  ;2) get all tag-paths for 1).
  ;3) use these tag-paths to tie up with target tag-paths to get source id to target id map.
  ;4) for source tag-paths without corresponding target tags-paths, copy over the source tags and remap their ids
  ;source tags that already have a corresponding import tag-name don't need a new id.
  ;source tag paths need to be matched with target-imported tag paths.
  [source-doc target-doc item-ids]
  (let [;remove trashed entries from target-doc
        target-doc-notrash (reduce-kv (fn [target-doc k v] (if (:trashed v)
                                                             (dissoc target-doc k)
                                                             target-doc)
                                        ) target-doc target-doc)
        ;remove any static items (with keyword ids)
        ;remove tags. Maybe the should be an option; moved-item tags are copied by default
        item-ids (filter #(and (string? %)
                               (-> % source-doc :kind (not= :tag))
                               ) item-ids)

        target-tag-ids (map :id (filter #(= (:kind %) :tag) (vals target-doc-notrash)))
        ;get the tag-ids and tag-ids of tags for the selected items
        deep-src-tag-ids (deep-tag-ids source-doc (reduce (fn [tags id] (concat tags (get-in source-doc [id :tags])))
                                                          () item-ids))
        ;_ (debug log 'deep-src-tag-ids deep-src-tag-ids)
        ;The tag paths for the items being copied,
        ; these will be matched with existing target imports or recreated under imports
        src-tag-path-for-id (map-path source-doc deep-src-tag-ids)
        _ (debug log 'source-tag-path-for-id (pprintl src-tag-path-for-id))
        ;The existing target import paths for their id
        _ (debug log 'target-tag-ids (pprintl target-tag-ids))
        _ (debug log 'target-doc-notrash (pprintl target-doc-notrash))
        target-id-for-child-path (map-path target-doc-notrash target-tag-ids)
        _ (debug log 'import-target-id-for-child-path (pprintl target-id-for-child-path))
        ;create map of source tags to existing import tags where they exist:
        id-map (into {} (for [[src-tag-id src-path] src-tag-path-for-id
                              :let [target-id (get target-id-for-child-path src-path)]
                              :when target-id
                              ]
                          [src-tag-id target-id]))
        _ (debug log 'id-map 'matched (pprintl id-map))
        ;create new ids for unmatched tags and items being copied:
        copy-ids (distinct (concat
                             (remove id-map (keys src-tag-path-for-id))
                             item-ids
                             ))
        id-map (into id-map (map vector
                                 copy-ids (utils/new-item-ids target-doc)))

        ;_ (debug log 'id-map 'create (pprintl id-map))
        ;add the unmatched source tags to the target and remap their ids:
        target-doc (into target-doc (for [{:keys [tags] :as src-tag} (map source-doc copy-ids)]
                                      (let [id (id-map (:id src-tag))
                                            tag (assoc src-tag
                                                  :id id
                                                  :tags (keep id-map tags)
                                                  )]
                                        [id tag])))
        change-ids (vals id-map)
        ]
    [target-doc change-ids]))

(defn- copy-items
  "copy items with item-ids from source-doc to target-doc. The item ids are remapped to new ids.
  References between items in item-ids like included tags are maintained.
  References to tags not include in item-ids are added as a child of the tag named 'imported' retaining only their name,
  where the tag name already exists as a child of the 'imported' tag, this will be reused as the reference target.
  "
  [source-doc target-doc item-ids]
  ;todo make option
  ;(copy-items-to-imported source-doc target-doc item-ids)
  (copy-items-to-root source-doc target-doc item-ids)
  )

(defn copy-items! [source-doc target-doc-or-id item-ids listeners]
  (assert (or (map? target-doc-or-id) (string? target-doc-or-id)) [target-doc-or-id])
  (do-sync!
    #(go?
       (let [target-doc (if (map? target-doc-or-id)
                          target-doc-or-id
                          (or (<? (<read-local-doc target-doc-or-id)) {:doc-id target-doc-or-id}))
             [target-doc target-change-ids] (copy-items source-doc target-doc item-ids)
             target-doc (update-timestamps! target-doc target-change-ids)
             ]
         (<? (<write-local-doc target-doc))
         target-doc
         )) listeners))

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

(defn- drive-change-status
  "Precondition: signed into Drive.
  Determines the sync status of current document compared to the drive file.
  "
  [file-data idx-entry]
  (assert (signed-in?))
  (let [{:keys [file-change]} file-data
        {:keys [doc-changes] idx-file-change :file-change} idx-entry
        debug-data {:file-change file-change :idx-file-change idx-file-change
                    :doc-changes doc-changes :file-doc-id (:doc-id file-data) :local-doc-id (:doc-id idx-entry)}
        status (cond
                 ;file, no local
                 (and file-data (not idx-entry)) [:overwrite-from-file :file-only debug-data]
                 ;no file, no changes:
                 (and (not file-data) (not doc-changes)) [:in-sync :local-only debug-data]
                 ;no file, changes:
                 (and (not file-data) doc-changes) [:overwrite-file :local-only debug-data]
                 ;unchanged file, changes:
                 (and (= file-change idx-file-change) doc-changes) [:overwrite-file :local-changed debug-data]
                 ;unchanged file, no changes:
                 (and (= file-change idx-file-change) (not doc-changes)) [:in-sync :in-sync debug-data]
                 ;file changes, no changes:
                 (and (not= file-change idx-file-change) (not doc-changes)) [:overwrite-from-file :file-changed debug-data]
                 ;file changes, changes:
                 (and (not= file-change idx-file-change) doc-changes) [:resolve-conflicts :both-changed debug-data]
                 )
        ]
    status))

(defn sync-doc-index!
  "Reads the Drive file list and updates the
  "
  [{:keys [on-doc-status] :as listeners}]
  (do-sync!
    #(go?
       (if (signed-in?)
         (let [local-index (<? (<read-local-index))
               files-data (<? (<read-file-data-list))
               doc-status (into {} (for [doc-id (distinct (concat (keys local-index) (keys files-data)))
                                         :let [file-data (get files-data doc-id)
                                               {:keys [file-trashed file-change]} file-data
                                               index-entry (get local-index doc-id)
                                               {idx-file-change :file-change} index-entry
                                               index-entry (when-not (and file-data file-trashed
                                                                          (and index-entry
                                                                               (= idx-file-change file-change)
                                                                               ))
                                                             index-entry)
                                               file-data (and (not file-trashed) file-data)
                                               ]
                                         :when (or index-entry file-data)
                                         ]
                                     ;:synched is the file changed date when the file was synched
                                     ;for a particular doc-id there could be a missing file or localstore entry
                                     (let [{:keys [file-id file-name file-description]} file-data
                                           [_ file-name-part _ext] (and file-name (re-find #"(^.*)\.(.*$)" file-name))
                                           [_ status debug-data] (drive-change-status file-data index-entry)
                                           ]
                                       ;(debug log 'sync-doc-index! doc-id [status debug-data])
                                       ;(debug log 'doc-status-index! 'files-data (pprintl files-data))
                                       ;(debug log 'doc-status-index! 'local-index (pprintl local-index))
                                       [doc-id {:doc-id           doc-id
                                                :title            (get index-entry :title)
                                                :subtitle         (get index-entry :subtitle)
                                                :full-file-name   file-name
                                                :file-name        (not-empty file-name-part)
                                                :file-description file-description
                                                :status           status
                                                :file-id          file-id
                                                }])))
               ]
           (on-doc-status doc-status)
           ;If a file has been trashed by another device it needs removing from this devices localstore
           (when-let [remove-doc-ids (not-empty
                                       (for [{:keys [doc-id file-change]} (filter :trashed (vals files-data))
                                             :let [idx-entry (get local-index doc-id)]
                                             ;Don't remove if an un-synched local change has been made
                                             :when (and idx-entry (not= file-change (:file-change idx-entry)))
                                             ]
                                         doc-id))
                      ]
             (trace log 'sync-doc-index! 'remove-trashed-docs-locally remove-doc-ids)
             (doseq [doc-id remove-doc-ids]
               (<? (ldb/<remove-item doc-id)))
             (<? (<write-local-index (apply dissoc local-index remove-doc-ids))))
           nil)
         (let [local-index (<? (<read-local-index))
               doc-status (into {} (for [doc-id (keys local-index)]
                                     [doc-id (assoc (select-keys (get local-index doc-id) [:doc-id :title :subtitle])
                                               :status :offline)]
                                     ))
               ]
           (on-doc-status doc-status)
           nil))
       ) listeners))

(defn- <create-file [file-name doc-id]
  (go?
    (let [file-name (str file-name ".ydn")
          _ (trace log '<create-file file-name)
          {id :id} (<? (drive/<create-file {:file-name  file-name
                                            :properties {:doc-id doc-id}
                                            }))
          ]
      id)))

(defn sync-localstore!
  "Checks for external localstore changes (from another browser instance).
  Reads localstore doc compares with specified doc change date.
  "
  [{:keys [doc-id change]} {:keys [on-ls-change on-in-sync on-virgin-doc]}]
  (do-sync!
    #(go?
       (if (not change)
         (and on-virgin-doc (on-virgin-doc))                ;new unchanged doc (only save after change)
         (let [{:keys [doc-change]} (get (<? (<read-local-index)) doc-id)]
           ;(debug log 'sync-localstore! {:doc-change doc-change :change change})
           (if (and doc-change (> doc-change change))
             (and on-ls-change (on-ls-change (<? (<read-local-doc doc-id))))
             (and on-in-sync (on-in-sync))
             ))))))

(defn- <sync-drive-file!-
  "Sync doc with its drive file and updates localstore and drive accordingly."
  ;Drive file could have been updated from another device.
  ;index-entry provides the file-change timestamp of the previous sync so it can be compared with the current
  ;file-change timestamp from the drive file-meta.
  ;If there is a mismatch local doc and Drive doc need to be synchronised.
  [index-entry file-metadata {:keys [doc-id options] :as doc} {:keys [on-sync-status
                                                                      on-in-sync
                                                                      on-overwrite-from-file
                                                                      on-overwrite-file
                                                                      on-conflicts-resolved
                                                                      on-synced-file
                                                                      src
                                                                      ]}]
  (go?
    (let [{:keys [doc-title doc-subtitle]} options
          file-id (:file-id file-metadata)
          {:keys [doc-changes]} index-entry
          [status _ :as change-status] (drive-change-status file-metadata index-entry)
          ]
      ;(debug log '<sync-drive-file!- 'file-metadata file-metadata)
      (info log '<sync-drive-file!- src 'sync-status doc-id change-status)
      (and on-sync-status (on-sync-status status))
      (case status
        :in-sync
        (do
          (and on-in-sync (on-in-sync))
          (and on-synced-file (on-synced-file doc))
          nil)
        :overwrite-from-file
        (let [drive-doc (<? (<read-file-content file-id #{:update-index}))
              <c (<write-local-doc drive-doc)
              ]
          (and on-synced-file (on-synced-file drive-doc))
          (and on-overwrite-from-file (on-overwrite-from-file drive-doc))
          (<? <c)                                           ;wait for completion
          nil)
        :overwrite-file
        (let [file-id (or file-id (<? (<create-file (or doc-title doc-subtitle doc-id) doc-id)))
              <c1 (<write-file-content file-id doc {:update-index true})
              <c2 (<write-local-doc doc)
              ]
          (and on-synced-file (on-synced-file doc))
          (and on-overwrite-file (on-overwrite-file))
          (<? <c1)
          (<? <c2)
          nil)
        :resolve-conflicts
        (let [drive-doc (<? (<read-file-content file-id))
              content-changed? (not= (get drive-doc :change) (get doc :change))
              ;verify doc content has changed rather than just file modifiedTime
              synched-doc (if content-changed? (sync-doc-content doc-changes drive-doc doc) doc)
              <c1 (<write-file-content file-id synched-doc {:update-index true})
              <c2 (<write-local-doc synched-doc)
              ]
          (and on-synced-file (on-synced-file synched-doc))
          (and on-conflicts-resolved (on-conflicts-resolved synched-doc))
          (<? <c1)
          (<? <c2)
          nil)
        ))))

(defn sync-drive-file!
  "Synchronises local doc with its Drive file doc.
  If doc-or-id is a doc-id then doc is read from localstore.
  "
  ;To perform the sync we need the local doc index-entry & file-metadata doc to check the sync status.
  ;index-entry provides the file-meta of the previous sync so it can be compared with the current file-meta
  ;to check if the Drive file has been updated from another device.
  ;If there is a mismatch local doc and Drive doc need to be synchronised.
  [doc-or-id listeners]
  (assert (or (string? doc-or-id) (map? doc-or-id)) {:doc-or-id doc-or-id :src (:src listeners)})
  (do-sync!
    #(go?
       (let [[doc doc-id] (if (map? doc-or-id)
                            [doc-or-id (:doc-id doc-or-id)]
                            [(or (<? (<read-local-doc doc-or-id)) nil) doc-or-id])
             {:keys [file-id] :as index-entry} (get (<? (<read-local-index)) doc-id)
             ;If this device has seen the file before it will be in the index otherwise search Drive
             file-metadata (or (and file-id (<? (<file-data file-id)))
                               (and doc-id (<? (<find-file-data doc-id)))
                               nil)
             ]
         ;(debug log 'sync-drive-file! 'file-data (pprintl file-data))
         (<! (<sync-drive-file!- index-entry file-metadata doc listeners))
         )) listeners))

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