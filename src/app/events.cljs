(ns app.events
  (:require
    [re-frame.core :as rc
     :refer [reg-event-db reg-event-fx dispatch]
     ;:rename {dispatch dispatch!}
     ]
    ;[cljs-uuid-utils.core :as uuid]
    [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
    [lib.log :as log :refer [trace debug info warn fatal pprintl]]
    [lib.debug :as debug :refer [we wd]]
    [lib.utils :as utils :refer [time-now-ms iso-time->date-time new-item-id]]
    [lib.goog-drive :as drive]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go-loop go]]
    ;exceptions are reported by handlers
    [lib.asyncutils :refer [put-last!] :refer-macros [<? go-try]]
    ;[app.localstore :as ls :refer [<write-doc]]
    [app.store :as store]
    [app.ui.utils :as ui-utils]
    [cljs.pprint :refer [pprint]]
    [accountant.core :refer [configure-navigation! navigate! dispatch-current!]]
    [app.route :refer [path-decode map->query-string]]
    [cljs.reader :refer [read-string]]
    [clojure.data :refer [diff]]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [app.ui.registry :as reg]
    ["react-device-detect" :refer [browserName browserVersion fullBrowserVersion osVersion
                                   deviceType engineName deviceDetect osName getUA
                                   mobileVendor mobileModel engineVersion
                                   ]]
    ))

(def log (log/logger 'app.events))

(def dispatch! rc/dispatch)

(def platform {:browser-name         browserName
               :browser-version      browserVersion
               :full-browser-version fullBrowserVersion
               :device-type          deviceType
               :engine-name          engineName
               :engine-version       engineVersion
               :os-name              osName
               :os-version           osVersion
               :user-agent           getUA
               :mobile-vendor        mobileVendor
               :mobile-model         mobileModel
               })

(info log 'platform \newline (with-out-str (pprint platform)))

(reg-event-db
  ::initialize-db
  (fn-traced [db _]
    (assoc db
      :persist-doc {}
      :persist-device {}
      :tag-drawer-open? false
      :index-drawer-open? false
      :index-view :index-history
      :doc {}
      :open-items ()
      :editing {}
      :save-pending? false
      :doc-file-index {}
      :status {}
      :online-status false
      :keep-doc-in-sync? true
      :saving? false
      :platform platform
      )))

;------------------debug-support---------------------

(reg-event-db
  ::dump-doc
  (fn-traced [{doc :doc :as db} _]
    ;(go (utils/throw-error "e1"))
    (let [doc-meta (select-keys doc (filter keyword? (keys doc)))]
      (println "Doc keyword entries:")
      (pprint doc-meta)
      )
    db))

(defn- fix-content-style [s]
  (let [style-vec (map #(str/split % #":") (str/split s #";"))]
    (into {}
          (for [[k v] style-vec]
            [(keyword (str/trim k)) (str/trim v)]))))

(defn fix-content [content]
  (if-let [[tag attrs children] (ui-utils/normalize content)]
    (let [attrs (when attrs
                  (into {} (map (fn [[k v :as e]]
                                  (cond
                                    (and (= (name k) "style") (string? v)) [k (fix-content-style v)]
                                    :default e)
                                  ) attrs)))
          ]
      (cond-> [tag]
              (and attrs (not= tag :br)) (conj attrs)
              children (into (map fix-content children))
              ))
    content))


(reg-event-db
  ::check-doc
  (fn-traced [{doc :doc :as db} _]
    (println "==== check-doc ====")
    ;(dispatch! [::close-all-items])
    (let [t1 #(doseq [[k v] doc]
                (when (and (string? k) (-> v :kind not))
                  (println ::doc-issue [k v])
                  ))
          t2 #(doseq [[k v] doc]
                (when-not (or (string? k) (keyword? k))
                  (println ::doc-issue [k v])
                  ))
          ]
      (t1)
      (t2)
      db)))

(reg-event-db
  ::fix-doc
  (fn-traced [{doc :doc :as db} _]
    (println "fix-doc====")
    (dispatch! [::close-all-items])
    (let [doc-junk #(into {} (for [[k item :as e] doc
                                   :let [{:keys [id]} (and (map? item) item)]
                                   :when (not= k id)
                                   ]
                               e))
          fix-style #(into {} (for [[k v :as e] doc #_(select-keys doc ["nusb" "nv1z" "nuru"])]
                                (if (#{:note :tag} (:kind v))
                                  (let [{:keys [content]} v
                                        fixed (not-empty (map fix-content content))
                                        v (if fixed
                                            (assoc v :content fixed)
                                            (dissoc v :content))
                                        ]
                                    (when (not= fixed content)
                                      (dispatch! [::open-item k]))
                                    [k v])
                                  e)))
          fix-map #(reduce-kv (fn [doc k v]
                                (if (or (keyword? k) (map? v))
                                  doc
                                  (do
                                    (println 'removed-item [k v])
                                    (dissoc doc k))
                                  )) doc doc)
          fix-ids #(reduce-kv (fn [doc k _v]
                                (if (or (string? k) (keyword? k))
                                  doc
                                  (dissoc doc k))
                                ) doc doc)
          fix-kind #(reduce-kv (fn [doc k v]
                                (if (and (string? k) (-> v :kind not))
                                  (dissoc doc k)
                                  doc)
                                ) doc doc)
          t1 #(doseq [[k v] doc]
                (when (and (string? k) (-> v :kind not))
                  (println ::fixdoc-issue [k v])
                  ))
          t2 #(doseq [[k v] doc]
                (when-not (or (string? k) (keyword? k))
                  (println ::fixdoc-issue [k v])
                  ))
          ]
      ;(pprint (fix-style))
      db
      ;(assoc db :doc (fix-kind))
      ;(assoc db :doc (fix-ids))
      ;(assoc db :doc (fix-map))
      ;(assoc db :doc (merge doc (fix-style)))
      )))

(reg-event-db
  ::dump-file-meta
  (fn-traced [{{:keys [doc-id]} :doc :as db} _]
    (go
      (let [{:keys [file-id] :as idx} (get (<? (store/<read-local-index)) doc-id)
            _ (println ::dump-file-meta :doc-d doc-id :file-id file-id :idx idx)
            ;meta (<? (store/<file-meta file-id [:modifiedTime]))
            meta (<? (store/<file-meta file-id))
            ]
        (println "File meta:")
        (pprint meta)
        ))
    db))

(reg-event-db
  ::debug-find-file
  (fn-traced [db _]
    (go
      (let [files (<? (store/<list-app-drive-files {:fields  "files(id, name, modifiedTime, trashed, appProperties)"
                                                    ;:name    "kgrsc300.ydn"
                                                    :trashed false
                                                    :doc-id  "kgrsc300"
                                                    }
                                                   ))]
        (pprint files)
        ))
    db))

(reg-event-db
  ::dump-item-content
  (fn-traced [db [_ item-id]]
    (println "vvvvvvvvvvvvvvvv")
    (pprint (get-in db [:doc item-id]))
    (println "^^^^^^^^^^^^^^^^")
    db))

(reg-event-db
  ::dump-index
  (fn-traced [{{:keys [doc-id]} :doc :as db} [_]]
    (go
      (println "\nlocal index entry:")
      (pprint (<? (store/<read-local-index)))
      (println "\nfiles data:")
      (pprint (<? (store/<read-file-data-list)))
      (println "this doc-id: " doc-id)
      (println)
      )
    db))

(reg-event-db
  ::debug-list-app-drive-files
  (fn-traced [db _]
    (go
      (let [files (<? (store/<list-app-drive-files {:fields  "files(id, name, modifiedTime, trashed, appProperties)"
                                                    ;:name    "kgrsc300.ydn"
                                                    :trashed false}
                                                   ))]
        (pprint files)
        ))
    db))

(reg-event-db
  ::debug-trash-file
  (fn-traced [db _]
    (go
      (let [file-id "1zSgHNyQ3Z6h3lNkkFtppxmJ7ivvS5sht"
            file-id "1ZYqrs1QLvoB5P4GhV8Q9Hz0FsyDDPSIR"     ;"kgrsc300.ydn"
            response (<? (store/<trash-file file-id))
            ]
        (println "Trash file")
        (pprint (-> response js->clj))
        (pprint (<? (store/<file-meta file-id)))
        ))
    db))

(reg-event-db
  ::debug-file-content
  (fn-traced [db _]
    (go
      (let [file-id "1zSgHNyQ3Z6h3lNkkFtppxmJ7ivvS5sht"
            file-id "1ZYqrs1QLvoB5P4GhV8Q9Hz0FsyDDPSIR"     ;"kgrsc300.ydn"
            response (<? (drive/<get-file-content file-id))
            ]
        (debug log ::debug-file-content response)
        ))
    db))

(reg-event-db
  ::debug-add-properties
  (fn-traced [db _]
    (go
      (let [file-id "1R8JZWxzjLAWYCXIb5Y493AemAoj9G-8W"     ;"kgrsc300.ydn"
            file-id "1CQXBtftHN-cUxgC-Au9-VpuWKyJbLxjc"     ;
            response (<? (store/<add-properties file-id {:doc-id "kgtbg5v1"}))
            ]
        (println "Add property")
        (pprint (get-in response [:appProperties :doc-id]))
        (pprint response)
        (pprint (<? (store/<file-meta file-id)))
        ))
    db))

;=============================================================================
;----------app-status----------------

(def min-status-display-time-ms 5000)

(rc/clear-global-interceptor)                               ;seems to be required. Re-reg bug?
(rc/reg-global-interceptor
  (rc/->interceptor
    :id ::clear-app-status-on-event
    :before (fn [context]
              (trace log 'before (get-in context [:coeffects :event 0]))
              context)
    :after (fn [context]
             (trace log 'after (get-in context [:coeffects :event 0]))
             (let [coeffects-db (get-in context [:coeffects :db])
                   effects-db (get-in context [:effects :db])
                   ms (get-in coeffects-db [:status :time-ms])
                   old-persist-doc (get-in coeffects-db [:persist-doc])
                   persist-doc (get-in effects-db [:persist-doc])
                   old-persist-device (get-in coeffects-db [:persist-device])
                   persist-device (get-in effects-db [:persist-device])
                   ]
               (when-not (identical? old-persist-doc persist-doc)
                 (store/<write-persist-doc (get-in effects-db [:doc :doc-id]) persist-doc))
               (when-not (identical? old-persist-device persist-device)
                 (store/<write-persist-device persist-device))
               (when (and ms (> (time-now-ms) (+ ms min-status-display-time-ms)))
                 (dispatch! [::clear-app-status]))
               context))))

(reg-event-db
  ::set-app-status
  (fn-traced [db [_ status type]]
    ;(debug log ::set-app-status status)
    (let [default-type :info]
      (assoc db :status (assoc (if (map? status)
                                 (let [{:keys [type]} status] (assoc status :type (or type default-type)))
                                 {:text (str status) :type (or type default-type)})
                          :time-ms (time-now-ms))))))

(reg-event-db
  ::clear-app-status
  (fn-traced [db _]
    (assoc db :status {})))

;--------------------navigation---------------------------

(configure-navigation!
  {:nav-handler
   (fn [path]
     ;Called with the url initially by dispatch-current!
     (let [{{:keys [open]} :query doc-id :fragment :as d} (path-decode path)]
       (if doc-id
         (dispatch! [::read-doc-by-id-nonav- doc-id {:open-items (read-string open)}])
         (dispatch! [::new-local-doc-nonav-]))
       ))
   :path-exists?
   (fn [path]
     ;true stops page reload
     ;we want the page to reload if the back button hits an external page
     (let [exists (not= path "/")]
       (info log 'path-exists? path exists)
       exists)
     )})

;only run on initial load.
(defonce _run-once (dispatch-current!))

(def update-nav-bar
  (rc/->interceptor
    :id ::update-nav-bar
    :after (fn [{{{old-open-items :open-items {old-doc-id :doc-id} :doc} :db [event-id] :event} :coeffects
                 {{:keys [open-items] {:keys [doc-id]} :doc} :db}                               :effects
                 :as                                                                            context
                 }]
             ;(debug log :id event-id :old-open old-open-items :open open-items)
             (when (or (not= old-doc-id doc-id) (not= old-open-items open-items))
               (let [query-str (when (not-empty open-items)
                                 (str \? (map->query-string {:open (str/replace (pr-str open-items) \space \,)})))
                     path (str \/ query-str \# doc-id)
                     ]
                 (info log :navigate! path)
                 (navigate! path)
                 ))
             context)))

;----------------------localstore-------------------

(reg-event-fx
  ;save doc but aggregate changes over 100ms
  ::save-doc
  (fn-traced [{{:keys [save-pending?] :as db} :db} _]
    (when-not save-pending?
      {:db             (assoc db :save-pending? true)
       :dispatch-later [{:ms 100 :dispatch [::save-doc-with-sync-]}]
       })))

(reg-event-db
  ::file-id
  (fn-traced [db [_ id]]
    (assoc db :file-id id)))

(reg-event-db
  ::drive-data-file-id
  (fn-traced [db [_ id]]
    (assoc db :drive-data-file-id id)))

(defn prn-diff [ma a mb b]
  (let [[a b _both] (diff a b)]
    (println ::diff ma)
    (pprint a)
    (println ::diff mb)
    (pprint b)
    ))

(reg-event-db
  ::sync-drive-file
  (fn-traced [{doc :doc :as db} [_ updated-doc]]
    (dispatch! [::online-status :online])
    (if (store/signed-in?)
      (store/sync-drive-file! updated-doc
                              {:on-sync-status         (fn [sync-status]
                                                         (when-let [status ({:overwrite-from-file :downloading
                                                                             :overwrite-file      :uploading
                                                                             :resolve-conflicts   :syncing
                                                                             } sync-status)]
                                                           (dispatch! [::online-status status])))
                               :on-complete            (fn []
                                                         ;(:keep-doc-in-sync? db)
                                                         ;(dispatch! [::sync-drive-docs {:exclude-docs [(:doc-id updated-doc)]}])
                                                         (dispatch! [::sync-doc-index])
                                                         (dispatch! [::online-status :synced])
                                                         )
                               ;:on-in-sync             #()
                               :on-overwrite-from-file (fn [drive-doc]
                                                         (dispatch! [::update-doc- drive-doc doc "Updated from Drive"]))
                               :on-overwrite-file      (fn []
                                                         (dispatch! [::set-app-status "Drive updated" :info]))
                               :on-conflicts-resolved  (fn [synched-doc]
                                                         (dispatch! [::update-doc- synched-doc doc "Synched with Drive"]))
                               :on-error               (fn [error]
                                                         (warn log ::sync-drive-file 'sync error)
                                                         (dispatch! [::online-status :error]))
                               })
      (dispatch! [::sync-doc-index]))
    db))

(reg-event-db
  ::save-doc-with-sync-
  (fn-traced [{doc :doc :as db} _]
    (info log "saving doc...")
    ;write to localstore
    (store/write-local-doc! doc {:on-written #(dispatch! [::sync-drive-file doc])})
    (assoc db :save-pending? false :saving? false)))

(reg-event-db
  ::sync-drive-docs
  (fn-traced [db [_ options]]
    (when (store/signed-in?)
      (store/sync-drive-docs! (merge
                                {:on-synched #(dispatch! [::sync-doc-index])
                                 }
                                options)))
    db))

(reg-event-db
  ::sync-doc-index
  (fn-traced [db _]
    (store/doc-status-index! {:on-result (fn [status-index]
                                           (dispatch! [::set-doc-status-index- status-index])
                                           )})
    db))

(reg-event-db
  ::sign-in
  (fn-traced [db [_]]
    (store/sign-in)
    db))

(reg-event-db
  ::sign-out
  (fn-traced [db [_]]
    (store/sign-out)
    db))

(reg-event-db
  ::online-status
  (fn-traced [{:keys [online-status] :as db} [_ status]]
    (assert (#{:online :syncing :synced :uploading :downloading :error} status)) ;false = offline
    (assoc db :online-status (and online-status status))))

(reg-event-db
  ::signed-in
  (fn-traced [{doc :doc :as db} [_ signed-in?]]
    (when signed-in?
      (store/trash-files-pending! {:on-complete #(dispatch! [::sync-drive-file doc])})
      )
    (assoc db :online-status (and signed-in? :online))
    ))

(reg-event-db
  ;private handler
  ::set-doc-status-index-
  (fn-traced [db [_ doc-index]]
    (assoc db :doc-file-index doc-index)))

(defn verified-open-items
  "return only item (ids) that are present in the document"
  [doc items]
  (if (not-empty items)
    (let [valid-ids (set (keys doc))
          valid-keywords (set (reg/singleton-ids))
          ]
      (filter #(or (valid-keywords %) (valid-ids %)) items))
    ()))

(defn-traced read-doc-by-id [{{old-doc-id :doc-id :as doc} :doc :as db} [_ doc-id {:keys [open-items] :as app-state}]]
  (if (= old-doc-id doc-id)
    (assoc db :open-items (verified-open-items doc open-items))
    (do
      (go
        (let [doc (or (<? (store/<read-local-doc doc-id)) {:doc-id doc-id})
              persist-doc (or (<? (store/<read-persist-doc doc-id)) {})
              ]
          (dispatch! [::open-local-doc- doc persist-doc app-state])
          (dispatch! [::sync-drive-file doc])
          ))
      db)))

(reg-event-db
  ;private; response handler for read-local-doc
  ::read-doc-by-id-
  [update-nav-bar]
  read-doc-by-id
  )

(reg-event-db
  ::read-doc-by-id-nonav-
  read-doc-by-id
  )

(reg-event-db
  ;private; response handler for read-local-doc
  ::open-local-doc-
  [update-nav-bar]
  (fn-traced [{current-open :open-items :as db} [_ doc persist-doc {:keys [open-items]}]]
    (assoc db
      :doc doc
      :persist-doc persist-doc
      :open-items (verified-open-items doc (or open-items current-open))
      )))

(defn-traced new-local-doc [db _]
  (assoc db :open-items () :doc {:doc-id (utils/simple-uuid)}))

(reg-event-db
  ;private; response handler for read-local-doc
  ::new-local-doc-
  [update-nav-bar]
  new-local-doc)

(reg-event-db
  ::new-local-doc-nonav-
  new-local-doc)

(reg-event-db
  ;private; response handler for read-local-doc
  ::delete-doc
  [update-nav-bar]
  (fn-traced [{:keys [doc] :as db} _]
    (store/delete-doc! doc {:on-deleted #(dispatch! [::sync-doc-index])})
    ;Replace deleted doc with a new one:
    (dispatch! [::new-local-doc-])
    db))

(reg-event-db
  ::update-doc-
  (fn-traced [{:keys [doc open-items] :as db} [_ updated-doc old-doc status-message]]
    (let [doc-change-during-sync? (and old-doc (not (identical? doc old-doc)))]
      (when doc-change-during-sync?
        (warn log "Doc changed during file-synch")
        (dispatch! [::set-app-status status-message :error])
        )
      (when (and status-message (not doc-change-during-sync?))
        (dispatch! [::set-app-status status-message :info]))
      (dispatch! [::sync-doc-index])
      (assoc db :doc updated-doc :open-items (verified-open-items updated-doc open-items))
      )))

(reg-event-db
  ::window-focused
  (fn-traced [{:keys [doc saving?] :as db} _]
    ;Check for external changes and sync if required:
    ;localstore - by another browser window - Just replace doc if it has changed. conflicts with open editors
    ;must be resolved on save, just compare change times and disable save. warning could be given on focus.
    ;drive file - by another device.
    ;faster to check localstore first
    ;first merge localstore than sync with the drive file
    (when-not saving?
      (info log "window focused")
      (store/sync-localstore! doc {:on-ls-change  (fn [ls-doc]
                                                    (dispatch! [::update-doc- ls-doc doc "Updated from Localstore"])
                                                    ;re-enter to check for file changes
                                                    (dispatch! [::window-focused])
                                                    )
                                   :on-in-sync    (fn []
                                                    ;now sync with Drive
                                                    (dispatch! [::sync-drive-file doc]))
                                   :on-virgin-doc (fn []
                                                    ;new doc - don't save until changed
                                                    (info log "no localstore entry")
                                                    (dispatch! [::sync-doc-index])
                                                    )
                                   }))
    db))

;--------------------------------Panel selection-------------------------------

(reg-event-db
  ::open-tag-drawer
  (fn-traced [db [_ open?]]
    (assoc db :tag-drawer-open? open?)))

(reg-event-db
  ::open-index-drawer
  (fn-traced [db [_ open?]]
    (assoc db :index-drawer-open? open?)))

(reg-event-db
  ::select-index-view
  (fn-traced [db [_ view]]
    (assoc db :index-view view)))

;-------------------view-item---------------

(defn- editing? [db item-id]
  (let [e (get-in db [:editing item-id])]
    (and e (not (:accept-as e)))
    ))

(reg-event-db
  ::open-item
  [update-nav-bar]
  ;Add or move new item to head.
  ;Remove head item on reselection.
  (fn-traced [{:keys [open-items] :as db} [_ item-id {:keys [disable-toggle]}]]
    (assoc db :open-items (if (and (= (first open-items) item-id) (not (editing? db item-id)))
                            (if disable-toggle open-items (drop 1 open-items))
                            (conj (filter #(not= item-id %) open-items) item-id)))))

(reg-event-db
  ::open-tag-children
  [update-nav-bar]
  ;Add or move new item to head.
  ;Remove head item on reselection.
  (fn-traced [{:keys [doc open-items] :as db} [_ tag-id]]
    (assoc db :open-items (distinct (concat
                                      open-items
                                      (keep (fn [[k v]]
                                              (when (some (partial = tag-id) (:tags v))
                                                k)
                                              ) doc)
                                      )))))

;---------------close-item------------

(reg-event-db
  ::close-item
  [update-nav-bar]
  (fn-traced [{:keys [open-items editing] :as db} [_ item-id]]
    (assoc db :open-items (filter #(or (not= item-id %) (editing? db %))
                                  open-items))))

(reg-event-db
  ::close-other-items
  [update-nav-bar]
  (fn-traced [{:keys [open-items] :as db} [_ item-id]]
    (assoc db :open-items (filter #(or (= item-id %) (editing? db %))
                                  open-items))))

(reg-event-db
  ::close-all-items
  [update-nav-bar]
  (fn-traced [{:keys [open-items] :as db} _]
    (assoc db :open-items (filter #(editing? db %)
                                  open-items))))

(reg-event-db
  ::close-trashed
  [update-nav-bar]
  (fn-traced [{:keys [open-items doc] :as db} _]
    (assoc db :open-items (remove #(-> % doc :trashed) open-items))))

;---------------------edit-item---------

(reg-event-db
  ::start-edit
  (fn-traced [db [_ item-id]]
    (update-in db [:editing] (fn [editing]
                               ;remove completed edits and add new
                               (assoc (into {} (filter #(-> % second :accept-as not) editing))
                                 item-id {:source (or (get-in db [:doc item-id]) {})}
                                 )))))

(reg-event-fx
  ::start-edit-new-
  (fn-traced [{{doc :doc :as db} :db} [_ kind]]
    (let [item-id (new-item-id doc)
          iso-date-time (utils/iso-time-now)
          ]
      {:db         (assoc-in db [:doc item-id] {:id     item-id
                                                :kind   kind
                                                :create iso-date-time
                                                })
       :dispatch-n [[::open-item item-id]
                    [::start-edit item-id]
                    ]
       })))

(reg-event-fx
  ::start-edit-new-note
  (fn-traced [_ _]
    {:dispatch [::start-edit-new- :note]
     }))

(reg-event-db
  ::accept-edit
  (fn-traced [{doc :doc :as db} [_ item-id]]
    ;initiates a save:
    ; set editing to ::accept-edit > close-editor > editor sends event like ::new-content > save new content to doc
    (debug log ::accept-edit 'editing (get-in db [:editing]))
    (if (reg/rget item-id :suppress-doc-entry)
      (assoc-in db [:editing item-id :accept-as] item-id)
      (let [{:keys [create change]} (get doc item-id)
            {icreate :create ichange :change :as base-item} (get-in db [:editing item-id :source])
            external-change? (and (string? item-id) (not= (or change create) (or ichange icreate)))
            [item-id o-item-id doc] (if external-change?
                                      ;give changes to new item id
                                      (let [nid (new-item-id doc)
                                            doc (assoc doc nid (assoc base-item :id nid :conflict-id item-id))
                                            ]
                                        (dispatch! [::set-app-status "Edit conflict: item branched" :warn])
                                        [nid item-id doc])
                                      [item-id item-id doc])
            ]
        (info log ::saving)
        (when (not= item-id o-item-id) (dispatch! [::open-item item-id]))
        (-> db
            (assoc :doc (store/update-timestamp! doc item-id))
            (assoc-in [:editing o-item-id :accept-as] item-id)
            (assoc :saving? true)
            )))))

(reg-event-db
  ::cancel-edit
  (fn-traced [db [_ item-id]]
    (update-in db [:editing] dissoc item-id)))

;--------------------------update-doc--------------------------

(def save-on-doc-change
  (rc/->interceptor
    :id :save-on-doc-change
    :after (fn [{{{old-doc :doc} :db [event-id] :event} :coeffects
                 {{:keys [doc save-pending?]} :db}      :effects
                 :as                                    context
                 }]
             (when (and doc                                 ;required to support breakpoint events, why?
                        (not save-pending?)
                        (not= old-doc doc))
               (trace log :trigger-save event-id)
               (dispatch! [::save-doc]))
             context)))

(reg-event-db
  ::delete-item-permanent
  [save-on-doc-change]
  (fn-traced [{:keys [doc] :as db} [_ item-id]]
    (let [doc (dissoc doc item-id)
          doc (store/update-timestamp! doc item-id)
          ]
      ;(dispatch! [::cancel-edit item-id])
      (dispatch! [::close-item item-id])
      (assoc db :doc doc))))

(reg-event-db
  ::empty-trash
  [save-on-doc-change]
  (fn-traced [{:keys [doc open-items] :as db} _]
    (if-let [trashed-ids (not-empty (keep #(when (:trashed %) (:id %)) (vals doc)))]
      (let [doc (store/update-timestamps! doc trashed-ids)
            doc (apply dissoc doc trashed-ids)
            ]
        (assoc db :doc doc
                  :open-items (verified-open-items doc open-items)
                  ))
      db)))

(reg-event-db
  ::trash-item
  [save-on-doc-change]
  (fn-traced [{doc :doc :as db} [_ item-id]]
    (if (string? item-id)
      (let [doc (update doc item-id assoc :trashed true)
            doc (store/update-timestamp! doc item-id)
            ]
        (dispatch! [::cancel-edit item-id])
        (dispatch! [::close-item item-id])
        (assoc db :doc doc))
      db)))

(reg-event-db
  ::restore-item
  [save-on-doc-change]
  (fn-traced [{doc :doc :as db} [_ item-id]]
    (let [doc (update doc item-id dissoc :trashed)
          doc (store/update-timestamp! doc item-id)
          ]
      (assoc db :doc doc))))

(reg-event-db
  ::restore-all-trashed
  [save-on-doc-change]
  (fn-traced [db _]
    (update db :doc (fn [doc]
                      (let [trashed-ids (map :id (filter :trashed (vals doc)))
                            doc (reduce (fn [m id]
                                          (update m id dissoc :trashed)
                                          ) doc trashed-ids)
                            ]
                        (store/update-timestamps! doc trashed-ids)
                        )))))

(reg-event-db
  ;write content only after accept-edit
  ::new-content
  [save-on-doc-change]
  (fn-traced [db [_ item-id content]]
    ;potentially saves to new-id if original has external change.
    (if-let [item-id (get-in db [:editing item-id :accept-as])]
      (update-in db [:doc item-id] (fn [{:keys [mchange] old-content :content :as item}]
                                     (let [content (not-empty content)]
                                       (if (= old-content content)
                                         item
                                         (assoc item :change mchange
                                                     :content content
                                                     )))))
      db)))

(reg-event-db
  ;write content only after accept-edit
  ::new-title
  [save-on-doc-change]
  (fn-traced [db [_ item-id title]]
    ;potentially saves to new-id if original has external change.
    (if-let [item-id (get-in db [:editing item-id :accept-as])]
      (update-in db [:doc item-id] (fn [{:keys [mchange] old-title :title :as item}]
                                     (let [title (not-empty title)]
                                       (if (= old-title title)
                                         item
                                         (assoc item :change mchange
                                                     :title title
                                                     )))))
      db)))

(reg-event-db
  ;write options only after accept-edit
  ::options
  [save-on-doc-change]
  (fn-traced [db [_ options]]
    (if (and (get-in db [:editing :options]) (not-empty options))
      ;the initial save will just have the :change entry so need to add the id.
      (update-in db [:doc :options] merge {:id :options} options)
      db)))


(reg-event-db
  ;write options only after accept-edit
  ::set-log-config
  [save-on-doc-change]
  (fn-traced [db [_ log-config]]
    (if (get-in db [:editing :log-config])
      (do
        (trace log ::set-log-config log-config)
        (log/set-config! log-config))
      db)))


(reg-event-db
  ;write content only after accept-edit
  ::new-tags
  [save-on-doc-change]
  (fn-traced [{doc :doc :as db} [_ item-id tag-ids new-tags]]
    (if (get-in db [:editing item-id])
      (let [new-tags (when (not-empty new-tags)
                       (into {} (for [[id {:keys [title]}] (map vector
                                                                (utils/new-item-ids doc)
                                                                (vals new-tags))]
                                  [id {:title title, :id id, :kind :tag}]
                                  )))
            doc (merge doc new-tags)
            tags (not-empty (concat tag-ids (keys new-tags)))
            doc (if tags (assoc-in doc [item-id :tags] tags)
                         (update doc item-id dissoc :tags))
            doc (store/update-timestamps! doc (keys new-tags) #{:add-create?})
            ]
        (assoc db :doc doc))
      db)))

;------------------------file-ops-----------------------

(reg-event-db
  ::open-doc-file
  (fn-traced [db [_ doc]]
    (assoc db :doc doc)))

;----------------------------------------------------

(reg-event-db
  ::logger-config
  (fn-traced [db [_ logger-config]]
    (assoc db :logger-config logger-config)))

;-------------------------------------------------------

(reg-event-db
  ::toggle-start-move-items
  (fn-traced [db _]
    (update db :moving-items (fn [moving?] (and (not moving?)
                                                (some (complement keyword?) (:open-items db))
                                                )))))

(reg-event-db
  ::finish-move-items-
  [save-on-doc-change]
  (fn-traced [db [_ target-doc move-items]]
    (when (store/signed-in?)
      (store/sync-drive-file! target-doc
                              {:on-sync-status #(info log ::finish-move-items- 'target-sync-status %)
                               :on-complete    #(dispatch! [::sync-doc-index])
                               }))
    (dispatch! [::close-trashed])
    (update db :doc (fn [doc]
                      (let [doc (reduce (fn [doc id]
                                          (assoc-in doc [id :trashed] :moved)
                                          ) doc move-items)
                            doc (store/update-timestamps! doc move-items)
                            ]
                        doc)))))

(reg-event-db
  ::move-items
  (fn-traced [{:keys [moving-items open-items] source-doc :doc :as db} [_ target-doc-id]]
    (assert (and moving-items (not= (:doc-id source-doc) target-doc-id)))
    ;first sync target doc

    (when-let [;remove tags. Maybe the should be an option; moved-item tags are copied by default
               move-items (not-empty (filter #(and (string? %)
                                                   (-> % source-doc :kind (not= :tag))
                                                   ) open-items))]
      (if (store/signed-in?)
        (store/sync-drive-file!
          target-doc-id
          {:on-sync-status #(info log ::move-items 'target-sync-status %)
           :on-complete    #(dispatch! [::sync-doc-index])
           :on-synced-file (fn [target-doc]
                             (store/copy-items! source-doc target-doc move-items
                                                {:on-complete #(dispatch! [::finish-move-items- % move-items])
                                                 :on-error    (fn [error]
                                                                (warn log ::move-items 'copy error)
                                                                (dispatch! [::set-app-status "Copy failed" :warn])
                                                                )
                                                 }))
           })
        (store/copy-items!
          source-doc target-doc-id move-items
          {:on-complete #(dispatch! [::finish-move-items- % move-items])
           :on-error    (fn [error]
                          (warn log ::move-items 'copy-offline error)
                          (dispatch! [::set-app-status "Copy failed" :warn])
                          )
           })))
    (-> db
        (assoc :moving-items false)
        )))


