(ns app.events
  (:require
    [re-frame.core :as rc
     :refer [reg-event-db reg-event-fx dispatch]
     ;:rename {dispatch dispatch!}
     ]
    ;[cljs-uuid-utils.core :as uuid]
    [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd]]
    [lib.utils :as utils :refer [iso-time->date-time find-next-item-num find-next-item-id]]
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
      ;(println :has-ids (contains? (get doc (first (filter string? (keys doc)))) :id))
      (println :has-ids (contains? (first (filter map? (vals doc))) :id))
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
  ::fix-doc
  (fn-traced [{doc :doc :as db} _]
    (println "fix-doc====")
    (dispatch! [::close-all-items])
    (let [doc-junk #(into {} (for [[k item :as e] doc
                                   :let [{:keys [id]} (and (map? item) item)]
                                   :when (not= k id)
                                   ]
                               e
                               ))
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
          ]
      ;(pprint (fix-style))
      ;(assoc db :doc doc)
      (assoc db :doc (merge doc (fix-style)))
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
      (pprint (<? (store/<read-files-data)))
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

(defn current-time-ms []
  (-> (utils/time-now) utils/date-time->ms))

(def min-status-display-time-ms 5000)

(rc/clear-global-interceptor)                               ;seems to be required. Re-reg bug?
(rc/reg-global-interceptor
  (rc/->interceptor
    :id ::clear-app-status-on-event
    :before (fn [context]
              (trace log :before (get-in context [:coeffects :event 0]))
              context)
    :after (fn [{{{{ms :time-ms} :status} :db} :coeffects :as context}]
             (trace log :after (get-in context [:coeffects :event 0]))
             (when (and ms (> (current-time-ms) (+ ms min-status-display-time-ms)))
               (dispatch! [::clear-app-status]))
             context)))

(reg-event-db
  ::set-app-status
  (fn-traced [db [_ status type]]
    ;(debug log ::set-app-status status)
    (let [default-type :info]
      (assoc db :status (assoc (if (map? status)
                                 (let [{:keys [type]} status] (assoc status :type (or type default-type)))
                                 {:text (str status) :type (or type default-type)})
                          :time-ms (current-time-ms))))))

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
     (info log :path-exists? path)
     true
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
    (trace log "sync-drive-file:")
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
                               ;:on-in-synch              #()
                               :on-overwrite-from-file (fn [drive-doc]
                                                         (dispatch! [::update-doc- drive-doc doc "Updated from Drive"]))
                               :on-overwrite-file      (fn []
                                                         (dispatch! [::set-app-status "Drive updated" :info]))
                               :on-conflicts-resolved  (fn [synched-doc]
                                                         (dispatch! [::update-doc- synched-doc doc "Synched with Drive"]))
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
    (trace log ::sync-doc-index)
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
    (assert (#{:online :syncing :synced :uploading :downloading} status)) ;false = offline
    (assoc db :online-status (and online-status status))))

(reg-event-db
  ::signed-in
  (fn-traced [{doc :doc :as db} [_ signed-in?]]
    (trace log ::signed-in signed-in?)
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
    (let [valid-set (into #{} (keys doc))]
      (filter #(or (keyword? %) (valid-set %)) items))
    ()))

(defn-traced read-doc-by-id [{{old-doc-id :doc-id :as doc} :doc :as db} [_ doc-id {:keys [open-items] :as app-state}]]
  (if (= old-doc-id doc-id)
    (assoc db :open-items (verified-open-items doc open-items))
    (do
      (go
        (let [doc (or (<? (store/<read-local-doc doc-id)) {:doc-id doc-id})]
          (dispatch! [::open-local-doc- doc app-state])
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
  (fn-traced [{current-open :open-items :as db} [_ doc {:keys [open-items]}]]
    (assoc db :doc doc :open-items (verified-open-items doc (or open-items current-open)))))

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
        (js/console.error "Doc changed during file-synch")
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

(reg-event-db
  ::open-item
  [update-nav-bar]
  ;Add or move new item to head.
  ;Remove head item on reselection.
  (fn-traced [{:keys [open-items editing] :as db} [_ item-id {:keys [disable-toggle]}]]
    (assoc db :open-items (if (and (= (first open-items) item-id) (not= true (get editing item-id)))
                            (if disable-toggle open-items (drop 1 open-items))
                            (conj (filter #(not= item-id %) open-items) item-id)))))

;---------------close-item------------

(reg-event-db
  ::close-item
  [update-nav-bar]
  (fn-traced [{:keys [open-items editing] :as db} [_ item-id]]
    (assoc db :open-items (filter #(or (not= item-id %) (true? (get editing %)))
                                  open-items))))

(reg-event-db
  ::close-other-items
  [update-nav-bar]
  (fn-traced [{:keys [open-items editing] :as db} [_ item-id]]
    (assoc db :open-items (filter #(or (= item-id %) (true? (get editing %)))
                                  open-items))))

(reg-event-db
  ::close-all-items
  [update-nav-bar]
  (fn-traced [{:keys [open-items editing] :as db} _]
    (assoc db :open-items (filter #(true? (get editing %))
                                  open-items))))

;---------------------edit-item---------

(reg-event-db
  ::start-edit
  (fn-traced [db [_ item-id]]
    (assoc-in db [:editing item-id] true)))

(reg-event-fx
  ::start-edit-new-
  (fn-traced [{{doc :doc :as db} :db} [_ kind]]
    (let [item-id (find-next-item-id doc)
          iso-date-time (utils/date-time->iso-time (utils/time-now))
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
  (fn-traced [{{:keys [doc-id] :as doc} :doc :as db} [_ item-id]]
    ;initiates a save:
    ; set editing to ::accept-edit > close-editor > editor sends event like ::new-content > save new content to doc
    (if (get doc item-id)                                   ;must not 'create' entries! eg :log-config
      (let [iso-date-time (utils/date-time->iso-time (utils/time-now))]
        (store/add-doc-change! doc-id item-id (get-in doc [item-id :change]))
        (let [doc (assoc-in doc [item-id :change] iso-date-time)
              doc (assoc doc :change iso-date-time)
              ]
          (info log ::saving)
          (-> db
              (assoc :doc doc)
              (assoc-in [:editing item-id] :accept-edit)
              (assoc :saving? true)
              ))
        )
      (assoc-in db [:editing item-id] :accept-edit))))

(reg-event-db
  ::cancel-edit
  (fn-traced [db [_ item-id]]
    (assoc-in db [:editing item-id] false)))

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
  (fn-traced [{:keys [doc open-items] :as db} [_ item-id]]
    (store/add-doc-change! (:doc-id doc) item-id (get-in doc [item-id :change]))
    (let [iso-date-time (utils/date-time->iso-time (utils/time-now))
          doc (assoc doc :change iso-date-time)
          doc (dissoc doc item-id)
          ]
      ;(dispatch! [::cancel-edit item-id])
      (dispatch! [::close-item item-id])
      (assoc db :doc doc
                :open-items (verified-open-items doc open-items)
                ))))

(reg-event-db
  ::empty-trash
  [save-on-doc-change]
  (fn-traced [{:keys [doc open-items] :as db} _]
    (if-let [trashed (not-empty (filter :trashed (vals doc)))]
      (let [iso-date-time (utils/date-time->iso-time (utils/time-now))
            doc (assoc doc :change iso-date-time)
            doc (apply dissoc doc (map :id trashed))
            ]
        (store/add-doc-changes! (:doc-id doc) trashed)
        (assoc db :doc doc
                  :open-items (verified-open-items doc open-items)
                  ))
      db)))

(reg-event-db
  ::trash-item
  [save-on-doc-change]
  (fn-traced [{{:keys [doc-id] :as doc} :doc :as db} [_ item-id]]
    (if (string? item-id)
      (let [iso-date-time (utils/date-time->iso-time (utils/time-now))]
        (store/add-doc-change! doc-id item-id (get-in doc [item-id :change]))
        (let [doc (assoc doc :change iso-date-time)
              doc (assoc-in doc [item-id :trashed] true)
              ]
          (dispatch! [::cancel-edit item-id])
          (dispatch! [::close-item item-id])
          (assoc db :doc doc))
        )
      db)))

(reg-event-db
  ::restore-item
  [save-on-doc-change]
  (fn-traced [{{:keys [doc-id] :as doc} :doc :as db} [_ item-id]]
    (let [iso-date-time (utils/date-time->iso-time (utils/time-now))]
      (store/add-doc-change! doc-id item-id (get-in doc [item-id :change]))
      (let [doc (assoc doc :change iso-date-time)
            doc (update doc item-id dissoc :trashed)
            ]
        (assoc db :doc doc))
      )))

(reg-event-db
  ;write content only after accept-edit
  ::new-content
  [save-on-doc-change]
  (fn-traced [db [_ item-id content]]
    (if (= (get-in db [:editing item-id]) :accept-edit)
      (assoc-in db [:doc item-id :content] (not-empty content))
      db)))

(reg-event-db
  ;write content only after accept-edit
  ::new-title
  [save-on-doc-change]
  (fn-traced [db [_ item-id title]]
    (if (= (get-in db [:editing item-id]) :accept-edit)
      (assoc-in db [:doc item-id :title] (not-empty title))
      db)))

(reg-event-db
  ;write options only after accept-edit
  ::options
  [save-on-doc-change]
  (fn-traced [db [_ options]]
    (if (and (= (get-in db [:editing :options]) :accept-edit) (not-empty options))
      ;the initial save will just have the :change entry so need to add the id.
      (update-in db [:doc :options] merge {:id :options} options)
      db)))

(reg-event-db
  ;write content only after accept-edit
  ::new-tags
  [save-on-doc-change]
  (fn-traced [{doc :doc :as db} [_ item-id tag-ids new-tags]]
    (if (= (get-in db [:editing item-id]) :accept-edit)
      (let [iso-date-time (utils/date-time->iso-time (utils/time-now))
            new-tags (when (not-empty new-tags)
                       (let [next-item-num (find-next-item-num doc)
                             new-ids (map utils/to-str-36
                                          (range next-item-num (+ next-item-num (count new-tags))))
                             ]
                         (into {} (for [[id {:keys [title]}] (map vector new-ids (vals new-tags))]
                                    [id {:title  title
                                         :id     id
                                         :kind   :tag
                                         :create iso-date-time
                                         }]))))
            doc (merge doc new-tags)
            tags (not-empty (concat tag-ids (keys new-tags)))
            doc (if tags (assoc-in doc [item-id :tags] tags)
                         (update doc item-id dissoc :tags))
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




