(ns app.events
  (:require
   [re-frame.core :as rc
    :refer [reg-event-db reg-event-fx]
     ;:rename {dispatch dispatch!}
    ]
    ;[cljs-uuid-utils.core :as uuid]
   [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
   [lib.log :as log :refer [trace debug info warn fatal pprintl trace-diff]]
   [lib.debug :as debug :refer [we wd]]
   [lib.utils :as utils :refer [time-now-ms iso-time->date-time new-item-id]]
   [lib.goog-drive :as drive]
   [lib.html-parse :as html-parse]
   [clojure.pprint :refer [cl-format]]
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

(def new-doc
  {:doc            nil
   :persist-doc    nil
   :persist-device nil
   :editing        {}
   :save-pending?  false
   :saving?        false
   })

(reg-event-db
  ::initialize-db
  (fn-traced [db _]
    (merge db {:tag-drawer-open?   false
               :index-drawer-open? false
               :local-file-dialog  nil
               :index-view         :index-history
               :open-items         nil
               :doc-file-index     {}
               :status             {}
               :online-status      false
               :keep-doc-in-sync?  true
               :platform           platform
               } new-doc)))

;----------app-status----------------

(def min-status-display-time-ms 5000)

(defn save-on-doc-change-interceptor! [old-db {:keys [doc save-pending?]}]
  (let [old-doc (:doc old-db)]
    ;(debug log 'save-on-doc-change-interceptor! (trace-diff 'old-doc old-doc 'doc doc {:include-both true}))
    ;(debug log 'save-on-doc-change-interceptor! 'save-pending? save-pending?)
    (when (and (not save-pending?)
               (not (= old-doc doc))
               (= (:doc-id old-doc) (:doc-id doc))
               (not-empty (dissoc doc :doc-id))
               )
      ;(debug log 'save-on-doc-change-interceptor! 'save-doc)
      (dispatch! [::save-doc]))))

(defn navbar-interceptor! [old-db db]
  (let [{old-open-items :open-items {old-doc-id :doc-id} :doc} old-db
        {:keys [open-items] {:keys [doc-id]} :doc} db
        ]
    (when (and doc-id (or (not= old-doc-id doc-id) (not= old-open-items open-items)))
      (let [query-str (when (not-empty open-items)
                        (str \? (map->query-string {:open (str/replace (pr-str open-items) \space \,)})))
            path (str \/ query-str \# doc-id)
            ]
        (info log 'navbar-interceptor! 'navigate! path)
        (navigate! path)
        ))))

(rc/clear-global-interceptor)                               ;seems to be required. Re-reg bug?
(rc/reg-global-interceptor
  (rc/->interceptor
    :id ::clear-app-status-on-event
    :before (fn [context]
              (trace log 'before (get-in context [:coeffects :event 0]))
              context)
    :after (fn [context]
             (let [handler-id (get-in context [:coeffects :event 0])
                   _ (trace log 'after handler-id)
                   old-db (get-in context [:coeffects :db])
                   db (get-in context [:effects :db])
                   on-change (fn [path] (let [v (get-in db path)]
                                          (when-not (identical? (get-in old-db path) v)
                                            v)))
                   ]
               #_(when-let [v (on-change [:online-status])]
                   (trace log 'global-interceptor 'online-status v))
               ;db & old-db can be nil for some handlers
               ;just db can be null for reg-event-fx
               (if (and db (not (identical? old-db db)))
                 (let [ms (get-in old-db [:status :time-ms])]
                   (when (and ms (> (time-now-ms) (+ ms min-status-display-time-ms)))
                     (dispatch! [::clear-app-status]))
                   (save-on-doc-change-interceptor! old-db db)
                   (navbar-interceptor! old-db db)
                   (when-let [persist-doc (on-change [:persist-doc])]
                     (store/<write-persist-doc (get-in db [:doc :doc-id]) persist-doc))
                   (when-let [persist-device (on-change [:persist-device])]
                     (store/<write-persist-device persist-device))
                   (trace log 'after-intercept handler-id)
                   context)
                 context)))))

(reg-event-db
  ::set-app-status
  (fn-traced [db [_ status type]]
    ;(debug log ::set-app-status status)
    (let [default-type :info]
      (assoc db :status (assoc
                         (cond
                           (map? status) (let [{:keys [type]} status] (assoc status :type (or type default-type)))
                           (utils/error? status) {:text (str status) :type :error}
                           :else {:text (str status) :type (or type default-type)})
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
       (trace log 'configure-navigation! doc-id)
       (if (string? (not-empty doc-id))
         (dispatch! [::read-doc-by-id- doc-id {:open-items (read-string open)}])
         (dispatch! [::new-local-doc-])                     ;navigation required to support url without doc-id
         )))
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

(reg-event-db
  ::sync-drive-file
  (fn-traced [{doc :doc :as db} [_ updated-doc {:keys [src]}]]
    (dispatch! [::online-status :online])
    (if (store/signed-in?)
      (store/sync-drive-file! updated-doc
                              {:src                    src
                               :on-sync-status         (fn [sync-status]
                                                         (when-let [status ({:overwrite-from-file :downloading
                                                                             :overwrite-file      :uploading
                                                                             :resolve-conflicts   :syncing
                                                                             } sync-status)]
                                                           (dispatch! [::online-status status])))
                               :on-success             (fn []
                                                         ;(:keep-doc-in-sync? db)
                                                         ;(dispatch! [::sync-drive-docs {:exclude-docs [(:doc-id updated-doc)]}])
                                                         (dispatch! [::online-status :synced])
                                                         )
                               :on-in-sync             #(dispatch! [::sync-doc-index])
                               :on-overwrite-from-file (fn [drive-doc]
                                                         (dispatch! [::update-doc- drive-doc doc "Updated from Drive"]))
                               :on-overwrite-file      (fn []
                                                         (dispatch! [::set-app-status "Drive updated" :info])
                                                         (dispatch! [::sync-doc-index]))
                               :on-conflicts-resolved  (fn [synched-doc]
                                                         (dispatch! [::update-doc- synched-doc doc "Synched with Drive"]))
                               :on-error               (fn [error]
                                                         (warn log ::sync-drive-file 'sync error)
                                                         (dispatch! [::online-status :error])
                                                         (dispatch!  [::set-app-status error]))
                               })
      (dispatch! [::sync-doc-index]))
    db))

(reg-event-db
  ::save-doc-with-sync-
  (fn-traced [{doc :doc :as db} _]
    (info log "saving doc...")
    ;write to localstore
    (store/write-local-doc! doc {:on-success #(dispatch! [::sync-drive-file doc {:src ::save-doc-with-sync-}])})
    (assoc db
      :save-pending? false
      :saving? false
      )))

(reg-event-db
  ::sync-doc-index
  (fn-traced [db _]
    (store/sync-doc-index! {:on-doc-status (fn [status-index]
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
    (let [status (and online-status status)]
      (trace log ::online-status status)
      (assoc db :online-status status))))

(reg-event-db
  ::signed-in
  (fn-traced [{doc :doc :as db} [_ signed-in?]]
    (when (and signed-in? (:doc-id doc))
      (store/trash-files-pending! {:on-success #(dispatch! [::sync-drive-file doc {:src ::signed-in}])}))
    (assoc db :online-status (and signed-in? :online))))

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

(reg-event-db
  ::read-doc-by-id-
  (fn-traced [{{old-doc-id :doc-id :as doc} :doc :as db} [_ doc-id {:keys [open-items]}]]
    (assert (string? doc-id))
    (if (= old-doc-id doc-id)
      (assoc db :open-items (verified-open-items doc open-items))
      (do
        (go
          (dispatch! [::read-doc-by-id-handler- {:doc-id      doc-id
                                                 :doc         (or (<? (store/<read-local-doc doc-id)) nil)
                                                 :persist-doc (or (<? (store/<read-persist-doc doc-id)) nil)
                                                 :open-items  open-items
                                                 }]))
        db))))

(reg-event-db
  ::read-doc-by-id-handler-
  (fn-traced [db [_ loaded]]
    (let [{:keys [doc-id doc persist-doc open-items]} loaded
          doc (or doc {:doc-id doc-id})
          ]
      (dispatch! [::sync-drive-file doc {:src ::read-doc-by-id-handler-}])
      (assoc db :doc doc
                :persist-doc (or persist-doc {})
                :open-items (verified-open-items doc open-items)
                ))))

(reg-event-db
  ;private; response handler for read-local-doc
  ::new-local-doc-
  (fn-traced [db _]
    (assoc db :open-items () :doc {:doc-id (utils/simple-uuid)})))

(reg-event-db
  ;private; response handler for read-local-doc
  ::delete-doc
  (fn-traced [{{:keys [doc-id]} :doc :as db} [_ options]]
    (store/delete-doc! doc-id (merge {:on-success #(dispatch! [::sync-doc-index])} options))
    ;Replace deleted doc with a new one:
    (dispatch! [::new-local-doc-])
    (merge db new-doc)))

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
  ::sync-local-new
  (fn-traced [{:keys [doc saving?] :as db} _]
    ;Check for external changes and sync if required:
    ;localstore - by another browser window - Just replace doc if it has changed. conflicts with open editors
    ;must be resolved on save, just compare change times and disable save. warning could be given on focus.
    ;drive file - by another device.
    ;faster to check localstore first
    ;first merge localstore than sync with the drive file
    (when-not saving?

      )
    db))

(reg-event-db
  ::sync-local
  (fn-traced [{:keys [doc saving?] :as db} _]
    ;Check for external changes and sync if required:
    ;localstore - by another browser window - Just replace doc if it has changed. conflicts with open editors
    ;must be resolved on save, just compare change times and disable save. warning could be given on focus.
    ;drive file - by another device.
    ;faster to check localstore first
    ;first merge localstore than sync with the drive file
    (when-not saving?
      (store/sync-localstore! doc {:on-ls-change  (fn [ls-doc]
                                                    (dispatch! [::update-doc- ls-doc doc "Updated from Localstore"])
                                                    ;re-enter to check for file changes
                                                    (dispatch! [::sync-local])
                                                    )
                                   :on-in-sync    (fn []
                                                    ;now sync with Drive
                                                    (dispatch! [::sync-drive-file doc {:src ::sync-local}]))
                                   :on-virgin-doc (fn []
                                                    ;new doc - don't save until changed
                                                    (info log "no localstore entry")
                                                    (dispatch! [::sync-doc-index])
                                                    )
                                   }))
    db))

(reg-event-db
  ::window-focused
  (fn-traced [db _]
    (info log "window focused")
    (dispatch! [::sync-local])
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
  ;Add or move new item to head.
  ;Remove head item on reselection.
  (fn-traced [{:keys [open-items] :as db} [_ item-id {:keys [disable-toggle]}]]
    (assoc db :open-items (if (and (= (first open-items) item-id) (not (editing? db item-id)))
                            (if disable-toggle open-items (drop 1 open-items))
                            (conj (filter #(not= item-id %) open-items) item-id)))))

(reg-event-db
  ::open-tag-children
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
  (fn-traced [{:keys [open-items editing] :as db} [_ item-id]]
    (assoc db :open-items (filter #(or (not= item-id %) (editing? db %))
                                  open-items))))

(reg-event-db
  ::close-other-items
  (fn-traced [{:keys [open-items] :as db} [_ item-id]]
    (assoc db :open-items (filter #(or (= item-id %) (editing? db %))
                                  open-items))))

(reg-event-db
  ::close-all-items
  (fn-traced [{:keys [open-items] :as db} _]
    (assoc db :open-items (filter #(editing? db %)
                                  open-items))))

(reg-event-db
  ::close-trashed
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
  (fn-traced [{:keys [doc editing] :as db} [_ item-id]]
    ;initiates a save:
    ; set editing to ::accept-edit > close-editor > editor sends event like ::new-content > save new content to doc
    ;(debug log ::accept-edit 'editing (get-in db [:editing]))
    (if (and (keyword? item-id) (not (reg/rget item-id :has-doc-entry)))
      (assoc-in db [:editing item-id :accept-as] item-id)
      (let [{:keys [create change mchange]} (get doc item-id)
            {icreate :create ichange :change imchange :mchange :as base-item} (get-in db [:editing item-id :source])
            ;if entry has a create date assume it can be merged
            external-change? (not= (or mchange change create) (or imchange ichange icreate))
            [item-id o-item-id doc] (if (and external-change? (string? item-id))
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
        (assoc db
          :doc (store/update-timestamps! doc [item-id])
          :editing (assoc-in editing [o-item-id :accept-as] item-id)
          :saving? true
          )))))

(reg-event-db
  ::cancel-edit
  (fn-traced [db [_ item-id]]
    (update-in db [:editing] dissoc item-id)))

;--------------------------update-doc--------------------------

(reg-event-db
  ::delete-item-permanent
  (fn-traced [{:keys [doc doc-changes] :as db} [_ item-id]]
    (let [doc (dissoc doc item-id)]
      ;(dispatch! [::cancel-edit item-id])
      (dispatch! [::close-item item-id])
      (assoc db :doc (store/update-timestamps! doc [item-id])))))

(reg-event-db
  ::empty-trash
  (fn-traced [{:keys [doc doc-changes open-items] :as db} _]
    (if-let [trashed-ids (not-empty (keep #(when (:trashed %) (:id %)) (vals doc)))]
      (let [doc (apply dissoc doc trashed-ids)]
        (assoc db :doc (store/update-timestamps! doc trashed-ids)
                  :open-items (verified-open-items doc open-items)
                  ))
      db)))

(reg-event-db
  ::trash-item
  (fn-traced [{:keys [doc doc-changes] :as db} [_ item-id]]
    (if (string? item-id)
      (let [doc (update doc item-id assoc :trashed true)]
        (dispatch! [::cancel-edit item-id])
        (dispatch! [::close-item item-id])
        (assoc db :doc (store/update-timestamps! doc [item-id])))
      db)))

(reg-event-db
  ::restore-item
  (fn-traced [{:keys [doc doc-changes] :as db} [_ item-id]]
    (let [doc (update doc item-id dissoc :trashed)]
      (assoc db :doc (store/update-timestamps! doc [item-id])))))

(reg-event-db
  ::restore-all-trashed
  (fn-traced [{:keys [doc doc-changes] :as db} _]
    (let [trashed-ids (map :id (filter :trashed (vals doc)))
          doc (reduce (fn [doc id]
                        (update doc id dissoc :trashed)
                        ) doc trashed-ids)
          ]
      (assoc db :doc (store/update-timestamps! doc trashed-ids)))))

(reg-event-db
  ;write content only after accept-edit
  ::new-content
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
  (fn-traced [db [_ item-id title]]
    ;potentially saves to new-id if original has external change.
    (if-let [item-id (get-in db [:editing item-id :accept-as])]
      (update-in db [:doc item-id] (fn [{old-title :title :as item}]
                                     (let [title (not-empty title)]
                                       (if (= old-title title)
                                         item
                                         (assoc item :title title)))))
      db)))

(reg-event-db
  ::rename-file
  (fn-traced [db [_ options]]
    (store/rename-file! (get-in db [:doc :doc-id]) options {:on-success #(dispatch! [::sync-doc-index])})
    db))

(reg-event-db
  ;write options only after accept-edit
  ::options
  (fn-traced [db [_ options-update]]
    (if (and (get-in db [:editing :options]) (not-empty options-update))
      ;the initial save will just have the :change entry so need to add the id.
      (let [old-options (get-in db [:doc :options])
            options (merge old-options options-update)
            on-change (fn [path] (let [v (get-in options path)]
                                   (when (not= (get-in old-options path) v)
                                     v)))
            ]
        (when (or (on-change [:doc-title]) (on-change [:doc-subtitle]))
          (dispatch! [::rename-file options]))
        (update-in db [:doc :options] merge options))
      db)))

(reg-event-db
  ;write options only after accept-edit
  ::set-log-config
  (fn-traced [db [_ log-config]]
    (if (get-in db [:editing :log-config])
      (do
        (trace log ::set-log-config log-config)
        (log/set-config! log-config))
      db)))

(reg-event-db
  ;write content only after accept-edit
  ::new-tags
  (fn-traced [{:keys [doc] :as db} [_ item-id tag-ids new-tags]]
    (if (get-in db [:editing item-id])
      (let [new-tags (when (not-empty new-tags)
                       (into {} (for [[id {:keys [title]}] (map vector
                                                                (utils/new-item-ids doc)
                                                                (vals new-tags))]
                                  [id {:title title, :id id, :kind :tag}]
                                  )))
            new-ids (keys new-tags)
            doc (merge doc new-tags)
            tags (not-empty (concat tag-ids new-ids))
            doc (if tags (assoc-in doc [item-id :tags] tags)
                         (update doc item-id dissoc :tags))
            ]
        (assoc db :doc (store/update-timestamps! doc new-ids)))
      db)))

;------------------------file-ops-----------------------

(reg-event-db
  ::open-doc-file
  (fn-traced [db [_ content]]
    (let [doc (store/decode content)]
      #_(store/open-local-file! db doc {:on-success #(dispatch! [::finish-open-doc-file- doc])
                                      :local-file-dialog-open? #()
                                      ;:on-error #()
                                      })
      (store/open-local-file! db doc {:on-show-dialog #(dispatch! [::open-doc-file-dialog- %])
                                      :on-open-doc #(::finish-open-doc-file- %)
                                      :on-error (fn [error]
                                                  (warn log ::sync-drive-file 'sync error)
                                                  (dispatch!  [::set-app-status error]))}))
             ))

(reg-event-db
 ::open-doc-file-dialog-
 (fn-traced [db [_ doc]]
            (assoc db :local-file-dialog {})))

#_(reg-event-db
 ::update-db
 (fn-traced [db [_ update]]
            (update db)))

(reg-event-db
 ::finish-open-doc-file-
 (fn-traced [db [_ doc {:keys [new-doc-id?]}]]
            (let [doc (if new-doc-id?
                        (assoc doc :doc-id (utils/simple-uuid))
                        doc)]
              (assoc db
                     :doc doc
                     :persist-doc nil
                     :open-items (verified-open-items doc (:open-items db))))))

;----------------------------------------------------

(reg-event-db
  ::logger-config
  (fn-traced [db [_ logger-config]]
    (assoc db :logger-config logger-config)))

;-----------------move-items-------------------------

(reg-event-db
  ::toggle-start-move-items
  (fn-traced [db _]
    (update db :moving-items? (fn [moving?] (boolean (and (not moving?)
                                                          (some string? (:open-items db))
                                                          ))))))

(reg-event-db
  ::finish-move-items-
  (fn-traced [{:keys [doc] :as db} [_ target-doc move-items]]
    ;(debug log ::finish-move-items- 'target-doc (pprintl target-doc))
    (when (store/signed-in?)
      (store/sync-drive-file! target-doc
                              {:on-sync-status #(info log ::finish-move-items- 'target-sync-status %)
                               :on-success     #(dispatch! [::sync-doc-index])
                               }))
    (dispatch! [::close-trashed])
    ;trash moved items
    (let [doc (reduce (fn [doc id]
                        (assoc-in doc [id :trashed] :moved)
                        ) doc move-items)]
      (assoc db :doc (store/update-timestamps! doc move-items)))))

(reg-event-db
  ::move-items
  (fn-traced [{:keys [moving-items? open-items] source-doc :doc :as db} [_ target-doc-id]]
    (assert (and moving-items? (not= (:doc-id source-doc) target-doc-id)))
    ;first sync target doc

    (when-let [;exclude source tags; moved-item tags are copied by default.
               move-items (not-empty (filter #(and (string? %)
                                                   (-> % source-doc :kind (not= :tag))
                                                   ) open-items))]
      (if (store/signed-in?)
        (store/sync-drive-file!
          target-doc-id
          {:src            ::move-items
           :on-sync-status #(info log ::move-items 'target-sync-status %)
           :on-success     #(dispatch! [::sync-doc-index])
           :on-synced-file (fn [target-doc]
                             (store/copy-items! source-doc target-doc move-items
                                                {:on-success #(dispatch! [::finish-move-items- % move-items])
                                                 :on-error   (fn [error]
                                                               (warn log ::move-items 'copy error)
                                                               (dispatch! [::set-app-status "Copy failed" :warn])
                                                               )
                                                 }))
           })
        (store/copy-items!
          source-doc target-doc-id move-items
          {:on-success #(dispatch! [::finish-move-items- % move-items])
           :on-error   (fn [error]
                         (warn log ::move-items 'copy-offline error)
                         (dispatch! [::set-app-status "Copy failed" :warn])
                         )
           })))
    (-> db
        (assoc :moving-items? false)
        )))

;===============================================================
;------------------debug-support---------------------

(reg-event-db
 ::refresh-drive-token
 (fn-traced [db _]
    (store/refresh-drive-token! {:on-success #(dispatch! [::set-app-status "Drive token refreshed"])
                                 :on-error (fn [error]
                                             (dispatch! [::set-app-status error])
                                             (dispatch! [::online-status :error]))
                                 })
     db))

(reg-event-db
  ::dump-doc-meta
  (fn-traced [{doc :doc :as db} _]
    ;(go (utils/throw-error "e1"))
    (let [doc-meta (select-keys doc (filter keyword? (keys doc)))]
      (println "Doc keyword entries:")
      (pprint doc-meta)
      )
    db))

(reg-event-db
  ::dump-doc
  (fn-traced [{doc :doc :as db} _]
    (pprint doc)
    db))

(defn fix-content [content]
  (if-let [[tag attrs children] (ui-utils/normalize content)]
    (let [attrs (when attrs
                  (into {} (map (fn [[k v :as e]]
                                  (cond
                                    (and (= (name k) "style") (string? v)) [k (html-parse/parse-attrs v)]
                                    :else e)
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
    (doseq [[k v] doc]
      (when-not (or (string? k) (keyword? k))
        (println ::doc-issue-key [k v])
        )
      (when (and (string? k) (-> v :kind not))
        (println ::doc-issue-kind [k v])
        )
      (when (and (string? k) (not= k (:id v)))
        (println ::doc-issue-id [k v])
        ))
    db))

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
          fix-style #(into {} (for [[k v :as e] % #_(select-keys doc ["nusb" "nv1z" "nuru"])]
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
                                  )) % %)
          fix-ids #(reduce-kv (fn [doc k _v]
                                (if (or (string? k) (keyword? k))
                                  doc
                                  (dissoc doc k))
                                ) % %)
          fix-kind #(reduce-kv (fn [doc k v]
                                 (if (and (string? k) (-> v :kind not))
                                   (dissoc doc k)
                                   doc)
                                 ) % %)
          ]
      ;(pprint (fix-style))
      (update db :doc
              (fn [doc]
                (-> doc fix-map fix-kind fix-ids)
                ;(dissoc doc :mchange)
                )
              )
      ;(assoc-in db [:doc "nv8k" :content] fix)
      ;(assoc db :doc (merge doc (fix-style)))
      )))

(reg-event-db
  ::dump-file-meta
  (fn-traced [{{:keys [doc-id]} :doc :as db} _]
    (go
      (let [{:keys [file-id] :as idx} (get (<? (store/<read-local-index)) doc-id)
            _ (println ::dump-file-meta :doc-d doc-id :file-id file-id :idx idx)
            ;meta (<? (store/<file-meta file-id [:modifiedTime]))
            ;meta (<? (store/<file-meta file-id))
            meta (<? (store/<find-file-data doc-id))
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
      (let [local-index (<? (store/<read-local-index))]
        (println "\nlocal index entry:")
        (pprint local-index)
        (println "\nfiles data:")
        (pprint (<? (store/<read-file-data-list)))
        (println "this local index entry: " doc-id)
        (pprint (get local-index doc-id))
        (println))
      )
    db))

(reg-event-db
  ::debug-list-app-drive-files
  (fn-traced [db _]
    (go
      (let [files (<? (store/<list-app-drive-files {:fields "files(id, name, modifiedTime, trashed, appProperties)"
                                                    ;:name    "kgrsc300.ydn"
                                                    ;:trashed false
                                                    }))]
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
            response (<? (drive/<read-file-edn file-id))
            ]
        (info log ::debug-file-content response)
        ))
    db))

(reg-event-db
 ::debug-file-compress
 (fn-traced [db _]
            (go
              (let [;file-id (<? (store/<create-file "compress-test" nil))
                    value (fn [c] (.charCodeAt c 0))
                    file-id "1QDGeNA9aIWD7KDN60wVKv9r-FY5gZK8y"
                    read (<? (drive/<read-file-edn file-id))
                    read (or read (let [content {:en :lz
                                                 :d (-> db :doc pr-str store/compress)
                                                 :r (-> db :doc pr-str)}
                                        fields (<? (drive/<write-file-content file-id content {:content-type :edn}))]
                                    (debug log ::debug-file-compress 'write-fields fields)
                                    (<? (drive/<read-file-edn file-id))))
                    {:keys [d r]} read
                    good (-> r store/compress)
                    compare (filter identity (map (fn [dc rc]
                                                    (when (not= dc rc) 
                                                      (cl-format nil "file: ~s ~b; good: ~s ~b" dc (value dc) rc (value rc))
                                                      ;[dc rc (value dc) (value rc)]
                                                      )) d good))]
                
                ;(debug log ::debug-file-compress 'file-id file-id)
                ;(info log ::debug-file-compress  'read read)
                (info log ::debug-file-compress  'equal (= good d))
                (info log ::debug-file-compress  'equal (pprintl {:d d :g good}))
                (info log ::debug-file-compress {:d-count (-> d count)
                                                 :r-count (-> good count)})
                (info log ::debug-file-compress  'decompress-file (-> d store/decompress))
                (info log ::debug-file-compress  'decompress-local (-> r store/compress store/decompress))
                (info log ::debug-file-compress  'compare (pprintl compare))
                ))
            db))

(reg-event-db
  ::debug-rename-file
  (fn-traced [db _]
    (store/rename-file! "klhurigk" {:title "My File Name"
                                    }
                        {:on-success #(debug log ::debug-rename-file 'response %)})
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
