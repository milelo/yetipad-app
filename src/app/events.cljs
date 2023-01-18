(ns app.events
  (:require
   [lib.db :as db]
    ;[cljs-uuid-utils.core :as uuid]
   [lib.log :as log :refer [trace debug info warn fatal pprintl trace-diff]]
   [lib.debug :as debug :refer [we wd]]
   [lib.utils :as utils :refer [time-now-ms iso-time->date-time new-item-id]]
   [lib.goog-drive :as drive]
   [lib.html-parse :as html-parse]
   [clojure.pprint :refer [pprint cl-format]]
   [app.store :as store]
   [app.ui.utils :as ui-utils]
   [accountant.core :refer [configure-navigation! navigate! dispatch-current!]]
   [app.route :refer [path-decode map->query-string]]
   [cljs.reader :refer [read-string]]
   [clojure.data :refer [diff]]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [app.ui.registry :as reg]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [promesa.core :as p]
   ["react-device-detect" :refer [browserName browserVersion fullBrowserVersion osVersion
                                  deviceType engineName deviceDetect osName getUA
                                  mobileVendor mobileModel engineVersion]]))

(def log (log/logger 'app.events))

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
               :mobile-model         mobileModel})

(info log 'platform \newline (with-out-str (pprint platform)))

(def new-doc
  {:doc            nil
   :persist-doc    nil
   :persist-device nil
   :editing        {}
   :save-pending?  false
   :saving?        false})

(defn initialize-db []
  (db/update-db!
   {:label 'initialize-db}
   (fn [db]
     (merge db {:tag-drawer-open?   false
                :index-drawer-open? false
                :local-file-dialog  nil
                :index-view         :index-history
                :open-items         nil
                :doc-file-index     {}
                :status             {}
                :online-status      false
                :keep-doc-in-sync?  true
                :platform           platform}
            new-doc))))

;----------app-status----------------

(def min-status-display-time-ms 5000)

(declare clear-app-status)
(declare save-doc)

(defn run-later
  ([f delay-ms]
   (js/setTimeout f delay-ms))
  ([f] (run-later f 0)))

(defn save-on-doc-change-interceptor! [old-db {:keys [doc save-pending?]}]
  (let [old-doc (:doc old-db)]
    ;(debug log 'save-on-doc-change-interceptor! (trace-diff 'old-doc old-doc 'doc doc {:include-both true}))
    ;(debug log 'save-on-doc-change-interceptor! 'save-pending? save-pending?)
    (when (and (not save-pending?)
               (not (= old-doc doc))
               (= (:doc-id old-doc) (:doc-id doc))
               (not-empty (dissoc doc :doc-id)))
      ;(debug log 'save-on-doc-change-interceptor! 'save-doc)
      (save-doc))))

(defn navbar-interceptor! [old-db db]
  (let [{old-open-items :open-items {old-doc-id :doc-id} :doc} old-db
        {:keys [open-items] {:keys [doc-id]} :doc} db]
    (when (and doc-id (or (not= old-doc-id doc-id) (not= old-open-items open-items)))
      (let [query-str (when (not-empty open-items)
                        (str \? (map->query-string {:open (str/replace (pr-str open-items) \space \,)})))
            path (str \/ query-str \# doc-id)]
        (info log 'navbar-interceptor! 'navigate! path)
        (navigate! path)))))

(defn after-db-change! [old-db db]
  (run-later
   (let [on-change (fn [path] (let [v (get-in db path)]
                                (when-not (identical? (get-in old-db path) v)
                                  v)))]
     (when (and db (not (identical? old-db db)))
       (let [ms (get-in old-db [:status :time-ms])]
         (when (and ms (> (time-now-ms) (+ ms min-status-display-time-ms)))
           (clear-app-status))
         (save-on-doc-change-interceptor! old-db db)
         (navbar-interceptor! old-db db)
         (when-let [persist-doc (on-change [:persist-doc])]
           (store/write-persist-doc (get-in db [:doc :doc-id]) persist-doc))
         (when-let [persist-device (on-change [:persist-device])]
           (store/write-persist-device persist-device)))))))

;;Eliminate ASAP
(reset! db/after-db-change* after-db-change!)

(defn firex
  ([f]
   (db/firex f {:after! after-db-change!}))
  ([f {:keys [after!] :as options}]
   (db/firex f (assoc options :after! (fn [old-db db]
                                        (and after! (after! old-db db))
                                        (after-db-change! old-db db))))))

(defn set-app-status [status & [type]]
  (db/update-db!
   {:label 'set-app-status}
   (fn [db]
     ;(debug log ::set-app-status status)
     (let [default-type :info]
       (assoc db :status (assoc
                          (cond
                            (map? status) (let [{:keys [type]} status] (assoc status :type (or type default-type)))
                            (utils/error? status) {:text (str status) :type :error}
                            :else {:text (str status) :type (or type default-type)})
                          :time-ms (time-now-ms)))))))

(defn clear-app-status []
  (db/update-db!
   {:label 'clear-app-status}
   (fn [db]
     (assoc db :status {}))))

(defn- verified-open-items
  "return only item (ids) that are present in the document"
  [doc items]
  (if (not-empty items)
    (let [valid-ids (set (keys doc))
          valid-keywords (set (reg/singleton-ids))]
      (filter #(or (valid-keywords %) (valid-ids %)) items))
    ()))

;--------------------navigation---------------------------

(declare read-doc-by-id-handler-)

(defn read-doc-by-id-
  ([doc-id {:keys [open-items]}]
   (firex
    (fn [{{old-doc-id :doc-id :as doc} :doc :as db}]
      (assert (string? doc-id))
      (if (= old-doc-id doc-id)
        (assoc db :open-items (verified-open-items doc open-items))
        (do
          (p/let [local-doc (store/read-local-doc doc-id)
                  p-doc (store/read-persist-doc doc-id)]
            (read-doc-by-id-handler- {:doc-id      doc-id
                                      :doc         (or local-doc nil)
                                      :persist-doc (or p-doc nil)
                                      :open-items  open-items}))
          db))) {:label 'read-doc-by-id-}))
  ([doc-id] (read-doc-by-id- doc-id nil)))

(declare new-local-doc-)

(defonce init-status* (atom {}))
(defn init-once [id f]
  (when-not (get init-status* id)
    (let [r (f)]
      (swap! init-status* assoc id true)
      r)))

(defn init-navigation! []
  (configure-navigation!
   {:nav-handler
    (fn [path]
     ;Called with the url initially by dispatch-current!
      (let [{{:keys [open]} :query doc-id :fragment :as d} (path-decode path)]
        (trace log 'configure-navigation! doc-id)
        (if (string? (not-empty doc-id))
          (read-doc-by-id- doc-id {:open-items (read-string open)})
          (new-local-doc-)                     ;navigation required to support url without doc-id
          )))
    :path-exists?
    (fn [path]
     ;true stops page reload
     ;we want the page to reload if the back button hits an external page
      (let [exists (not= path "/")]
        (info log 'path-exists? path exists)
        exists))})
  (init-once :dispatch-current! dispatch-current!))

;----------------------localstore-------------------

(declare sync-drive-file)

(defn- save-doc-with-sync- []
  (firex
   (fn [{doc :doc :as db}]
     (info log "saving doc...")
     ;write to localstore
     (store/write-local-doc! doc {:on-success #(sync-drive-file doc {:src ::save-doc-with-sync-})})
     (assoc db
            :save-pending? false
            :saving? false)) {:label 'save-doc-with-sync-}))

(defn save-doc []
  (firex
   (fn [{:keys [save-pending?] :as db}]
     (when-not save-pending?
       {:db (assoc db :save-pending? true)
        :on-updated #(run-later save-doc-with-sync- 100)}))
   {:label 'save-doc}))

(defn- set-doc-status-index- [doc-index]
  (firex
   (fn [db]
     (assoc db :doc-file-index doc-index)) {:label 'set-doc-status-index-}))

(defn sync-doc-index! []
  (store/sync-doc-index! {:on-doc-status (fn [status-index]
                                           (set-doc-status-index- status-index))}))

(defn online-status [status]
  (db/update-db!
   {:label 'online-status}
   (fn [{:keys [online-status] :as db}]
     (assert (#{:online :syncing :synced :uploading :downloading :error} status)) ;false = offline
     (let [status (and online-status status)]
       (trace log ::online-status status)
       (assoc db :online-status status)))))

(defn- update-doc- [updated-doc old-doc status-message]
  (firex
   (fn [{:keys [doc open-items] :as db}]
     (let [doc-change-during-sync? (and old-doc (not (identical? doc old-doc)))]
       (when doc-change-during-sync?
         (warn log "Doc changed during file-synch")
         (set-app-status status-message :error))
       (when (and status-message (not doc-change-during-sync?))
         (set-app-status status-message :info))
       (sync-doc-index!)
       (assoc db :doc updated-doc :open-items (verified-open-items updated-doc open-items))))
   {:label 'update-doc-}))

(defn sync-drive-file [updated-doc {:keys [src]}]
  (firex
   (fn [{doc :doc :as db}]
     (online-status :online)
     (if (store/signed-in?)
       (store/sync-drive-file! updated-doc
                               {:src                    src
                                :on-sync-status         (fn [sync-status]
                                                          (when-let [status ({:overwrite-from-file :downloading
                                                                              :overwrite-file      :uploading
                                                                              :resolve-conflicts   :syncing} sync-status)]
                                                            (online-status status)))
                                :on-success             (fn []
                                                         ;(:keep-doc-in-sync? db)
                                                         ;(dispatch! [::sync-drive-docs {:exclude-docs [(:doc-id updated-doc)]}])
                                                          (online-status :synced))
                                :on-in-sync             sync-doc-index!
                                :on-overwrite-from-file (fn [drive-doc]
                                                          (update-doc- drive-doc doc "Updated from Drive"))
                                :on-overwrite-file      (fn []
                                                          (set-app-status "Drive updated" :info)
                                                          (sync-doc-index!))
                                :on-conflicts-resolved  (fn [synched-doc]
                                                          (update-doc- synched-doc doc "Synched with Drive"))
                                :on-error               (fn [error]
                                                          (warn log ::sync-drive-file 'sync error)
                                                          (online-status :error)
                                                          (set-app-status error))})
       (sync-doc-index!))
     db)
   {:label 'sync-drive-file}))

#_(defn sync-drive-file [updated-doc {:keys [src]}]
    (if (store/signed-in?)
      (store/sync-drive-file! updated-doc
                              {:src                    src
                               :on-sync-status         (fn [sync-status]
                                                         (when-let [status ({:overwrite-from-file :downloading
                                                                             :overwrite-file      :uploading
                                                                             :resolve-conflicts   :syncing} sync-status)]
                                                           (online-status status)))
                               :on-success             (fn []
                                                         ;(:keep-doc-in-sync? db)
                                                         ;(dispatch! [::sync-drive-docs {:exclude-docs [(:doc-id updated-doc)]}])
                                                         (online-status :synced))
                               :on-in-sync             sync-doc-index!
                               :on-overwrite-from-file (fn [drive-doc]
                                                         (update-doc- drive-doc doc "Updated from Drive"))
                               :on-overwrite-file      (fn []
                                                         (set-app-status "Drive updated" :info)
                                                         (sync-doc-index!))
                               :on-conflicts-resolved  (fn [synched-doc]
                                                         (update-doc- synched-doc doc "Synched with Drive"))
                               :on-error               (fn [error]
                                                         (warn log ::sync-drive-file 'sync error)
                                                         (online-status :error)
                                                         (set-app-status error))})
      (sync-doc-index!)))

;(def sign-in store/sign-in)
;(def sign-out store/sign-out)

(defn sign-in! []
  (drive/sign-in!))

(defn sign-out! []
  (drive/sign-out!))

(defn signed-in [signed-in?]
  (firex
   (fn [{doc :doc :as db}]
     (when (and signed-in? (:doc-id doc))
       (store/trash-files-pending! {:on-success #(sync-drive-file doc {:src ::signed-in})}))
     (assoc db :online-status (and signed-in? :online)))
   {:label 'signed-in}))

(defn read-doc-by-id-handler- [loaded]
  (firex
   (fn [db]
     (let [{:keys [doc-id doc persist-doc open-items]} loaded
           doc (or doc {:doc-id doc-id})]
       (sync-drive-file doc {:src ::read-doc-by-id-handler-})
       (assoc db 
              :doc doc
              :persist-doc (or persist-doc {})
              :open-items (verified-open-items doc open-items))))
   {:label 'read-doc-by-id-handler-}))

(defn- new-local-doc-
  "private response handler for read-local-doc"
  []
  (firex
   (fn [db]
     (assoc db :open-items () :doc {:doc-id (utils/simple-uuid)}))
   {:label 'new-local-doc-}))

(defn delete-doc [options]
  (firex
   (fn [{{:keys [doc-id]} :doc :as db}]
     (store/delete-doc! doc-id options {:on-success sync-doc-index!})
    ;Replace deleted doc with a new one:
     (new-local-doc-)
     (merge db new-doc))
   {:label 'delete-doc}))

(defn sync-local []
  (firex
   (fn [{:keys [doc saving?] :as db}]
         ;Check for external changes and sync if required:
    ;localstore - by another browser window - Just replace doc if it has changed. conflicts with open editors
    ;must be resolved on save, just compare change times and disable save. warning could be given on focus.
    ;drive file - by another device.
    ;faster to check localstore first
    ;first merge localstore than sync with the drive file
     (when-not saving?
       (store/sync-localstore! doc {:on-ls-change  (fn [ls-doc]
                                                     (trace log 'sync-local :on-ls-change)
                                                     (update-doc- ls-doc doc "Updated from Localstore")
                                                    ;re-enter to check for file changes
                                                     (sync-local))
                                    :on-in-sync    (fn []
                                                     (trace log 'sync-local :on-in-sync)
                                                    ;now sync with Drive
                                                     (sync-drive-file doc {:src ::sync-local}))
                                    :on-virgin-doc (fn []
                                                     (trace log 'sync-local :on-virgin-doc)
                                                    ;new doc - don't save until changed
                                                     (info log "no localstore entry")
                                                     (sync-doc-index!))}))
     db)
   {:label 'sync-local}))

(defn window-focused []
  (info log "window focused")
  (sync-local))

;--------------------------------Panel selection-------------------------------

(defn open-tag-drawer [open?]
  (firex (fn [db]
           (assoc db :tag-drawer-open? open?))))

(defn open-index-drawer [open?]
  (firex
   (fn [db]
     (assoc db :index-drawer-open? open?))
   {:label 'open-index-drawer}))

(defn select-index-view [view]
  (firex
   (fn [db]
     (assoc db :index-view view))
   {:label 'select-index-view}))

;-------------------view-item---------------

(defn- editing? [db item-id]
  (let [e (get-in db [:editing item-id])]
    (and e (not (:accept-as e)))))

(defn open-item
  ([item-id {:keys [disable-toggle]}]
   (firex
    (fn [{:keys [open-items] :as db}]
      (assoc db :open-items (if (and (= (first open-items) item-id) (not (editing? db item-id)))
                              (if disable-toggle open-items (drop 1 open-items))
                              (conj (filter #(not= item-id %) open-items) item-id))))))
  ([item-id] (open-item item-id nil)
             {:label 'open-item}))

(defn open-tag-children [tag-id]
  (firex
   (fn [{:keys [doc open-items] :as db}]
     (assoc db :open-items (distinct (concat
                                      open-items
                                      (keep (fn [[k v]]
                                              (when (some (partial = tag-id) (:tags v))
                                                k)) doc)))))))

;---------------close-item------------

(defn close-item [item-id]
  (firex (fn [{:keys [open-items editing] :as db}]
           (assoc db :open-items (filter #(or (not= item-id %) (editing? db %))
                                         open-items)))))

(defn close-other-items [item-id]
  (firex (fn [{:keys [open-items] :as db}]
           (assoc db :open-items (filter #(or (= item-id %) (editing? db %))
                                         open-items)))))

(defn close-all-items []
  (firex (fn [{:keys [open-items] :as db}]
           (assoc db :open-items (filter #(editing? db %)
                                         open-items)))))

(defn close-trashed []
  (firex
   (fn [{:keys [open-items doc] :as db}]
     (assoc db :open-items (remove #(-> % doc :trashed) open-items)))))

;---------------------edit-item---------

(defn start-edit [item-id]
  (firex
   (fn [db]
     (update-in db [:editing] (fn [editing]
                               ;remove completed edits and add new
                                (assoc (into {} (filter #(-> % second :accept-as not) editing))
                                       item-id {:source (or (get-in db [:doc item-id]) {})}))))))

(defn start-edit-new- [kind]
  (db/do-async
   (fn [{doc :doc}]
     (let [item-id (new-item-id doc)
           iso-date-time (utils/iso-time-now)]
       (db/update-db! (fn [db]
                     (assoc-in db [:doc item-id] {:id     item-id
                                                  :kind   kind
                                                  :create iso-date-time})))
       (open-item item-id)
       (start-edit item-id)))))

(defn start-edit-new-note []
  (println :start-edit-new-note)
  (start-edit-new- :note))

(defn accept-edit [item-id]
  (firex
   (fn [{:keys [doc editing] :as db}]
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
                                             doc (assoc doc nid (assoc base-item :id nid :conflict-id item-id))]
                                         (set-app-status "Edit conflict: item branched" :warn)
                                         [nid item-id doc])
                                       [item-id item-id doc])]
         (info log ::saving)
         (when (not= item-id o-item-id) (open-item item-id))
         (assoc db
                :doc (store/update-timestamps! doc [item-id])
                :editing (assoc-in editing [o-item-id :accept-as] item-id)
                :saving? true))))))

(defn cancel-edit [item-id]
  (firex
   (fn [db]
     (update-in db [:editing] dissoc item-id))))

;--------------------------update-doc--------------------------

(defn delete-item-permanent [item-id]
  (firex
   (fn [{:keys [doc] :as db}]
     (let [doc (dissoc doc item-id)]
       ;(dispatch! [::cancel-edit item-id])
       (close-item item-id)
       (assoc db :doc (store/update-timestamps! doc [item-id]))))))

(defn empty-trash []
  (firex
   (fn [{:keys [doc open-items] :as db}]
     (if-let [trashed-ids (not-empty (keep #(when (:trashed %) (:id %)) (vals doc)))]
       (let [doc (apply dissoc doc trashed-ids)]
         (assoc db :doc (store/update-timestamps! doc trashed-ids)
                :open-items (verified-open-items doc open-items)))
       db))))

(defn trash-item [item-id]
  (firex
   (fn [{:keys [doc] :as db}]
     (if (string? item-id)
       (let [doc (update doc item-id assoc :trashed true)]
         (run-later (fn []
                      (cancel-edit item-id)
                      (close-item item-id)))
         (assoc db :doc (store/update-timestamps! doc [item-id])))
       db))))

(defn restore-item [item-id]
  (firex
   (fn [{:keys [doc] :as db}]
     (let [doc (update doc item-id dissoc :trashed)]
       (assoc db :doc (store/update-timestamps! doc [item-id]))))))

(defn restore-all-trashed []
  (firex
   (fn [{:keys [doc] :as db}]
     (let [trashed-ids (map :id (filter :trashed (vals doc)))
           doc (reduce (fn [doc id]
                         (update doc id dissoc :trashed)) doc trashed-ids)]
       (assoc db :doc (store/update-timestamps! doc trashed-ids))))))

(defn new-content [item-id content]
  ;write content only after accept-edit
  (firex
   (fn [db]
     ;potentially saves to new-id if original has external change.
     (if-let [item-id (get-in db [:editing item-id :accept-as])]
       (update-in db [:doc item-id] (fn [{:keys [mchange] old-content :content :as item}]
                                      (let [content (not-empty content)]
                                        (if (= old-content content)
                                          item
                                          (assoc item :change mchange
                                                 :content content)))))
       db))))

(defn new-title [item-id title]
  ;write content only after accept-edit
  (firex
   (fn [db]
     ;potentially saves to new-id if original has external change.
     (if-let [item-id (get-in db [:editing item-id :accept-as])]
       (update-in db [:doc item-id] (fn [{old-title :title :as item}]
                                      (let [title (not-empty title)]
                                        (if (= old-title title)
                                          item
                                          (assoc item :title title)))))
       db))))

(defn rename-file [options]
  (firex
   (fn [db]
     (store/rename-file! (get-in db [:doc :doc-id]) options {:on-success sync-doc-index!})
     db)))

(defn options [options-update]
  ;write options only after accept-edit
  (firex
   (fn [db]
     (if (and (get-in db [:editing :options]) (not-empty options-update))
      ;the initial save will just have the :change entry so need to add the id.
       (let [old-options (get-in db [:doc :options])
             options (merge old-options options-update)
             on-change (fn [path] (let [v (get-in options path)]
                                    (when (not= (get-in old-options path) v)
                                      v)))]
         (when (or (on-change [:doc-title]) (on-change [:doc-subtitle]))
           (rename-file options))
         (update-in db [:doc :options] merge options))
       db))))

(defn set-log-config [log-config]
  ;write options only after accept-edit
  (firex
   (fn [db]
     (if (get-in db [:editing :log-config])
       (do
         (trace log ::set-log-config log-config)
         (log/set-config! log-config))
       db))
   {:label 'set-log-config}))

(defn new-tags [item-id tag-ids new-tags]
  (firex
   (fn [{:keys [doc] :as db}]
     (if (get-in db [:editing item-id])
       (let [new-tags (when (not-empty new-tags)
                        (into {} (for [[id {:keys [title]}] (map vector
                                                                 (utils/new-item-ids doc)
                                                                 (vals new-tags))]
                                   [id {:title title, :id id, :kind :tag}])))
             new-ids (keys new-tags)
             doc (merge doc new-tags)
             tags (not-empty (concat tag-ids new-ids))
             doc (if tags (assoc-in doc [item-id :tags] tags)
                     (update doc item-id dissoc :tags))]
         (assoc db :doc (store/update-timestamps! doc new-ids)))
       db))))

;------------------------file-ops-----------------------

(defn- open-doc-file-dialog- [doc]
  (firex
   (fn [db]
     (assoc db :local-file-dialog {}))))

(defn finish-open-doc-file- [doc {:keys [new-doc-id?]}]
  (firex
   (fn [db]
     (let [doc (if new-doc-id?
                 (assoc doc :doc-id (utils/simple-uuid))
                 doc)]
       (assoc db
              :doc doc
              :persist-doc nil
              :open-items (verified-open-items doc (:open-items db)))))))

(defn open-doc-file [content]
  (firex
   (fn [db]
     (let [doc (store/decode content)]
       #_(store/open-local-file! db doc {:on-success #(dispatch! [::finish-open-doc-file- doc])
                                         :local-file-dialog-open? #()
                                      ;:on-error #()
                                         })
       (store/open-local-file! db doc {:on-show-dialog #(open-doc-file-dialog- %)
                                       :on-open-doc #(finish-open-doc-file- % {})
                                       :on-error (fn [error]
                                                   (warn log ::sync-drive-file 'sync error)
                                                   (set-app-status error))})))))

;----------------------------------------------------

(defn logger-config [logger-config]
  (firex
   (fn [db]
     (assoc db :logger-config logger-config))
   {:label 'logger-config}))

;-----------------move-items-------------------------

(defn toggle-start-move-items []
  (firex
   (fn [db]
     (update db :moving-items? (fn [moving?] (boolean (and (not moving?)
                                                           (some string? (:open-items db)))))))))

(defn- finish-move-items- [target-doc move-items]
  (firex
   (fn [{:keys [doc] :as db}]
         ;(debug log ::finish-move-items- 'target-doc (pprintl target-doc))
     (when (store/signed-in?)
       (store/sync-drive-file! target-doc
                               {:on-sync-status #(info log ::finish-move-items- 'target-sync-status %)
                                :on-success     sync-doc-index!}))
     (close-trashed)
    ;trash moved items
     (let [doc (reduce (fn [doc id]
                         (assoc-in doc [id :trashed] :moved)) doc move-items)]
       (assoc db :doc (store/update-timestamps! doc move-items))))))

(defn move-items [target-doc-id]
  (firex
   (fn [{:keys [moving-items? open-items] source-doc :doc :as db}]
     (assert (and moving-items? (not= (:doc-id source-doc) target-doc-id)))
            ;first sync target doc
     (when-let [;exclude source tags; moved-item tags are copied by default.
                move-items (not-empty (filter #(and (string? %)
                                                    (-> % source-doc :kind (not= :tag))) open-items))]
       (if (store/signed-in?)
         (store/sync-drive-file!
          target-doc-id
          {:src            ::move-items
           :on-sync-status #(info log ::move-items 'target-sync-status %)
           :on-success     sync-doc-index!
           :on-synced-file (fn [target-doc]
                             (store/copy-items! source-doc target-doc move-items
                                                {:on-success #(finish-move-items- % move-items)
                                                 :on-error   (fn [error]
                                                               (warn log ::move-items 'copy error)
                                                               (set-app-status "Copy failed" :warn))}))})
         (store/copy-items!
          source-doc target-doc-id move-items
          {:on-success #(finish-move-items- % move-items)
           :on-error   (fn [error]
                         (warn log ::move-items 'copy-offline error)
                         (set-app-status "Copy failed" :warn))})))
     (-> db
         (assoc :moving-items? false)))))

;===============================================================

(defn got-access-token [token]
  (trace log 'got-access-token "token:" (-> token bean pprintl))
  (signed-in true)
  (firex
   (fn [db]
     (assoc db :signed-in? true))))


;===============================================================
;------------------debug-support---------------------

(defn refresh-drive-token []
  (store/refresh-drive-token! {:on-success #(set-app-status "Drive token refreshed")
                               :on-error (fn [error]
                                           (set-app-status error)
                                           (online-status :error))}))

(defn dump-doc-meta []
  (firex
   (fn [{doc :doc :as db}]
     ;(go (utils/throw-error "e1"))
     (let [doc-meta (select-keys doc (filter keyword? (keys doc)))]
       (println "Doc keyword entries:")
       (pprint doc-meta))
     db)))

(defn dump-doc []
  (firex
   (fn [{doc :doc :as db}]
     (pprint doc)
     db)))

(defn fix-content [content]
  (if-let [[tag attrs children] (ui-utils/normalize content)]
    (let [attrs (when attrs
                  (into {} (map (fn [[k v :as e]]
                                  (cond
                                    (and (= (name k) "style") (string? v)) [k (html-parse/parse-attrs v)]
                                    :else e)) attrs)))]
      (cond-> [tag]
        (and attrs (not= tag :br)) (conj attrs)
        children (into (map fix-content children))))
    content))

(defn check-doc []
  (firex
   (fn [{doc :doc :as db}]
     (println "==== check-doc ====")
    ;(dispatch! [::close-all-items])
     (doseq [[k v] doc]
       (when-not (or (string? k) (keyword? k))
         (println ::doc-issue-key [k v]))
       (when (and (string? k) (-> v :kind not))
         (println ::doc-issue-kind [k v]))
       (when (and (string? k) (not= k (:id v)))
         (println ::doc-issue-id [k v])))
     db)))

(defn fix-doc []
  (firex
   (fn [{doc :doc :as db}]
     (println "fix-doc====")
     (close-all-items)
     (let [doc-junk #(into {} (for [[k item :as e] doc
                                    :let [{:keys [id]} (and (map? item) item)]
                                    :when (not= k id)]
                                e))
           fix-style #(into {} (for [[k v :as e] % #_(select-keys doc ["nusb" "nv1z" "nuru"])]
                                 (if (#{:note :tag} (:kind v))
                                   (let [{:keys [content]} v
                                         fixed (not-empty (map fix-content content))
                                         v (if fixed
                                             (assoc v :content fixed)
                                             (dissoc v :content))]
                                     (when (not= fixed content)
                                       (open-item k))
                                     [k v])
                                   e)))
           fix-map #(reduce-kv (fn [doc k v]
                                 (if (or (keyword? k) (map? v))
                                   doc
                                   (do
                                     (println 'removed-item [k v])
                                     (dissoc doc k)))) % %)
           fix-ids #(reduce-kv (fn [doc k _v]
                                 (if (or (string? k) (keyword? k))
                                   doc
                                   (dissoc doc k))) % %)
           fix-kind #(reduce-kv (fn [doc k v]
                                  (if (and (string? k) (-> v :kind not))
                                    (dissoc doc k)
                                    doc)) % %)]
      ;(pprint (fix-style))
       (update db :doc
               (fn [doc]
                 (-> doc fix-map fix-kind fix-ids)
                ;(dissoc doc :mchange)
                 ))
      ;(assoc-in db [:doc "nv8k" :content] fix)
      ;(assoc db :doc (merge doc (fix-style)))
       ))))

(defn dump-file-meta []
  (firex
   (fn [{{:keys [doc-id]} :doc :as db}]
     (p/let [idx (store/read-local-index)
             {:keys [file-id] :as idxe} (get idx doc-id)
             _ (println ::dump-file-meta :doc-d doc-id :file-id file-id :idxe idxe)
             ;meta (store/file-meta file-id [:modifiedTime])
             ;meta (store/file-meta file-id)
             meta (store/find-file-data doc-id)]
       (println "File meta:")
       (pprint meta))
     db)))

(defn debug-find-file []
  (p/let [files (store/list-app-drive-files {:fields  "files(id, name, modifiedTime, trashed, appProperties)"
                                                    ;:name    "kgrsc300.ydn"
                                             :trashed false
                                             :doc-id  "kgrsc300"})]
    (pprint files)))

(defn dump-item-content [item-id]
  (firex
   (fn [db]
     (println "vvvvvvvvvvvvvvvv")
     (pprint (get-in db [:doc item-id]))
     (println "^^^^^^^^^^^^^^^^")
     db)))

(defn dump-index []
  (firex
   (fn [{{:keys [doc-id]} :doc :as db}]
     (p/let [local-index (store/read-local-index)
             file-data-list (store/read-file-data-list)]
       (println "\nlocal index entry:")
       (pprint local-index)
       (println "\nfiles data:")
       (pprint file-data-list)
       (println "this local index entry: " doc-id)
       (pprint (get local-index doc-id))
       (println))
     db)))

(defn debug-list-app-drive-files []
  (p/let [files (store/list-app-drive-files {:fields "files(id, name, modifiedTime, trashed, appProperties)"
                                                    ;:name    "kgrsc300.ydn"
                                                    ;:trashed false
                                             })]
    (pprint files)))

(defn debug-trash-file []
  (p/let [file-id "1zSgHNyQ3Z6h3lNkkFtppxmJ7ivvS5sht"
          file-id "1ZYqrs1QLvoB5P4GhV8Q9Hz0FsyDDPSIR"     ;"kgrsc300.ydn"
          response (drive/trash-file file-id)
          file-meta (store/file-meta file-id)]
    (println "Trash file")
    (pprint (-> response js->clj))
    (pprint file-meta)))

(defn debug-file-content []
  (let [file-id "1zSgHNyQ3Z6h3lNkkFtppxmJ7ivvS5sht"
        file-id "1ZYqrs1QLvoB5P4GhV8Q9Hz0FsyDDPSIR"     ;"kgrsc300.ydn"
        ]
    (p/let [response (drive/read-file-edn file-id)]
      (info log ::debug-file-content response))))

(defn debug-file-compress []
  (firex
   (fn [db]
     (p/let [;file-id (store/create-file "compress-test" nil)
             value (fn [c] (.charCodeAt c 0))
             file-id "1QDGeNA9aIWD7KDN60wVKv9r-FY5gZK8y"
             read (drive/read-file-edn file-id)
             read (or read (p/let [content {:en :lz
                                            :d (-> db :doc pr-str store/compress)
                                            :r (-> db :doc pr-str)}
                                   fields (drive/write-file-content file-id content {:content-type :edn})]
                             (debug log ::debug-file-compress 'write-fields fields)
                             (drive/read-file-edn file-id)))
             {:keys [d r]} read
             good (-> r store/compress)
             compare (filter identity (map (fn [dc rc]
                                             (when (not= dc rc)
                                               (cl-format nil "file: ~s ~b; good: ~s ~b" dc (value dc) rc (value rc))
                                                      ;[dc rc (value dc) (value rc)]
                                               ))d good))]

                ;(debug log ::debug-file-compress 'file-id file-id)
                ;(info log ::debug-file-compress  'read read)
       (info log ::debug-file-compress  'equal (= good d))
       (info log ::debug-file-compress  'equal (pprintl {:d d :g good}))
       (info log ::debug-file-compress {:d-count (-> d count)
                                        :r-count (-> good count)})
       (info log ::debug-file-compress  'decompress-file (-> d store/decompress))
       (info log ::debug-file-compress  'decompress-local (-> r store/compress store/decompress))
       (info log ::debug-file-compress  'compare (pprintl compare)))
     db)))

(defn debug-rename-file []
  (store/rename-file! "klhurigk" {:title "My File Name"}
                      {:on-success #(debug log ::debug-rename-file 'response %)}))

(defn debug-add-properties []
  (p/let [file-id "1R8JZWxzjLAWYCXIb5Y493AemAoj9G-8W"     ;"kgrsc300.ydn"
          file-id "1CQXBtftHN-cUxgC-Au9-VpuWKyJbLxjc"     ;
          response (drive/add-properties file-id {:doc-id "kgtbg5v1"})
          file-meta (store/file-meta file-id)]
    (println "Add property")
    (pprint (get-in response [:appProperties :doc-id]))
    (pprint response)
    (pprint file-meta)))

