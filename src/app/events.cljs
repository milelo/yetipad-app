(ns app.events
  (:require
   [lib.db :as db :refer [$do-sync do-async update-db!]]
    ;[cljs-uuid-utils.core :as uuid]
   [lib.log :as log :refer-macros [trace stack debug info warn error fatal] :refer [pprintl trace-diff]]
   [lib.debug :as debug :refer [we wd]]
   [lib.utils :as utils :refer-macros [fn-name] :refer [time-now-ms iso-time->date-time new-item-id]]
   [lib.goog-drive :as drive]
   [lib.localstore :as ls]
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
   [taoensso.truss :as truss :refer-macros [have have! have? have!?]]
   ["react-device-detect" :refer [browserName browserVersion fullBrowserVersion osVersion
                                  deviceType engineName deviceDetect osName getUA
                                  mobileVendor mobileModel engineVersion]])
  (:require-macros
   [lib.assertion :as assert]))

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

(info log 'platform \newline (pprintl platform))

(add-watch drive/online-status* ::status-watch
           (fn [k r {old-online? :online? old-status :status} {:keys [online? status]}]
             (when (or (not= old-online? online?) (not= old-status status))
               (update-db! ::status-watch
                           (fn [db]
                             (assoc db  :online? online? :online-status (-> status name keyword)))))))

(def clean-db
  {:doc            nil
   :persist-doc    nil
   :editing        {}
   :open-items     nil
   :save-pending?  false
   :saving?        false})

(defn initialize-db! []
  (db/update-db! 'initialize-db!
                 (fn [db]
                   (merge db {:tag-drawer-open?   false
                              :index-drawer-open? false
                              :local-file-dialog  nil
                              :index-view         :index-history
                              :doc-file-index     {}
                              :status             {}
                              :online?            false?
                              :online-status      nil
                              :sync-status        false
                              :keep-doc-in-sync?  true 
                              :persist-device     nil
                              :platform           platform}
                          clean-db))))

(defn init-persist-device!! []
  ($do-sync 'init-persist-device!!
            (fn [_db] 
              (p/let [persist-device (store/$read-persist-device)]
                (update-db! (fn [db]
                              (assoc db :persist-device persist-device)))))))

;----------app-status----------------

(def min-status-display-time-ms 5000)

(declare save-doc-with-sync!!)

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
      (db/update-db! 'save-on-doc-change-interceptor!
                     (fn [db]
                       (assoc db :save-pending? true)))
      (run-later save-doc-with-sync!! 100))))

(defn navbar-interceptor! [old-db db]
  (let [{old-open-items :open-items {old-doc-id :doc-id} :doc} old-db
        {:keys [open-items] {:keys [doc-id]} :doc} db]
    (when (and doc-id (or (not= old-doc-id doc-id) (not= old-open-items open-items)))
      (let [query-str (when (not-empty open-items)
                        (str \? (map->query-string {:open (str/replace (pr-str open-items) \space \,)})))
            path (str \/ query-str \# doc-id)]
        (info log 'navbar-interceptor! 'navigate! path)
        (navigate! path)))))

(defn clear-app-status! []
  (db/update-db! 'clear-app-status!
                 (fn [db]
                   (assoc db :status {}))))

(defn after-db-change! [old-db db]
  (run-later
   (let [on-change (fn [path] (let [v (get-in db path)]
                                (when-not (identical? (get-in old-db path) v)
                                  v)))]
     (when (and db (not (identical? old-db db)))
       (let [ms (get-in old-db [:status :time-ms])]
         (when (and ms (> (time-now-ms) (+ ms min-status-display-time-ms)))
           (clear-app-status!))
         (save-on-doc-change-interceptor! old-db db)
         (navbar-interceptor! old-db db)
         (when-let [persist-doc (on-change [:persist-doc])]
           ;Write persistent data to the doc
           (store/$write-persist-doc (get-in db [:doc :doc-id]) persist-doc))
         (when-let [persist-device (on-change [:persist-device])]
           ;Write persistent data to this device
           (store/$write-persist-device persist-device)))))))

(add-watch db/db* :db-monitor (fn [k r o n]
                                (when (not= o n)
                                  #_(when-not (= (:sync-status o) (:sync-status n))
                                      (stack log "sync-status:" {:old (:sync-status o) :new (:sync-status n)}))
                                  (after-db-change! o n))))

(defn set-app-status! [status & [type]]
  (assert (#{:info :warn :error nil} type))
  (db/update-db! 'set-app-status!
                 (fn [db]
                   (assoc db :status (store/app-status status type)))))

(defn- verified-open-items
  "return only item (ids) that are present in the document"
  [doc items]
  (if (not-empty items)
    (let [valid-ids (set (keys doc))
          valid-keywords (set (reg/singleton-ids))]
      (filter #(or (valid-keywords %) (valid-ids %)) items))
    ()))

;--------------------navigation---------------------------

(declare new-local-doc!)

(defonce init-status* (atom {}))

(defn init-once [id f]
  (when-not (get init-status* id)
    (let [r (f)]
      (swap! init-status* assoc id true)
      r)))

(declare read-doc-by-id!!)

(defn init-navigation! []
  (configure-navigation!
   {:nav-handler
    (fn [path]
     ;Called with the url initially by dispatch-current!
      (let [{{:keys [open]} :query doc-id :fragment :as d} (path-decode path)]
        (trace log 'configure-navigation! doc-id)
        (if (string? (not-empty doc-id))
          (read-doc-by-id!! doc-id {:open-items (read-string open)})
          (new-local-doc!)                     ;navigation required to support url without doc-id
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

(defn $sync-doc-index []
  (store/$sync-doc-index
   {:on-doc-status (fn [doc-index]
                     (update-db! '$sync-doc-index
                                 (fn [db]
                                   ;(trace log 'updated-index (pprintl doc-index))
                                   (assoc db :doc-file-index doc-index))))}))

(declare $sync-drive-file)

(defn- save-doc-with-sync!! []
  ($do-sync 'save-doc-with-sync!!
            (fn [{active-doc :doc :as db}]
              (info log "saving doc...")
              (p/do
                (store/$write-local-doc active-doc)
                ($sync-drive-file active-doc active-doc {:src ::save-doc-with-sync-})
                (update-db! (fn [db]
                              (assoc db
                                     :save-pending? false
                                     :saving? false))))) {:label 'save-doc-with-sync-}))

(defn sync-doc-index!! []
  (do-async 'sync-doc-index!
            $sync-doc-index))

(comment
  (:sync-status @db/db*))

(defn- $update-doc
  "Update the app doc"
  [updated-doc app-doc status-message]
  (update-db! '$update-doc
              (fn [{:keys [doc open-items] :as db}]
                (let [;doc-change-during-sync? (and app-doc (not (identical? doc app-doc)))
                      doc-change-during-sync? false]
                  (assoc db
                         :doc updated-doc
                         :open-items (verified-open-items updated-doc open-items)
                         :status (cond
                                   doc-change-during-sync?
                                   (do
                                     ;is this just when switching documents?
                                     (warn log "Doc changed during file-synch" \n
                                           (trace-diff :app-doc app-doc :updated-doc updated-doc))

                                     (store/app-status status-message :error))
                                   ;
                                   (and status-message
                                        (not doc-change-during-sync?))
                                   (store/app-status status-message :info))))))
  ($sync-doc-index))

(defn set-sync-status! [status]
  (db/update-db! 'set-sync-status!
                 (fn [db]
                   (assert (#{:online :syncing :synced :uploading :downloading :error} status)) ;false = offline
                   (trace log :status status)
                   (assoc db :sync-status status))))

(defn $sync-drive-file
  "app-doc: doc loaded in the app.
   local-doc: doc in the localstore shared by local doc instances on a common domain.
   drive-doc: doc stored on google-drive.
   "
  [app-doc local-doc-or-id {:keys [src]}]
  (p/do
    (trace log)
    (-> (store/$sync-drive-file local-doc-or-id
                                {:src                    src
                                 :on-sync-status         (fn [sync-status]
                                                           (when-let [status ({:overwrite-from-file :downloading
                                                                               :overwrite-file      :uploading
                                                                               :resolve-conflicts   :syncing} sync-status)]
                                                             (set-sync-status! status)))
                                 :on-in-sync             $sync-doc-index
                                 :on-overwrite-from-file (fn [drive-doc]
                                                           ($update-doc drive-doc app-doc "Updated from Drive"))
                                 :on-overwrite-file      (fn []
                                                           (set-app-status! "Drive updated" :info)
                                                           ($sync-doc-index))
                                 :on-conflicts-resolved  (fn [synched-doc]
                                                           ($update-doc synched-doc app-doc "Synched with Drive"))})
        (p/then (fn [r]
                  (set-sync-status! :synced)
                  r))
        (p/catch (fn [error]
                   (warn log 'sync error)
                   (set-sync-status! :error)
                   (set-app-status! (or (:message error) (:status error) (str error)) :error)
                   error
                   ;(p/rejected error)
                   )))))

(defn sign-in! []
  (drive/$ensure-authorized?))

(defn sign-out! []
  (drive/sign-out!))

(defn trash-files-pending!! [listeners]
  (db/$do-sync 'trash-files-pending!!
               store/$trash-files-pending listeners))

(defn read-doc-by-id!!
  ;""
  ([doc-id {:keys [open-items]}]
   (trace log)
   (do-async 'read-doc-by-id!!
             (fn  [{{old-doc-id :doc-id :as app-doc} :doc}]
               (assert (string? doc-id))
               (if (= old-doc-id doc-id)
                 (update-db! (fn [db] (assoc db :open-items (verified-open-items app-doc open-items))))
                 ($do-sync
                  (fn [_db]
                    (p/let [local-doc (store/$read-local-doc doc-id)
                            local-doc (or local-doc {:doc-id doc-id})
                            persist-doc (store/$read-persist-doc doc-id)
                            persist-doc (or persist-doc {})]
                      (update-db! (fn [db] (assoc db
                                                  :doc local-doc
                                                  :persist-doc persist-doc
                                                  :open-items (verified-open-items local-doc open-items))))
                      (when-not (= (drive/get-status) ::drive/initialising)
                        ($sync-drive-file app-doc local-doc {:src ::read-doc-by-id!!})))))))))
  ([doc-id] (read-doc-by-id!! doc-id nil)))

(defn- new-local-doc!
  []
  (update-db! 'new-local-doc!
              (fn [db]
                (merge db clean-db {:doc {:doc-id (utils/simple-uuid)}}))))

(defn delete-doc!! [options]
  ($do-sync 'delete-doc!!
            (fn [{{:keys [doc-id]} :doc}]
              (p/do
                (store/$delete-doc doc-id options)
           ;Replace deleted doc with a new one: 
                (new-local-doc!)
                ($sync-doc-index)))))

(defn sync-doc!! []
  ($do-sync 'sync-doc!!
            (fn [{:keys [saving?] app-doc :doc}]
              (debug log :nesting (:nesting db/*context*))
         ;Check for external changes and sync if required:
    ;localstore - by another browser window - Just replace doc if it has changed. conflicts with open editors
    ;must be resolved on save, just compare change times and disable save. warning could be given on focus.
    ;drive file - by another device.
    ;faster to check localstore first
    ;first merge localstore than sync with the drive file
              (when-not saving?
                (store/$sync<-localstore app-doc {:on-ls-change  (fn [ls-doc]
                                                                   (p/do
                                                                     (trace log :on-ls-change)
                                                                     ($update-doc ls-doc app-doc "Updated from Localstore")
                                                                ;re-enter to check for file changes
                                                                     (run-later sync-doc!!)))
                                                  :on-in-sync    (fn []
                                                                   (p/do
                                                                     (trace log :on-in-sync)
                                                                ;now sync with Drive
                                                                     ($sync-drive-file app-doc app-doc {:src ::sync-local})
                                                                 ;(run-later #(sync-drive-file!! app-doc {:src ::sync-local}));
                                                                     ))
                                                  :on-virgin-doc (fn []
                                                                   (trace log :on-virgin-doc)
                                                               ;new doc - don't save until changed
                                                                   (info log "no localstore entry")
                                                                   ($sync-doc-index)
                                                               ;(run-later #(sync-doc-index!!));
                                                                   )})))))


(defn window-focused []
  (info log)
  (sync-doc!!))

;--------------------------------Panel selection-------------------------------

(defn open-tag-drawer! [open?]
  (update-db! 'open-tag-drawer!
              (fn [db]
                (assoc db :tag-drawer-open? open?))))

(defn open-index-drawer! [open?]
  (update-db! 'open-index-drawer!
              (fn [db]
                (assoc db :index-drawer-open? open?))))

(defn select-index-view! [view]
  (update-db! 'select-index-view!
              (fn [db]
                (assoc db :index-view view))))

;-------------------view-item---------------

(defn- editing? [db item-id]
  (let [e (get-in db [:editing item-id])]
    (and e (not (:accept-as e)))))

(defn open-item!
  ([item-id {:keys [disable-toggle]}]
   (update-db! 'open-item!
               (fn [{:keys [open-items] :as db}]
                 (assoc db :open-items (if (and (= (first open-items) item-id) (not (editing? db item-id)))
                                         (if disable-toggle open-items (drop 1 open-items))
                                         (conj (filter #(not= item-id %) open-items) item-id))))))
  ([item-id] (open-item! item-id nil)))

(defn open-tag-children! [tag-id]
  (update-db! 'open-tag-children!
              (fn [{:keys [doc open-items] :as db}]
                (assoc db :open-items (distinct (concat
                                                 open-items
                                                 (keep (fn [[k v]]
                                                         (when (some (partial = tag-id) (:tags v))
                                                           k)) doc)))))))

;---------------close-item------------

(defn close-item! [item-id]
  (update-db! 'close-item!
              (fn [{:keys [open-items editing] :as db}]
                (assoc db :open-items (filter #(or (not= item-id %) (editing? db %))
                                              open-items)))))

(defn close-other-items! [item-id]
  (update-db! 'close-other-items!
              (fn [{:keys [open-items] :as db}]
                (assoc db :open-items (filter #(or (= item-id %) (editing? db %))
                                              open-items)))))

(defn close-all-items! []
  (update-db! 'close-all-items!
              (fn [{:keys [open-items] :as db}]
                (assoc db :open-items (filter #(editing? db %)
                                              open-items)))))

(defn close-trashed! []
  (update-db! 'close-trashed!
              (fn [{:keys [open-items doc] :as db}]
                (assoc db :open-items (remove #(-> % doc :trashed) open-items)))))

;---------------------edit-item---------

(defn start-edit! [item-id]
  (update-db! 'start-edit!
              (fn [db]
                (update-in db [:editing] (fn [editing]
                               ;remove completed edits and add new
                                           (assoc (into {} (filter #(-> % second :accept-as not) editing))
                                                  item-id {:source (or (get-in db [:doc item-id]) {})}))))))

(defn start-edit-new! [kind]
  (do-async 'start-edit-new!
            (fn [{doc :doc}]
              (let [item-id (new-item-id doc)
                    iso-date-time (utils/iso-time-now)]
                (db/update-db! (fn [db]
                                 (assoc-in db [:doc item-id] {:id     item-id
                                                              :kind   kind
                                                              :create iso-date-time})))
                (open-item! item-id)
                (start-edit! item-id)))))

(defn start-edit-new-note! []
  (trace log)
  (start-edit-new! :note))

(defn accept-edit! [item-id]
  (update-db! 'accept-edit!
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
                                                    (set-app-status! "Edit conflict: item branched" :warn)
                                                    [nid item-id doc])
                                                  [item-id item-id doc])]
                    (info log :saving)
                    (when (not= item-id o-item-id) (open-item! item-id))
                    (assoc db
                           :doc (store/update-timestamps! doc [item-id])
                           :editing (assoc-in editing [o-item-id :accept-as] item-id)
                           :saving? true))))))

(defn cancel-edit! [item-id]
  (update-db! 'cancel-edit!
              (fn [db]
                (update-in db [:editing] dissoc item-id))))

;--------------------------update-doc--------------------------

(defn delete-item-permanent!! [item-id]
  ($do-sync 'delete-item-permanent!!
            (fn [{:keys [doc]}]
              (let [doc (store/update-timestamps! (dissoc doc item-id) [item-id])]
       ;(dispatch! [::cancel-edit item-id])
                (close-item! item-id)
                (update-db! (fn [db] (assoc db :doc doc)))))))

(defn empty-trash!! []
  ($do-sync 'empty-trash!!
            (fn [{:keys [doc open-items] :as db}]
              (if-let [trashed-ids (not-empty (keep #(when (:trashed %) (:id %)) (vals doc)))]
                (let [doc (apply dissoc doc trashed-ids)
                      doc (store/update-timestamps! doc trashed-ids)]
                  (update-db! (fn [db]
                                (assoc db
                                       :doc doc
                                       :open-items (verified-open-items doc open-items)))))))))

(defn trash-item! [item-id]
  (db/update-db! 'trash-item!
                 (fn [{:keys [doc] :as db}]
                   (if (string? item-id)
                     (let [doc (update doc item-id assoc :trashed true)]
                       (assoc db :doc (store/update-timestamps! doc [item-id])))
                     db)))
  (cancel-edit! item-id)
  (close-item! item-id))

(defn restore-item! [item-id]
  (db/update-db! 'restore-item!
                 (fn [{:keys [doc] :as db}]
                   (let [doc (update doc item-id dissoc :trashed)]
                     (assoc db :doc (store/update-timestamps! doc [item-id]))))))

(defn restore-all-trashed! []
  (db/update-db!
   (fn [{:keys [doc] :as db}]
     (let [trashed-ids (map :id (filter :trashed (vals doc)))
           doc (reduce (fn [doc id]
                         (update doc id dissoc :trashed)) doc trashed-ids)]
       (assoc db :doc (store/update-timestamps! doc trashed-ids))))))

(defn new-content! [item-id content]
  ;write content only after accept-edit
  (db/update-db! 'new-content!
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

(defn new-title! [item-id title]
  ;write content only after accept-edit
  (db/update-db! 'new-title!
                 (fn [db]
     ;potentially saves to new-id if original has external change.
                   (if-let [item-id (get-in db [:editing item-id :accept-as])]
                     (update-in db [:doc item-id] (fn [{old-title :title :as item}]
                                                    (let [title (not-empty title)]
                                                      (if (= old-title title)
                                                        item
                                                        (assoc item :title title)))))
                     db))))

(defn rename-file!! [params]
  ($do-sync 'rename-file!!
            (fn [db]
              (store/$rename-file (get-in db [:doc :doc-id]) params)) {:on-success $sync-doc-index}))

(defn doc-options! [options-update]
  ;write options only after accept-edit
  (update-db! 'doc-options!
              (fn [db]
                (if (and (get-in db [:editing :options]) (not-empty options-update))
      ;the initial save will just have the :change entry so need to add the id.
                  (let [old-options (get-in db [:doc :options]) 
                        options (merge old-options options-update)
                        on-change (fn [path] (let [v (get-in options path)]
                                               (when (not= (get-in old-options path) v)
                                                 v)))]
                    (trace log :options-update options-update)
                    (when (or (on-change [:doc-title]) (on-change [:doc-subtitle]))
                      (rename-file!! options))
                    (update-in db [:doc :options] merge options))
                  db))))

(defn device-options! [options-update]
  (update-db! 'device-options!
              (fn [db]
                (update db :persist-device merge options-update))))

(defn set-log-config! [log-config]
  ;write options only after accept-edit
  (update-db! 'set-log-config!
              (fn [db]
                (if (get-in db [:editing :log-config])
                  (do
                    (trace log log-config)
                    (log/set-config! log-config))
                  db))))

(defn new-tags! [item-id tag-ids new-tags]
  (update-db! 'new-tags!
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

(defn- open-doc-file-dialog! [doc]
  (update-db! 'open-doc-file-dialog!
              (fn [db]
                (assoc db :local-file-dialog {}))))

(defn finish-open-doc-file! [doc {:keys [new-doc-id?]}]
  (update-db! 'finish-open-doc-file!!
              (fn [db]
                (let [doc (if new-doc-id?
                            (assoc doc :doc-id (utils/simple-uuid))
                            doc)]
                  (assoc db
                         :doc doc
                         :persist-doc nil
                         :open-items (verified-open-items doc (:open-items db)))))))

(defn open-doc-file!! [content]
  ($do-sync 'open-doc-file!!
            (fn [db]
              (let [doc (store/decode content)]
                #_(store/open-local-file! db doc {:on-success #(dispatch! [::finish-open-doc-file- doc])
                                                  :local-file-dialog-open? #()
                                      ;:on-error #()
                                                  })
                (store/$open-local-file db doc {:on-show-dialog #(open-doc-file-dialog! %)
                                                :on-open-doc #(finish-open-doc-file! % {})
                                                :on-error (fn [error]
                                                            (warn log 'sync error)
                                                            (set-app-status! error))})))))

;----------------------------------------------------

(defn logger-config! [logger-config]
  (update-db! 'logger-config!
              (fn [db]
                (assoc db :logger-config logger-config))))

;-----------------move-items-------------------------

(defn toggle-start-move-items! []
  (update-db!
   (fn [db]
     (update db :moving-items? (fn [moving?] (boolean (and (not moving?)
                                                           (some string? (:open-items db)))))))))

(defn- $finish-move-items! [{:keys [doc] :as db} target-doc move-items]
      ;(debug log ::finish-move-items- 'target-doc (pprintl target-doc))
  (p/let [authorized? (drive/$ensure-authorized?)]
    (when authorized?
      (p/do
        (store/$sync-drive-file target-doc
                                {:on-sync-status #(info log 'target-sync-status %)})
        ($sync-doc-index)))
    (update-db! '$finish-move-items!
                (fn [{:keys [open-items doc] :as db}]
                  (let [;trash moved items
                        doc-cleaned (reduce (fn [doc id]
                                              (assoc-in doc [id :trashed] :moved)) doc move-items)]
                    (assoc db
              ;close trashed
                           :open-items (remove #(-> % doc :trashed) open-items)
                           :doc (store/update-timestamps! doc-cleaned move-items)))))))

(defn move-items!! [target-doc-id]
  ($do-sync 'move-items!!
            (fn [{:keys [moving-items? open-items] source-doc :doc :as db}]
              (assert (and moving-items? (not= (:doc-id source-doc) target-doc-id)))
            ;first sync target doc
              (when-let [;exclude source tags; moved-item tags are copied by default.
                         move-items (not-empty (filter #(and (string? %)
                                                             (-> % source-doc :kind (not= :tag))) open-items))]
                (if (drive/allow-drive-request?)
                  (store/$sync-drive-file
                   target-doc-id
                   {:src            ::move-items
                    :on-sync-status #(info log 'target-sync-status %)
                    :on-success     $sync-doc-index
                    :on-synced-file (fn [target-doc]
                                      (store/copy-items! source-doc target-doc move-items
                                                         {:on-success #($finish-move-items! db % move-items)
                                                          :on-error   (fn [error]
                                                                        (warn log 'copy error)
                                                                        (set-app-status! "Copy failed" :warn))}))})
                  (p/do
                    (p/catch
                     (p/let [target-doc (store/$copy-items source-doc target-doc-id move-items)]
                       ($finish-move-items! db target-doc move-items))
                     (fn [error]
                       (warn log 'copy-offline error)
                       (set-app-status! "Copy failed" :warn)))
                    (update-db! (fn [db]
                                  (assoc db :moving-items? false)))))))))

;===============================================================

(defn on-authorized! [{:keys [token]}]
  (trace log "token:" (-> token bean pprintl))
  ($do-sync 'got-access-token!
            (fn [{doc :doc}]
              (when (drive/allow-drive-request?)
                (trace log "can sync drive file")
                (p/do (when (:doc-id doc)
                        (store/$trash-files-pending)
                        ($sync-drive-file doc (get doc :doc-id) {:src ::signed-in}))
                      #_(update-db! (fn [db]
                                      (assoc db :sync-status :online))))))))


;===============================================================
;------------------debug-support---------------------

(defn dump-doc-meta []
  (do-async 'dump-doc-meta
            (fn [{doc :doc :as db}]
     ;(go (utils/throw-error "e1"))
              (let [doc-meta (select-keys doc (filter keyword? (keys doc)))]
                (println "Doc keyword entries:")
                (pprint doc-meta)))))

(defn dump-doc []
  (do-async 'dump-doc
            (fn [{doc :doc :as db}]
              (pprint doc))))

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
  (do-async 'check-doc
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
  (do-async 'fix-doc
            (fn [{doc :doc :as db}]
              (println "fix-doc====")
              (close-all-items!)
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
                                                (open-item! k))
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
                (update-db! (fn [db]
                              (update db :doc
                                      (fn [doc]
                                        (-> doc fix-map fix-kind fix-ids)
                ;(dissoc doc :mchange)
                                        ))))
      ;(assoc-in db [:doc "nv8k" :content] fix)
      ;(assoc db :doc (merge doc (fix-style)))
                ))))

(defn dump-file-meta []
  (do-async 'dump-file-meta
            (fn [{{:keys [doc-id]} :doc :as db}]
              (p/let [idx (store/$read-local-index)
                      {:keys [file-id] :as idxe} (get idx doc-id)
                      _ (println ::dump-file-meta :doc-d doc-id :file-id file-id :idxe idxe)
             ;meta (store/file-meta file-id [:modifiedTime])
             ;meta (store/file-meta file-id)
                      meta (store/$find-file-data doc-id)]
                (println "File meta:")
                (pprint meta)))))

(defn debug-find-file []
  (p/let [files (store/$list-app-drive-files {:fields  "files(id, name, modifiedTime, trashed, appProperties)"
                                                    ;:name    "kgrsc300.ydn"
                                              :trashed false
                                              :doc-id  "kgrsc300"})]
    (pprint files)))

(defn dump-item-content [item-id]
  (do-async 'dump-item-content
            (fn [db]
              (println "vvvvvvvvvvvvvvvv")
              (pprint (get-in db [:doc item-id]))
              (println "^^^^^^^^^^^^^^^^"))))

(defn dump-index []
  (do-async 'dump-index
            (fn [{{:keys [doc-id]} :doc :as db}]
              (p/let [local-index (store/$read-local-index)
                      file-data-list (store/$read-drive-file-data-list)]
                (println "\nlocal index entry:")
                (pprint local-index)
                (println "\nfiles data:")
                (pprint file-data-list)
                (println "this local index entry: " doc-id)
                (pprint (get local-index doc-id))
                (println)))))

(defn debug-list-app-drive-files []
  (p/let [files (store/$list-app-drive-files {:fields "files(id, name, modifiedTime, trashed, appProperties)"
                                                    ;:name    "kgrsc300.ydn"
                                                    ;:trashed false
                                              })]
    (pprint files)))

(defn debug-trash-file []
  (p/let [file-id "1zSgHNyQ3Z6h3lNkkFtppxmJ7ivvS5sht"
          file-id "1ZYqrs1QLvoB5P4GhV8Q9Hz0FsyDDPSIR"     ;"kgrsc300.ydn"
          response (drive/$trash-file file-id)
          file-meta (store/$file-meta file-id)]
    (println "Trash file")
    (pprint (-> response js->clj))
    (pprint file-meta)))

(defn debug-file-content []
  (let [file-id "1zSgHNyQ3Z6h3lNkkFtppxmJ7ivvS5sht"
        file-id "1ZYqrs1QLvoB5P4GhV8Q9Hz0FsyDDPSIR"     ;"kgrsc300.ydn"
        ]
    (p/let [response (drive/$read-file-edn file-id)]
      (info log response))))

(defn debug-file-compress []
  (do-async 'debug-file-compress
            (fn [db]
              (p/let [;file-id (store/create-file "compress-test" nil)
                      value (fn [c] (.charCodeAt c 0))
                      file-id "1QDGeNA9aIWD7KDN60wVKv9r-FY5gZK8y"
                      read (drive/$read-file-edn file-id)
                      read (or read (p/let [content {:en :lz
                                                     :d (-> db :doc pr-str store/compress)
                                                     :r (-> db :doc pr-str)}
                                            fields (drive/$write-file-content file-id content {:content-type :edn})]
                                      (debug log 'write-fields fields)
                                      (drive/$read-file-edn file-id)))
                      {:keys [d r]} read
                      good (-> r store/compress)
                      compare (filter identity (map (fn [dc rc]
                                                      (when (not= dc rc)
                                                        (cl-format nil "file: ~s ~b; good: ~s ~b" dc (value dc) rc (value rc))
                                                      ;[dc rc (value dc) (value rc)]
                                                        ))d good))]

                ;(debug log 'file-id file-id)
                ;(info log 'read read)
                (info log  'equal (= good d))
                (info log  'equal (pprintl {:d d :g good}))
                (info log {:d-count (-> d count)
                           :r-count (-> good count)})
                (info log 'decompress-file (-> d store/decompress))
                (info log 'decompress-local (-> r store/compress store/decompress))
                (info log 'compare (pprintl compare))))))

(defn debug-rename-file!! []
  (db/$do-sync 'debug-rename-file!!
               (fn [] (store/$rename-file "klhurigk" {:title "My File Name"}))
               {:on-success #(debug log 'response %)}))

(defn debug-add-properties []
  (p/let [file-id "1R8JZWxzjLAWYCXIb5Y493AemAoj9G-8W"     ;"kgrsc300.ydn"
          file-id "1CQXBtftHN-cUxgC-Au9-VpuWKyJbLxjc"     ;
          response (drive/$add-properties file-id {:doc-id "kgtbg5v1"})
          file-meta (store/$file-meta file-id)]
    (println "Add property")
    (pprint (get-in response [:appProperties :doc-id]))
    (pprint response)
    (pprint file-meta)))

