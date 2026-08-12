(ns app.events
  (:require
   [lib.db :as db :refer [$enqueue! update-db!]]
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

(defn- run-now! [label f]
  (try
    (trace log 'start label)
    (f @db/db*)
    (catch :default e
      (error log label e))))

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
   :doc-load       nil
   :doc-session    nil
   :doc-revision   0
   :save-pending?  false
   :saving?        false})

(defn- document-snapshot
  [{:keys [doc doc-session doc-revision]}]
  {:doc-id (:doc-id doc)
   :session doc-session
   :revision doc-revision
   :doc doc})

(defn- snapshot-current?
  [{:keys [doc doc-session doc-revision]}
   {:keys [doc-id session revision]}]
  (and (= doc-id (:doc-id doc))
       (= session doc-session)
       (= revision doc-revision)))

(defn- document-update
  "Applies a short in-memory command and advances the revision if its active
  document changed. Document switches establish their session explicitly."
  [db f]
  (let [updated (or (f db) db)]
    (if (not= (:doc db) (:doc updated))
      (update updated :doc-revision (fnil inc 0))
      updated)))

(defn- update-document-db! [label f]
  (update-db! label #(document-update % f)))

(declare verified-open-items)

(defn- install-document
  [db doc persist-doc open-items]
  (assoc db
         :doc doc
         :doc-session (utils/simple-uuid)
         :doc-revision 0
         :persist-doc persist-doc
         :editing {}
         :open-items (verified-open-items doc open-items)))

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
  ($enqueue! 'init-persist-device!!
            (fn [_db] 
              (p/let [persist-device (store/$read-persist-device)]
                (update-db! (fn [db]
                              (assoc db :persist-device persist-device)))))))

(defn init-manifest!! []
  ($enqueue! 'init-manifest!!
            (fn [_db]
              (-> (p/let [f (js/fetch "manifest.json")
                          json (.json f)
                          manifest (->clj json)]
                    (info log 'app-version (:version manifest))
                    (update-db! (fn [db]
                                  (assoc db :manifest manifest))))
                  (p/catch (fn [e]
                             (warn log 'manifest-unavailable e)
                             nil))))))

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

(defn persist-active-session!
  "Persist the current document selection immediately for app relaunch."
  []
  (let [{:keys [doc open-items]} @db/db*
        doc-id (:doc-id doc)]
    (info log 'persist-active-session {:doc-id doc-id :open-items open-items})
    (when (string? doc-id)
      (let [result (store/$write-active-session
                    {:doc-id doc-id
                     :open-items (or open-items [])})]
        (info log 'persist-active-session-result result)
        result))))

(defn after-db-change! [old-db db]
  (when (and (string? (get-in db [:doc :doc-id]))
             (or (not= (get-in old-db [:doc :doc-id]) (get-in db [:doc :doc-id]))
                 (not= (:open-items old-db) (:open-items db))))
    (persist-active-session!))
  (run-later
   (fn []
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
             (store/$write-persist-device persist-device))))))))

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
(defonce initial-navigation?* (atom true))

(defn init-once [id f]
  (when-not (get init-status* id)
    (let [r (f)]
      (swap! init-status* assoc id true)
      r)))

(declare read-doc-by-id!!)
(declare open-index-drawer! select-index-view!)

(defn- restore-active-session!!
  []
  (p/let [session (store/$read-active-session)]
    (info log 'restore-active-session session)
    (if-let [doc-id (:doc-id session)]
      (read-doc-by-id!! doc-id {:open-items (:open-items session)})
      (new-local-doc!))))

(defn- initial-document-navigation!!
  "Resolve Android's retained startup URL against the persisted session. When
   both identify the same document, the persisted open-item list is newer
   than a potentially stale URL query. Different-document URLs remain valid
   deep links and keep their explicit query override."
  [path query fragment]
  (p/let [session (store/$read-active-session)
          same-document? (= fragment (:doc-id session))
          open-items (if same-document?
                       (:open-items session)
                       (when (contains? query :open)
                         (read-string (:open query))))]
    (info log 'initial-document-navigation
          {:path path :url-open (:open query) :session session
           :using-session? same-document?})
    (-> (read-doc-by-id!! fragment {:open-items open-items})
        (p/then (fn [snapshot]
                  (when (and snapshot (empty? open-items))
                    (select-index-view! :index-history)
                    (open-index-drawer! true)))))))

(defn init-navigation! []
  (configure-navigation!
   {:nav-handler
    (fn [path]
      ;Called with the url initially by dispatch-current!
      (let [{:keys [query fragment]} (path-decode path)
            open (:open query)
            doc-id fragment
            initial? (and (string? (not-empty doc-id))
                          @initial-navigation?*)]
          (when initial?
            (reset! initial-navigation?* false))
          (info log 'configure-navigation! {:path path :query query :fragment fragment})
          (cond
            (and initial? (string? (not-empty doc-id)))
            (initial-document-navigation!! path query doc-id)

            (string? (not-empty doc-id))
            (let [open-items (when (contains? query :open)
                               (read-string open))]
              (-> (read-doc-by-id!! doc-id {:open-items open-items})
                  (p/then (fn [snapshot]
                            (when (and snapshot (empty? open-items))
                              (select-index-view! :index-history)
                              (open-index-drawer! true))))))

            :else
            (restore-active-session!!))))
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
  (-> (p/let [doc-index (store/$sync-doc-index)]
        (update-db! '$sync-doc-index
                    (fn [db]
                      (assoc db :doc-file-index doc-index)))
        doc-index)
      (p/catch (fn [e]
                 (warn log 'sync-doc-index-unavailable e)
                 (:doc-file-index @db/db*)))))

(declare request-drive-sync!)

(defn- save-doc-with-sync!! []
  ($enqueue! 'save-doc-with-sync!!
             (fn [current-db]
               (let [{:keys [doc] :as snapshot} (document-snapshot current-db)]
                 (when (:doc-id snapshot)
                   (update-db! 'local-save-started
                               #(assoc % :save-pending? false :saving? true))
                   (-> (db/$enqueue-local! 'save-active-document
                                           (fn [_] (store/$write-local-doc doc)))
                       (p/then (fn [_]
                                 ($enqueue! 'local-save-complete
                                            (fn [db]
                                              (update-db! 'local-save-complete
                                                          (fn [latest]
                                                            (if (snapshot-current? latest snapshot)
                                                              (assoc latest :saving? false)
                                                              latest)))))))
                       (p/catch (fn [e]
                                  (update-db! 'local-save-error
                                              #(assoc %
                                                      :save-pending? true
                                                      :saving? false
                                                      :status (store/app-status e :error)))
                                  nil)))
                   (request-drive-sync! snapshot ::save-doc-with-sync-))
                 snapshot))))

(defn sync-doc-index!! []
  (db/$enqueue-drive! 'sync-doc-index! (fn [_] ($sync-doc-index))))

(comment
  (:sync-status @db/db*))

(defn- update-active-doc!
  "Replace the active document from within a serialized document operation."
  [updated-doc status-message]
  (update-document-db! '$update-doc
              (fn [{:keys [doc open-items] :as db}]
                (assoc db
                       :doc updated-doc
                       :open-items (verified-open-items updated-doc open-items)
                       :status (when status-message
                                 (store/app-status status-message :info))))))

(defn set-sync-status! [status]
  (db/update-db! 'set-sync-status!
                 (fn [db]
                   (assert (#{:online :syncing :synced :uploading :downloading :error} status)) ;false = offline
                   (trace log :status status)
                   (assoc db :sync-status status))))

(defn- sync-progress-status [drive-sync-status]
  ({:overwrite-from-file :downloading
    :overwrite-file :uploading
    :resolve-conflicts :syncing}
   drive-sync-status))

(defonce drive-sync-coordinator* (atom {:running? false :pending nil}))

(defn- request-latest-drive-rerun! []
  (let [snapshot (document-snapshot @db/db*)]
    (when (:doc-id snapshot)
      (swap! drive-sync-coordinator* assoc :pending snapshot))))

(defn- $commit-drive-candidate!
  [snapshot sync-plan candidate status-message]
  ($enqueue! 'commit-drive-candidate
             (fn [current-db]
               (when (snapshot-current? current-db snapshot)
                 (let [updated-db (document-update
                                   current-db
                                   (fn [{:keys [open-items] :as current-db}]
                                     (assoc current-db
                                            :doc candidate
                                            :open-items (verified-open-items candidate open-items)
                                            :status (store/app-status status-message :info))))
                       accepted-snapshot (document-snapshot updated-db)]
                   (update-db! 'commit-drive-candidate (constantly updated-db))
                   (db/$enqueue-local! 'record-drive-download
                                       (fn [_]
                                         (store/$record-drive-download sync-plan candidate)))
                   accepted-snapshot)))))

(defn- $perform-drive-sync [snapshot]
  (p/let [{:keys [status] :as sync-plan} (store/$prepare-drive-sync (:doc snapshot))
          _ (when-let [progress-status (sync-progress-status status)]
              (set-sync-status! progress-status))]
    (case status
      :in-sync snapshot

      :overwrite-from-file
      (p/let [{candidate :doc} (store/$fetch-drive-sync-candidate sync-plan)
              accepted ($commit-drive-candidate! snapshot sync-plan candidate "Updated from Drive")]
        (when-not accepted (request-latest-drive-rerun!))
        accepted)

      :overwrite-file
      (p/let [file-data (store/$upload-drive-sync-doc sync-plan (:doc snapshot))
              latest? (db/$enqueue-local! 'record-drive-upload
                                           (fn [_]
                                             (store/$record-drive-upload (:doc snapshot) file-data)))]
        (if (and latest? (snapshot-current? @db/db* snapshot))
          (do (set-app-status! "Drive updated" :info) snapshot)
          (do (request-latest-drive-rerun!) nil)))

      :resolve-conflicts
      (p/let [{candidate :doc} (store/$fetch-drive-sync-candidate sync-plan)
              accepted ($commit-drive-candidate! snapshot sync-plan candidate "Synched with Drive")]
        (if accepted
          (p/let [file-data (store/$upload-drive-sync-doc sync-plan candidate)
                  latest? (db/$enqueue-local! 'record-merged-drive-upload
                                               (fn [_]
                                                 (store/$record-drive-upload candidate file-data)))]
            (when-not latest? (request-latest-drive-rerun!))
            accepted)
          (do (request-latest-drive-rerun!) nil)))

      snapshot)))

(declare start-drive-sync!)

(defn- finish-drive-sync! [completed-snapshot]
  (let [next* (atom nil)]
    (swap! drive-sync-coordinator*
           (fn [{:keys [pending] :as state}]
             (if pending
               (do (reset! next* pending) (assoc state :pending nil))
               (assoc state :running? false))))
    (if-let [next-snapshot @next*]
      (start-drive-sync! next-snapshot)
      (when (and completed-snapshot
                 (snapshot-current? @db/db* completed-snapshot))
        (set-sync-status! :synced)))))

(defn- start-drive-sync! [snapshot]
  (set-sync-status! :syncing)
  (-> (db/$enqueue-drive! 'drive-document-sync
                          (fn [_]
                            (p/let [completed ($perform-drive-sync snapshot)
                                    _ ($sync-doc-index)]
                              completed)))
      (p/catch (fn [error]
                 (warn log 'drive-sync-error error)
                 (set-sync-status! :error)
                 (set-app-status! (or (:message error) (:status error) (str error)) :error)
                 nil))
      (p/then finish-drive-sync!)))

(defn request-drive-sync!
  ([snapshot] (request-drive-sync! snapshot nil))
  ([snapshot src]
   (trace log 'request-drive-sync src (:doc-id snapshot) (:revision snapshot))
   (when (and (:doc-id snapshot)
              (not= (drive/get-status) ::drive/initialising)
              (drive/allow-drive-request?))
     (let [start? (atom false)]
       (swap! drive-sync-coordinator*
              (fn [{:keys [running?] :as state}]
                (if running?
                  (assoc state :pending snapshot)
                  (do (reset! start? true)
                      (assoc state :running? true :pending nil)))))
       (when @start? (start-drive-sync! snapshot))))))

(defn $sync-drive-file [local-doc-or-id {:keys [src]}]
  (let [snapshot (if (map? local-doc-or-id)
                   (assoc (document-snapshot @db/db*) :doc local-doc-or-id
                          :doc-id (:doc-id local-doc-or-id))
                   (document-snapshot @db/db*))]
    (request-drive-sync! snapshot src)
    (p/resolved snapshot)))

(defn sign-in! []
  (drive/$ensure-authorized?))

(defn sign-out! []
  (drive/sign-out!))

(defn trash-files-pending!! []
  (db/$enqueue-drive! 'trash-files-pending!!
                      (fn [_] (store/$trash-files-pending))))

(defn read-doc-by-id!!
  ;""
  ([doc-id {:keys [open-items]}]
   (trace log)
   (assert (string? doc-id))
   (if (= doc-id (get-in @db/db* [:doc-load :doc-id]))
     (trace log 'document-load-already-pending doc-id)
     (let [token (utils/simple-uuid)]
       (update-db! 'request-doc-load
                   (fn [db]
                     (assoc db :doc-load {:token token :doc-id doc-id})))
       (let [local-read (db/$enqueue-local! 'read-local-document
                                            (fn [_]
                                              (p/let [local-doc (store/$read-local-doc doc-id)
                                                      persist-doc (store/$read-persist-doc doc-id)]
                                                {:doc (or local-doc {:doc-id doc-id})
                                                 :persist-doc (or persist-doc {})})))]
         (-> ($enqueue! 'commit-local-document
                        (fn [{{old-doc-id :doc-id :as app-doc} :doc :as current-db}]
                          (p/let [{local-doc :doc persist-doc :persist-doc} local-read]
                            (cond
                              (not= token (get-in current-db [:doc-load :token])) nil

                              (= old-doc-id doc-id)
                              (do
                                (update-db! 'finish-duplicate-doc-load
                                            #(assoc %
                                                    :doc-load nil
                                                    :open-items (verified-open-items app-doc open-items)))
                                (document-snapshot @db/db*))

                              :else
                              (let [loaded-db (-> current-db
                                                  (install-document local-doc persist-doc open-items)
                                                  (assoc :doc-load nil))
                                    snapshot (document-snapshot loaded-db)]
                                (update-db! 'commit-local-doc-load (constantly loaded-db))
                                snapshot)))))
           (p/then (fn [snapshot]
                     (when snapshot
                       (request-drive-sync! snapshot ::document-load))
                     snapshot))
         (p/catch (fn [e]
                    (update-db! 'doc-load-error
                                (fn [db]
                                  (if (= token (get-in db [:doc-load :token]))
                                    (assoc db :doc-load nil :status (store/app-status e :error))
                                    db)))
                    nil)))))))
  ([doc-id] (read-doc-by-id!! doc-id nil)))

(defn- new-local-doc!
  []
  ($enqueue! 'new-local-doc!
             (fn [_]
               (update-db! 'new-local-doc!
                           (fn [db]
                             (-> (merge db clean-db)
                                 (install-document {:doc-id (utils/simple-uuid)} nil nil)))))))

(defn delete-doc!! [{:keys [doc-id] :as options}]
  (let [current-doc-id (get-in @db/db* [:doc :doc-id])
        doc-id (or doc-id current-doc-id)]
    (when (string? doc-id)
      (-> (db/$enqueue-drive! 'delete-doc!!
                              (fn [_]
                                (p/do
                                  (store/$delete-doc doc-id options)
                                  ($sync-doc-index))))
          (p/then (fn [_]
                    (when (= doc-id (get-in @db/db* [:doc :doc-id]))
                      ($enqueue! 'new-local-doc-after-delete
                                 (fn [db]
                                   (update-db! 'new-local-doc-after-delete
                                               (fn [latest]
                                                 (-> (merge latest clean-db)
                                                     (install-document {:doc-id (utils/simple-uuid)} nil nil)))))))))))))

(defn sync-doc!! []
  (let [snapshot (document-snapshot @db/db*)]
    (when (:doc-id snapshot)
      (-> (db/$enqueue-local! 'check-local-document
                              (fn [_] (store/$sync<-localstore (:doc snapshot))))
          (p/then (fn [{:keys [status doc]}]
                    (if (= status :local-change)
                      ($enqueue! 'commit-external-local-document
                                 (fn [current-db]
                                   (when (snapshot-current? current-db snapshot)
                                     (update-active-doc! doc "Updated from Localstore")
                                     (request-drive-sync! (document-snapshot @db/db*) ::sync-local))))
                      (request-drive-sync! snapshot ::sync-local))))
          (p/catch (fn [e]
                     (set-app-status! e :error)
                     nil))))))


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

(defn new-document!
  "Create a new local document and show its history in the documents drawer."
  []
  (-> (new-local-doc!)
      (p/then (fn [_]
                (select-index-view! :index-history)
                (open-index-drawer! true)))))

;-------------------view-item---------------

(declare enqueue-db-update!)

(defn- editing? [db item-id]
  (let [e (get-in db [:editing item-id])]
    (and e (not (:accept-as e)))))

(defn open-item!
  ([item-id {:keys [disable-toggle]}]
   (enqueue-db-update! 'open-item!
               (fn [{:keys [open-items] :as db}]
                 (assoc db :open-items (if (and (= (first open-items) item-id) (not (editing? db item-id)))
                                         (if disable-toggle open-items (drop 1 open-items))
                                         (conj (filter #(not= item-id %) open-items) item-id))))))
  ([item-id] (open-item! item-id nil)))

(defn open-tag-children! [tag-id]
  (enqueue-db-update! 'open-tag-children!
              (fn [{:keys [doc open-items] :as db}]
                (assoc db :open-items (distinct (concat
                                                 open-items
                                                 (keep (fn [[k v]]
                                                         (when (some (partial = tag-id) (:tags v))
                                                           k)) doc)))))))

;---------------close-item------------

(defn close-item! [item-id]
  (enqueue-db-update! 'close-item!
              (fn [{:keys [open-items editing] :as db}]
                (assoc db :open-items (filter #(or (not= item-id %) (editing? db %))
                                              open-items)))))

(defn close-other-items! [item-id]
  (enqueue-db-update! 'close-other-items!
              (fn [{:keys [open-items] :as db}]
                (assoc db :open-items (filter #(or (= item-id %) (editing? db %))
                                              open-items)))))

(defn close-all-items! []
  (enqueue-db-update! 'close-all-items!
              (fn [{:keys [open-items] :as db}]
                (assoc db :open-items (filter #(editing? db %)
                                              open-items)))))

(defn close-trashed! []
  (enqueue-db-update! 'close-trashed!
              (fn [{:keys [open-items doc] :as db}]
                (assoc db :open-items (remove #(-> % doc :trashed) open-items)))))

;---------------------edit-item---------

(defn- enqueue-db-update! [label f]
  ($enqueue! label (fn [_] (update-document-db! label f))))

(defn start-edit! [item-id]
  (enqueue-db-update! 'start-edit!
                      (fn [db]
                        (update-in db [:editing] (fn [editing]
                                                  (assoc (into {} (filter #(-> % second :accept-as not) editing))
                                                         item-id {:source (or (get-in db [:doc item-id]) {})
                                                                  :editor :goog-editor}))))))

(defn start-edit-new! [kind]
  ($enqueue! 'start-edit-new!
             (fn [{doc :doc}]
               (let [item-id (new-item-id doc)
                     item {:id item-id :kind kind :create (utils/iso-time-now)}]
                 (update-document-db! 'start-edit-new!
                                (fn [db]
                                  (-> db
                                      (assoc-in [:doc item-id] item)
                                      (update :open-items #(distinct (conj % item-id)))
                                      (assoc-in [:editing item-id]
                                                {:source item :editor :goog-editor}))))))))

(defn start-edit-new-note! []
  (trace log)
  (start-edit-new! :note))

(defn accept-edit! [item-id]
  (enqueue-db-update! 'accept-edit!
              (fn [{:keys [doc editing open-items] :as db}]
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
                                                    [nid item-id doc])
                                                  [item-id item-id doc])]
                    (info log :saving)
                    (assoc db
                           :doc (store/update-timestamps! doc [item-id])
                           :editing (assoc-in editing [o-item-id :accept-as] item-id)
                           :open-items (if (not= item-id o-item-id)
                                         (conj (filter #(not= item-id %) open-items) item-id)
                                         open-items)
                           :status (when (not= item-id o-item-id)
                                     (store/app-status "Edit conflict: item branched" :warn))
                           :saving? true))))))

(defn cancel-edit! [item-id]
  (enqueue-db-update! 'cancel-edit!
              (fn [db]
                (update-in db [:editing] dissoc item-id))))

;--------------------------update-doc--------------------------

(defn delete-item-permanent!! [item-id]
  ($enqueue! 'delete-item-permanent!!
            (fn [{:keys [doc]}]
              (let [doc (store/update-timestamps! (dissoc doc item-id) [item-id])]
                (update-document-db! 'delete-item-permanent!! (fn [db]
                              (-> db
                                  (assoc :doc doc)
                                  (update :editing dissoc item-id)
                                  (update :open-items #(remove #{item-id} %)))))))))

(defn empty-trash!! []
  ($enqueue! 'empty-trash!!
            (fn [{:keys [doc open-items] :as db}]
              (if-let [trashed-ids (not-empty (keep #(when (:trashed %) (:id %)) (vals doc)))]
                (let [doc (apply dissoc doc trashed-ids)
                      doc (store/update-timestamps! doc trashed-ids)]
                  (update-document-db! 'empty-trash!! (fn [db]
                                (assoc db
                                       :doc doc
                                       :open-items (verified-open-items doc open-items)))))))))

(defn trash-item! [item-id]
  (enqueue-db-update! 'trash-item!
                      (fn [{:keys [doc] :as db}]
                        (if (string? item-id)
                          (let [doc (-> doc
                                        (update item-id assoc :trashed true)
                                        (store/update-timestamps! [item-id]))]
                            (-> db
                                (assoc :doc doc)
                                (update :editing dissoc item-id)
                                (update :open-items #(remove #{item-id} %))))
                          db))))

(defn restore-item! [item-id]
  (enqueue-db-update! 'restore-item!
                 (fn [{:keys [doc] :as db}]
                   (let [doc (update doc item-id dissoc :trashed)]
                     (assoc db :doc (store/update-timestamps! doc [item-id]))))))

(defn restore-all-trashed! []
  (enqueue-db-update! 'restore-all-trashed!
   (fn [{:keys [doc] :as db}]
     (let [trashed-ids (map :id (filter :trashed (vals doc)))
           doc (reduce (fn [doc id]
                         (update doc id dissoc :trashed)) doc trashed-ids)]
       (assoc db :doc (store/update-timestamps! doc trashed-ids))))))

(defn new-content! [item-id content]
  ;write content only after accept-edit
  (enqueue-db-update! 'new-content!
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
  (enqueue-db-update! 'new-title!
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
  (db/$enqueue-drive! 'rename-file!!
             (fn [db]
               (p/do
                 (store/$rename-file (get-in db [:doc :doc-id]) params)
                 ($sync-doc-index)))))

(defn doc-options! [options-update]
  ;write options only after accept-edit
  (enqueue-db-update! 'doc-options!
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
                (when (get-in db [:editing :log-config])
                  (info log :log-config (pprintl log-config))
                  (log/set-config! log-config)
                  (update db :logger-config merge log-config)))))

(defn new-tags! [item-id tag-ids new-tags]
  (enqueue-db-update! 'new-tags!
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
  (enqueue-db-update! 'finish-open-doc-file!!
                      (fn [db]
                        (let [doc (if new-doc-id?
                                    (assoc doc :doc-id (utils/simple-uuid))
                                    doc)]
                          (install-document db doc nil (:open-items db))))))

(defn open-doc-file!! [content]
  (-> ($enqueue! 'open-doc-file!!
                 (fn [db]
                   (p/let [{:keys [doc conflict?]} (store/$open-local-file db (store/decode content))]
                     (if conflict?
                       (open-doc-file-dialog! doc)
                       (update-db! 'finish-open-doc-file!!
                                   (fn [db]
                                     (install-document db doc nil (:open-items db))))))))
      (p/catch (fn [error]
                 (warn log 'open-file-error error)
                 (set-app-status! error :error)))))

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
                                                           (some (fn [id]
                                                                   (and (string? id)
                                                                        (not= :tag (get-in db [:doc id :kind]))))
                                                                 (:open-items db)))))))))

(defn- finish-move-items! [move-items]
  (update-document-db! '$finish-move-items!
              (fn [{:keys [open-items doc] :as db}]
                (let [doc-cleaned (reduce (fn [doc id]
                                            (assoc-in doc [id :trashed] :moved)) doc move-items)
                      updated-doc (store/update-timestamps! doc-cleaned move-items)]
                  (assoc db
                         :moving-items? false
                         :open-items (remove #(-> % updated-doc :trashed) open-items)
                         :doc updated-doc)))))

(defn- $move-items-drive! [source-doc target-doc-id move-items]
  (-> (db/$enqueue-drive!
       'move-items-drive-work
       (fn [_]
         (p/let [online? (drive/allow-drive-request?)
                 sync-result (when online? (store/$sync-drive-file target-doc-id))
                 local-target (when-not (:doc sync-result)
                                (store/$read-local-doc target-doc-id))
                 target-doc (or (:doc sync-result) local-target {:doc-id target-doc-id})
                 target-doc (store/$copy-items source-doc target-doc move-items)
                 _ (when online? (store/$sync-drive-file target-doc))]
           ($sync-doc-index)
           ($enqueue! 'finish-move-items
                      (fn [_]
                        (info log 'move-items-complete target-doc-id target-doc-id
                              'item-ids move-items)
                        (finish-move-items! move-items))))))
      (p/catch (fn [error]
                 (warn log 'move-items-error error)
                 (set-sync-status! :error)
                 (set-app-status! "Copy failed" :warn)
                 (update-db! #(assoc % :moving-items? false))
                 nil))))

(defn move-items!! [target-doc-id]
  ($enqueue! 'move-items!!
             (fn [{:keys [moving-items? open-items] source-doc :doc}]
               (when (and moving-items?
                          (map? source-doc)
                          (string? target-doc-id)
                          (not= (:doc-id source-doc) target-doc-id))
                 (info log 'move-items 'source-doc-id (:doc-id source-doc)
                       'target-doc-id target-doc-id)
                 (when-let [move-items (not-empty (filter #(and (string? %)
                                                                (-> % source-doc :kind (not= :tag)))
                                                          open-items))]
                   ($move-items-drive! source-doc target-doc-id move-items)))
               nil)))

;===============================================================

(defn on-authorized! [{:keys [token]}]
  (trace log "token:" (-> token bean pprintl))
  (db/$enqueue-drive! 'got-access-token!
            (fn [{doc :doc}]
              (when (drive/allow-drive-request?)
                (trace log "can sync drive file")
                (p/do (when (:doc-id doc)
                        (store/$trash-files-pending)
                        ($sync-drive-file doc {:src ::signed-in}))
                      #_(update-db! (fn [db]
                                      (assoc db :sync-status :online))))))))


;===============================================================
;------------------debug-support---------------------

(defn dump-doc-meta []
  (run-now! 'dump-doc-meta
            (fn [{doc :doc :as db}]
     ;(go (utils/throw-error "e1"))
              (let [doc-meta (select-keys doc (filter keyword? (keys doc)))]
                (println "Doc keyword entries:")
                (pprint doc-meta)))))

(defn dump-doc []
  (run-now! 'dump-doc
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
  (run-now! 'check-doc
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
  (run-now! 'fix-doc
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
  (run-now! 'dump-file-meta
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
  (run-now! 'dump-item-content
            (fn [db]
              (println "vvvvvvvvvvvvvvvv")
              (pprint (get-in db [:doc item-id]))
              (println "^^^^^^^^^^^^^^^^"))))

(defn dump-index []
  (run-now! 'dump-index
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
  (run-now! 'debug-file-compress
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
  (p/then (db/$enqueue! 'debug-rename-file!!
                        (fn [_] (store/$rename-file "klhurigk" {:title "My File Name"})))
          #(debug log 'response %)))

(defn debug-add-properties []
  (p/let [file-id "1R8JZWxzjLAWYCXIb5Y493AemAoj9G-8W"     ;"kgrsc300.ydn"
          file-id "1CQXBtftHN-cUxgC-Au9-VpuWKyJbLxjc"     ;
          response (drive/$add-properties file-id {:doc-id "kgtbg5v1"})
          file-meta (store/$file-meta file-id)]
    (println "Add property")
    (pprint (get-in response [:appProperties :doc-id]))
    (pprint response)
    (pprint file-meta)))

