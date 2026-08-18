(ns lib.goog-drive
  (:require
   [lib.log :as log :refer-macros [trace stack debug info warn fatal] :refer [pprintl]]
   [lib.debug :as debug :refer [we wd]]
   [promesa.core :as p]
   [cljs.reader :as reader]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [clojure.string :as str]))

(def log (log/logger 'lib.goog-drive))

(def ydn-mime "application/vnd.google.drive.ext-type.ydn")
(def text-mime "text/plain")

(defn $fix-promise [p]
  ;https://github.com/funcool/promesa/issues/149
  (p/create (fn [resolve reject]
              (-> p
                  (.then resolve)
                  (.catch reject)))))

(defn read-string [s]
  (try
    (reader/read-string s)
    (catch :default e
      (warn log s)
      (warn log 'error e)
      e)))

(def !online-status (atom {:online? false} #_{:validator (fn [{:keys [status]}]
                                                           (#{:offline :online} status))}))
(def request-timeout-ms 15000)
(defonce !pending-requests (atom {}))
(defonce !request-sequence (atom 0))
(defonce !late-settlement-listener (atom nil))

(declare !authorization-promise)

(defn- request-error [type message]
  {:id ::request-failed :type type :message message})

(defn $bounded
  "Logically settles promise within timeout-ms. Underlying Google operations
  may still finish; late settlement is reported so callers can reconcile."
  ([promise label] ($bounded promise label request-timeout-ms))
  ([promise label timeout-ms]
   (let [request-id (swap! !request-sequence inc)
         !settled? (atom false)
         !timed-out? (atom false)]
     (p/create
      (fn [resolve reject]
        (let [finish! (fn [handler value]
                        (if (compare-and-set! !settled? false true)
                          (do
                            (swap! !pending-requests dissoc request-id)
                            (handler value))
                          (when @!timed-out?
                            (swap! !online-status update :late-settlement (fnil inc 0))
                            (when-let [listener @!late-settlement-listener]
                              (listener)))))
              cancel! (fn [error]
                        (reset! !timed-out? true)
                        (finish! reject error))
              timer (js/setTimeout
                     #(cancel! (request-error :timeout (str label " timed out")))
                     timeout-ms)]
          (swap! !pending-requests assoc request-id
                 (fn [error]
                   (js/clearTimeout timer)
                   (cancel! error)))
          (-> (.resolve js/Promise promise)
              (.then (fn [value]
                       (js/clearTimeout timer)
                       (finish! resolve value)))
              (.catch (fn [error]
                        (js/clearTimeout timer)
                        (finish! reject error))))))))))

(defn connectivity-changed!
  "Immediately settles active logical requests when the browser goes offline."
  [online?]
  (swap! !online-status assoc :online? online?)
  (when-not online?
    (let [error (request-error :offline "Drive is offline")
          pending (vals @!pending-requests)]
      (reset! !pending-requests {})
      (doseq [cancel! pending]
        (cancel! error))
      (reset! !authorization-promise nil))))

(defn set-late-settlement-listener! [listener]
  (assert (or (nil? listener) (fn? listener)))
  (reset! !late-settlement-listener listener))

(defn- $request- [request return-type {:keys [default] :as opt}]
  (assert (fn? request))
  (when opt (trace log 'request-opt opt))
  (-> (request)
      $fix-promise
      ($bounded 'drive-request)
      (p/then (fn [response]
                (trace log 'response return-type)
                (let [response (case return-type
                                 :body-edn (-> response .-body read-string)
                                 :body (-> response .-body)
                                 :result (some-> response .-result ->clj)
                                 :response (->clj response)
                                 :raw response)]
                  (or response default))))))

(declare $ensure-authorized?)

(declare allow-drive-request?)
(declare get-status)

(defn- $request [request return-type & [opt]]
  (letfn [(request-error [response]
            (let [response (->clj response)
                  err (some-> response :result :error)
                  code (:code err)
                  status (:status err)]
              ;codes: -1 network-error (eg no internet access)
              (trace log :code code :status status (-> (or err response) pprintl))
              (swap! !online-status assoc :online? (not= code -1))
              {:response response :error (or err response) :code code}))
          (authorization-failure [error]
            (p/rejected (assoc (if (map? error) error {:cause error})
                               :authorization-error? true)))
          (attempt [reauthorize?]
            (if (allow-drive-request?)
              (-> ($request- request return-type opt)
                  (p/then (fn [resolved]
                            (swap! !online-status assoc :online? true)
                            resolved))
                  (p/catch (fn [response]
                             (let [{:keys [error code]} (request-error response)]
                               (cond
                                 (= code 401)
                                 (do
                                   (js/gapi.client.setToken nil)
                                   (get-status)
                                   (if reauthorize?
                                     (-> ($ensure-authorized? :automatic)
                                         (p/then (fn [authorized?]
                                                   (if authorized?
                                                     (attempt false)
                                                     (authorization-failure error))))
                                         (p/catch (fn [_]
                                                    (authorization-failure error))))
                                     (authorization-failure error)))

                                 (= code 403) (p/rejected error)
                                 :else (p/rejected error))))))
              (p/rejected {:message "access denied" :id ::access-denied})))]
    (attempt true)))

;=================================== Requests =======================================
(defn $create-file [{:keys [file-name mime-type parents app-data? properties]}]
  (trace log file-name)
  (let [metadata {:name          file-name                  ;"yetipad.ydn"
                  :mimeType      (or mime-type text-mime)   ;ydn-mime
                  :fields        "id, appProperties"
                  :appProperties properties
                  :parents       (cond
                                   parents (->js parents)
                                   app-data? ["appDataFolder"])}]
    ($request #(js/gapi.client.drive.files.create (->js metadata)) :result)))

(defn $list-app-data-files [{:keys [query]}]
  (trace log query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {:spaces "appDataFolder"
                :fields "files(id, name, modifiedTime, appProperties)"
                :q      query}]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    ($request #(js/gapi.client.drive.files.list (->js params)) :result)))

(defn $list-app-files [{:keys [query fields]}]
  (trace log query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {;https://developers.google.com/drive/api/v3/reference/files
                :fields fields
                ;https://developers.google.com/drive/api/v3/search-files
                :q      query}]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    ($request #(js/gapi.client.drive.files.list (->js params)) :result)))

(defn $write-file-content
  "Write or overwrite the content of an existing file."
  [file-id content & [{:keys [mime-type content-type fields]}]]
  (trace log file-id)
  (assert file-id)
  (let [body (case content-type
               :edn (pr-str content)
               content)
        req-params {:path   (str "/upload/drive/v3/files/" file-id) ;The URL to handle the request.
                    :method :PATCH                          ;default get
                    :params {;URL params
                             :uploadType :media
                             :fields     (or fields "id, name, modifiedTime, trashed, appProperties")
                             :mimeType   (or mime-type text-mime)}
                    :body   body                            ;string | object	The HTTP request body (applies to PUT or POST).
                    }]
    ;https://github.com/google/google-api-javascript-client/blob/master/docs/reference.md
    ($request #(js/gapi.client.request (->js req-params)) :result)))

(defn $read-file-edn [file-id & [options]]
  (trace log file-id)
  (assert file-id)
  ;https://developers.google.com/drive/api/v3/manage-downloads
  (let [params {:fileId file-id
                :alt    "media"}]
    ($request #(js/gapi.client.drive.files.get (->js params)) :body-edn options)))

(defn $get-file-meta
  ;warning: on error, doesn't respond
  [file-id & [{:keys [fields]}]]
  (trace log :file-id file-id)
  (assert file-id)
  (let [params {:fileId file-id
                :fields (or (and (vector? fields) (str/join \, (map name fields)))
                            fields)}]
    ;https://developers.google.com/drive/api/v3/fields-parameter
    ($request #(js/gapi.client.drive.files.get (->js params)) :result)))

(defn $delete-file
  "Permanently deletes a file owned by the user without moving it to the trash."
  ;todo not tested
  [file-id]
  ;https://developers.google.com/drive/api/v3/reference/files/delete
  (let [params {:fileId file-id}]
    ($request #(js/gapi.client.drive.files.delete (->js params)) :result)))

(defn $trash-file [file-id]
  (trace log file-id)
  ;https://developers.google.com/drive/api/v3/reference/files/update
  ;https://developers.google.com/drive/api/v3/reference/files#resource-representations
  (let [params {:fileId  file-id
                :trashed true}]
    ($request #(js/gapi.client.drive.files.update (->js params)) :result)))

(defn $add-properties
  "Add custom properties to the file as a map.
  To remove a property, set its value to nil.
  modifiedTime is updated.
  "
  [file-id property-map]
  (trace log file-id property-map)
  ;https://developers.google.com/drive/api/v3/properties
  (let [params {:fileId        file-id
                :appProperties property-map
                :fields        "appProperties, id, name"}]
    ($request #(js/gapi.client.drive.files.update (->js params)) :result)))

(defn $update-file
  "Sets a files metadata like file-name and description.
  Responds with the specified meta-data fields (:fields)
  "
  [file-id {:keys [description mime-type fields name]}]
  (assert file-id)
  ;https://developers.google.com/drive/api/v3/reference/files/update
  ;Note some fields are read-only:
  ;https://developers.google.com/drive/api/v3/reference/files#resource-representations
  (let [field-values [[:fileId file-id]
                      [:name name]
                      [:description description]
                      [:mimeType mime-type]
                      [:fields (and fields (str/join \, (map cljs.core/name fields)))]]
        params (into {} (for [f field-values, :when (second f)] f))]
    ($request #(js/gapi.client.drive.files.update (->js params)) :result)))

;======================================= Authentication =============================================
(defonce !token-client (atom {}))
(defonce !sdk-bootstrap (atom {:status :idle :promise nil}))
(defonce !authorization-promise (atom nil))

(def google-api-script-id "yetipad-google-api")
(def google-identity-script-id "yetipad-google-identity")
(def google-api-script-src "https://apis.google.com/js/api.js")
(def google-identity-script-src "https://accounts.google.com/gsi/client")
(def sdk-script-timeout-ms 15000)

(defn configure!
  "Configures the retryable Drive bootstrap before loading either Google SDK."
  [credentials]
  (swap! !token-client assoc :credentials credentials))

(defn- remove-script! [id]
  (when-let [script (.getElementById js/document id)]
    (.remove script)))

(defn- $load-script! [id src ready?]
  (if (ready?)
    (p/resolved true)
    (p/create
     (fn [resolve reject]
       (remove-script! id)
       (let [script (.createElement js/document "script")
             !settled? (atom false)
             !timer (atom nil)
             succeed! (fn []
                        (when (compare-and-set! !settled? false true)
                          (js/clearTimeout @!timer)
                          (resolve true)))
             fail! (fn []
                     (when (compare-and-set! !settled? false true)
                       (js/clearTimeout @!timer)
                       (remove-script! id)
                       (reject {:id ::sdk-load-failed
                                :message (str "Unable to load " src)})))]
         (set! (.-id script) id)
         (set! (.-src script) src)
         (set! (.-async script) true)
         (set! (.-defer script) true)
         (set! (.-onload script) succeed!)
         (set! (.-onerror script) (fn [_] (fail!)))
         (reset! !timer (js/setTimeout fail! sdk-script-timeout-ms))
         (.appendChild (.-head js/document) script))))))

(defn- $initialize-gapi! []
  (if (:gapi? @!token-client)
    (p/resolved true)
    (p/create
     (fn [resolve reject]
       (try
         (js/gapi.load
          "client:picker"
          (fn []
            (-> (p/do
                  ($bounded ($fix-promise (js/gapi.client.init #js {}))
                            'gapi-client-init)
                  ($bounded ($fix-promise
                             (js/gapi.client.load
                              "https://www.googleapis.com/discovery/v1/apis/drive/v3/rest"))
                            'drive-discovery-load)
                  (swap! !token-client assoc :gapi? true)
                  true)
                (p/then resolve)
                (p/catch reject))))
         (catch :default e
           (reject e)))))))

(defn- initialize-gis! []
  (when-not (:token-client @!token-client)
    (let [credentials (:credentials @!token-client)]
      (assert credentials)
      (swap! !token-client assoc
             :token-client
             (js/google.accounts.oauth2.initTokenClient
              (->js (select-keys credentials [:client_id :scope :hint]))))))
  true)

(defn $ensure-sdk-ready!
  "Loads and initializes both Google SDKs. Concurrent callers share an attempt;
  a failed attempt is cleared so focus or a user click can retry it."
  []
  (let [{:keys [status promise]} @!sdk-bootstrap]
    (cond
      (= status :ready) (p/resolved true)
      (and (= status :loading) promise) promise
      :else
      (let [attempt (-> (p/let [_ (p/all [($load-script! google-api-script-id
                                                         google-api-script-src
                                                         #(exists? js/gapi))
                                           ($load-script! google-identity-script-id
                                                          google-identity-script-src
                                                          #(exists? js/google))])
                                  _ ($initialize-gapi!)
                                  _ (initialize-gis!)]
                           (swap! !sdk-bootstrap assoc :status :ready :promise nil)
                           (swap! !online-status assoc :online? true)
                           (get-status)
                           true)
                        (p/catch (fn [e]
                                   (swap! !sdk-bootstrap assoc :status :failed :promise nil)
                                   (swap! !online-status assoc :online? false :status ::initialising)
                                   (p/rejected e))))]
        (swap! !sdk-bootstrap assoc :status :loading :promise attempt)
        attempt))))

(defn- get-token []
  (when (and (exists? js/gapi) js/gapi.client)
    (js/gapi.client.getToken)))

(defn get-status
  ;no internet access?
  []
  (let [{:keys [gapi? token-client aborted-sign-in?]} @!token-client
        hasGrantedAllScopes (when (exists? js/google)
                              js/google.accounts.oauth2.hasGrantedAllScopes)
        token (get-token)
        status (cond
                 (not (and gapi? token-client)) ::initialising
                 aborted-sign-in? ::aborted-sign-in
                 (not token) ::sign-in-pending
                 (and hasGrantedAllScopes
                      (hasGrantedAllScopes token "https://www.googleapis.com/auth/drive.file")) ::authorized
                 :else ::failed-authorization)]
    (trace log :status status)
    (swap! !online-status assoc :status status)
    status))

(comment
  (get-status))

(defn allow-drive-request?
  "True only when Drive is initialized and already authorized."
  []
  (and (not= false (.-onLine js/navigator))
       (= ::authorized (get-status))))

(defn- connectivity-error? [error]
  (let [error-code (or (:error error)
                       (get-in error [:response :error])
                       (:type error))]
    (or (= false (.-onLine js/navigator))
        (#{:timeout :offline "network_error" "temporarily_unavailable" "unknown"}
         error-code))))

(defn- $request-access-token! [^Object token-client status authorization]
  ($bounded
   (p/create
    (fn [resolve reject]
     (swap! !token-client assoc :aborted-sign-in? false)
     (set! (.-callback token-client)
           (fn [response]
             (try
               (let [response (->clj response)]
                 (if (:error response)
                   (do
                     (swap! !online-status assoc :online? true)
                     (swap! !token-client assoc :aborted-sign-in? true)
                     (reject {:response response :message (:error response)}))
                   (do
                     (swap! !online-status assoc :online? true)
                     (resolve (js/gapi.client.getToken)))))
               (catch :default e
                 (reject (-> e bean))))))
     (set! (.-error_callback token-client)
           (fn [err]
             (let [err (->clj err)]
               (swap! !token-client assoc :aborted-sign-in? true)
               (reject err))))
     (let [prompt (if (and (= authorization :interactive)
                           (or (= status ::failed-authorization)
                               (= status ::aborted-sign-in)
                               (:automatic-authorization-failed? @!token-client)))
                    "consent"
                    "")]
       (.requestAccessToken token-client #js {:prompt prompt}))))
   'drive-authorization))

(defn $ensure-authorized?
  "Requests Drive authorization in :automatic or :interactive mode. An
  interactive caller upgrades a failed concurrent automatic attempt."
  [authorization]
  (assert (#{:automatic :interactive} authorization))
  (when (= authorization :interactive)
    (swap! !token-client assoc
           :automatic-authorization-failed? false
           :automatic-authorization-disabled? false))
  (if-let [{existing-mode :authorization existing-promise :promise}
           @!authorization-promise]
    (if (and (= authorization :interactive)
             (= existing-mode :automatic))
      (-> existing-promise
          (p/catch (fn [_] ($ensure-authorized? :interactive))))
      existing-promise)
    (if-let [{:keys [^Object token-client]} @!token-client]
      (let [status (get-status)]
        (cond
          (= status ::authorized) (p/resolved true)
          (= status ::initialising) (p/resolved false)
          :else
          (let [attempt (-> ($request-access-token! token-client status authorization)
                            (p/then (fn [_]
                                      (swap! !token-client assoc
                                             :automatic-authorization-failed? false)
                                      (= (get-status) ::authorized)))
                            (p/catch (fn [error]
                                       (let [retryable? (connectivity-error? error)]
                                         (when (and (= authorization :automatic)
                                                    (not retryable?))
                                           (swap! !token-client assoc
                                                  :automatic-authorization-failed? true))
                                         (p/rejected
                                          (assoc (if (map? error) error {:cause error})
                                                 :authorization-error? (not retryable?)))))))
                managed (-> attempt
                            (p/then (fn [value]
                                      (reset! !authorization-promise nil)
                                      value))
                            (p/catch (fn [e]
                                       (reset! !authorization-promise nil)
                                       (p/rejected e))))]
            (reset! !authorization-promise {:authorization authorization
                                            :promise managed})
            managed)))
      (p/resolved false))))

(defn $ensure-drive-access!
  "Ensures SDK readiness and returns a structured connection result."
  [{:keys [authorization]}]
  (assert (#{:automatic :interactive :none} authorization))
  (-> (p/let [_ ($ensure-sdk-ready!)
              status (get-status)]
        (cond
          (= status ::authorized) {:status :authorized}
          (= authorization :none) {:status :authorization-required}
          (and (= authorization :automatic)
               (or (:automatic-authorization-failed? @!token-client)
                   (:automatic-authorization-disabled? @!token-client)))
          {:status :authorization-required}
          :else
          (p/let [authorized? ($ensure-authorized? authorization)]
            {:status (if authorized? :authorized :authorization-required)})))
      (p/catch (fn [error]
                 (if (:authorization-error? error)
                   {:status :authorization-required :error error}
                   {:status :retryable-error :error error})))))

(defn sign-out!
  "Revokes authentication (log out).
   Optionally revoke authorizations - all scopes."
  ([revoke-authorization?]
   (let [token (get-token)
         access-token (and token (.-access_token token))]
     (swap! !token-client assoc
            :aborted-sign-in? false
            :automatic-authorization-disabled? true)
     (when (and revoke-authorization? token)
       (js/google.accounts.oauth2.revoke access-token (fn [response]
                                                        (let [response (->clj response)]
                                                          (info log :revoke-response response)
                                                         ;currently doesn't report an error
                                                         ;so can't update !offline-status
                                                          )))
       (js/gapi.client.setToken nil)
       (trace log "token revoked")
       (get-status);update-status
       nil)))
  ([] (sign-out! true)))

(comment
  (let [$f #(js/gapi.client.drive.files.get #js {:fileId "1CQXBtftHN-cUxgC-Au9-VpuWKyJbLxjc", :fields "id, name, modifiedTime, trashed, appProperties"})]
    (-> ($f)
        (.then #(prn :v (-> % ->clj)))
        (.catch #(prn :e (-> % ->clj))))))
