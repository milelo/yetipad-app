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

(defn read-string [s]
  (try
    (reader/read-string s)
    (catch :default e
      (warn log s)
      (warn log 'error e)
      e)))

(def online-status* (atom {:online? false} #_{:validator (fn [{:keys [status]}]
                                                           (#{:offline :online} status))}))
(defn- $request- [request return-type {:keys [default] :as opt}]
  (assert (fn? request))
  (when opt (trace log 'request-opt opt))
  (-> (request)
      (p/then (fn [response]
                (trace log 'response return-type)
                (let [response (case return-type
                                 :body-edn (-> response .-body read-string)
                                 :body (-> response .-body)
                                 :result (some-> response .-result ->clj)
                                 :response (->clj response)
                                 :raw response)]
                  (or response default))))))

(declare $ensure-authentication?)

(declare allow-drive-request?)

(defn- $request [request return-type & [opt]]
  (if (allow-drive-request?)
    (-> ($request- request return-type opt)
        (p/then (fn [resolved]
                  (swap! online-status* assoc :online? true)
                  resolved))
        (p/catch (fn [^js/Object response]
                   (let [response (->clj response)
                         err (some-> response :result :error)
                         code (:code err)
                         status (:status err)]
                     ;codes: -1 network-error (eg no internet access)
                     (trace log :code code :status status (-> (or err response) pprintl))
                     (swap! online-status* assoc :online? (not= code -1))
                     (if (or (= code 401) (= code 403))
                       (do
                         (when (and (= code 401) #_(= status "UNAUTHENTICATED"))
                           ;Stop behaving as authenticated
                           (js/gapi.client.setToken ""))
                         (-> ($ensure-authentication?)
                             (p/then #($request- request return-type opt))
                             (p/catch (fn [e]
                                        (warn log "sign in error" e)
                                        (p/rejected e)))))
                       (p/rejected err))))))
    (p/rejected {:message "access denied" :id ::access-denied})))

;=================================== Requests =======================================
(defn $create-file [{:keys [file-name mime-type parents app-data? properties]}]
  (trace log file-name)
  (let [metadata {:name          file-name                  ;"yetipad.ydn"
                  :mimeType      (or mime-type text-mime)   ;ydn-mime
                  :fields        "id, appProperties"
                  :appProperties properties
                  :parents       (cond
                                   parents (clj->js parents)
                                   app-data? ["appDataFolder"])}]
    ($request #(js/gapi.client.drive.files.create (clj->js metadata)) :result)))

(defn $list-app-data-files [{:keys [query]}]
  (trace log query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {:spaces "appDataFolder"
                :fields "files(id, name, modifiedTime, appProperties)"
                :q      query}]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    ($request #(js/gapi.client.drive.files.list (clj->js params)) :result)))

(defn $list-app-files [{:keys [query fields]}]
  (trace log query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {;https://developers.google.com/drive/api/v3/reference/files
                :fields fields
                ;https://developers.google.com/drive/api/v3/search-files
                :q      query}]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    ($request #(js/gapi.client.drive.files.list (clj->js params)) :result)))

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
    ($request #(js/gapi.client.request (clj->js req-params)) :result)))

(defn $read-file-edn [file-id & [options]]
  (trace log file-id)
  (assert file-id)
  ;https://developers.google.com/drive/api/v3/manage-downloads
  (let [params {:fileId file-id
                :alt    "media"}]
    ($request #(js/gapi.client.drive.files.get (clj->js params)) :body-edn options)))

(defn $get-file-meta
  ;warning: on error, doesn't respond
  [file-id & [{:keys [fields]}]]
  (trace log file-id)
  (assert file-id)
  (let [params {:fileId file-id
                :fields (or (and (vector? fields) (str/join \, (map name fields)))
                            fields)}]
    ;https://developers.google.com/drive/api/v3/fields-parameter
    ($request #(js/gapi.client.drive.files.get (clj->js params)) :result)))

(defn $delete-file
  "Permanently deletes a file owned by the user without moving it to the trash."
  ;todo not tested
  [file-id]
  ;https://developers.google.com/drive/api/v3/reference/files/delete
  (let [params {:fileId file-id}]
    ($request #(js/gapi.client.drive.files.delete (clj->js params)) :result)))

(defn $trash-file [file-id]
  (trace log file-id)
  ;https://developers.google.com/drive/api/v3/reference/files/update
  ;https://developers.google.com/drive/api/v3/reference/files#resource-representations
  (let [params {:fileId  file-id
                :trashed true}]
    ($request #(js/gapi.client.drive.files.update (clj->js params)) :result)))

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
    ($request #(js/gapi.client.drive.files.update (clj->js params)) :result)))

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
    ($request #(js/gapi.client.drive.files.update (clj->js params)) :result)))

;======================================= Authentication =============================================
(defonce token-client* (atom {}))

(defn- get-token []
  (and js/gapi.client (js/gapi.client.getToken)))

(defn get-status
  ;no internet access?
  []
  (let [{:keys [gapi? token-client aborted-sign-in?]} @token-client*
        hasGrantedAllScopes js/google.accounts.oauth2.hasGrantedAllScopes
        token (get-token)
        status (cond
                 (not (and gapi? token-client)) ::initialising
                 aborted-sign-in? ::aborted-sign-in
                 (not token) ::sign-in-pending
                 (and token (hasGrantedAllScopes token "https://www.googleapis.com/auth/drive.file")) ::authenticated
                 :else ::failed-authentication)]
    (trace log "status: " status)
    (swap! online-status* assoc :status status)
    #_(when (= status ::authenticated)
        (stack log "status: " status))
    status))

(comment
  (get-status))

(defn basic-user-profile []
  (let [google-auth (js/gapi.auth2.getAuthInstance)]
    (-> google-auth .-currentUser .get .getBasicProfile)))

(defn get-user-email []
  ;(.getEmail (basic-user-profile))
  nil)

(defn allow-drive-request?
  "Allow a request that may succeed or trigger a user sign-in or authentication request."
  []
  (#{::sign-in-pending ::authenticated} (get-status)))

(defn $ensure-authentication?
  "Ensure or attempt Drive access authentication.
   Return true if successful."
  []
  (when-let [{:keys [^js/Object token-client on-authorized]} @token-client*]
    (trace log)
    ;For prompt values see: https://developers.google.com/identity/oauth2/web/reference/js-reference#TokenClientConfig
    ;ALWAYS PROMPTS with localhost: https://stackoverflow.com/questions/73519031/how-do-i-let-the-browser-store-my-login-status-with-google-identity-services
    (let [status (get-status)]
      (cond
        (= status ::authenticated) (p/resolved true)
        (= status ::initialising) (p/resolved false)
        :else  (p/do
                 (p/create (fn [resolve reject]
                             (trace log "register callback" #_(-> token-client bean pprintl))
                             (set! (.-callback token-client)
                                   (fn [response]
                                     (try
                                       (trace log :response response)
                                       (let [response (->clj response)]
                                         (trace log "callback:" (-> response pprintl))
                                         (if (:error response)
                                           (do
                                             (swap! online-status* assoc :online? true)
                                             (swap! token-client* assoc ::aborted-sign-in? true)
                                             (reject {:response response :message (:error response)}))
                                           ;GIS has automatically updated gapi.client with the newly issued access token.
                                           (let [token (js/gapi.client.getToken)]
                                             (swap! online-status* assoc :online? true)
                                             (resolve token))))
                                       (catch :default e (reject (-> e bean))))))
                             (set! (.-error_callback token-client)
                                   (fn [err]
                                     (let [err (->clj err)]
                                       (trace log :error_callback err)
                                       (swap! token-client* assoc ::aborted-sign-in? true)
                                       (reject err))))
                             (let [prompt (if (= status ::failed-authentication) "consent" "")]
                               (trace log :prompt prompt)
                               (.requestAccessToken  token-client #js {:prompt prompt}))))
                 (= (get-status) ::authenticated))))))

(defn sign-out!
  "Revokes authentication (log out).
   Optionally revoke authorizations - all scopes."
  ([revoke-authorization?]
   (let [token (get-token)
         access-token (and token (.-access_token token))]
     (swap! token-client* assoc ::aborted-sign-in? false)
     (when (and revoke-authorization? token)
       (js/google.accounts.oauth2.revoke access-token (fn [response]
                                                        (let [response (->clj response)]
                                                          (info log :response response)
                                                         ;currently doesn't report an error
                                                         ;so can't update offline-status*
                                                          )))
       (js/gapi.client.setToken "")
       (trace log "token revoked")
       (get-status);update-status
       nil)))
  ([] (sign-out! true)))

(defn- start-after-init! []
  (let [{:keys [gapi? token-client on-authorized]} @token-client*]
    (when (and gapi? token-client)
      (trace log)
      ;This call to $ensure-authentication? not initiated from user action so may be blocked by browser.
      ;That should be ok, user authentication pop-up will be initiated if the user selects
      ;sign-in or presses the
      ;online sync status button.
      (when (and ($ensure-authentication?) on-authorized)
        (on-authorized {:token (get-token) :email (get-user-email)})
        ;(on-authorized {:token (get-token)})
        ))))

(defn- gapi-init! [credentials]
  (trace log)
  (p/do
    ;(js/gapi.client.init (->js (select-keys credentials [:apiKey :discoveryDocs])))
    (js/gapi.client.init #js {})
    (js/gapi.client.load "https://www.googleapis.com/discovery/v1/apis/drive/v3/rest")
    ;https://developers.google.com/identity/sign-in/web/reference
    ;(js/gapi.auth2.init (->js (select-keys credentials [:client_id]))) ;required to get userinfo
    (swap! token-client* assoc :gapi? true)
    (start-after-init!)))

(defn gapi-load!
  "Google API load"
  [credentials]
  (trace log)
  (js/gapi.load "client:auth2:picker" (partial gapi-init! credentials)))

(defn gis-init!
  "Google Identity Service init"
  [credentials on-authorized]
  (trace log)
  (swap! token-client* assoc
         :credentials credentials
         :token-client (js/google.accounts.oauth2.initTokenClient
                        (->js (select-keys credentials [:client_id :scope :hint])))
         :on-authorized on-authorized)
  (start-after-init!))
