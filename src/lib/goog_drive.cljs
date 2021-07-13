(ns lib.goog-drive
  (:require
    [lib.log :as log :refer [trace debug info warn fatal pprintl]]
    [lib.utils :as utils :refer [js->cljs]]
    [cljs.reader :as reader]
    [lib.debug :as debug :refer [we wd wee expose]]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go go-loop]]
    [lib.asyncutils :refer [put-last!] :refer-macros [<? go? go-let go-try]]
    [clojure.string :as str]
    ))

(def log (log/logger 'app.goog-drive))

(def ydn-mime "application/vnd.google.drive.ext-type.ydn")
(def text-mime "text/plain")

(defn read-string [s]
  (try
    (reader/read-string s)
    (catch :default e
      (println s)
      (js/console.error e)
      e)))

(defn simulate-error [type]
  ;server errors to support testing.
  (get {:safari-auth-fail {:errors  [{:domain       "usageLimits",
                                      :reason       "ipRefererBlocked",
                                      :message      "The referrer https://mikelongworth.co.uk does not match the referrer restrictions configured on your API key. Please use the API Console to update your key restrictions.",
                                      :extendedHelp "https://console.developers.google.com/apis/credentials?project=582900055519"}],
                           :code    403,
                           :message "The referrer https://mikelongworth.co.uk does not match the referrer restrictions configured on your API key. Please use the API Console to update your key restrictions."
                           }
        :token-expired    {:errors  [{:domain       "global",
                                      :reason       "authError",
                                      :message      "Invalid Credentials",
                                      :locationType "header",
                                      :location     "Authorization"}],
                           :code    401,
                           :message "Invalid Credentials"
                           }
        } type))

(defn- js-error
  "Adapt an error response to a js error"
  [error]
  (let [e (js/Error (pr-str error))]
    (set! (.-data e) error)
    e))

(def token-refresh-margin 100000)
;(def token-refresh-margin (* 1000 60 59)); hack: timeout in 1min

(defn- dump-token
  ([auth-response]
   (assoc auth-response
          :now+ (-> (utils/time-now-ms) utils/format-ms)
          :refresh-at+ (some-> auth-response :expires_at (- token-refresh-margin) utils/format-ms)
          :expires_at+ (some-> auth-response :expires_at utils/format-ms)
          :first_issued_at+ (some-> auth-response :first_issued_at utils/format-ms)
          ))
  ([]
   (let [google-auth (js/gapi.auth2.getAuthInstance)
         auth-response (-> google-auth .-currentUser .get (.getAuthResponse false) js->cljs)
         ]
     (dump-token auth-response))))

(defn- <thenable- [thenable return-type {:keys [default] :as opt}]
  (assert (fn? thenable))
  (when opt (trace log 'thenable-opt opt))
  (let [<c (chan)]
    (.then (thenable)
           (fn [response]
             (trace log '<thenable 'response return-type)
             ;(js/console.log response)
             (let [response (case return-type
                              :body-edn (-> response .-body read-string)
                              :result (some-> response .-result js->cljs)
                              :response (js->cljs response)
                              :raw response)]
               (put-last! <c (or response default false))))
           (fn [error-response]
             (trace log '<thenable 'error return-type)
             (let [error (some-> error-response .-result .-error js->cljs)
                   error (or error (js->cljs error-response))]
               (warn log '<thenable 'error-response (pprintl error))
               (put-last! <c (js-error error)))))
    <c))

(defonce client-args* (atom nil))

(defn- <init-client
  [credentials signed-in-listener]
  (reset! client-args* [credentials signed-in-listener])
  (go
    (info log '<init-client)
    ;https://github.com/google/google-api-javascript-client/blob/master/docs/reference.md
    (let [status (<? (<thenable- #(js/gapi.client.init
                                   (clj->js (if :popup
                                              credentials
                                              (let [redirect_uri js/window.location.href]
                                                (debug log 'redirect_uri redirect_uri)
                                                (merge credentials
                                                        ;redirect
                                                       {:ux_mode      "redirect"
                                                         ;https://developers.google.com/identity/protocols/oauth2/openid-connect#setredirecturi
                                                        :redirect_uri redirect_uri}))))) :response nil)
                     (fn [e] (.-data e)))]
      (if status
        (info log '<init-client
              (pprintl status))
        (info log '<init-client
              'ok))
      (if (:error status)
        (signed-in-listener false)
        (let [google-auth (js/gapi.auth2.getAuthInstance)
              signed-in? (-> google-auth .-isSignedIn .get)
              signed-in-listener (fn [signed-in?]
                                   (when signed-in? (trace log '<init-client
                                                           'token dump-token))
                                   (signed-in-listener signed-in?))]
          ;(debug log 'users-name (.getName (basic-user-profile)))
          (-> google-auth .-isSignedIn (.listen signed-in-listener))
          (signed-in-listener signed-in?)
          ;(.signOut auth2)
          (when-not signed-in?
            (trace log '<init-client 'sign-in)
            ;(.signIn ^js google-auth); this shouldn't give a warning
            (#(.signIn ^js google-auth));work around for compiler warning bug
            ))))
          :client-initialized))

(defn- <reinit-client []
  (info log '<reinit-client)
  (assert @client-args*)
  (apply <init-client @client-args*))

(defn- <ensure-authorised
  "Check and refresh token if its expired.
   User can sign-out at any time.
   Mobiles platforms can suspend the app so periodic background token-refreshes can't be used.
   "
  ;TODO handle user can sign-out at any time.
  [& [{:keys [refresh-now]}]]
  (let [google-auth (js/gapi.auth2.getAuthInstance)
        signed-in? (-> google-auth .-isSignedIn .get)
        google-user (-> google-auth .-currentUser .get)
        auth-response (-> google-user .getAuthResponse js->cljs)
        now (utils/time-now-ms)
        refresh-at (- (:expires_at auth-response) token-refresh-margin)
        <c (async/timeout 5000);refresh token response timeout (ms)
        ]
    (trace log '<ensure-authorised 'check-token-expires #(-> auth-response :expires_at utils/format-ms))
    (assert signed-in?)
    ;(debug log '<ensure-authorised (fn [] {:now (utils/format-ms now) :expires-at (utils/format-ms expires-at)}))
    (if (and (< now refresh-at) (not refresh-now))
      (put-last! <c :token-valid)
      (do
        (trace log '<ensure-authorised 'refresh-token #(dump-token auth-response))
        ;(put-last! <c false);hack: simulate token timeout (comment out next expression)
        (.then (.reloadAuthResponse google-user)
               (fn [response]
                 (trace log '<ensure-authorised "got response")
                 (let [auth-response (js->cljs response)]
                   (trace log '<ensure-authorised 'token-refreshed #(dump-token auth-response))
                   (info log '<ensure-authorised 'next-token-refresh-at (-> auth-response :expires_at (- token-refresh-margin) utils/format-ms))
                   (put-last! <c :token-refreshed)
                   ))
               (fn [error-response]
                 (trace log '<ensure-authorised "got error-response")
                 (let [error (js->cljs error-response)]
                   (warn log '<ensure-authorised 'error (pprintl error))
                   (put-last! <c (js-error error))
                   )))))
    (go?
      ;reinit client on refresh token response time out. 
      ;Google server bug? Why does it only occur from mobile app?
     (or (<? <c)
         ;(<! (<reinit-client))
         (js/Error. "No response from Token refresh request")
         ))))

(defn <refresh-drive-token []
  (<ensure-authorised {:refresh-now true}))

(defn <thenable [thenable return-type & [opt]]
  (go?
   (<? (<ensure-authorised))
   (<! (<thenable- thenable return-type opt))))

(defn sign-in! []
  ;https://developers.google.com/identity/sign-in/web/reference
  (when-let [auth2 (js/gapi.auth2.getAuthInstance)]
    (when-not (-> auth2 .-isSignedIn .get)
      (.signIn auth2))))

(defn sign-out! []
  (when-let [auth2 (js/gapi.auth2.getAuthInstance)]
    (when (-> auth2 .-isSignedIn .get)
      (.signOut auth2))))

(defn basic-user-profile []
  (let [google-auth (js/gapi.auth2.getAuthInstance)]
    (-> google-auth .-currentUser .get .getBasicProfile)))

(defn load-client! [credentials signed-in-listener]
  (js/gapi.load "client:auth2:picker" #(<init-client credentials signed-in-listener)) ;':' separator
  )

;=======================================================================

(defn <create-file [{:keys [file-name mime-type parents app-data? properties]}]
  (trace log '<create-file file-name)
  (let [metadata {
                  :name          file-name                  ;"yetipad.ydn"
                  :mimeType      (or mime-type text-mime)   ;ydn-mime
                  :fields        "id, appProperties"
                  :appProperties properties
                  :parents       (cond
                                   parents (clj->js parents)
                                   app-data? ["appDataFolder"]
                                   )
                  }]
    (<thenable #(js/gapi.client.drive.files.create (clj->js metadata)) :result)
    ))

(defn <list-app-data-files [{:keys [query]}]
  (trace log '<list-app-data-files query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {:spaces "appDataFolder"
                :fields "files(id, name, modifiedTime, appProperties)"
                :q      query
                }]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    (<thenable #(js/gapi.client.drive.files.list (clj->js params)) :result)
    ))

(defn <list-app-files [{:keys [query fields]}]
  (trace log '<list-app-files query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {
                ;https://developers.google.com/drive/api/v3/reference/files
                :fields fields
                ;https://developers.google.com/drive/api/v3/search-files
                :q      query
                }]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    (<thenable #(js/gapi.client.drive.files.list (clj->js params)) :result)
    ))

(defn <write-file-content
  "Write or overwrite the content of an existing file."
  [file-id content & [{:keys [#_mime-type content-type fields]}]]
  (trace log '<write-file-content file-id)
  (assert file-id)
  (let [body (case content-type
               :edn (pr-str content)
               content)
        req-params {:path   (str "/upload/drive/v3/files/" file-id) ;The URL to handle the request.
                    :method :PATCH                          ;default get
                    :params {;URL params
                             :uploadType :media
                             :fields     (or fields "id, name, modifiedTime, trashed, appProperties")
                             ;:mimeType   (or mime-type text-mime)
                             }
                    :body   body                            ;string | object	The HTTP request body (applies to PUT or POST).
                    }]
    ;https://github.com/google/google-api-javascript-client/blob/master/docs/reference.md
    (<thenable #(js/gapi.client.request (clj->js req-params)) :result)
    ))

(defn <read-file-content [file-id & [options]]
  (trace log '<read-file-content file-id)
  (assert file-id)
  ;https://developers.google.com/drive/api/v3/manage-downloads
  (let [params {:fileId file-id
                :alt    "media"
                }]
    (<thenable #(js/gapi.client.drive.files.get (clj->js params)) :body-edn options)
    ))

(defn <get-file-meta
  ;warning: on error, doesn't respond
  [file-id & [{:keys [fields]}]]
  (trace log '<get-file-meta file-id)
  (assert file-id)
  (let [params {:fileId file-id
                :fields (or (and (vector? fields) (str/join \, (map name fields)))
                            fields)
                }]
    ;https://developers.google.com/drive/api/v3/fields-parameter
    (<thenable #(js/gapi.client.drive.files.get (clj->js params)) :result)
    ))

(defn delete-file
  "Permanently deletes a file owned by the user without moving it to the trash."
  ;todo not tested
  [file-id]
  ;https://developers.google.com/drive/api/v3/reference/files/delete
  (let [params {:fileId file-id}]
    (<thenable #(js/gapi.client.drive.files.delete (clj->js params)) :result)
    ))

(defn <trash-file [file-id]
  (trace log '<trash-file file-id)
  ;https://developers.google.com/drive/api/v3/reference/files/update
  ;https://developers.google.com/drive/api/v3/reference/files#resource-representations
  (let [params {:fileId  file-id
                :trashed true
                }]
    (<thenable #(js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

(defn <add-properties
  "Add custom properties to the file as a map.
  To remove a property, set its value to nil.
  modifiedTime is updated.
  "
  [file-id property-map]
  (trace log '<add-properties file-id property-map)
  ;https://developers.google.com/drive/api/v3/properties
  (let [params {:fileId        file-id
                :appProperties property-map
                :fields        "appProperties, id, name"
                }]
    (<thenable #(js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

(defn <update-file
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
                      [:fields (and fields (str/join \, (map cljs.core/name fields)))]
                      ]
        params (into {} (for [f field-values, :when (second f)] f))
        ]
    (<thenable #(js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

#_(defn- <watch-file [file-id]
    ;https://developers.google.com/drive/api/v3/push
    ;needs to respond to a url
    (let [params {:fileId file-id}]
      (<thenable #(js/gapi.client.drive.files.watch (clj->js params)))
      ))
