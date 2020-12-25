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
      e
      )))

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
#_"
o User can sign-out any time
o Token sometimes fails to refresh on mobile (because app is suspended?)
"

(defn js-error [error]
  (let [e (js/Error (pr-str error))]
    (set! (.-data e) error)
    e))

(defn <ensure-authorised
  "Ensure the access-token hasn't expired, if it has refresh it and re-issue the request.
  This is necessary for mobile platforms that appear to put the app to sleep so it can't do a periodic
  token refresh."
  [error]
  (let [<c (chan)]
    (cond
      (= (:code error) 401) (let [_ (trace log '<ensure-authorised 'check-token)
                                  google-user (-> (js/gapi.auth2.getAuthInstance) .-currentUser .get)
                                  auth-resp (-> google-user .getAuthResponse js->cljs)
                                  now (utils/time-now-ms)
                                  token-expired? (> now (:expires_at auth-resp))
                                  ]
                              #_(debug log 'auth-resp (pprintl (select-keys auth-resp [:expires_at :expires_in :access_token])))
                              #_(debug log 'access-token-expired token-expired?
                                       (some-> auth-resp :expires_at utils/format-ms)
                                       'now now (-> now utils/format-ms))
                              (if token-expired?
                                (go
                                  (trace log '<ensure-authorised 'reload-auth-response)
                                  (.then (.reloadAuthResponse google-user)
                                         (fn [response]
                                           (trace log '<ensure-authorised 'response)
                                           (let [auth-resp (js->cljs response)]
                                             ;(debug log 'refresh-auth-resp (pprints (select-keys auth-resp [:expires_at :expires_in :access_token])))
                                             (info log 'refresh-token-expiry (-> auth-resp :expires_at utils/format-ms))
                                             (put-last! <c true)))
                                         (fn [error-response]
                                           (trace log '<ensure-authorised 'error-response)
                                           (let [error (js->cljs error-response)]
                                             (warn log 'refresh-error-response (pprintl error))
                                             (put-last! <c (js-error error))
                                             ))))
                                (put-last! <c false)
                                ))
      :else (put-last! <c false)
      ) <c))

(defn <thenable [thenable return-type & [{:keys [default] :as opt}]]
  (assert (fn? thenable))
  (when opt (trace log 'thenable-opt opt))
  (let [<c (chan)
        get-auth-response #(-> (js/gapi.auth2.getAuthInstance) .-currentUser .get .getAuthResponse js->cljs)
        ]
    (js/setTimeout (fn []
                     (put! <c (js/Error "Response timeout")
                           (fn [put?]
                             (when put?
                               (warn log '<thenable 'response-timeout (pprintl (get-auth-response))))
                             ))
                     (close! <c)
                     ) 3000)
    (.then (thenable)
           (fn [response]
             (trace log 'thenable-response return-type)
             ;(js/console.log response)
             (let [response (case return-type
                              :body-edn (-> response .-body read-string)
                              :result (some-> response .-result js->cljs)
                              :response (js->cljs response)
                              :raw response
                              )
                   ]
               (put-last! <c (or response default false)))
             )
           (fn [error-response]
             (go-try
               (trace log 'thenable-error return-type)
               (let [error (some-> error-response .-result .-error js->cljs)
                     error (or error (js->cljs error-response))
                     ]
                 (warn log 'error-response (pprintl error))
                 ;don't use ExceptionInfo! re-frame doesn't recognise it
                 (if (<? (<ensure-authorised error))
                   (let [_ (trace log '<thenable 'authorised)
                         response (<! (<thenable thenable return-type opt))
                         ]
                     (trace log '<thenable 'retry-response)
                     (put-last! <c response))
                   (do
                     (trace log '<thenable 'authorised-error)
                     (put-last! <c (js-error error)))
                   ))
               (catch :default e (do (trace log '<thenable 'caught (pprintl e)) (put-last! <c e))))))
    <c))

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
  [file-id content & [{:keys [mime-type content-type]}]]
  (trace log '<write-file-content file-id)
  (assert file-id)
  (let [body (case content-type
               :edn (pr-str content)
               content)
        req-params {:path   (str "/upload/drive/v3/files/" file-id) ;The URL to handle the request.
                    :method :PATCH                          ;default get
                    :params {;URL params
                             :uploadType :media
                             :fields     "id, name, modifiedTime"
                             ;:mimeType   (or mime-type text-mime)
                             }
                    :body   body                            ;string | object	The HTTP request body (applies to PUT or POST).
                    }]
    ;https://github.com/google/google-api-javascript-client/blob/master/docs/reference.md
    (<thenable #(js/gapi.client.request (clj->js req-params)) :result)
    ))

(defn <get-file-content [file-id & [options]]
  (trace log '<get-file-content file-id)
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

(defn <rename-file2 [file-id title]
  ;untested
  ;https://developers.google.com/drive/api/v3/reference/files/update
  (let [params {:fileId   file-id
                :resource {:title title}
                }]
    (<thenable #(js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

(defn <rename-file [file-id name]
  ;untested
  ;https://developers.google.com/drive/api/v3/reference/files/update
  ;https://developers.google.com/drive/api/v3/reference/files#resource-representations
  (let [params {:fileId file-id
                :name   name
                }]
    (<thenable #(js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

#_(defn- <watch-file [file-id]
    ;https://developers.google.com/drive/api/v3/push
    ;needs to respond to a url
    (let [params {:fileId file-id}]
      (<thenable #(js/gapi.client.drive.files.watch (clj->js params)))
      ))

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

(defn start-token-refresh []
  (let [google-auth (js/gapi.auth2.getAuthInstance)
        auth-resp (-> google-auth .-currentUser .get .getAuthResponse js->cljs)
        access-token (:access_token auth-resp)
        refresh-in (* (:expires_in auth-resp) 800)
        ;refresh-in 60000
        ]
    (info log 'start-token-refresh 'access-token-expiry (-> auth-resp :expires_at utils/format-ms))
    (info log 'start-token-refresh 'refresh-access-token-at (utils/format-ms (+ (utils/time-now-ms) refresh-in)))
    ;(take! (<ensure-authorised (simulate-error :token-expired)) #(debug log 'sim-token-refresh %))
    (js/setTimeout (fn []
                     (trace log 'start-token-refresh 'refreshing)
                     (let [signed-in? (-> google-auth .-isSignedIn .get)
                           current-access-token (-> google-auth .-currentUser .get .getAuthResponse .-access_token)
                           ]
                       (when (and signed-in? (= access-token current-access-token))
                         ;Issue https://github.com/google/google-api-javascript-client/issues/232
                         (.then (-> google-auth .-currentUser .get .reloadAuthResponse)
                                (fn [response]
                                  (let [auth-resp (js->cljs response)]
                                    ;(debug log 'refresh-auth-resp (pprints (select-keys auth-resp [:expires_at :expires_in :access_token])))
                                    (info log 'start-token-refresh 'refreshed-token-expiry (-> auth-resp :expires_at utils/format-ms))
                                    ))
                                (fn [error-response]
                                  (let [error (js->cljs error-response)]
                                    (warn log 'start-token-refresh 'refresh-error-response (pprintl error))
                                    )))
                         (start-token-refresh)
                         ))) refresh-in)
    ))

(defn init-client! [credentials signed-in-listener]
  (go
    (info log 'init-client!)
    ;https://github.com/google/google-api-javascript-client/blob/master/docs/reference.md
    (let [status (<? (<thenable #(js/gapi.client.init
                                   (clj->js (if :popup
                                              credentials
                                              (let [redirect_uri js/window.location.href]
                                                (debug log 'redirect_uri redirect_uri)
                                                (merge credentials
                                                       ;redirect
                                                       {:ux_mode      "redirect"
                                                        ;https://developers.google.com/identity/protocols/oauth2/openid-connect#setredirecturi
                                                        :redirect_uri redirect_uri
                                                        }
                                                       ))))) :response)
                     (fn [e] (.-data e)))
          ]
      (if status
        (info log 'init-client! (pprintl status))
        (info log 'init-client! 'ok))
      (if (:error status)
        (signed-in-listener false)
        (let [google-auth (js/gapi.auth2.getAuthInstance)
              signed-in? (-> google-auth .-isSignedIn .get)
              signed-in-listener (fn [signed-in?]
                                   (when signed-in? (start-token-refresh))
                                   (signed-in-listener signed-in?))
              ]
          ;(debug log 'users-name (.getName (basic-user-profile)))
          (-> google-auth .-isSignedIn (.listen signed-in-listener))
          (signed-in-listener signed-in?)
          ;(.signOut auth2)
          (when-not signed-in?
            (trace log 'init-client! 'sign-in)
            (.signIn google-auth)
            )
          )))) nil)

(defn load-client! [credentials signed-in-listener]
  (js/gapi.load "client:auth2:picker" #(init-client! credentials signed-in-listener)) ;':' separator
  )