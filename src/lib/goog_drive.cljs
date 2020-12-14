(ns lib.goog-drive
  (:require
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.utils :as utils :refer [js->cljs]]
    [cljs.reader :as reader]
    [lib.debug :as debug :refer [we wd wee expose]]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go go-loop]]
    [lib.asyncutils :refer [put-last!] :refer-macros [<? go?]]
    [cljs.pprint :refer [pprint]]
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

(defn <thenable [thenable return-type & [{:keys [default] :as opt}]]
  (when opt (trace log 'thenable-opt opt))
  (let [c (chan)]
    (.then thenable
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
               (put-last! c (or response default false)))
             )
           (fn [error-response]
             (trace log 'thenable-error return-type)
             (let [error (some-> error-response .-result .-error js->cljs)
                   ; error (some-> error-response .-result .-error .-errors (aget 0) js->cljs)
                   error (or error (js->cljs error-response))
                   ]
               (warn log 'error-response \newline (with-out-str (pprint error)))
               ;don't use ExceptionInfo! re-frame doesn't recognise it
               (let [e (js/Error (pr-str error))]
                 (set! (.-data e) error)
                 (put-last! c e))
               )))
    c))

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
    (<thenable (js/gapi.client.drive.files.create (clj->js metadata)) :result)
    ))

(defn <list-app-data-files [{:keys [query]}]
  (trace log '<list-app-data-files query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {:spaces "appDataFolder"
                :fields "files(id, name, modifiedTime, appProperties)"
                :q      query
                }]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    (<thenable (js/gapi.client.drive.files.list (clj->js params)) :result)
    ))

(defn <list-app-files [{:keys [query fields]}]
  (trace log '<list-app-data-files query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {
                ;https://developers.google.com/drive/api/v3/reference/files
                :fields fields
                ;https://developers.google.com/drive/api/v3/search-files
                :q      query
                }]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    (<thenable (js/gapi.client.drive.files.list (clj->js params)) :result)
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
    (<thenable (js/gapi.client.request (clj->js req-params)) :result)
    ))

(defn <get-file-content [file-id & [options]]
  (trace log '<get-file-content file-id)
  (assert file-id)
  ;https://developers.google.com/drive/api/v3/manage-downloads
  (let [params {:fileId file-id
                :alt    "media"
                }]
    (<thenable (js/gapi.client.drive.files.get (clj->js params)) :body-edn options)
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
    (<thenable (js/gapi.client.drive.files.get (clj->js params)) :result)
    ))

(defn delete-file
  "Permanently deletes a file owned by the user without moving it to the trash."
  ;todo not tested
  [file-id]
  ;https://developers.google.com/drive/api/v3/reference/files/delete
  (let [params {:fileId file-id}]
    (<thenable (js/gapi.client.drive.files.delete (clj->js params)) :result)
    ))

(defn <trash-file [file-id]
  (trace log '<trash-file file-id)
  ;https://developers.google.com/drive/api/v3/reference/files/update
  ;https://developers.google.com/drive/api/v3/reference/files#resource-representations
  (let [params {:fileId  file-id
                :trashed true
                }]
    (<thenable (js/gapi.client.drive.files.update (clj->js params)) :result)
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
    (<thenable (js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

(defn <rename-file2 [file-id title]
  ;untested
  ;https://developers.google.com/drive/api/v3/reference/files/update
  (let [params {:fileId   file-id
                :resource {:title title}
                }]
    (<thenable (js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

(defn <rename-file [file-id name]
  ;untested
  ;https://developers.google.com/drive/api/v3/reference/files/update
  ;https://developers.google.com/drive/api/v3/reference/files#resource-representations
  (let [params {:fileId file-id
                :name   name
                }]
    (<thenable (js/gapi.client.drive.files.update (clj->js params)) :result)
    ))

#_(defn- <watch-file [file-id]
    ;https://developers.google.com/drive/api/v3/push
    ;needs to respond to a url
    (let [params {:fileId file-id}]
      (<thenable (js/gapi.client.drive.files.watch (clj->js params)))
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

(defn init-client! [credentials signed-in-listener]
  (go
    (info log 'init-client!)
    ;https://github.com/google/google-api-javascript-client/blob/master/docs/reference.md
    (let [status (<? (<thenable (js/gapi.client.init
                                  (clj->js (if :popup
                                             credentials
                                             (merge credentials
                                                    ;redirect
                                                    {:ux_mode      "redirect"
                                                     ;https://developers.google.com/identity/protocols/oauth2/openid-connect#setredirecturi
                                                     :redirect_uri js/window.location.href
                                                     }
                                                    )))) :response)
                     (fn [e] (.-data e)))
          ]
      (if status
        (info log 'init-client! \newline (with-out-str (pprint status)))
        (info log 'init-client! 'ok))
      (if (:error status)
        (signed-in-listener false)
        (let [google-auth (js/gapi.auth2.getAuthInstance)
              signed-in? (-> google-auth .-isSignedIn .get)
              auth-resp (-> google-auth .-currentUser .get .getAuthResponse js->cljs)
              ]
          ;(debug log 'users-name (.getName (basic-user-profile)))
          (-> google-auth .-isSignedIn (.listen signed-in-listener))
          (signed-in-listener signed-in?)
          ;(.signOut auth2)
          (when-not signed-in?
            (trace log 'init-client! 'sign-in)
            (.signIn google-auth)
            )
          (info log 'access-token-expiry (-> auth-resp :expires_at utils/format-ms))
          ;re-initialise before access token expiry (+1h)
          (let [refresh-in (* (:expires_in auth-resp) 800)]
            (info log 'refresh-access-token-at= (utils/format-ms (+ (utils/time-now-ms) refresh-in)))
            (js/setTimeout #(init-client! credentials signed-in-listener) refresh-in))))
      )
    nil))

(defn load-client! [credentials signed-in-listener]
  (js/gapi.load "client:auth2:picker" #(init-client! credentials signed-in-listener)) ;':' separator
  )