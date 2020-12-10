(ns lib.goog-drive
  (:require
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.utils :as utils :refer [js->cljs]]
    [cljs.reader :as reader]
    [cljs.core.async :as async :refer [<! >! chan put! take! close!] :refer-macros [go go-loop]]
    [lib.asyncutils :refer [put-last!] :refer-macros [<? go?]]
    [cljs.pprint :refer [pprint]]
    [clojure.string :as str]
    ))

(def log (log/logger 'app.goog-drive))

(def ydn-mime "application/vnd.google.drive.ext-type.ydn")
(def text-mime "text/plain")

(def yetipad-credentials
  ;This is only valid from specified domains:
  ;https://console.developers.google.com/apis/credentials?project=yetipad
  {
   :apiKey        "AIzaSyBkHq4UU3q_TBPe80MteDpD1ar28tj_Jjg"
   :clientId      "582900055519-d660gmj115ds3se0hm8l6t773cdledr7.apps.googleusercontent.com",
   :discoveryDocs ["https://www.googleapis.com/discovery/v1/apis/drive/v3/rest"]
   ;scope see: https://developers.google.com/drive/api/v3/about-auth
   ;https://developers.google.com/drive/api/v3/about-auth#migrate_an_existing_app_to_a_recommended_scope
   ;https://www.googleapis.com/auth/drive.file - can be used with google file-picker
   ;https://developers.google.com/drive/api/v3/picker
   ;https://developers.google.com/drive/api/v3/appdata
   :scope         "https://www.googleapis.com/auth/drive.appdata https://www.googleapis.com/auth/drive.file"
   })

(defn read-string [s]
  (try
    (reader/read-string s)
    (catch :default e
      (println s)
      (js/console.error e)
      e
      )))

(defn <thenable [thenable return-type & [{:keys [default] :as opt}]]
  (when opt (trace log :thenable-opt opt))
  (let [c (chan)]
    (.then thenable
           (fn [response]
             (trace log :thenable-response return-type)
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
             (trace log :thenable-error return-type)
             (let [error (some-> error-response .-result .-error js->cljs)
                   ; error (some-> error-response .-result .-error .-errors (aget 0) js->cljs)
                   error (or error (js->cljs error-response))
                   ]
               (warn log :error-response \newline (with-out-str (pprint error)))
               ;don't use ExceptionInfo! re-frame doesn't recognise it
               (let [e (js/Error (pr-str error))]
                 (set! (.-data e) error)
                 (put-last! c e))
               )))
    c))

(defn <create-file [{:keys [file-name mime-type parents app-data? properties]}]
  (trace log :<create-file file-name)
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
  (trace log :<list-app-data-files query)
  ;https://developers.google.com/drive/api/v3/appdata
  (let [params {:spaces "appDataFolder"
                :fields "files(id, name, modifiedTime, appProperties)"
                :q      query
                }]
    ;https://developers.google.com/drive/api/v3/reference/files/list
    (<thenable (js/gapi.client.drive.files.list (clj->js params)) :result)
    ))

(defn <list-app-files [{:keys [query fields]}]
  (trace log :<list-app-data-files query)
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
  (trace log :<write-file-content file-id)
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
  (trace log :<get-file-content file-id)
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
  (trace log :<get-file-meta file-id)
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
  (trace :<trash-file file-id)
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
  (trace :<add-properties file-id property-map)
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

(defn init-client! [signed-in-listener]
  (go
    (info log "init-client!")
    (let [status (<? (<thenable (js/gapi.client.init
                                  (clj->js (if :popup
                                             yetipad-credentials
                                             (merge yetipad-credentials
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
        (info log 'init-client! :ok))
      (if (:error status)
        (signed-in-listener false)
        (let [auth2 (js/gapi.auth2.getAuthInstance)
              signed-in? (-> auth2 .-isSignedIn .get)
              ]
          (-> auth2 .-isSignedIn (.listen signed-in-listener))
          (signed-in-listener signed-in?)
          ;(.signOut auth2)
          (when-not signed-in?
            (trace log :init-client! :sign-in)
            (.signIn auth2)
            )))
      )
    nil))

(defn load-client! [signed-in-listener]
  (js/gapi.load "client:auth2:picker" #(init-client! signed-in-listener)) ;':' separator
  )