(ns app.credentials)

(def yetipad-credentials-old
  ;The nature of the app means these can't be kept secret.
  ;To ensure security they are only valid for specified API's and white-listed domains:
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

(def yetipad-credentials
  {:client_id "582900055519-d660gmj115ds3se0hm8l6t773cdledr7.apps.googleusercontent.com"
   ;:scope     "https://www.googleapis.com/auth/drive.appdata https://www.googleapis.com/auth/drive.file"
   :scope     "https://www.googleapis.com/auth/drive.file"
   :callback ""})
