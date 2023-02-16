(ns app.core
  (:require
    ;make logger first item
   [lib.log :as log :refer-macros [trace debug info warn fatal]]
   [app.store :as store]
   [reagent.core :as reagent]
   [promesa.core :as p]
   [reagent.dom :as rdom]
   [app.events :as events]
   [app.ui.views :as views]
   [app.config :as config]
   [goog.events :as gevents]
   [app.ui.note-pane]
   [app.ui.options-pane]
   [app.ui.tag-pane]
   [app.ui.trash-pane]
   [app.ui.log-pane]
   [app.ui.log-config-pane]
   [app.ui.about-pane]
   [lib.goog-drive :as drive]
   [app.credentials]
   [lib.localstore :as ls]))

(def log (log/logger 'app.core))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/app-root] root-el)))

(defn on-window-focus [_e]
  ;(js/console.log js/document.activeElement)
  ;accept-edit click can trigger window-focus so delay so event synch
  ; can be inhibited by 'db.saving?'.
  (js/setTimeout events/window-focused 100))

(defn init []
  (trace log :init)
  (events/initialize-db!)
  (dev-setup)
  (mount-root)
  (debug log :add-focus-listener)
  (gevents/listen js/window "focus" on-window-focus)
  (add-watch log/config* ::logger-config (fn [_k _r o n]
                                           (when-not (identical? o n)
                                             (events/logger-config! n))))
  (events/logger-config! @log/config*)
  (events/init-navigation!))

#_(defn ^:export handle-client-load []
  (trace log :handle-client-load)
  (store/load-client (fn [signed-in?]
                       (events/signed-in signed-in?))))

(defn ^:export gapi-load []
  (drive/gapi-load! app.credentials/yetipad-credentials))

(defn ^:export gis-init []
  (p/let [{:keys [email]} (ls/$get-data :*user-info)]
    (trace log 'email email)
    (drive/gis-init! (into app.credentials/yetipad-credentials [(when email [:hint email])])
                     events/on-authorized!)))

