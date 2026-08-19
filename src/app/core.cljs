(ns app.core
  (:require
    ;make logger first item
   [lib.log :as log :refer-macros [trace debug info warn fatal]]
   [app.store :as store]
   [reagent.core :as reagent]
   [promesa.core :as p]
   [reagent.dom :as rdom]
   [reagent.dom.client :as rdomc]
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
    (rdomc/render (rdomc/create-root root-el) [views/app-root])
    ;(rdom/render [views/app-root] root-el)
    ))

(defn on-window-focus [_e]
  ;(js/console.log js/document.activeElement)
  ;accept-edit click can trigger window-focus so delay so event synch
  ; can be inhibited by 'db.saving?'.
  (js/setTimeout events/window-focused 100))

(defn initialize-drive-connectivity! []
  ;Configure synchronously so an early focus/click can already bootstrap Drive.
  (drive/configure! app.credentials/yetipad-credentials)
  (-> (p/let [{:keys [sign-in-email reduce-drive-popup-flash?]}
              (store/$read-persist-device)
              _ (drive/set-preferences!
                 {:reduce-drive-popup-flash? (boolean reduce-drive-popup-flash?)})
              _ (drive/configure!
                 (into app.credentials/yetipad-credentials
                       [(when sign-in-email [:login_hint sign-in-email])]))]
        (when (not= false (.-onLine js/navigator))
          (events/reconnect-and-sync!! {:authorization :automatic :src ::startup})))
      (p/catch (fn [e]
                 (warn log 'drive-startup-unavailable e)
                 nil))))

(defn init []
  (trace log :init)
  (events/initialize-db!)
  (events/init-persist-device!!)
  (dev-setup)
  (mount-root)
  (debug log :add-focus-listener)
  (gevents/listen js/window "focus" on-window-focus)
  (gevents/listen js/window "online" (fn [_] (events/connectivity-changed! true)))
  (gevents/listen js/window "offline" (fn [_] (events/connectivity-changed! false)))
  (gevents/listen js/window "pagehide" (fn [_] (events/persist-active-session!)))
  (gevents/listen js/document "visibilitychange"
                  (fn [_]
                    (when (= "hidden" (.-visibilityState js/document))
                      (events/persist-active-session!))))
  (add-watch log/!config ::logger-config (fn [_k _r o n]
                                           (when-not (identical? o n)
                                             (events/logger-config! n))))
  (events/logger-config! @log/!config)
  (events/init-navigation!)
  (initialize-drive-connectivity!)
  ; Manifest metadata is optional and must never delay local document startup.
  (events/init-manifest!!))

