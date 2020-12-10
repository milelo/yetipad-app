(ns app.core
  (:require
    ;make logger first item
    [lib.log :as log :refer [trace debug info warn fatal]]
    [app.store :as store]
    [reagent.core :as reagent]
    [reagent.dom :as rdom]
    [re-frame.core :as re-frame]
    [breaking-point.core :as bp]
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
    ))

(def log (log/logger 'app.core))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/app-root] root-el)))

(defn on-window-focus [e]
  ;(js/console.log js/document.activeElement)
  ;accept-edit click can trigger window-focus so delay so event synch
  ; can be inhibited by 'db.saving?'.
  (js/setTimeout #(re-frame/dispatch [::events/window-focused]) 100)
  )

(defn init []
  (trace log :init)
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [::bp/set-breakpoints
                           {:breakpoints [:mobile
                                          768
                                          :tablet
                                          992
                                          :small-monitor
                                          1200
                                          :large-monitor]
                            :debounce-ms 166}])
  (dev-setup)
  ;(events/init-navigation!)
  (mount-root)
  (debug log :add-focus-listener)
  (gevents/listen js/window "focus" on-window-focus)
  (add-watch log/config* ::logger-config (fn [_k _r o n]
                                           (when-not (identical? o n)
                                             (re-frame/dispatch [::events/logger-config n]))))
  (re-frame/dispatch [::events/logger-config @log/config*])
  )

(defn ^:export handle-client-load []
  (trace log :handle-client-load)
  (store/load-client (fn [signed-in?]
                       (re-frame/dispatch [::events/signed-in signed-in?])
                       )))

