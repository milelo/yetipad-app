(ns app.ui.search)

(defn cancel-debounce! [timer*]
  (when-let [timer @timer*]
    (js/clearTimeout timer)
    (reset! timer* nil)))

(defn debounce!
  "Schedules f after delay-ms, cancelling any previously scheduled call."
  [timer* delay-ms f]
  (cancel-debounce! timer*)
  (let [timer (js/setTimeout
                (fn []
                  (reset! timer* nil)
                  (f))
                delay-ms)]
    (reset! timer* timer)
    timer))
