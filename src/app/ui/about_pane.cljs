(ns app.ui.about-pane
  (:require
    [app.ui.registry :as reg]
    [app.ui.ui :as ui]
    [lib.utils :as utils :refer-macros [for-all]]
    ["@material-ui/core" :refer [Tooltip Typography
                                 TableContainer TableBody Table TableHead TableRow TableCell
                                 ]]
    ["@material-ui/icons/Info" :default about-icon]
    ["@material-ui/icons/SystemUpdateTwoTone" :default update-icon]))

(defn table [data]
  [:<>
   [:> Typography {:variant :h6} "Yetipad - Development release"]
   [:> TableContainer
    [:> Table {:size :small}
     [:> TableBody
      (for-all [{:keys [title content]} data]
        ^{:key title} [:> TableRow [:> TableCell title] [:> TableCell content]]
        )]]]])


(defn read-log-button []
  [ui/item-button update-icon "update-app" #(js/window.location.reload true)])

(defn about-pane [_context]
  (let [item {:id    :about
              :kind  :about
              :title "About app"
              }
        ]
    [ui/viewer-pane item
     (let [home "https://github.com/milelo/yetipad-app"
           app "https://yetipad.mikelongworth.uk"
           ]
       [table [{:title "Home-page" :content [:a {:href home} home]}
               {:title "App: new document" :content [:a {:href app} app]}
               {:title "Author" :content "Mike Longworth"}
               ]])
     nil
     {:buttons [read-log-button ui/fullscreen-button]}
     ]))

(reg/register {:kind  :about
               :title "About"
               :icon  about-icon
               :pane  about-pane
               })
