(ns app.ui.about-pane
  (:require
    [app.ui.registry :as reg]
    [app.ui.ui :as ui]
    [app.subs :as subs]
    [lib.utils :as utils :refer-macros [for-all]]
    ["@mui/material" :refer [Tooltip Typography
                                 TableContainer TableBody Table TableHead TableRow TableCell
                                 ]]
    ["@mui/icons-material/Info" :default about-icon]))

(defn table [data]
  [:<>
   [:> Typography {:variant :h6} "YetiPad App"]
   [:> TableContainer
    [:> Table {:size :small}
     [:> TableBody
      (for-all [{:keys [title content]} data]
        ^{:key title} [:> TableRow [:> TableCell title] [:> TableCell content]]
        )]]]])


(defn about-pane [_context]
  (let [item {:id    :about
              :kind  :about
              :title "About app"
              }
        ]
    [ui/viewer-pane item
     {:body    (let [home "https://github.com/milelo/yetipad-app"]
                 [table [{:title "Home-page" :content [:a {:href home} home]}
                         {:title "Version" :content @subs/!app-version}
                         {:title "Author" :content "Mike Longworth"}
                         ]])
      :buttons [ui/fullscreen-button]
      }]))

(reg/register {:id    :about
               :title "About"
               :icon  about-icon
               :pane  about-pane
               })
