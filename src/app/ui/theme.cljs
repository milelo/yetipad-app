;;based on: https://github.com/lane-s/cljs-react-material-ui
(ns app.ui.theme
  (:require
    ))

(def theme {::small-icon   {:style {:font-size 18}}
            ::index-list   {:style {:padding "0 0 0 8px"}}
            ::pane         {:style {:margin-top 10}}
            ::items        {:id    "items"
                            :style {:height     "calc(100vh - 64px)"
                                    :overflow-y :auto}}
            ::pane-tags    {:class "pane-tags"
                            :style {:position        :sticky
                                    :bottom          0
                                    :z-index         1
                                    :background-color :white
                                    :margin-top      4
                                    :padding         10
                                    :border-top      "solid 1px LightGrey"}}
            ::pane-buttons {:class "pane-toolbar"
                            :style {:display         :flex
                                    :justify-content :flex-end
                                    :position        :sticky
                                    :top             0
                                    :z-index         1
                                    :background-color :white}}
            })
