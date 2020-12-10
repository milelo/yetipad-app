;;based on: https://github.com/lane-s/cljs-react-material-ui
(ns app.ui.theme
  (:require
    ))

(def theme {::small-icon   {:style {:font-size 18}}
            ::index-list   {:style {:padding "0 0 0 8px"}}
            ::pane         {:style {:margin-top 10}}
            ::pane-buttons {:style {:display         :flex
                                    :justify-content :flex-end
                                    }}
            })