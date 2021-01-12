(ns app.ui.note-editor
  (:require
    [reagent.core :refer [create-class as-element]]
    [reagent.dom :as rdom :refer [dom-node]]
    [re-frame.core :as re-frame]
    ;[hickory.core :refer [parse-fragment as-hiccup]]        ;https://github.com/davidsantiago/hickory
    [app.ui.editor :refer [create-editor editor-buttons-standard]]
    [app.ui.utils :as ui-utils]
    [app.ui.ui :as ui :refer [error-boundary]]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [cljs.pprint :refer [pprint]]
    [lib.html-parse :as hp]
    [lib.debug :as debug :refer [we wd wee expose]]
    [app.events :as evts]
    ))
;(-> "<a href=\"foo\">foo</a>" parse as-hiccup)

(def log (log/logger 'app.note-editor))

(def rsubs (comp deref re-frame/subscribe))
(def dispatch! re-frame/dispatch)

(defn not-empty-content [content]
  (when (some #(if (vector? %)
                 (let [[kind] %] (when (not= kind :br) %))
                 %) content)
    content
    ))

;has issues
#_(defn html->clj [html]
    (map as-hiccup (parse-fragment html)))

(defn parse-content [editor]
  (when editor
    (let [html (.getCleanContents editor)
          content (not-empty-content (hp/html->clj html))
          ]
      content
      )))

(defn content-editor [{:keys [id content]}]
  (let [
        toolbar-id (str id :toolbar)
        editor-id (str id :edit-pane)
        editor* (atom nil)
        ]
    ;Doesn't re-render when title is edited so inner render function not required.
    [:div.content-editor
     [:div {:id toolbar-id}]
     [(with-meta (fn [] ^{:key :content} [:div {:id editor-id :style {:min-height "6em"}} content])
                 {:component-did-mount
                  (fn [_this]
                    (let [editor (create-editor toolbar-id editor-id editor-buttons-standard)]
                      (reset! editor* editor)
                      ;(.focus (rdom/dom-node this))
                      ))
                  :component-will-unmount
                  (fn [_this]
                    (let [{:keys [dispose field reposition float-enable]} @editor*
                          content (parse-content field)
                          ]
                      ;(debug log "writing content: ")
                      ;(pprint content)
                      (dispatch! [::evts/new-content id content])
                      (dispose)
                      (reset! editor* nil)
                      ))
                  })]]))