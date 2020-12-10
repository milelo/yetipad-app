(ns app.ui.editor
  (:require
    [reagent.core :as reagent]
    [re-frame.core :as re-frame]
    [lib.log :as log :refer [trace debug info warn fatal]]
    [lib.debug :as debug :refer [we wd wee]]
    [lib.utils :as u]
    ;[lib.html-parse :as hp]
    [cljs.core.async :as async :refer [chan put! take!]]
    ;[dommy.core :as dom]
    ;[hickory.core :refer [parse as-hiccup]]                 ;https://github.com/davidsantiago/hickory
    )
  (:import
    [goog events]
    [goog.events EventType]
    [goog.editor Command Field SeamlessField]
    [goog.editor.plugins BasicTextFormatter RemoveFormatting UndoRedo ListTabHandler SpacesTabHandler
                         EnterHandler HeaderFormatter LinkDialogPlugin LinkBubble]
    [goog.ui ScrollFloater]
    [goog.ui.editor DefaultToolbar ToolbarController]
    ))


(def log (log/logger 'app.ui.editor))

(def <sub (comp deref re-frame/subscribe))
(def >evt re-frame/dispatch)

(def editor-buttons-standard [
                              Command.FORMAT_BLOCK
                              Command.BOLD,
                              Command.ITALIC,
                              Command.UNDERLINE,
                              Command.FONT_COLOR,
                              Command.BACKGROUND_COLOR,
                              Command.FONT_FACE,
                              Command.FONT_SIZE,
                              Command.LINK,
                              Command.UNDO,
                              Command.REDO,
                              Command.UNORDERED_LIST,
                              Command.ORDERED_LIST,
                              Command.INDENT,
                              Command.OUTDENT,
                              Command.JUSTIFY_LEFT,
                              Command.JUSTIFY_CENTER,
                              Command.JUSTIFY_RIGHT,
                              Command.SUBSCRIPT,
                              Command.SUPERSCRIPT,
                              Command.STRIKE_THROUGH,
                              Command.REMOVE_FORMAT
                              ;Command.EDIT_HTML ;- implementation: https://code.google.com/p/closure-library/issues/detail?id=97
                              ;Command.IMAGE
                              ])

(defn create-editor
  ;https://google.github.io/closure-library/api/goog.ui.ScrollFloater.html
  [toolbar-id editor-id buttons]
  (let [
        link-plugin (LinkDialogPlugin.)
        plugins [
                 (BasicTextFormatter.)
                 (RemoveFormatting.)
                 (UndoRedo.)
                 (ListTabHandler.)
                 (SpacesTabHandler.)
                 (EnterHandler.)
                 (HeaderFormatter.)
                 link-plugin
                 (LinkBubble.)
                 ]
        scroll-floater (ScrollFloater.)
        editor (SeamlessField. editor-id)                   ;create the editor
        toolbar-div (goog.dom.getElement toolbar-id)
        toolbar (DefaultToolbar.makeToolbar (clj->js buttons) toolbar-div)

        controller (ToolbarController. editor, toolbar)
        ;         controller (MyToolbarController. editor, toolbar)
        dispose (fn [] (wd :dispose [toolbar-id editor-id]) (doseq [d [scroll-floater editor toolbar controller]] (.dispose d)))
        reposition (fn [] (.update scroll-floater))
        ;reposition (fn [] (.handleResize_ scroll-floater))
        float-enable (fn [enabled?] (.setScrollingEnabled scroll-floater enabled?))
        ]
    (.showOpenLinkInNewWindow link-plugin true)
    (doseq [plugin plugins]
      (.registerPlugin editor plugin))
    (.makeEditable editor)
    (.setViewportTopOffset scroll-floater 64)
    (.decorate scroll-floater toolbar-div)
    ;      (-> scroll-floater (.getHandler) (.listen js/window goog.events.EventTypeRESIZE reposition))
    {:dispose dispose :editor editor :reposition reposition :float-enable float-enable}
    ))

