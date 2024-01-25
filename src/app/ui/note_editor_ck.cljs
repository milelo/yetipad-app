(ns app.ui.note-editor-ck
  (:require
   [lib.log :as log :refer-macros [trace debug info warn error fatal] :refer [pprintl]]
   [reagent.core :as r]
   [cljs-bean.core :refer [bean ->clj ->js]]
   [reagent.dom.server :as rdoms]
   [lib.html-parse :as hp]
   [app.events :as evts]
   [app.ui.ui :as ui]
   ["@ckeditor/ckeditor5-react" :refer [CKEditor]]
   ["@ckeditor/ckeditor5-build-classic" :as ClassicEditor]
  ;;  ["@ckeditor/ckeditor5-editor-classic" :as ClassicEditor]
  ;;  ["@ckeditor/ckeditor5-alignment" :refer [Alignment]]
  ;;  ["@ckeditor/ckeditor5-autoformat" :refer [Autoformat]]
  ;;  ["@ckeditor/ckeditor5-basic-styles" :refer [Bold Code Italic Strikethrough Subscript Superscript Underline]]
  ;;  ["@ckeditor/ckeditor5-block-quote" :refer [BlockQuote]]
  ;;  ["@ckeditor/ckeditor5-code-block" :refer [CodeBlock]]
  ;;  ["@ckeditor/ckeditor5-core" :refer [EditorConfig]]
  ;;  ["@ckeditor/ckeditor5-essentials" :refer [Essentials]]
  ;;  ["@ckeditor/ckeditor5-find-and-replace" :refer [FindAndReplace]]
  ;;  ["@ckeditor/ckeditor5-font" :refer [FontBackgroundColor FontColor FontFamily FontSize]]
  ;;  ["@ckeditor/ckeditor5-heading" :refer [Heading]]
  ;;  ["@ckeditor/ckeditor5-highlight" :refer [Highlight]]
  ;;  ["@ckeditor/ckeditor5-horizontal-line" :refer [HorizontalLine]]
  ;;  ["@ckeditor/ckeditor5-html-embed" :refer [HtmlEmbed]]
  ;;  ["@ckeditor/ckeditor5-html-support" :refer [GeneralHtmlSupport HtmlComment]]
  ;;  ["@ckeditor/ckeditor5-image" :refer [AutoImage Image ImageCaption ImageInsert ImageResize ImageStyle ImageToolbar ImageUpload]]
  ;;  ["@ckeditor/ckeditor5-indent" :refer [Indent IndentBlock]]
  ;;  ["@ckeditor/ckeditor5-link" :refer [AutoLink Link LinkImage]]
  ;;  ["@ckeditor/ckeditor5-list" :refer [List ListProperties TodoList]]
  ;;  ["@ckeditor/ckeditor5-markdown-gfm" :refer [Markdown]]
  ;;  ["@ckeditor/ckeditor5-media-embed" :refer [MediaEmbed MediaEmbedToolbar]]
  ;;  ["@ckeditor/ckeditor5-mention" :refer [Mention]]
  ;;  ["@ckeditor/ckeditor5-page-break" :refer [PageBreak]]
  ;;  ["@ckeditor/ckeditor5-paragraph" :refer [Paragraph]]
  ;;  ["@ckeditor/ckeditor5-paste-from-office" :refer [PasteFromOffice]]
  ;;  ["@ckeditor/ckeditor5-remove-format" :refer [RemoveFormat]]
  ;;  ["@ckeditor/ckeditor5-select-all" :refer [SelectAll]]
  ;;  ["@ckeditor/ckeditor5-show-blocks" :refer [ShowBlocks]]
  ;;  ["@ckeditor/ckeditor5-source-editing" :refer [SourceEditing]]
  ;;  ["@ckeditor/ckeditor5-special-characters" :refer [SpecialCharacters SpecialCharactersMathematical SpecialCharactersText]]
  ;;  ["@ckeditor/ckeditor5-table" :refer [Table TableCellProperties TableColumnResize TableProperties TableToolbar]]
  ;;  ["@ckeditor/ckeditor5-typing" :refer [TextTransformation]]
  ;;  ["@ckeditor/ckeditor5-undo" :refer [Undo]]
  ;;  ["@ckeditor/ckeditor5-watchdog" :refer [EditorWatchdog]]
   ))

(def log (log/logger 'app.ui.note-editor-ck))

(defn ck-editor [id content]
  (let [editor* (atom nil)]
    (r/create-class
     {:display-name "ck-editor"
      :reagent-render
      (fn [id content]
        ^{:key (str :editor- id)} [:> CKEditor
                                   {:editor ClassicEditor
                                    :config {:toolbar {:shouldNotGroupWhenFull true
                                                       #_:items #_[:undo
                                                               :redo
                                                               \|
                                                               :heading
                                                               \|
                                                               :bold
                                                               :italic
                                                               :strikethrough
                                                               :superscript
                                                               \|
                                                               :link
                                                               :bulletedList
                                                               :numberedList
                                                               :outdent
                                                               :indentBlock
                                                               :indent
                                                               \|
                                                               :blockQuote
                                                               :codeBlock
                                                               :uploadImage
                                                               :insertTable
                                                               :mediaEmbed]}
                                             :ui {:viewport-offset {:top 60}}
                                             ;:plugins [Bold Italic Strikethrough]
                                             }
                                    :data (rdoms/render-to-string content)
                                    :on-ready #(reset! editor* %)}])
      
      :component-will-unmount
      (fn [_this]
        (evts/new-content! id (-> @editor* .getData hp/html->clj not-empty))
        (reset! editor* nil))
      })))


(defn content-editor [{:keys [id content]}]
  [ui/error-boundary ::ck-editor
   [:div.content-editor {}
    [ck-editor id content]]])