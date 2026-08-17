(ns app.ui.tag-pane-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [app.ui.note-pane :as note-pane]
   [app.ui.tag-pane :as tag-pane]))

(defn contains-content-editor? [form]
   (boolean
   (some #(and (vector? %)
               (= ::note-pane/note-editor (second %)))
         (tree-seq coll? seq form))))

(deftest tag-existing-content-test
  (testing "non-empty tag content enables the content editor"
    (is (true? (tag-pane/existing-content? {:content [[:p "content"]]})))
    (is (true? (tag-pane/existing-content? (:source {:source {:content [[:p "content"]]}})))))
  (testing "missing and empty tag content disables the content editor"
    (is (false? (tag-pane/existing-content? {})))
    (is (false? (tag-pane/existing-content? {:content ""})))
    (is (false? (tag-pane/existing-content? {:content []})))))

(deftest tag-conversion-button-test
  (testing "the conversion button is shown only for contentful tags"
    (let [buttons-with-content (get-in (tag-pane/tag-view {:id "tag"
                                                            :kind :tag
                                                            :content [[:p "content"]]})
                                       [2 :buttons])
          buttons-without-content (get-in (tag-pane/tag-view {:id "tag"
                                                               :kind :tag})
                                          [2 :buttons])]
      (is (some #{tag-pane/convert-tag-to-note-button} buttons-with-content))
      (is (not (some #{tag-pane/convert-tag-to-note-button} buttons-without-content))))))

(deftest note-editor-content-option-test
  (let [item {:source {:id "item-1" :title "Item" :content [[:p "content"]]}}
        default-body (get-in (note-pane/note-editor item) [2 :body])
        disabled-body (get-in (note-pane/note-editor item {:content-editor? false}) [2 :body])]
    (testing "note editing keeps the content editor by default"
      (is (contains-content-editor? default-body)))
    (testing "the optional setting removes only the content editor"
      (is (not (contains-content-editor? disabled-body))))))
