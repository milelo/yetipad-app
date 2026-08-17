(ns app.events-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [app.events :as events]))

(def tag {:id "a"
          :kind :tag
          :title "A tag"
          :tags ["1"]
          :content [[:p "content"]]
          :create "created"})

(deftest convert-tag-to-note-test
  (let [db {:doc-id "doc"
            :doc {"a" tag "1" {:id "1" :kind :tag}
                  :doc-id "doc"}
            :open-items ["a"]}
        converted (events/convert-tag-to-note db "a")
        note-id (first (remove #{"a" "1" :doc-id} (keys (:doc converted))))
        note (get-in converted [:doc note-id])]
    (testing "copies the tag into a new note"
      (is (string? note-id))
      (is (not= "a" note-id))
      (is (= :note (:kind note)))
      (is (= (:title tag) (:title note)))
      (is (= (:tags tag) (:tags note)))
      (is (= (:content tag) (:content note))))
    (testing "clears only content from the original tag and opens both"
      (is (= :tag (get-in converted [:doc "a" :kind])))
      (is (nil? (get-in converted [:doc "a" :content])))
      (is (= ["a" note-id] (:open-items converted)))))
  (testing "does nothing for empty content or non-tags"
    (let [base {:doc {"empty" {:id "empty" :kind :tag}
                      "note" {:id "note" :kind :note :content [[:p "content"]]}}
               :open-items []}]
      (is (= base (events/convert-tag-to-note base "empty")))
      (is (= base (events/convert-tag-to-note base "note"))))))
