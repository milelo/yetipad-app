(ns app.events-test
  (:require
   [cljs.test :refer [async deftest is testing]]
   [app.events :as events]
   [lib.db :as db]
   [lib.goog-drive :as drive]
   [promesa.core :as p]))

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

(deftest sync-status-phase-test
  (testing "a Drive check has its own accepted status"
    (events/initialize-db!)
    (events/set-sync-status! :checking)
    (is (= :checking (:sync-status @db/!db))))
  (testing "only conflict resolution uses the bidirectional merge phase"
    (is (nil? (#'events/sync-progress-status :in-sync)))
    (is (= :downloading (#'events/sync-progress-status :overwrite-from-file)))
    (is (= :uploading (#'events/sync-progress-status :overwrite-file)))
    (is (= :syncing (#'events/sync-progress-status :resolve-conflicts)))))

(deftest drive-popup-preference-propagation-test
  (events/initialize-db!)
  (reset! drive/!preferences {:reduce-drive-popup-flash? false})
  (events/device-options! {:reduce-drive-popup-flash? true})
  (is (true? (:reduce-drive-popup-flash? @drive/!preferences)))
  (is (true? (get-in @db/!db [:persist-device :reduce-drive-popup-flash?]))))

(deftest metadata-edits-preserve-history-timestamp-test
  (async done
    (let [history-time "2026-08-20T10:00:00.000Z"
          base-item {:id "item"
                     :kind :note
                     :title "Original"
                     :content [[:p "Original"]]
                     :tags ["old-tag"]
                     :change "2026-08-19T10:00:00.000Z"
                     :mchange history-time}
          base-db {::db/db? true
                   :doc {:doc-id "doc" "item" base-item}
                   :editing {"item" {:accept-as "item"}}}]
      (reset! db/!db base-db)
      (events/new-title! "item" "Renamed")
      (-> (db/$queue-idle)
          (p/then (fn [_]
                    (testing "title edits preserve the history timestamp"
                      (is (= "Renamed" (get-in @db/!db [:doc "item" :title])))
                      (is (= history-time (get-in @db/!db [:doc "item" :change]))))
                    (reset! db/!db base-db)
                    (events/new-content! "item" [[:p "Updated"]])
                    (db/$queue-idle)))
          (p/then (fn [_]
                    (testing "content edits retain the existing timestamp behavior"
                      (is (= [[:p "Updated"]] (get-in @db/!db [:doc "item" :content])))
                      (is (= history-time (get-in @db/!db [:doc "item" :change]))))
                    (reset! db/!db base-db)
                    (events/new-tags! "item" ["new-tag"] nil)
                    (db/$queue-idle)))
          (p/then (fn [_]
                    (testing "tag assignment preserves the parent history timestamp"
                      (is (= ["new-tag"] (get-in @db/!db [:doc "item" :tags])))
                      (is (= history-time (get-in @db/!db [:doc "item" :change]))))
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (done)))))))
