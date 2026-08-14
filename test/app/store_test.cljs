(ns app.store-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [app.store :as store]))

(deftest active-session-normalization-test
  (testing "a valid session keeps its association and normalizes items"
    (is (= {:doc-id "doc-1" :open-items [:note-1 :note-2]}
           (store/normalize-active-session
            {:doc-id "doc-1" :open-items '(:note-1 :note-2)}))))
  (testing "malformed sessions are ignored"
    (is (nil? (store/normalize-active-session nil)))
    (is (nil? (store/normalize-active-session
               {:doc-id "doc-1" :open-items nil})))
    (is (nil? (store/normalize-active-session
               {:doc-id :doc-1 :open-items []})))))

(deftest document-sessions-normalization-test
  (testing "valid document sessions keep ordered open items"
    (is (= {"doc-1" [:note-1 :note-2]}
           (store/normalize-document-sessions
            {"doc-1" '(:note-1 :note-2)}))))
  (testing "malformed document sessions are ignored"
    (is (nil? (store/normalize-document-sessions nil)))
    (is (nil? (store/normalize-document-sessions
               {"doc-1" nil})))
    (is (nil? (store/normalize-document-sessions
               {:doc-1 []})))))
