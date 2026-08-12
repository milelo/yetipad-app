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
