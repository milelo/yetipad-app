(ns app.route-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [app.route :as route]))

(deftest document-only-url-has-no-open-override-test
  (testing "a document URL without open has no open-item query value"
    (let [{:keys [query fragment]} (route/path-decode "/#doc123")]
      (is (= "doc123" fragment))
      (is (not (contains? query :open))))))

(deftest document-url-preserves-open-override-test
  (testing "an open query is retained for document startup"
    (let [{:keys [query fragment]} (route/path-decode "/?open=(:note1,:note2)#doc123")]
      (is (= "doc123" fragment))
      (is (= "(:note1,:note2)" (:open query))))))
