(ns app.subs-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [app.subs :as subs]))

(deftest signed-in-test
  (testing "only an authorized Drive status is signed in"
    (is (subs/signed-in? {:online-status :authorized}))
    (is (not (subs/signed-in? {:online-status :authorization-required})))
    (is (not (subs/signed-in? {:online-status nil})))))
