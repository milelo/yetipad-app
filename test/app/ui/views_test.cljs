(ns app.ui.views-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [app.ui.views :as views]))

(deftest index-list-title-style-test
  (testing "tags with content are underlined"
    (is (= {:font-size 12 :text-decoration "underline"}
           (views/index-list-title-style "A tag" :tag [[:p "content"]]))))
  (testing "tags without content retain the normal title style"
    (is (= {:font-size 12}
           (views/index-list-title-style "A tag" :tag nil))))
  (testing "non-tag items are not underlined"
    (is (= {:font-size 12}
           (views/index-list-title-style "A note" :note [[:p "content"]]))))
  (testing "untitled items retain italic styling"
    (is (= {:font-size 12 :font-style :italic}
           (views/index-list-title-style nil :tag [[:p "content"]])))))
