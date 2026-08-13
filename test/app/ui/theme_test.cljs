(ns app.ui.theme-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [app.ui.theme :as theme :refer [theme]]
   [app.subs :as subs]))

(deftest sticky-pane-toolbar-markup-test
  (testing "pane toolbars expose the reusable sticky toolbar contract"
    (let [{:keys [class style]} (theme ::theme/pane-buttons)]
      (is (= "pane-toolbar" class))
      (is (= :sticky (:position style)))
      (is (= 0 (:top style)))
      (is (= 1 (:z-index style)))
      (is (= :white (:background-color style)))
      (is (= :flex (:display style)))
      (is (= :flex-end (:justify-content style))))))

(deftest items-scroll-container-markup-test
  (testing "open items render inside the fixed-height scroll container"
    (let [{:keys [id style]} (theme ::theme/items)]
      (is (= "items" id))
      (is (= "calc(100vh - 64px)" (:height style)))
      (is (= :auto (:overflow-y style))))))

(deftest sticky-editor-tags-option-test
  (testing "the editor tag-bar setting defaults to enabled"
    (is (true? @subs/sticky-editor-tags?*))))

(deftest sticky-editor-toolbar-markup-test
  (testing "editor toolbars share the below-pane-toolbar contract"
    (let [{:keys [class style]} (theme ::theme/editor-toolbar)]
      (is (= "editor-toolbar" class))
      (is (= :sticky (:position style)))
      (is (= "var(--pane-toolbar-height, 48px)" (:top style)))
      (is (= 2 (:z-index style)))
      (is (= :white (:background-color style))))))
