(ns app.ui.search-test
  (:require
   [cljs.test :refer [async deftest is testing]]
   [app.ui.search :as search]))

(def debounce-ms 400)

(deftest debounce-waits-for-input-to-stop-test
  (async done
    (let [timer* (atom nil)
          values* (atom [])]
      (search/debounce! timer* debounce-ms #(swap! values* conj :first))
      (js/setTimeout
       (fn []
         (is (= [] @values*))
         (js/setTimeout
          (fn []
            (is (= [:first] @values*))
            (done))
          50))
       350))))

(deftest debounce-only-applies-latest-value-test
  (async done
    (let [timer* (atom nil)
          values* (atom [])]
      (search/debounce! timer* debounce-ms #(swap! values* conj :first))
      (js/setTimeout
       (fn []
         (search/debounce! timer* debounce-ms #(swap! values* conj :latest)))
       100)
      (js/setTimeout
       (fn []
         (is (= [:latest] @values*))
         (search/debounce! timer* debounce-ms #(swap! values* conj :cleared))
         (js/setTimeout
          (fn []
            (is (= [:latest :cleared] @values*))
            (done))
          450))
       550))))
