(ns lib.localstore-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [lib.localstore :as localstore]))

(deftest synchronous-data-round-trip-test
  (let [values* (atom {})
        previous-descriptor (.getOwnPropertyDescriptor js/Object js/globalThis "localStorage")
        storage #js {:setItem (fn [k v] (swap! values* assoc k v))
                     :getItem (fn [k] (get @values* k))}]
    (try
      (.defineProperty js/Object js/globalThis "localStorage"
                       #js {:value storage :configurable true})
      (testing "small Clojure data can be written and read synchronously"
        (is (true? (localstore/put-data-sync!
                    :session {:doc-id "doc-1" :open-items [:note-1]})))
        (is (= {:doc-id "doc-1" :open-items [:note-1]}
               (localstore/get-data-sync :session))))
      (testing "a missing key returns nil"
        (is (nil? (localstore/get-data-sync :missing))))
      (finally
        (if previous-descriptor
          (.defineProperty js/Object js/globalThis "localStorage" previous-descriptor)
          (js-delete js/globalThis "localStorage"))))))

(deftest synchronous-session-data-round-trip-test
  (let [values* (atom {})
        previous-descriptor (.getOwnPropertyDescriptor js/Object js/globalThis "sessionStorage")
        storage #js {:setItem (fn [k v] (swap! values* assoc k v))
                     :getItem (fn [k] (get @values* k))}]
    (try
      (.defineProperty js/Object js/globalThis "sessionStorage"
                       #js {:value storage :configurable true})
      (is (true? (localstore/put-session-data-sync!
                  :session {:doc-1 [:note-1]})))
      (is (= {:doc-1 [:note-1]}
             (localstore/get-session-data-sync :session)))
      (finally
        (if previous-descriptor
          (.defineProperty js/Object js/globalThis "sessionStorage" previous-descriptor)
          (js-delete js/globalThis "sessionStorage"))))))
