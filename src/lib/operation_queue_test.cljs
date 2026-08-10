(ns lib.operation-queue-test
  (:require
   [cljs.test :refer [async deftest is testing]]
   [lib.operation-queue :as queue]
   [promesa.core :as p]))

(defn deferred []
  (p/deferred))

(defn test-context [state]
  {:tail (queue/create) :state (atom state)})

(defn watchdog [done]
  (js/setTimeout (fn []
                   (is false "asynchronous test timed out")
                   (done))
                 2000))

(deftest promise-fifo-queue-test
  (async done
    (let [timer (watchdog done)
          {:keys [tail state]} (test-context {:order []})
          gate (deferred)
          first-op (queue/enqueue! tail
                                 (fn []
                                   (p/let [_ gate]
                                     (swap! state update :order conj :first)
                                     :first-result)))
          second-op (queue/enqueue! tail
                                  (fn []
                                    (swap! state update :order conj :second)
                                    :second-result))]
      (is (= [] (:order @state)))
      (p/resolve! gate true)
      (-> (p/all [first-op second-op])
          (p/then (fn [results]
                    (testing "operations run in submission order"
                      (is (= [:first :second] (:order @state)))
                      (is (= [:first-result :second-result] results)))
                    (js/clearTimeout timer)
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (js/clearTimeout timer)
                     (done)))))))

(deftest rejected-operation-does-not-poison-queue-test
  (async done
    (let [timer (watchdog done)
          {:keys [tail state]} (test-context {:order []})
          failed (-> (queue/enqueue! tail
                                   (fn []
                                     (throw (js/Error. "expected"))))
                     (p/catch (fn [_]
                                (swap! state assoc :failure :caught)
                                :caught)))
          succeeded (queue/enqueue! tail
                                  (fn []
                                    (swap! state update :order conj :succeeded)
                                    :ok))]
      (-> (queue/idle tail)
          (p/then (fn [_]
                    (is (= :caught (:failure @state)))
                    (is (= [:succeeded] (:order @state)))
                    (js/clearTimeout timer)
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (js/clearTimeout timer)
                     (done)))))))

(deftest new-note-waits-for-document-load-test
  (async done
    (let [timer (watchdog done)
          {:keys [tail state]} (test-context {:doc {:doc-id "old" "old-note" {:id "old-note"}}
                                              :editing {}
                                              :open-items []})
          load-gate (deferred)
          load-op (queue/enqueue! tail
                                (fn []
                                  (p/let [loaded-doc load-gate]
                                    (swap! state assoc :doc loaded-doc :editing {} :open-items []))))
          note-op (queue/enqueue! tail
                                  (fn []
                                    (let [note-id "new-note"
                                          note {:id note-id :kind :note}]
                                      (swap! state #(-> %
                                                        (assoc-in [:doc note-id] note)
                                                        (assoc-in [:editing note-id] {:source note})
                                                        (update :open-items conj note-id))))))]
      (p/resolve! load-gate {:doc-id "loaded"
                             "existing" {:id "existing" :content "preserved"}})
      (-> (p/all [load-op note-op])
          (p/then (fn [_]
                    (let [{:keys [doc editing open-items]} @state]
                      (is (= "loaded" (:doc-id doc)))
                      (is (= "preserved" (get-in doc ["existing" :content])))
                      (is (= :note (get-in doc ["new-note" :kind])))
                      (is (contains? editing "new-note"))
                      (is (= ["new-note"] open-items)))
                    (js/clearTimeout timer)
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (js/clearTimeout timer)
                     (done)))))))

(deftest mutation-waits-for-save-test
  (async done
    (let [timer (watchdog done)
          {:keys [tail state]} (test-context {:doc {:doc-id "doc"} :saved []})
          save-gate (deferred)
          save-op (queue/enqueue! tail
                                  (fn []
                                    (let [{:keys [doc]} @state]
                                      (p/let [_ save-gate]
                                        (swap! state update :saved conj doc)))))
          mutation-op (queue/enqueue! tail
                                    (fn []
                                      (swap! state assoc-in [:doc "note"] {:id "note"})))]
      (is (nil? (get-in @state [:doc "note"])))
      (p/resolve! save-gate true)
      (-> (p/all [save-op mutation-op])
          (p/then (fn [_]
                    (is (= [{:doc-id "doc"}] (:saved @state)))
                    (is (= "note" (get-in @state [:doc "note" :id])))
                    (js/clearTimeout timer)
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (js/clearTimeout timer)
                     (done)))))))

(deftest latest-document-load-wins-test
  (async done
    (let [timer (watchdog done)
          {:keys [tail state]} (test-context {:doc {:doc-id "old"}
                                              :doc-load {:token :first :doc-id "first"}})
          first-gate (deferred)
          first-load (queue/enqueue!
                      tail
                      (fn []
                        (p/let [loaded-doc first-gate]
                          (swap! state
                                 (fn [db]
                                   (if (= :first (get-in db [:doc-load :token]))
                                     (assoc db :doc loaded-doc :doc-load nil)
                                     db))))))
          _ (swap! state assoc :doc-load {:token :second :doc-id "second"})
          second-load (queue/enqueue!
                       tail
                       (fn []
                         (swap! state
                                (fn [db]
                                  (if (= :second (get-in db [:doc-load :token]))
                                    (assoc db :doc {:doc-id "second"} :doc-load nil)
                                    db)))))
          note (queue/enqueue!
                tail
                (fn []
                  (swap! state assoc-in [:doc "note"] {:id "note" :kind :note})))]
      (p/resolve! first-gate {:doc-id "first"})
      (-> (p/all [first-load second-load note])
          (p/then (fn [_]
                    (is (= "second" (get-in @state [:doc :doc-id])))
                    (is (= :note (get-in @state [:doc "note" :kind])))
                    (is (nil? (:doc-load @state)))
                    (js/clearTimeout timer)
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (js/clearTimeout timer)
                     (done)))))))
