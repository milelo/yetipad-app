(ns lib.goog-drive-test
  (:require
   [cljs.test :refer [async deftest is testing]]
   [lib.goog-drive :as drive]
   [lib.localstore :as localstore]
   [promesa.core :as p]))

(def credentials
  {:client_id "test-client" :scope "https://www.googleapis.com/auth/drive.file"})

(defn install-google-mocks!
  [!fail-next-load? !token !authorization-requests !authorization-error !drive-responses
   & [!authorization-observations]]
  (let [!scripts (atom {})
        make-gapi (fn []
                    #js {:load (fn [_ callback] (callback))
                         :client #js {:init (fn [_] (.resolve js/Promise true))
                                      :load (fn [_] (.resolve js/Promise true))
                                      :getToken (fn [] @!token)
                         :setToken (fn [token] (reset! !token token))
                         :drive #js {:files #js {:get
                                                  (fn [_]
                                                    (let [{:keys [result error]} (first @!drive-responses)]
                                                      (swap! !drive-responses rest)
                                                      (if error
                                                        (.reject js/Promise error)
                                                        (.resolve js/Promise #js {:result result}))))}}}})
        make-google (fn []
                      #js {:accounts
                           #js {:oauth2
                                #js {:hasGrantedAllScopes (fn [token _] (boolean token))
                                     :revoke (fn [_ callback] (callback #js {}))
                                     :initTokenClient
                                     (fn [_]
                                       (let [client #js {}]
                                         (set! (.-requestAccessToken client)
                                               (fn [request-config]
                                                 (swap! !authorization-requests inc)
                                                 (when !authorization-observations
                                                   (swap! !authorization-observations conj
                                                          {:conceal? @drive/!conceal-next-popup?
                                                           :prompt (.-prompt request-config)}))
                                                 (if-let [error @!authorization-error]
                                                   ((.-error_callback client) (clj->js error))
                                                   (let [token #js {:access_token "token"
                                                                    :scope "https://www.googleapis.com/auth/drive.file"
                                                                    :token_type "Bearer"
                                                                    :expires_in 3600}]
                                                     (reset! !token token)
                                                     ((.-callback client) token)))))
                                         client))}}})
        document
        #js {:getElementById (fn [id] (get @!scripts id))
             :createElement
             (fn [_]
               (let [script #js {}]
                 (set! (.-remove script)
                       (fn [] (swap! !scripts dissoc (.-id script))))
                 script))
             :head
             #js {:appendChild
                  (fn [script]
                    (swap! !scripts assoc (.-id script) script)
                    (if (compare-and-set! !fail-next-load? true false)
                      ((.-onerror script) #js {})
                      (do
                        (if (= (.-id script) drive/google-api-script-id)
                          (aset js/globalThis "gapi" (make-gapi))
                          (aset js/globalThis "google" (make-google)))
                        ((.-onload script)))))}}]
    (aset js/globalThis "document" document)))

(defn reset-drive-state! []
  (drive/set-late-settlement-listener! nil)
  (reset! drive/!sdk-bootstrap {:status :idle :promise nil})
  (reset! drive/!authorization-promise nil)
  (reset! drive/!token-client {:credentials credentials})
  (reset! drive/!preferences {:reduce-drive-popup-flash? false})
  (reset! drive/!conceal-next-popup? false)
  (reset! drive/!online-status {:online? false :status ::drive/initialising})
  (reset! drive/!pending-requests {})
  (reset! drive/!request-sequence 0)
  (localstore/remove-data-sync! drive/token-storage-key)
  (js-delete js/globalThis "gapi")
  (js-delete js/globalThis "google"))

(defn install-local-storage! [!values]
  (let [previous-descriptor (.getOwnPropertyDescriptor js/Object js/globalThis "localStorage")
        storage #js {:setItem (fn [k v] (swap! !values assoc k v))
                     :getItem (fn [k] (get @!values k))
                     :removeItem (fn [k] (swap! !values dissoc k))}]
    (.defineProperty js/Object js/globalThis "localStorage"
                     #js {:value storage :configurable true})
    (fn []
      (if previous-descriptor
        (.defineProperty js/Object js/globalThis "localStorage" previous-descriptor)
        (js-delete js/globalThis "localStorage")))))

; app.events installs its browser integration listener while the Node test
; bundle loads namespaces; unit tests must not let it reach localForage.
(drive/set-late-settlement-listener! nil)

(defn deferred []
  (let [!resolve (atom nil)
        promise (js/Promise. (fn [resolve _reject]
                               (reset! !resolve resolve)))]
    {:promise promise :resolve! (fn [value] (@!resolve value))}))

(deftest failed-sdk-load-can-be-retried-test
  (async done
    (let [!fail-next-load? (atom true)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)]
      (reset-drive-state!)
      (install-google-mocks! !fail-next-load? !token !authorization-requests !authorization-error (atom []))
      (drive/configure! credentials)
      (-> (drive/$ensure-sdk-ready!)
          (p/catch (fn [_] :expected-failure))
          (p/then (fn [result]
                    (is (= :expected-failure result))
                    (is (= :failed (:status @drive/!sdk-bootstrap)))
                    (drive/$ensure-drive-access! {:authorization :automatic})))
          (p/then (fn [result]
                    (is (= :authorized (:status result)))
                    (is (= :ready (:status @drive/!sdk-bootstrap)))
                    (is (= 1 @!authorization-requests))
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (done)))))))

(deftest automatic-reconnect-requests-authorization-test
  (async done
    (let [!fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)]
      (reset-drive-state!)
      (install-google-mocks! !fail-next-load? !token !authorization-requests !authorization-error (atom []))
      (drive/configure! credentials)
      (-> (drive/$ensure-drive-access! {:authorization :automatic})
          (p/then (fn [result]
                    (is (= :authorized (:status result)))
                    (is (= 1 @!authorization-requests))
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (done)))))))

(deftest authorization-failure-waits-for-interactive-retry-test
  (async done
    (let [!fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom {:type "popup_failed_to_open"})]
      (reset-drive-state!)
      (install-google-mocks! !fail-next-load? !token !authorization-requests !authorization-error (atom []))
      (drive/configure! credentials)
      (-> (drive/$ensure-drive-access! {:authorization :automatic})
          (p/then (fn [result]
                    (is (= :authorization-required (:status result)))
                    (drive/$ensure-drive-access! {:authorization :automatic})))
          (p/then (fn [result]
                    (testing "focus does not repeat a failed automatic prompt"
                      (is (= :authorization-required (:status result)))
                      (is (= 1 @!authorization-requests)))
                    (reset! !authorization-error nil)
                    (drive/$ensure-drive-access! {:authorization :interactive})))
          (p/then (fn [result]
                    (testing "a click clears the latch and authorizes"
                      (is (= :authorized (:status result)))
                      (is (= 2 @!authorization-requests)))
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (done)))))))

(deftest sign-out-disables-automatic-reauthorization-test
  (async done
    (let [!fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)]
      (reset-drive-state!)
      (install-google-mocks! !fail-next-load? !token !authorization-requests !authorization-error (atom []))
      (drive/configure! credentials)
      (-> (drive/$ensure-drive-access! {:authorization :automatic})
          (p/then (fn [_]
                    (drive/sign-out!)
                    (drive/$ensure-drive-access! {:authorization :automatic})))
          (p/then (fn [result]
                    (is (= :authorization-required (:status result)))
                    (is (= 1 @!authorization-requests))
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (done)))))))

(deftest invalid-token-is-reauthorized-and-request-retried-once-test
  (async done
    (let [!fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)
          !drive-responses (atom
                            [{:error #js {:result #js {:error #js {:code 401
                                                                     :status "UNAUTHENTICATED"
                                                                     :message "invalid credentials"}}}}
                             {:result #js {:id "file-1"}}])]
      (reset-drive-state!)
      (install-google-mocks! !fail-next-load? !token !authorization-requests
                             !authorization-error !drive-responses)
      (drive/configure! credentials)
      (-> (drive/$ensure-drive-access! {:authorization :automatic})
          (p/then (fn [result]
                    (is (= :authorized (:status result)))
                    (drive/$get-file-meta "file-1")))
          (p/then (fn [result]
                    (is (= "file-1" (:id result)))
                    (is (= 2 @!authorization-requests))
                    (is (empty? @!drive-responses))
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (done)))))))

(deftest invalid-token-reauthorization-failure-requires-user-action-test
  (async done
    (let [!fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)
          !drive-responses (atom
                            [{:error #js {:result #js {:error #js {:code 401
                                                                     :status "UNAUTHENTICATED"
                                                                     :message "invalid credentials"}}}}])]
      (reset-drive-state!)
      (install-google-mocks! !fail-next-load? !token !authorization-requests
                             !authorization-error !drive-responses)
      (drive/configure! credentials)
      (-> (drive/$ensure-drive-access! {:authorization :automatic})
          (p/then (fn [_]
                    (reset! !authorization-error {:type "popup_failed_to_open"})
                    (drive/$get-file-meta "file-1")))
          (p/then (fn [_]
                    (is false "invalid credentials request should fail")
                    (done)))
          (p/catch (fn [error]
                     (is (:authorization-error? error))
                     (is (= 2 @!authorization-requests))
                     (-> (drive/$ensure-drive-access!
                          {:authorization :automatic})
                         (p/then (fn [result]
                                   (is (= :authorization-required (:status result)))
                                   (is (= 2 @!authorization-requests))
                                   (done)))
                         (p/catch (fn [e]
                                    (is false (str e))
                                    (done))))))))))

(deftest valid-cached-token-skips-authorization-test
  (async done
    (let [!storage (atom {})
          restore-storage! (install-local-storage! !storage)
          !fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)]
      (try
        (reset-drive-state!)
        (localstore/put-data-sync!
         drive/token-storage-key
         {:access-token "cached-token"
          :scope "https://www.googleapis.com/auth/drive.file"
          :token-type "Bearer"
          :expires-at (+ (.now js/Date) 3600000)})
        (install-google-mocks! !fail-next-load? !token !authorization-requests
                               !authorization-error (atom []))
        (drive/configure! credentials)
        (-> (drive/$ensure-drive-access! {:authorization :automatic})
            (p/then (fn [result]
                      (is (= :authorized (:status result)))
                      (is (= "cached-token" (.-access_token ^js @!token)))
                      (is (zero? @!authorization-requests))
                      (restore-storage!)
                      (done)))
            (p/catch (fn [e]
                       (restore-storage!)
                       (is false (str e))
                       (done))))
        (catch :default e
          (restore-storage!)
          (is false (str e))
          (done))))))

(deftest expired-cached-token-is-discarded-test
  (async done
    (let [!storage (atom {})
          restore-storage! (install-local-storage! !storage)
          !fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)]
      (try
        (reset-drive-state!)
        (localstore/put-data-sync!
         drive/token-storage-key
         {:access-token "nearly-expired-token"
          :scope "https://www.googleapis.com/auth/drive.file"
          :token-type "Bearer"
          :expires-at (+ (.now js/Date) 30000)})
        (install-google-mocks! !fail-next-load? !token !authorization-requests
                               !authorization-error (atom []))
        (drive/configure! credentials)
        (-> (drive/$ensure-sdk-ready!)
            (p/then (fn [_]
                      (is (nil? @!token))
                      (is (nil? (localstore/get-data-sync drive/token-storage-key)))
                      (restore-storage!)
                      (done)))
            (p/catch (fn [e]
                       (restore-storage!)
                       (is false (str e))
                       (done))))
        (catch :default e
          (restore-storage!)
          (is false (str e))
          (done))))))

(deftest successful-authorization-is-cached-and-sign-out-clears-it-test
  (async done
    (let [!storage (atom {})
          restore-storage! (install-local-storage! !storage)
          !fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)]
      (try
        (reset-drive-state!)
        (install-google-mocks! !fail-next-load? !token !authorization-requests
                               !authorization-error (atom []))
        (drive/configure! credentials)
        (-> (drive/$ensure-drive-access! {:authorization :automatic})
            (p/then (fn [result]
                      (is (= :authorized (:status result)))
                      (let [cached (localstore/get-data-sync drive/token-storage-key)]
                        (is (= "token" (:access-token cached)))
                        (is (= "https://www.googleapis.com/auth/drive.file" (:scope cached)))
                        (is (> (:expires-at cached) (.now js/Date))))
                      (drive/sign-out! false)
                      (is (nil? @!token))
                      (is (nil? (localstore/get-data-sync drive/token-storage-key)))
                      (restore-storage!)
                      (done)))
            (p/catch (fn [e]
                       (restore-storage!)
                       (is false (str e))
                       (done))))
        (catch :default e
          (restore-storage!)
          (is false (str e))
          (done))))))

(deftest popup-focus-choreography-test
  (async done
    (let [!popup-focus (atom 0)
          !popup-blur (atom 0)
          !app-focus (atom 0)
          previous-focus-descriptor (.getOwnPropertyDescriptor js/Object js/globalThis "focus")
          popup #js {:closed false
                     :focus #(swap! !popup-focus inc)
                     :blur #(swap! !popup-blur inc)}]
      (try
        (.defineProperty js/Object js/globalThis "focus"
                         #js {:value #(swap! !app-focus inc) :configurable true})
        (drive/conceal-popup! popup 5)
        (js/setTimeout
         (fn []
           (is (pos? @!popup-blur))
           (is (pos? @!app-focus))
           (is (= 1 @!popup-focus))
           (if previous-focus-descriptor
             (.defineProperty js/Object js/globalThis "focus" previous-focus-descriptor)
             (js-delete js/globalThis "focus"))
           (done))
         20)
        (catch :default e
          (if previous-focus-descriptor
            (.defineProperty js/Object js/globalThis "focus" previous-focus-descriptor)
            (js-delete js/globalThis "focus"))
          (is false (str e))
          (done))))))

(deftest popup-concealment-is-limited-to-automatic-authorization-test
  (async done
    (let [!fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)
          !observations (atom [])]
      (reset-drive-state!)
      (install-google-mocks! !fail-next-load? !token !authorization-requests
                             !authorization-error (atom []) !observations)
      (drive/configure! credentials)
      (-> (drive/$ensure-drive-access! {:authorization :automatic})
          (p/then (fn [_]
                    (drive/sign-out! false)
                    (drive/$ensure-drive-access! {:authorization :interactive})))
          (p/then (fn [_]
                    (is (= [{:conceal? true :prompt ""}
                            {:conceal? false :prompt ""}]
                           @!observations))
                    (done)))
          (p/catch (fn [e]
                     (is false (str e))
                     (done)))))))

(deftest cached-token-without-drive-scope-is-discarded-test
  (async done
    (let [!storage (atom {})
          restore-storage! (install-local-storage! !storage)
          !fail-next-load? (atom false)
          !token (atom nil)
          !authorization-requests (atom 0)
          !authorization-error (atom nil)]
      (try
        (reset-drive-state!)
        (localstore/put-data-sync!
         drive/token-storage-key
         {:access-token "wrong-scope-token"
          :scope "https://www.googleapis.com/auth/calendar.readonly"
          :token-type "Bearer"
          :expires-at (+ (.now js/Date) 3600000)})
        (install-google-mocks! !fail-next-load? !token !authorization-requests
                               !authorization-error (atom []))
        (drive/configure! credentials)
        (-> (drive/$ensure-sdk-ready!)
            (p/then (fn [_]
                      (is (nil? @!token))
                      (is (nil? (localstore/get-data-sync drive/token-storage-key)))
                      (restore-storage!)
                      (done)))
            (p/catch (fn [e]
                       (restore-storage!)
                       (is false (str e))
                       (done))))
        (catch :default e
          (restore-storage!)
          (is false (str e))
          (done))))))
