(ns lib.operation-queue
  (:require
   [promesa.core :as p]))

(defn create []
  (atom (p/resolved nil)))

(defn enqueue!
  "Append f to tail*. The returned Promise belongs only to this operation, while
  the shared tail always recovers so one rejection cannot poison later work."
  [tail* f]
  (let [result* (atom nil)]
    (swap! tail*
           (fn [tail]
             (let [operation (-> tail
                                 (p/catch (fn [_] nil))
                                 (p/then (fn [_] (f))))]
               (reset! result* operation)
               (p/catch operation (fn [_] nil)))))
    @result*))

(defn idle [tail*]
  @tail*)
