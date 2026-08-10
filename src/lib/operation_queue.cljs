(ns lib.operation-queue)

(defn create []
  (atom (.resolve js/Promise nil)))

(defn enqueue!
  "Append f to tail*. The returned Promise belongs only to this operation, while
  the shared tail always recovers so one rejection cannot poison later work."
  [tail* f]
  (let [handlers* (atom nil)
        result (js/Promise. (fn [resolve reject]
                              (reset! handlers* [resolve reject])))
        run! (fn [_]
               (let [[resolve reject] @handlers*]
                 (-> (.resolve js/Promise nil)
                     (.then (fn [_] (f)))
                     (.then (fn [value]
                              (resolve value)
                              nil))
                     (.catch (fn [e]
                               (reject e)
                               nil)))))]
    (swap! tail*
           (fn [tail]
             (-> tail
                 (.catch (fn [_] nil))
                 (.then run!))))
    result))

(defn idle [tail*]
  @tail*)
