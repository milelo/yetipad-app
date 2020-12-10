(ns lib.asyncutils)                                         ;macros

;(defmacro go [& body]
;   (cons 'cljs.core.async.macros/go body)
;   )
;
;(defmacro go-loop [bindings & body]
;   (concat `(cljs.core.async.macros/go-loop bindings) body)
;   )

(defmacro >!x
  "The same as (do (>! c val) (close! c))"
  [c val]
  `(let [c# ~c]
     (cljs.core.async/>! c# ~val)
     (cljs.core.async/close! c#)))

(defmacro go-try [& body]
  (list 'cljs.core.async/go (cons 'try body)))

(defmacro goc
  "'go' but catches and prints exceptions. Useful for the repl, go logs exceptions to the console
  but these aren't visible in the repl.
  Note: catch :default doesn't catch ExceptionInfo"
  [& body]
  ;for repl use
  ;cljs.core.async/go prints caught exceptions to the console but NOT the repl!!
  (list 'cljs.core.async/go (concat (cons 'try body) `((catch js/Error e (println e))
                                                       (catch cljs.core.ExceptionInfo e (println e))
                                                       )))
  )

(defmacro go? [& body]
  (list 'cljs.core.async/go (concat (cons 'try body) `((catch js/Error e e) (catch cljs.core.ExceptionInfo e e))))
  )

#_(defmacro go? [& body]
    `(go
       (try
         ~@body
         (catch js/Error e e)
         (catch cljs.core.ExceptionInfo e e)
         )))

(defmacro <?
  "Takes a value from a core.async channel, throwing the value if it
  is a js/Error."
  ([c e]
   `(let [val# (cljs.core.async/<! ~c)]
      (if (or (instance? js/Error val#) (instance? cljs.core.ExceptionInfo val#))
        (when ~e (~e val#))
        val#)))
  ([c]
   `(let [val# (cljs.core.async/<! ~c)]
      (if (or (instance? js/Error val#) (instance? cljs.core.ExceptionInfo val#))
        (throw val#)
        val#)))
  )

#_(defmacro go-let
    "Like (go (let ...))"
    [bindings & body]
    `(go (let ~bindings ~@body)))

(defmacro go-let
  "Like (go (let ...))"
  [bindings & body]
  (list 'go (cons 'let (cons bindings body))))

#_(defn test-<? []                                          ;copy to cljs
    (go
      (<? (go?
            (let [c< (chan 1)]
              (put! c< (ex-info "error" nil))
              (we :got (<? c< #(we :x %)))
              (put! c< (ex-info "error" nil))
              (we :got (<? c<))
              :z
              )) #(we :c %)
          )))