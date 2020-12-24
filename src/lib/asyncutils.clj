(ns lib.asyncutils)                                         ;macros

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
  (list 'cljs.core.async/go (concat (cons 'try body) `((catch :default val# (println val#)))))
  )

(defmacro go?
  "'go' enhanced to catch and return js/Error as channel value"
  [& body]
  (list 'cljs.core.async/go (concat (cons 'try body) `((catch :default val# val#))))
  )

(defmacro <?
  "Takes a value from a core.async channel, throwing the value if it
  is a js/Error."
  ([c e]
   `(let [val# (cljs.core.async/<! ~c)]
      (if (instance? js/Error val#)
        (when ~e (~e val#))
        val#)))
  ([c]
   `(let [val# (cljs.core.async/<! ~c)]
      (if (instance? js/Error val#)
        (throw val#)
        val#)))
  )

(defmacro go-let
  "Like (go (let ...))"
  [bindings & body]
  (list 'cljs.core.async/go (cons 'let (cons bindings body))))
