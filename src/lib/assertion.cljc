;source: https://gist.github.com/mjmeintjes/e277d014a3f971b3a3da5fddf29aee30
(ns lib.assertion
  (:require [taoensso.truss :as truss]
            #?(:clj [taoensso.truss.impl :as impl :refer [-invariant]])
            #?(:cljs [taoensso.truss.impl :as impl :refer-macros [-invariant]])
            [clojure.string :as str]))


(defn try-resolve [s & args]
  #?(:clj
     (try
       #_:clj-kondo/ignore
       (apply (requiring-resolve s) args)
       (catch java.io.FileNotFoundException ex
         #_(println "WARN - sc.api not in classpath")))))

#?(:clj
   (when *assert*
     (try-resolve 'sc.api.logging/register-cs-logger
                  ::logger (fn [cs]))))

(defn spy-emit [env form]
  #?(:clj
     (when *assert*
       #_:clj-kondo/ignore
       (try-resolve 'sc.api/spy-emit
                    {:sc/spy-cs-logger-id ::logger} nil env form))))

(defn truss-error-fn [data_]
  (let [data @data_
        ex  (ex-info (str/join
                      (take 500
                            (str (when-let [m (get-in data [:?data :msg])]
                                   (str m " - "))
                                 @(:msg_ data))))
                     (assoc (dissoc data :msg_)
                            ::truss-ex? (nil? (:?err data)))
                     (:?err data))]
    (throw ex)))
(truss/set-error-fn! #'truss-error-fn)


(defn truss-ex? [ex]
  (-> ex ex-data ::truss-ex?))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro try-catchall
  "A cross-platform variant of try-catch that catches all exceptions.
   Does not (yet) support finally, and does not need or want an exception class."
  [& body]
  (let [try-body (butlast body)
        [catch sym & catch-body :as catch-form] (last body)]
    (assert (= catch 'catch))
    (assert (symbol? sym))
    (if (cljs-env? &env)
      `(try ~@try-body (~'catch js/Object ~sym ~@catch-body))
      `(try ~@try-body (~'catch Throwable ~sym ~@catch-body)))))

(defmacro have
  "Takes a pred and one or more vals. Tests pred against each val,
  trapping errors. If any pred test fails, throws a detailed assertion error.
  Otherwise returns input val/vals for convenient inline-use/binding.
  Respects *assert* value so tests can be elided from production for zero
  runtime costs.
  Provides a small, simple, flexible feature subset to alternative tools like
  clojure.spec, core.typed, prismatic/schema, etc.
    ;; Will throw a detailed error message on invariant violation:
    (fn my-fn [x] (str/trim (have string? x)))
  You may attach arbitrary debug info to assertion violations like:
    `(have string? x :data {:my-arbitrary-debug-info \"foo\"})`
  See also `have?`, `have!`."
  {:arglists '([pred (:in) x] [pred (:in) x & more-xs])}
  [& args]
  `(try-catchall
    (-invariant :elidable nil ~(:line (meta &form)) ~args)
    (catch ex#
           (when (truss-ex? ex#)
             ~(spy-emit &env &form))
      (throw ex#))))


(comment
  (have 1)
  (have nil)

  ;; must have required keys
  (have ::should-exist {:shld-exist 123})

  ;; no nils in the collection
  (have some? :in [1 2 3 nil])

  ;; should be distinct
  (have #(apply distinct? %) [1 2 3 3])

  ;; should be sorted
  (have #(= (sort %) %) [1 2 3 2]))