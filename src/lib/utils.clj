(ns lib.utils)

(defn- to-property [sym]
  (symbol (str "-" sym)))

(defmacro goog-extend [type base-type ctor & methods]
  `(do
     (defn ~type ~@ctor)

     (goog/inherits ~type ~base-type)

     ~@(map
         (fn [method]
           `(set! (.. ~type -prototype ~(to-property (first method)))
                  (fn ~@(rest method))))
         methods)))

(defmacro js-apply [f target args]
  `(.apply ~f ~target (to-array ~args)))

(defmacro for-all [& body]
  (list 'doall (cons 'for body)))