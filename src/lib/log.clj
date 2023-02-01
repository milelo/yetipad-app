(ns lib.log
  (:require
   [cljs.analyzer :as analyzer]
   [clojure.string :as str]))

(defn scope [env]
  (not-empty (str/join \. (map #(-> % :name pr-str) (:fn-scope env)))))

(defmacro stack [logger* & args]
  (list `(partial lib.log/trace! ~logger* :stack) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) :scope (scope &env)}))

(defmacro trace [logger* & args]
  (list `(partial lib.log/trace! ~logger* :trace) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) :scope (scope &env)}))

(defmacro debug [logger* & args]
  (list `(partial lib.log/trace! ~logger* :debug) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) :scope (scope &env)}))

(defmacro info [logger* & args]
  (list `(partial lib.log/trace! ~logger* :info) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) :scope (scope &env)}))

(defmacro warn [logger* & args]
  (list `(partial lib.log/trace! ~logger* :warn) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) :scope (scope &env)}))

(defmacro error [logger* & args]
  (list `(partial lib.log/trace! ~logger* :error) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) :scope (scope &env)}))

(defmacro fatal [logger* & args]
  (list `(partial lib.log/trace! ~logger* :fatal) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) :scope (scope &env)}))
