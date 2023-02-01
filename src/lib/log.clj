(ns lib.log
  (:require 
   [cljs.analyzer :as analyzer]))

(defmacro trace [logger* & args]
  (list `(partial lib.log/trace! ~logger* :trace) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*)}))

(defmacro debug [logger* & args]
  (list `(partial lib.log/trace! ~logger* :debug) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*) }))

(defmacro info [logger* & args]
  (list `(partial lib.log/trace! ~logger* :info) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*)}))

(defmacro warn [logger* & args]
  (list `(partial lib.log/trace! ~logger* :warn) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*)}))

(defmacro error [logger* & args]
  (list `(partial lib.log/trace! ~logger* :error) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*)}))

(defmacro fatal [logger* & args]
  (list `(partial lib.log/trace! ~logger* :fatal) {:args (cons `list args) :meta (meta &form) :ns (name analyzer/*cljs-ns*)}))
