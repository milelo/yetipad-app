(ns lib.asyncutils
  (:require
   [cljs.core.async :as async :refer [<! >! chan put! take! close!]]
   [cljs.core.async.impl.protocols :as asyncp])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [lib.asyncutils :refer [<? go-let]]
    ;[lib.debug :refer [w wl]]
   ))

(defn put-last! [c< v]
  (put! c< v)
  (close! c<))

(defn chan? [x]
  (satisfies? asyncp/Channel x))




