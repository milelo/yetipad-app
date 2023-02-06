(ns app.route
  (:require
    [lib.debug :as debug :refer [we wd expose]]
    [lib.log :as log :refer-macros [trace debug info warn error fatal]]
    [goog.string :refer [urlDecode]]
    [clojure.string :as str :refer [split join]]
    ))

(def log (log/logger 'app.route))

; Originally tried to use secretary however it seems to have some bugs and quirks and isn't
; compliant with standard URI syntax:
; scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
; see: https://en.wikipedia.org/wiki/Uniform_Resource_Locator#Syntax
; We want http://localhost:8281/?config={:open [1 2]}#<doc-id>
; There is also juxt/bidi but doesn't handle queries so doesn't provide much value.

;A hash symbol '#' in a URL usually used to locate a page scroll position is referred to as a 'fragment'
;The fragment identifier follows the #. Any / before # will navigate the server so we use it after.
;In SPA's the fragment identifier usually starts with a '/' to identify the page but we use it to reference
; the document. We use query parameters are used to configure the state of the app.
;See full info: https://www.oho.com/blog/explained-60-seconds-hash-symbols-urls-and-seo

;better example?: https://github.com/gothinkster/clojurescript-reframe-realworld-example-app/blob/master/src/conduit/core.cljs

(defn map->query-string [m]
  (join "&" (for [[k v] m] (str (name k) "=" v))))

(defn query-string->map [query-string]
  (reduce (fn [m e]
            (let [[k v] (split e #"=")]
              (assoc m (keyword k) v)
              )) {} (split query-string #"&")))

(def path-re #"/(?:\?(.*))?#([a-z0-9]+)")
(defn path-decode [path]
  (when-let [[p q f] (re-matches path-re (urlDecode path))]
    {:path  p
     :query (some-> q query-string->map)
     :fragment f
     }))
