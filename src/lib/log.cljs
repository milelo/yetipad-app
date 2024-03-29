(ns lib.log
  (:require
   [cljs.core.async :as async :refer [put! chan]]
   [clojure.edn :refer [read-string]]
   [cljs.pprint :refer [pprint]]
   [clojure.data :refer [diff]]
   ["localforage" :as local-forage]))

(def log-levels [:default :trace :debug :info :warn :error :fatal])
(def base-default-config {:default-level   :trace
                          :console-enable? nil              ;default to true
                          :buffer-enable?  nil              ;default to true - trace to buffer
                          })

(defonce ref-time* (atom (js/Date.now)))
(defonce loggers* (atom {}))
(defonce config* (atom base-default-config))

(defn default-config []
  (into base-default-config (keep (fn [[k v]] (when (and (symbol? k) v)
                                                [k nil])) @config*)))

(def localstore-key "logger-config")

(add-watch config* ::logger-config (fn [_k _r o n]
                                     (when (not= o n)
                                       (.setItem local-forage localstore-key
                                                 (pr-str (into {} (filter second n)))
                                                 (fn [err]
                                                   (when err
                                                     (js/console.error err)))))))

(defn pprint-out [object]
  (with-out-str (pprint object)))

(defn pprintl
  "pprint for use as log argument.
  Will only be evaluated when rendering log."
  [o]
  #(str \newline (pprint-out o)))

(defn trace-diff [ma a mb b & [{:keys [include-match include-both]}]]
  (fn [] (let [[a b both] (diff a b)]
           (if (or a b)
             (str "\nDiff " ma \newline (pprint-out a)
                  "Diff " mb \newline (pprint-out b)
                  (when include-both
                    (str "both:" \newline (pprint-out both))))
             (str "Diff match: " ma \space mb
                  (if (or include-match)
                    (str "both:" \newline (pprint-out both))
                    (str " -no-differences")))))))

(defn- error? [x]
  ;ExceptionInfo is also an error!
  (instance? js/Error x))

(defn- js-apply [f target args]
  (.apply f target (to-array args)))

(defn arg-to-str [arg]
  (cond
    (string? arg) arg
    (error? arg) arg
    (fn? arg) (arg)
    :default (pr-str arg)))

(defn- print-to-console! [level package method time {:keys [args meta ns scope]}]
  ;only error log source-map stack traces, alternatively use stack fn.
  (let [args (concat [time level (if meta (str ns \: (:line meta) (when scope (str \. scope))) package)] args)]
    (js-apply method js/console (map arg-to-str args))
    (when-let [cause (ex-cause (last args))]
      (.call method js/console "cause: " cause))))

(defonce <logger (chan (async/sliding-buffer 30)))

(defn- trace-to-channel! [level package time data]
  (put! <logger (merge {:package package
                        :time    time
                        :level   level} data)))

(defn- configure-logger! [logger* package level {:keys [console-enable? buffer-enable?]}]
  (let [console-enable? (or console-enable? true)
        buffer-enable? (or buffer-enable? true)
        level-tracers (fn [level method]
                        [(when buffer-enable? (partial trace-to-channel! level package))
                         (when console-enable? (partial print-to-console! (name level) (name package) method))])
        tracers {:stack (level-tracers :stack (.-trace js/console))
                 :trace (level-tracers :trace (.-log js/console))
                 :debug (level-tracers :debug (.-debug js/console))
                 :info  (level-tracers :info (.-info js/console))
                 :warn  (level-tracers :warn (.-warn js/console))
                 :error (level-tracers :error (.-error js/console))
                 :fatal (level-tracers :fatal (.-error js/console))}]
    (reset! logger* (case level
                      :trace tracers
                      :debug (dissoc tracers :trace)
                      :info (dissoc tracers :stack :trace :debug)
                      :warn (dissoc tracers :stack :trace :debug :info)
                      :error (dissoc tracers :stack :trace :debug :info :warn)
                      :fatal (dissoc tracers :stack :trace :debug :info :warn :error)))))

(defn- reconfigure-loggers! [config]
  (doseq [[option level] config]
    (when-let [logger* (get @loggers* option)]
      ;(js/console.log ::set-config! package level)
      (configure-logger! logger* option (or level (:default-level config)) config))))

(.getItem local-forage localstore-key (fn [err v] (if err
                                                    (js/console.error err)
                                                    (let [config-changes (read-string v)
                                                          config (swap! config* merge config-changes)]
                                                      (js/console.info (str ::localstore-init-log) (pr-str config-changes))
                                                      (reconfigure-loggers! config)))))

(defn set-config! [config-changes]
  (let [config (swap! config* merge (into {} (for [[package level :as e] config-changes]
                                               (if (= level :default)
                                                 [package nil]
                                                 e))))]
    (reconfigure-loggers! config)))

(defn logger [package]
  (let [logger* (atom {})
        [_ package-config :as entry] (find @config* package)
        package-config (or package-config (get @config* :default-level))]
    (swap! loggers* assoc package logger*)
    (when-not entry
      ;ensure package is registered
      (swap! config* assoc package nil))
    (configure-logger! logger* package package-config @config*)
    logger*))

(defn- trace! [logger* level data]
  (let [[l1 l2] (get @logger* level)
        time (when (or l1 l2) (- (js/Date.now) @ref-time*))]
    (when l1 (l1 time data))
    (when l2 (l2 time data))))

(defn trace [logger* & args] ((partial trace! logger* :trace) {:args args}))
(defn debug [logger* & args] ((partial trace! logger* :debug) {:args args}))
(defn info [logger* & args] ((partial trace! logger* :info) {:args args}))
(defn warn [logger* & args] ((partial trace! logger* :warn) {:args args}))
(defn error [logger* & args] ((partial trace! logger* :error) {:args args}))
(defn fatal [logger* & args] ((partial trace! logger* :fatal) {:args args}))