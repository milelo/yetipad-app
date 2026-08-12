(ns release-info
  (:require [babashka.process :as process]
            [clojure.string :as str]))

(defn run-git [& args]
  (let [{:keys [exit out err]} (apply process/shell {:out :string :err :string}
                                                    (into ["git"] args))]
    (when-not (zero? exit)
      (throw (ex-info (str "git " (str/join " " args) " failed: " (str/trim err))
                      {:exit exit})))
    (str/trim out)))

(def supplied-version (first *command-line-args*))

(when (> (count *command-line-args*) 1)
  (binding [*out* *err*]
    (println "Usage: bb release-info [version]"))
  (System/exit 2))

(when (and supplied-version
           (not (re-matches #"v?[0-9]+" supplied-version)))
  (binding [*out* *err*]
    (println "Version must be numeric, for example 36 or v36."))
  (System/exit 2))

(def release-tag
  (if supplied-version
    (if (str/starts-with? supplied-version "v")
      supplied-version
      (str "v" supplied-version))
    (->> (str/split-lines (run-git "tag" "--list" "v[0-9]*"))
         (filter #(re-matches #"v[0-9]+" %))
         (sort-by #(Long/parseLong (subs % 1)) >)
         first)))

(when-not release-tag
  (throw (ex-info "No numeric release tags were found." {})))

(println (str "Release: " release-tag))
(println (run-git "log" "-1" "--format=%H%n%ad%n%s%n%b" "--date=iso-strict" release-tag))
