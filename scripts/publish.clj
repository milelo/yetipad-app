(ns publish
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

(when (or (> (count *command-line-args*) 1)
          (and supplied-version
               (not (re-matches #"[0-9]+" supplied-version))))
  (binding [*out* *err*]
    (println "Usage: bb publish [numeric-version]"))
  (System/exit 2))

(when-not (= "main" (run-git "branch" "--show-current"))
  (throw (ex-info "Releases must be published from the main branch." {})))

(when-not (str/blank? (run-git "status" "--porcelain"))
  (throw (ex-info "The working tree must be clean before publishing." {})))

(def numeric-tags
  (->> (str/split-lines (run-git "tag" "--list" "v*"))
       (filter #(re-matches #"v[0-9]+" %))
       (map #(Long/parseLong (subs % 1)))))

;; The existing Pages release is 29, but historical releases used v0.x tags.
;; This floor is only needed until the first numeric vN tag is published.
(def current-release-version 29)

(def version
  (or supplied-version
      (str (inc (long (max current-release-version
                           (if (seq numeric-tags)
                             (apply max numeric-tags)
                             0)))))))

(let [tag (str "v" version)]
  (when-not (str/blank? (run-git "tag" "--list" tag))
    (throw (ex-info (str "Release tag already exists: " tag) {})))
  (run-git "tag" "-a" tag "-m" (str "Release " version))
  (run-git "push" "origin" tag)
  (println (str "Published " tag ". GitHub Actions will deploy it to Pages.")))
