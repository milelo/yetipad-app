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
    (println "Usage: bb publish [minor-version]"))
  (System/exit 2))

(when-not (= "main" (run-git "branch" "--show-current"))
  (throw (ex-info "Releases must be published from the main branch." {})))

(when-not (str/blank? (run-git "status" "--porcelain"))
  (throw (ex-info "The working tree must be clean before publishing." {})))

(def minor-tags
  (->> (str/split-lines (run-git "tag" "--list" "v*"))
       (filter #(re-matches #"v1\.[0-9]+" %))
       (map #(Long/parseLong (second (re-matches #"v1\.([0-9]+)" %))))))

;; Existing vN tags are historical releases. The first v1.x release is v1.41.
(def current-release-minor 40)

(def minor-version
  (or supplied-version
      (str (inc (long (max current-release-minor
                           (if (seq minor-tags)
                             (apply max minor-tags)
                             0)))))))

(let [tag (str "v1." minor-version)]
  (when-not (str/blank? (run-git "tag" "--list" tag))
    (throw (ex-info (str "Release tag already exists: " tag) {})))
  (run-git "tag" "-a" tag "-m" (str "Release 1." minor-version))
  (run-git "push" "origin" tag)
  (println (str "Published " tag ". GitHub Actions will dispatch the Pages build from main."
                "\nPublish status: https://github.com/milelo/yetipad-app/actions")))
