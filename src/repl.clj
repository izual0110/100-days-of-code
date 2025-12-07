(ns repl
  (:require [nrepl.cmdline :as nrepl]))

(def default-opts
  {:port 5000
   :bind "0.0.0.0"
   :middleware ['cider.nrepl/cider-middleware]})

(defn- opts->args
  [{:keys [port bind middleware]}]
  (cond-> []
    port (conj "--port" (str port))
    bind (conj "--bind" bind)
    (seq middleware) (conj "--middleware" (pr-str middleware))))

(defn start
  ([] (start default-opts))
  ([opts]
   (set! *warn-on-reflection* true)
   (let [args (-> default-opts
                  (merge opts)
                  opts->args)]
     (apply nrepl/-main args))))


(defn init-day [d]
  (let [ns-name (symbol (str "aoc2025.d" d))
        file-path (str "src/aoc2025/d" d ".clj")
        resource-path (str "resources/aoc2025/" d "t.txt")]
    (if (or (.exists (clojure.java.io/file file-path))
            (.exists (clojure.java.io/file resource-path)))
      (println (str "Day " d " already exists, aborting."))
      (do
        (spit file-path (format "(ns aoc2025.d%s\n  (:require [clojure.string :as str]\n            [utils :as u]\n            [clj-async-profiler.core :as prof]))\n" d))
        (spit resource-path "")
        (println (str "Created " file-path))
        (println (str "Created " resource-path))))
    (require ns-name)))

(comment (init-day 8))