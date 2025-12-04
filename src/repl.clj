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
