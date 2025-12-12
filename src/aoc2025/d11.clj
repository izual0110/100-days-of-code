(ns aoc2025.d11
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))


(def test-devices (str/split (slurp "resources/aoc2025/11t.txt") #"\n"))
(def devices (str/split (slurp "resources/aoc2025/11.txt") #"\n"))


(defn parse [s]
  (let [[_ a b] (re-matches #"(.*): (.*)" s)
        p (into #{} (str/split b #" "))] [a p]))

(defn find-paths [m]
  (loop [[p & rest-p] ["you"] v #{} r 0]
    (cond (nil? p) r
          (= p "out") (recur rest-p v (inc r))
          :else (recur (apply conj rest-p (m p)) (conj v p) r))))

(assert (= 5 (->> test-devices
                  (map parse)
                  (into {})
                  find-paths)))

(assert (= 714 (->> devices
                    (map parse)
                    (into {})
                    find-paths)))
