(ns aoc2025.d10
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))


(def test-manual (str/split (slurp "resources/aoc2025/10t.txt") #"\n"))
(def manual (str/split (slurp "resources/aoc2025/10.txt") #"\n"))

(defn parse-manual [s]
  (let [[_ d b j] (re-matches #"\[(.*)\] (.*) \{(.*)\}" s)]
    [(mapv #(if (= \# %) true false) d)
     (->> (re-seq #"\(([^)]*)\)" b)
          (map second)
          (map #(str/split % #","))
          (mapv #(mapv parse-long %))) 
     (->> (str/split j #",")
          (mapv parse-long))]))

(defn update-lights [l i]
  (update l i #(not %)))

(assert (every? true? (update-lights [true false] 1)))

(defn find-presses [[lights buttons]]
  (loop [[c & rest-c] (u/full-combinations buttons)] 
    (cond (nil? c) 0
          (every? false? (reduce (fn [acc v] (update-lights acc v)) lights (flatten c))) (count c)
          :else (recur rest-c))))

(assert (= 7 (->> test-manual
     (mapv parse-manual)
     (mapv find-presses)
     (apply +))))

(assert (= 396 (->> manual
     (mapv parse-manual)
     (mapv find-presses)
     (apply +))))

