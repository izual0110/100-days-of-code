(ns aoc2025.d3  (:require [clojure.string :as str]
                          [utils :as u]
                          [clj-async-profiler.core :as prof]))

(def test_joltage (str/split (slurp "resources/aoc2025/3t.txt") #"\n"))
(def joltage (str/split (slurp "resources/aoc2025/3.txt") #"\n"))

(defn parse [id]
  (vec (map Long/parseLong (str/split id #""))))

(defn find-max [digits]
  (let [c (count digits)
        gd (fn [i] (get digits i))
        f (fn [si mv] (loop [i si mi si]
                  (if (> i (- c mv))
                    mi
                    (recur (inc i) (if (> (gd i) (gd mi)) i mi)))))
        mi1 (f 0 2)
        mi2 (f (inc mi1) 1)]
    (+ (* (gd mi1) 10) (gd mi2))))


(assert (= 357 (->> test_joltage
                    (map parse)
                    (map find-max)
                    (apply +))))

(assert (= 17166 (->> joltage
                      (map parse)
                      (map find-max)
                      (apply +))))

(defn find-max2 [digits]
  (loop [i 0 m [i]]
    (cond
      (> (count m) 12) (map #(get digits % 0) (take 12 m))
      (>= i (- (count digits) (- 12 (count m)))) (recur (inc (last m)) (conj m (inc (last m))))
      (> (get digits i) (get digits (last m))) (recur (inc i) (assoc m (dec (count m)) i))
      :else (recur (inc i) m))))

(assert (= 3121910778619 (->> test_joltage
                              (map parse)
                              (map find-max2)
                              (map #(apply str %))
                              (mapv #(Long/parseLong %))
                              (apply +))))

(assert (= 169077317650774 (->> joltage
                                (map parse)
                                (map find-max2)
                                (map #(apply str %))
                                (mapv #(Long/parseLong %))
                                (apply +))))