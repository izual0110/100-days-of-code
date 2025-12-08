(ns aoc2025.d8
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))

(def test-boxes (str/split (slurp "resources/aoc2025/8t.txt") #"\n"))
(def boxes (str/split (slurp "resources/aoc2025/8.txt") #"\n"))


(defn parse-box [s]
  (let [[_ l w h] (re-matches #"(\d+),(\d+),(\d+)" s)]
    [(Integer/parseInt l)
     (Integer/parseInt w)
     (Integer/parseInt h)]))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2)
                (Math/pow (- z2 z1) 2))))

(defn hack-distance [[x1 y1 z1] [x2 y2 z2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        dz (- z2 z1)]
    (+ (* dx dx) (* dy dy) (* dz dz))))


(defn find-group [groups v]
  (->> groups
       (filter #(contains? % v))
       first))

(defn group-boxes [connections boxes]
  (let [pairs (->> (for [[i v1] (map-indexed vector boxes)
                         v2 (drop (inc i) boxes)]
                     [[v1 v2] (hack-distance v1 v2)])
                   (sort-by second)
                   (take connections)
                   (map first))]
    (loop [[[f1 f2] & rest-pairs] pairs r (into #{} (map hash-set boxes))]
      (if (nil? f1)
        r
        (let [g1 (find-group r f1)
              g2 (find-group r f2)
              new-r  (if (= g1 g2) r
                         (-> r
                             (disj g1 g2)
                             (conj (into g1 g2))))]
          (recur rest-pairs new-r))))))

(assert (= 40 (->> test-boxes
                   (map parse-box)
                   (group-boxes 10)
                   (map count)
                   sort
                   (take-last 3)
                   (apply *))))


(assert (= 52668 (time (->> boxes
                            (map parse-box)
                            (group-boxes 1000)
                            (map count)
                            sort
                            (take-last 3)
                            (apply *)))))

(defn last-pair [boxes]
  (let [pairs (->> (for [[i v1] (map-indexed vector boxes)
                         v2 (drop (inc i) boxes)]
                     [[v1 v2] (hack-distance v1 v2)])
                   (sort-by second)
                   (map first))]
    (loop [[[f1 f2] & rest-pairs] pairs r (into #{} (map hash-set boxes)) last-pair [f1 f2]]
      (if (nil? f1)
        last-pair
        (let [g1 (find-group r f1)
              g2 (find-group r f2)]
          (if (= g1 g2)
            (recur rest-pairs r last-pair)
            (recur rest-pairs (-> r
                                  (disj g1 g2)
                                  (conj (into g1 g2))) [f1 f2])))))))




(assert (= 25272 (->> test-boxes
                      (map parse-box)
                      last-pair
                      (map first)
                      (apply *))))

(assert (= 1474050600 (time (->> boxes
                                 (map parse-box)
                                 last-pair
                                 (map first)
                                 (apply *)))))