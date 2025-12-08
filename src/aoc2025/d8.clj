(ns aoc2025.d8
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))

(def test-boxes (str/split (slurp "resources/aoc2025/8t.txt") #"\n"))
(def boxes (str/split (slurp "resources/aoc2025/8.txt") #"\n"))


(defn parse-box []
  (fn [s]
    (let [[_ l w h] (re-matches #"(\d+),(\d+),(\d+)" s)
          [l w h] (map #(Integer/parseInt %) [l w h])]
      [l w h])))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2)
                (Math/pow (- z2 z1) 2))))


(defn find-group [groups v]
  (->> groups
       (filter #(contains? % v))
       first))

(defn group-boxes [connections boxes]
  (let [pairs (->> (for [[i v1] (map-indexed vector boxes)
                          v2 (drop (inc i) boxes)]
                      [[v1 v2] (distance v1 v2)])
                    (sort-by second)
                    (take connections)
                    (map first))]
    (loop [[[f1 f2] & rest-pairs] pairs r (into #{} (map hash-set) boxes)]
      (if (nil? f1)
        r
        (let [g1 (find-group r f1)
              g2 (find-group r f2)
              new-r  (if (= g1 g2) r
                         (-> r
                             (disj g1 g2)
                             (conj (apply conj g1 g2))))]
          (recur rest-pairs new-r))))))

(assert (= 40 (->> test-boxes
                   (mapv (parse-box))
                   (group-boxes 10)
                   (map count)
                   sort
                   (take-last 3)
                   (apply *))))


(assert (= 52668 (time (->> boxes
                            (mapv (parse-box))
                            (group-boxes 1000)
                            (map count)
                            sort
                            (take-last 3)
                            (apply *)))))

(defn find-group2 [groups v]
  (->> groups
       (filter (fn [e]
                 (contains? (->> e second) v)))
       first))

(defn group-boxes2 [boxes]
  (let [groups (->> boxes
                    (map-indexed (fn [i v1]
                                   (mapv (fn [v2]
                                           (if (< (first v1) (first v2))
                                             [[v1 v2] (distance v1 v2)]
                                             [[v2 v1] (distance v1 v2)]))
                                         (nthnext boxes (inc i)))))
                    (mapcat identity)
                    (sort-by second))]
    (loop [distances (map first groups) r (set (map (fn [v] [v #{v}]) boxes)) last-value (first distances)]
      (if (empty? distances)
        last-value
        (let [f (first distances)
              f1 (first f)
              f2 (second f)
              g1 (find-group2 r f1)
              g2 (find-group2 r f2)
              new-r (if (= g1 g2) r (-> r
                                        (disj g1 g2)
                                        (conj [(first g1) (apply conj (second g1) (second g2))])))]
          (recur (next distances) new-r (if (= g1 g2) last-value f)))))))




(->> test-boxes
     (mapv (parse-box))
     (group-boxes2)
     (map first)
      (apply *))

(->> boxes
     (mapv (parse-box))
     (group-boxes2)
     (map first)
     (apply *))