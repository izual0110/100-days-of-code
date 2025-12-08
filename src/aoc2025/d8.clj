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

(find-group [#{[1 2] [3 4]} #{5 6}] [2 1])


(defn group-boxes [connections boxes]
  (let [groups (->> boxes
                    (map-indexed (fn [i v1]
                                   (mapv (fn [v2]
                                           {#{v1 v2} (distance v1 v2)})
                                         (nthnext boxes (inc i)))))
                    flatten
                    (into {})
                    (sort-by second))]
    (loop [distances (keys (take connections groups)) r (set(map (fn [v] #{v}) boxes))]
      (if (empty? distances)
        r
        (let [f (first distances)
              f1 (first f)
              f2 (second f)
              g1 (find-group r f1)
              g2 (find-group r f2)
              new-r  (-> r
                         (disj g1 g2)
                         (conj (apply conj g1 g2)))
              ] (recur (next distances) new-r))))))


(->> test-boxes
     (mapv (parse-box))
     (group-boxes 10)
     (map count)
     sort
     (take-last 3)
     (apply *))


(->> boxes
     (mapv (parse-box))
     (group-boxes 1000)
     (map count)
     sort
     (take-last 3)
     (apply *))