(ns aoc2025.d4  (:require [clojure.string :as str]
                          [utils :as u]
                          [clj-async-profiler.core :as prof]))

(def test_grid (str/split (slurp "resources/aoc2025/4t.txt") #"\n"))
(def grid (str/split (slurp "resources/aoc2025/4.txt") #"\n"))


(def positions [[-1 -1] [-1 0] [-1 1]
                [0  -1]           [0  1]
                [1  -1] [1  0] [1  1]])

(defn get-neighbors [grid x y]
  (let [max-x (dec (count grid))
        max-y (dec (count (first grid)))]
    (reduce (fn [acc [dx dy]]
              (if (let [nx (+ x dx)
                        ny (+ y dy)]
                    (and (>= nx 0)
                         (>= ny 0)
                         (<= nx max-x)
                         (<= ny max-y)
                         (= \@ (nth (nth grid nx) ny))))
                (inc acc)
                acc)) 
            0 positions)))


(assert (= 13 (time (let [g (->> test_grid
                                 (mapv vec))]
                      (->> g
                           (map-indexed (fn [x row]
                                          (map-indexed (fn [y v]
                                                         (if (= \@ v)
                                                           (get-neighbors g x y)
                                                           10))
                                                       row)))
                           (mapcat identity)
                           (filter #(< % 4))
                           count)))))

(assert (= 1435 (time (let [g (->> grid
                                   (mapv vec))]
                        (->> g
                             (map-indexed (fn [x row]
                                            (map-indexed (fn [y v]
                                                           (if (= \@ v)
                                                             (get-neighbors g x y)
                                                             10))
                                                         row)))
                             (mapcat identity)
                             (filter #(< % 4))
                             count)))))


(defn calc [grid]
  (loop [g grid]
    (let [new-g (vec (map-indexed (fn [x row]
                                    (vec (map-indexed (fn [y v]
                                                        (if (and (= \@ v) (< (get-neighbors g x y) 4))
                                                          \x
                                                          v))
                                                      row)))
                                  g))]
      (if (= new-g g)
        (->> new-g
             (mapcat identity)
             (filter #(= \x %))
             count)
        (recur new-g)))))

(assert (= 43 (->> test_grid
                   (mapv vec)
                   calc)))

(assert (= 8623 (time (->> grid
                           (mapv vec)
                           calc))))


(do
  (vec (for [i (range 10)] (time (->> grid
                                      (mapv vec)
                                      calc))))
  (println "start profiling")
  (prof/start)
  (time (->> grid
             (mapv vec)
             calc))
  (println (prof/stop))
  (println "end profiling"))