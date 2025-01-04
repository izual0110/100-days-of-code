(ns  aoc2024.d20
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof])
   (:use     [utils]))


(def test-racetrack (str/split (slurp "resources/aoc2024/d20_t") #"\n"))
(def racetrack (str/split (slurp "resources/aoc2024/d20") #"\n"))


(defn dijkstra [m [si sj] [ei ej]]
  (let [find-neighbours (fn [i j r] (for [[oi oj] [[-1 0][1 0][0 1][0 -1]]
                                        :let [i' (+ i oi)
                                              j' (+ j oj)
                                              x (get-in m [i' j'])]
                                        :when (and (not (nil? x)) (not= x \#))] [i' j' (inc r)]))]
    (loop [points (sorted-set-by (fn [[a1 b1 r1] [a2 b2 r2]] (compare [r1 a1 b1] [r2 a2 b2])) [si sj 0]) v {}]
      (cond 
        (empty? points) v
        (contains? v [ei ej]) v
        :else (let [[i j r] (first points)
                    neighbours (filter (fn [[i j r]] (let [v' (v [i j])] (or (nil? v') (< r v')))) (find-neighbours i j r))
                    points' (disj points [i j r])
                    tmp (v [i j])]
                (recur (apply conj points' neighbours) (if (or (nil? tmp) (< r tmp)) (assoc v [i j] r) v)))))))

(defn hack[sp m]
  (let [mi (dec (count m))
        ss (utils/find-start m \S)
        ee (utils/find-start m \E)
        baseline ((dijkstra m ss ee) ee)
        c (count m)] 
    (loop [j 0 i 0 r 0]
      (cond 
        (= j c) r
        (= i (count (get m j))) (recur (inc j) 0 r)
        (not= \# (get-in m [j i])) (recur j (inc i) r)
        (not (or 
               (and (not= \# (get-in m [j (inc i)]) \#) (not= \# (get-in m [j (dec i)] \#)))
               (and (not= \# (get-in m [(inc j) i]) \#) (not= \# (get-in m [(dec j) i] \#))))) (recur j (inc i) r)
        :else (let [tmp ((dijkstra (assoc-in m [j i] \.) ss ee) ee)]
                (if (>= (- baseline tmp) sp)
                  (recur j (inc i) (inc r))
                  (recur j (inc i) r)))))))

(->> test-racetrack
  (mapv vec)
  (hack 20))

(->> racetrack
  (mapv vec)
  (hack 100))

(defn hack2[sp m]
  (let [mi (dec (count m))
        ss (utils/find-start m \S)
        ee (utils/find-start m \E)
        visited (dijkstra m ss ee)
        find-neighbours (fn [i j] (for [[oi oj] [[-1 0][1 0][0 1][0 -1]]
                                        :let [i' (+ i oi)
                                              j' (+ j oj)
                                              r (visited [i j])
                                              x (visited [i' j'])]
                                        :when (= x (dec r))] [i' j']))
        all-points (loop [p [ee] r []]
                     (if (empty? p) 
                       (vec (sort r))
                       (recur (apply conj (next p) (apply find-neighbours (first p))) (conj r (first p)))))
        c (count m)] 
    ((comp count flatten) (for [a (range (count all-points))]
      (for [b (range a (count all-points))
            :let [[i1 j1] (get all-points a)
                  [i2 j2] (get all-points b)
                  d (+ (abs (- i2 i1)) (abs (- j2 j1)))]
            :when (and (<= d 20)
                    (>= (- (abs (- (visited [i1 j1]) (visited [i2 j2]))) d) sp))] 1)))))


(assert (= 285 (->> test-racetrack
  (mapv vec)
  (hack2 50))))

(assert (= 975376 (->> racetrack
  (mapv vec)
  (hack2 100))))

