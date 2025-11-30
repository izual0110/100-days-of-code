(ns  aoc2024.d6
  (:require [clojure.string :as str]))


(def test-guard-map (str/split (slurp "resources/aoc2024/d6_t") #"\n"))
(def guard-map (str/split (slurp "resources/aoc2024/d6_1") #"\n"))


(defn find-start [m]
  (let [c (count m)]
    (loop [i 0 j 0]
      (cond 
        (= j c) false
        (= i c) (recur 0 (inc j))
        (= \^ (get-in m [i j])) [i j]
        :else (recur (inc i) j))))) 

(def direction {:UP [-1 0] :DOWN [1 0] :LEFT [0 -1] :RIGHT [0 1]})

(defn next-direction [d]
  (case d
    :UP :RIGHT
    :RIGHT :DOWN
    :DOWN :LEFT
    :LEFT :UP
    :else (throw (Exception. "!!!!"))))

(defn calculate [m]
  (let [s (find-start m)] 
    (loop [s s d :UP r 0 v #{s}]
      (if (nil? (get-in m s)) 
        r
        (let [[i j] s
              [x y] (direction d)
              new-x (+ i x)
              new-y (+ j y)
              n (get-in m [new-x new-y])]
        (cond
          (= n \.) (recur [new-x new-y] d (if (contains? v [new-x new-y]) r (inc r)) (conj v [new-x new-y]))
          (= n \^) (recur [new-x new-y] d (if (contains? v [new-x new-y]) r (inc r)) (conj v [new-x new-y]))
          (= n \#) (recur [i j] (next-direction d) r v)
          :else (count v)))))))


(assert (= 41 (->> 
  test-guard-map
  (mapv vec)
  calculate)))


(assert (= 5453 (->> 
  guard-map
  (mapv vec)
  calculate)))


(def grid (mapv vec guard-map))

(defn turn [y x m]
  (case m
    0 [(inc y) x (mod (inc m) 4)]
    1 [y (dec x) (mod (inc m) 4)]
    2 [(dec y) x (mod (inc m) 4)]
    3 [y (inc x) (mod (inc m) 4)]))

(defn advance [y x m]
  (case m
    0 [(dec y) x m]
    1 [y (inc x) m]
    2 [(inc y) x m]
    3 [y (dec x) m]))

(def visited (loop [y (first (find-start grid))
                    x (second (find-start grid))
                    m 0
                    v #{}]
               (if (and (>= y 0)
                        (< y (count grid))
                        (>= x 0)
                        (< x  (count (first grid))))
                 (let [[y x m] (if (= (get-in grid [y x]) \#)
                                          (turn y x m)
                                          [y x m])
                       v' (conj v [y x])
                       [y' x' m'] (advance y x m)]
                   (recur y' x' m' v'))
                 v)))

(def ans (count visited))

(def ans2 (->> visited
               (map
                (fn [[blocky blockx]]
                  (loop [ans2 0
                         g (assoc-in grid [blocky blockx] \#)
                         y (first (find-start grid))
                         x (second (find-start grid))
                         m 0
                         v #{}]
                    (if (and (>= y 0)
                             (< y (count g))
                             (>= x 0)
                             (< x  (count (first g)))
                             (= 0 ans2))
                      (let [ans2' (if (contains? v [y x m]) 1 0)
                            [y x m] (if (= (get-in g [y x]) \#)
                                               (turn y x m)
                                               [y x m])
                            v' (conj v [y x m])
                            [y' x' m'] (advance y x m)]
                        (recur ans2' g y' x' m' v'))
                      ans2))))
               (reduce +)))

(assert (= 5453 ans))
(assert (= 2188 ans2))