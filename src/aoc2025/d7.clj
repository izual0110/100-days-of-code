(ns aoc2025.d7
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))

(def test_manifold (str/split (slurp "resources/aoc2025/7t.txt") #"\n"))
(def manifold (str/split (slurp "resources/aoc2025/7.txt") #"\n"))

(defn count-splitting [m]
  (let [c (count m)
        start-pos (u/find-start m \S)]
    (loop [m m s [start-pos] v #{} r 0]
      (let [[sj si] (last s)]
        (cond (= (inc sj) c) r
              (contains? v [sj si]) (recur m (drop-last s) v r)
              (= \^ (get-in m [(inc sj) si])) (recur (-> m
                                                         (assoc-in [(inc sj) (dec si)] \|)
                                                         (assoc-in [(inc sj) (inc si)] \|))
                                                     (conj (drop-last s) [(inc sj) (dec si)] [(inc sj) (inc si)]) (conj v [sj si]) (inc r))
              :else (recur (-> m
                               (assoc-in [(inc sj) si] \|))
                           (conj (drop-last s) [(inc sj) si]) v r))))))

(assert (= 21 (->> test_manifold
                   (mapv vec)
                   count-splitting)))


(assert (= 1687 (->> manifold
                     (mapv vec)
                     count-splitting)))

(def ways
  (memoize
   (fn [m r c]
     (let [h (count m)
           w (count (first m))]
       (if (or (< r 0) (>= r h) (< c 0) (>= c w))
         1
         (let [ch (get-in m [r c])]
           (cond
             (or (= ch \.) (= ch \S)) (ways m (inc r) c)
             (= ch \^) (+ (ways m (inc r) (dec c)) (ways m (inc r) (inc c)))
             :else 0)))))))

(defn count-timelines [m]
  (let [[sr sc] (u/find-start m \S)]
    (ways m sr sc)))

(assert (= 40 (->> test_manifold
                   (mapv vec)
                   count-timelines)))

(assert (= 390684413472684 (->> manifold
                                (mapv vec)
                                count-timelines)))