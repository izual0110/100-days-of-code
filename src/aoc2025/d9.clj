(ns aoc2025.d9
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))

(def test-form (str/split (slurp "resources/aoc2025/9t.txt") #"\n"))
(def form (str/split (slurp "resources/aoc2025/9.txt") #"\n"))

(defn parse-form [s]
  (let [[_ y x] (re-matches #"(\d+),(\d+)" s)]
    [(Integer/parseInt y)
     (Integer/parseInt x)]))

(defn calc-rectangle [[x1 y1] [x2 y2]]
  (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1)))))

(assert (= 50
           (calc-rectangle [2 5] [11 1])
           (calc-rectangle [11 1] [2 5])
           (calc-rectangle [1 11] [5 2])
           (calc-rectangle [5 2] [1 11])))

(defn get-all-rectangles [f]
  (for [[i v1] (map-indexed vector f)
        v2 (drop (inc i) f)]
    [[v1 v2] (calc-rectangle v1 v2)]))

(assert (= 50 (->> test-form
                   (mapv parse-form)
                   get-all-rectangles
                   (sort-by second)
                   last
                   second)))

;;time: 285ms
(assert (= 4763932976 (time (->> form
                                 (mapv parse-form)
                                 get-all-rectangles
                                 (sort-by second)
                                 last
                                 second))))

;;optimized version for part 1
;;time 20ms
(assert (= 4763932976 (time (loop [[l & rest-lines] form p [] r 0]
                              (if (nil? l) r
                                  (let [[x y] (parse-form l)
                                        new-p (conj p [x y])]
                                    (if (empty? p)
                                      (recur rest-lines new-p r)
                                      (recur rest-lines new-p (max r (apply max (for [i p] (calc-rectangle i [x y]))))))))))))

(defn rect
  ([[v1 v2]] (rect v1 v2))
  ([[x1 y1] [x2 y2]]
   [[(min x1 x2) (min y1 y2)]
    [(max x1 x2) (max y1 y2)]]))

(defn shrink-rect [[[x1 y1] [x2 y2]]]
  [[(inc x1) (inc y1)]
   [(dec x2) (dec y2)]])

(defn first-inside-rectangle [forms]
  (let [rectangles (->> (for [[i v1] (map-indexed vector forms)
                              v2     (drop (inc i) forms)]
                          [(rect v1 v2)
                           (calc-rectangle v1 v2)])
                        (sort-by second >))
        edges      (->> (concat forms [(first forms)])
                        (partition 2 1)
                        (mapv rect))]
    (->> rectangles
         (filter
          (fn [[rect]]
            (let [[[x1 y1] [x2 y2]] (shrink-rect rect)]
              (not-any?
               (fn [[[ex1 ey1] [ex2 ey2]]] (and (< x1 ex2) (> x2 ex1) (< y1 ey2) (> y2 ey1)))
               edges))))
         first)))

(assert (= 24 (->> test-form
                   (mapv parse-form)
                   first-inside-rectangle
                   second)))

(assert (= 1501292304 (time (->> form
                                 (mapv parse-form)
                                 first-inside-rectangle
                                 second))))



