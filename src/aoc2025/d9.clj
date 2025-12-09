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

(assert (= 4763932976 (->> form
                           (mapv parse-form)
                           get-all-rectangles
                           (sort-by second)
                           last
                           second)))

(defn rect
  ([[v1 v2]] (rect v1 v2))
  ([[x1 y1] [x2 y2]]
   [[(min x1 x2) (min y1 y2)]
    [(max x1 x2) (max y1 y2)]]))

;;use insted of fn [[[[x1 y1] [x2 y2]]]]
(defn shrink-rect [[[x1 y1] [x2 y2]]]
  [[(inc x1) (inc y1)]
   [(dec x2) (dec y2)]])

(defn filter-inside-rectangles [forms]
  (let [rectangles (->> (for [[i v1] (map-indexed vector forms)
                              v2     (drop (inc i) forms)]
                          [(rect v1 v2)
                           (calc-rectangle v1 v2)])
                        (sort-by second >))
        edges      (->> (concat forms [(first forms)])
                        (partition 2 1)
                        (map rect)
                        vec)]
    (filter
     (fn [[[[x1 y1] [x2 y2]]]]
       (let [ix1 (inc x1)
             iy1 (inc y1)
             ix2 (dec x2)
             iy2 (dec y2)]
         (not-any?
          (fn [[[ex1 ey1] [ex2 ey2]]]
            (and (< ix1 ex2)
                 (> ix2 ex1)
                 (< iy1 ey2)
                 (> iy2 ey1)))
          edges)))
     rectangles)))

(assert (= 24 (->> test-form
                   (mapv parse-form)
                   filter-inside-rectangles
                   (map second)
                   (apply max))))

(assert (= 1501292304 (time (->> form
                                 (mapv parse-form)
                                 filter-inside-rectangles
                                 (map second)
                                 (apply max)))))



