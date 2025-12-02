(ns aoc2025.d1
  (:require [clojure.string :as str]
            [utils :as u]))

(def test_pswd (str/split (slurp "resources/aoc2025/1t.txt") #"\n"))
(def pswd (str/split (slurp "resources/aoc2025/1.txt") #"\n"))


(defn parse [n]
  (let [[_ r d] (re-matches #"(\w)(\d*)" n)
        d (Integer/parseInt d)]
    (if (= r "R") d (- d))))

(defn calc [n]
  (loop [n n a 50 c 0]
    (if (empty? n)
      c
      (recur (next n)  (mod (+ (first n) a) 100) (if (= 0 a) (inc c) c)))))

(assert (= 98 (mod -2 100)))

(assert (= 3  (->> test_pswd
                   (map parse)
                   calc)))

(assert (= 1040  (time (->> pswd
                            (map parse)
                            calc))))

(defn calc2 [n]
  (loop [n n a 50 c 0]
    (if (empty? n)
      c
      (let [t (+ (first n) a)
            d (abs (long (/ t 100)))
            c (cond
                (= a 0) (+ c d)
                (< t 0) (+ 1 c d)
                (> t 0) (+ c d)
                :else (inc c))]
        (recur (next n) (mod t 100) c)))))

(assert (= 3 (calc2 [-50 -200])))

(assert (= 6 (->> test_pswd
                  (map parse)
                  calc2)))

(assert (= 6027 (time (->> pswd
                           (map parse)
                           calc2))))