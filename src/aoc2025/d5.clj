(ns aoc2025.d5
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))

(def test_database (str/split (slurp "resources/aoc2025/5t.txt") #"\n\n"))
(def database (str/split (slurp "resources/aoc2025/5.txt") #"\n\n"))

(defn parse-database [[ranges ids]]
  [(mapv #(let [[_ s e] (re-matches #"(\d+)-(\d+)" %)] [(Long/parseLong s) (Long/parseLong e)]) (str/split ranges #"\n"))
   (mapv #(Long/parseLong %) (str/split ids #"\n"))])

(defn calc [[ranges ids]]
  (let [sorted-ranges (sort-by first ranges)]
    (reduce (fn [acc id]
              (if (some (fn [[s e]] (and (<= s id) (>= e id))) sorted-ranges)
                (inc acc)
                acc))
            0 ids)))

(assert (= 3 (->> test_database
                  parse-database
                  calc)))

(assert (= 509 (->> database
                    parse-database
                    calc)))

(defn calc2 [ranges]
  (let [sorted-ranges (sort-by first ranges)]
    (reduce (fn [acc [s e]]
              (if
               (empty? acc) (conj acc [s e])
               (let [[ps pe] (last acc)] (if (<= (dec s) pe)
                                           (assoc acc (dec (count acc)) [ps (max e pe)])
                                           (conj acc [s e]))))) [] sorted-ranges)))

(assert (= [[1 6]] (calc2 [[1 3] [4 6]])))
(assert (=  [[1 20]] (calc2 [[1 6] [3 20] [3 10]])))
(assert (=  [[1 20]] (calc2 [[1 6] [3 10] [3 20]])))

(assert (=  14 (->> test_database
                    parse-database
                    first
                    calc2
                    (reduce (fn [acc [s e]] (+ 1 acc (- e s))) 0))))

(assert (= 336790092076620 (->> database
                                parse-database
                                first
                                calc2
                                (reduce (fn [acc [s e]]
                                          (if (= s e)
                                            (inc acc)
                                            (+ 1 acc (- e s)))) 0))))


