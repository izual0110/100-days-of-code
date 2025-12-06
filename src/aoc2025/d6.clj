(ns aoc2025.d6
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))

(def test_problems (str/split (slurp "resources/aoc2025/6t.txt") #"\n"))
(def problems (str/split (slurp "resources/aoc2025/6.txt") #"\n"))


(defn parse [p]
  (let [l (str/split (str/trim p) #"(\s+)")] l))


(defn calc [lines]
  (let [nums (drop-last lines)
        op (last lines)
        get-numbers (fn [i] (map #(Long/parseLong (nth % i)) nums))]
    (map-indexed (fn [i o]
                   (if (= "+" o)
                     (apply + (get-numbers i))
                     (apply * (get-numbers i)))) op)))

(assert (= 4277556 (->> test_problems
                        (mapv parse)
                        calc
                        (apply +))))

(assert (= 6891729672676 (->> problems
                              (mapv parse)
                              calc
                              (apply +))))

(defn rtl [l]
  (let [nums (vec (drop-last l))
        ops (str/split (last l) #"(\s+)")
        c (count (first nums))]
    (loop [o (map #(if (= "+" %) + *) ops) i (dec c) r [] t []]
      (if (empty? o) r
          (let [tmp (->> nums
                         (map #(get % i))
                         (apply str)
                         str/trim)]
            (if (= "" tmp)
              (recur (drop-last o) (dec i) (conj r (apply (last o) t)) [])
              (recur o (dec i) r (conj t (Long/parseLong tmp)))))))))

(assert (= 3263827 (->> test_problems
                        rtl
                        (apply +))))

(assert (= 9770311947567 (time (->> problems
                                    rtl
                                    (apply +)))))
