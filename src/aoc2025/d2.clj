(ns aoc2025.d2  (:require [clojure.string :as str]
                          [utils :as u]))

(def test_ids (str/split (slurp "resources/aoc2025/2t.txt") #","))
(def ids (str/split (slurp "resources/aoc2025/2.txt") #","))

(defn parse [id]
  (vec (map Long/parseLong (str/split id #"-"))))

(defn check-id [id]
  (cond
    (= 1 (mod (count (str id)) 2)) false
    :else (loop [i 0]
            (if (= i (/ (count (str id)) 2))
              true
              (if (= (nth (str id) i) (nth (str id) (+ (/ (count (str id)) 2) i)))
                (recur (inc i))
                false)))))

(assert (false? (check-id 1001)))

(defn check-ids [[s e]]
  (loop [i s c 0]
    (if (> i e)
      c
      (recur (inc i) (if (check-id i) (+ c i) c)))))


(defn char-count [pattern text]
  (count (re-seq (#(pattern)) text)))

(assert (= 3 (char-count #(re-pattern "123") "123123123")))

(assert (= "123" (subs "123456" 0 3)))

(defn check-id2 [id]
  (let [id (str id)]
    (loop [i 1]
      (cond
        (> i (/ (count id) 2)) false
        (not= (mod (count id) i) 0) (recur (inc i))
        (let [matches (char-count #(re-pattern (subs id 0 i)) id)]
          (and  (> matches 1) (= (count id) (* matches  (count (subs id 0 i)))))) true
        :else (recur (inc i))))))

(assert (check-id2 1188511885))


(defn check-ids2 [[s e]]
  (loop [i s c 0]
    (if (> i e)
      c
      (recur (inc i) (if (check-id2 i) (+ c i) c)))))

(assert (= 1227775554 (->> test_ids
                           (map parse)
                           (map check-ids)
                           (apply +))))

(assert (= 40214376723 (->> ids
                            (map parse)
                            (map check-ids)
                            (apply +))))

(assert (= 4174379265 (->> test_ids
                           (map parse)
                           (map check-ids2)
                           (apply +))))

(assert (= 50793864718 (->> ids
                            (map parse)
                            (map check-ids2)
                            (apply +))))

