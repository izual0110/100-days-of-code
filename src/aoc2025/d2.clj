(ns aoc2025.d2  (:require [clojure.string :as str]
                          [utils :as u]
                          [clj-async-profiler.core :as prof]))

(def test_ids (str/split (slurp "resources/aoc2025/2t.txt") #","))
(def ids (str/split (slurp "resources/aoc2025/2.txt") #","))

(defn parse [id]
  (vec (map Long/parseLong (str/split id #"-"))))

(defn check-id [id]
  (let [id (str id)
        c (count id)]
    (if (= 1 (mod c 2))
      false
      (loop [i 0]
        (if (= i (/ c 2))
          true
          (if (= (nth id i) (nth id (+ (/ c 2) i)))
            (recur (inc i))
            false))))))

(assert (false? (check-id 1001)))

(defn check-ids [[^long s ^long e]]
  (loop [i s c 0]
    (if (> i e)
      c
      (recur (inc i) (if (check-id i) (+ c i) c)))))

(defn check-id2 [id]
  (let [id (str id)
        c (count id)]
    (loop [i 1]
      (cond
        (> i (/ c 2)) false
        (not= (mod c i) 0) (recur (inc i))
        (empty? (str/replace id (subs id 0 i) "")) true
        :else (recur (inc i))))))

(assert (check-id2 1188511885))

(defn check-ids2 [[^long s ^long e]]
  (loop [i s  c  0]
    (if (> i e)
      c
      (recur (inc i) (if (check-id2 i) (+ c i) c)))))

(assert (= 1227775554 (->> test_ids
                           (map parse)
                           (map check-ids)
                           (apply +))))

(assert (= 40214376723 (time (->> ids
                                  (pmap parse)
                                  (pmap check-ids)
                                  (apply +)))))

(assert (= 4174379265 (->> test_ids
                           (map parse)
                           (map check-ids2)
                           (apply +))))

(assert (= 50793864718 (time (->> ids
                                  (pmap parse)
                                  (pmap check-ids2)
                                  (apply +)))))

; "Elapsed time: 2824.398355 msecs"
; "Elapsed time: 1357.628669 msecs"
; "Elapsed time: 461.9015 msecs"

(do
  (vec (for [i (range 10)] (time (->> ids
                                      (mapv parse)
                                      (pmap check-ids2)
                                      (apply +)))))
  (println "start profiling")
  (prof/start)
  (time (->> ids
             (pmap parse)
             (pmap check-ids2)
             (apply +)))
  (println (prof/stop))
  (println "end profiling"))

(set! *warn-on-reflection* true)