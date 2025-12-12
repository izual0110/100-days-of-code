(ns aoc2025.d11
  (:require [clojure.string :as str]
            [utils :as u]
            [clj-async-profiler.core :as prof]))


(def test-devices (str/split (slurp "resources/aoc2025/11t.txt") #"\n"))
(def test-devices2 (str/split (slurp "resources/aoc2025/11t2.txt") #"\n"))
(def devices (str/split (slurp "resources/aoc2025/11.txt") #"\n"))


(defn parse [s]
  (let [[_ a b] (re-matches #"(.*): (.*)" s)
        p (into #{} (str/split b #" "))] [a p]))

(defn find-paths [m]
  (loop [[p & rest-p] ["you"] r 0]
    (cond (nil? p) r
          (= p "out") (recur rest-p (inc r))
          :else (recur (apply conj rest-p (m p)) r))))

(assert (= 5 (->> test-devices
                  (map parse)
                  (into {})
                  find-paths)))

(assert (= 714 (->> devices
                    (map parse)
                    (into {})
                    find-paths)))

(defn find-uniq-paths [m]
  (let [cache (atom {})
        find-p (fn find-p [p dac? fft?]
                 (if-let [cached (@cache [p dac? fft?])]
                   cached
                   (let [res (cond
                               (and (= p "out") dac? fft?) 1
                               (= p "out") 0
                               :else (->> (m p)
                                          (map #(find-p % (or (= p "dac") dac?) (or fft? (= p "fft"))))
                                          (apply +)))]
                     (swap! cache assoc [p dac? fft?] res)
                     res)))]
    (find-p "svr" false false)))


(assert (= 2 (->> test-devices2
                  (map parse)
                  (into {})
                  (find-uniq-paths))))

(assert (= 333852915427200 (->> devices
                                (map parse)
                                (into {})
                                (find-uniq-paths))))