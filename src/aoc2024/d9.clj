(ns  aoc2024.d9
  (:require [clj-async-profiler.core :as prof]))

(def test-disk-map (slurp "resources/aoc2024/d9_t"))
(def disk-map (slurp "resources/aoc2024/d9_1"))


(defn vec-insert [v i e] (vec (concat (take i v) [e] (drop i v))))

(defn calculate [m]
  (loop [m m s 0 e (count m) i 0 f 0 r 0]
    (cond 
      (> s e) r
      (= 1 (mod s 2)) (recur m (inc s) e i (+ f (get m s)) r)
      (and (> f 0) (= 1 (mod e 2)) (> (get m (dec e)) 0)) (recur (update m (dec e) dec) s e (inc i) (dec f) (+ r (* i (int (/ e 2)))))
      (and (> f 0) (= 1 (mod e 2)) (= (get m (dec e)) 0)) (recur m s (- e 2) i f r)
      (> (get m s) 0) (recur (update m s dec) s e (inc i) f (+ r (* i (int (/ s 2)))))
      :else (recur m (inc s) e i f r))))


(assert (= 1928 (->> test-disk-map
  (mapv #(- (int %) 48))
  calculate)))

(assert (= 6435922584968 (->> disk-map
  (mapv #(- (int %) 48))
  calculate)))


(defn parse-disk-map [m]
  (loop [m m i 0 id 0 r []]
    (cond
      (empty? m) r
      (= 0 (mod i 2)) (recur (next m) (inc i) (inc id) (conj r {:r (first m) :id id}))
      :else (recur (next m) (inc i) id (conj r {:f (first m)})))))


(defn find-free-space [m s find-free-space-cache]
  (let [c (count m)]
    (loop [i (find-free-space-cache s 0)]
      (if (= i c) 
        [nil (assoc find-free-space-cache s c)]
        (let [g (get m i)
              t (g :f)]
          (if 
            (and (some? t) (>= t s)) [i (assoc find-free-space-cache s i)]
            (recur (inc i))))))))

(defn compact-disk [m] 
  (loop [m m r (dec (count m)) find-free-space-cache {}] 
    (cond 
      (= r 0) m
      (contains? (get m r) :f) (recur m (dec r) find-free-space-cache)
      :else (let [f (get m r)
                  reserved (f :r)
                  [i find-free-space-cache] (find-free-space m reserved find-free-space-cache)]
        (if (nil? i)
          (recur m (dec r) find-free-space-cache)
          (let [reminder (- ((get m i) :f) reserved)]
            (cond 
              (> i r) (recur m (dec r) find-free-space-cache)
              (= reminder 0) (recur (-> m
                                      (assoc r {:f reserved} i {:r reserved :id (f :id)})) (dec r) find-free-space-cache)
              :else (recur (-> m
                             (assoc r {:f reserved} i {:f reminder})
                             (vec-insert i {:r reserved :id (f :id)})) (dec r) find-free-space-cache))))))))

(defn calculate-2 [m]
  (let [c (count m)]
    (loop [m m i 0 ir 0 r 0]
      (if (= c i) 
        r
      	(let [f (get m i)]
          (cond 
            (contains? f :f) (recur m (inc i) (+ ir ^long (f :f)) r)
            (= 0 (f :r)) (recur m (inc i) ir r)
            :else (recur (update-in m [i :r] dec) i (inc ir) (+ r (* ir ^long (f :id))))))))))

(assert (= 2858 (->> test-disk-map
  (mapv #(- (int %) 48))
  parse-disk-map
  compact-disk
  calculate-2)))

(assert (= 6469636832766 (->> disk-map
  (mapv #(- (long %) 48))
  parse-disk-map
  compact-disk
  calculate-2)))

(comment (do
  (println "start profiling")
  (prof/start)
  (time (->> disk-map
  (mapv #(- (long %) 48))
  parse-disk-map
  compact-disk
  calculate-2))
  (println (prof/stop))
  (println "end profiling")))

(set! *warn-on-reflection* true)
