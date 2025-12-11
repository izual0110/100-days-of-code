(ns utils)


(defn vec-insert [v i e] (vec (concat (take i v) [e] (drop i v))))
(defn vec-insertm [v i e] (vec (concat (take i v) e (drop i v))))
(defn vec-remove [v i] (into (subvec v 0 i) (subvec v (inc i))))

(defn find-start [a v]
  (let [c (count a)]
    (loop [j 0 i 0]
      (cond 
        (= j c) nil
        (= i (count (get a j))) (recur (inc j) 0)
        (= v (get-in a [j i])) [j i]
        :else (recur j (inc i))))))


(defn find-all [a v]
  (let [c (count a)]
    (loop [j 0 i 0 r #{}]
      (cond 
        (= j c) r
        (= i (count (get a j))) (recur (inc j) 0 r)
        (= v (get-in a [j i])) (recur j (inc i) (conj r [j i]))
        :else (recur j (inc i) r)))))


(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(assert (= `([1 2] [2 1]) (permutations [1 2])))
(assert (= `([2 1] [1 2]) (permutations [2 1])))



(defn combinations [k coll]
  (cond
    (= k 0) (list '())
    (empty? coll) '()
    :else
    (concat
     (map #(cons (first coll) %) (combinations (dec k) (rest coll)))
     (combinations k (rest coll)))))


(defn full-combinations [coll]
  (mapcat #(combinations % coll) (range 1 (inc (count coll)))))

(assert (= `((1 2) (1 3) (2 3)) (combinations 2 [1 2 3])))
(assert (= `((1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) (full-combinations [1 2 3])))