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