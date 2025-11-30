(ns  aoc2024.d15
  (:require [clojure.string :as str]
            [utils :as u]))

(def test-movements (str/split (slurp "resources/aoc2024/d15_t") #"\n\n"))
(def movements (str/split (slurp "resources/aoc2024/d15_1") #"\n\n"))

(defn parse-movements [[r-map r-movements]]
  (let [r-map (str/split r-map #"#\n#")
        r-map (subvec r-map 1 (dec (count r-map)))]
       [(mapv vec r-map) (filterv #(not= \newline %) r-movements)]))

(defn direction [d]
  (case d
    \^ [-1 0]
    \v [1 0]
    \> [0 1]
    \< [0 -1]))

(defn find-start [a]
  (let [c (count a)]
    (loop [j 0 i 0]
      (cond 
        (= j c) nil
        (= i (count (get a j))) (recur (inc j) 0)
        (= \@ (get-in a [j i])) [j i]
        :else (recur j (inc i))))))

(defn move [d si sj r-map]
  (let [[oy ox] (direction d)
        [x y] (loop [i (+ si oy) j (+ sj ox)]
                (cond 
                  (= \. (get-in r-map [i j])) [i j]
                  (= \O (get-in r-map [i j])) (recur (+ i oy) (+ j ox))
                  :else [si sj]))]
    (if (and (= x si) (= y sj))
      [si sj r-map]
      [(+ si oy) (+ sj ox) (-> r-map
        (assoc-in [x y] \O)
        (assoc-in [(+ si oy) (+ sj ox)] \.))])))

(defn simulate[[r-map r-movements]]
  (let[[i j] (find-start r-map)
       r-map (assoc-in r-map [i j] \.)]
    (loop [rm r-movements i i j j r-map r-map]
      (if (empty? rm) 
        r-map
        (let [f (first rm)
              [i' j' r-map'] (move f i j r-map)]
          (recur (next rm) i' j' r-map'))))))

(defn calculate [a]
  (let [c (count a)]
    (loop [j 0 i 0 r 0]
      (cond 
        (= j c) r
        (= i (count (get a j))) (recur (inc j) 0 r)
        (= \O (get-in a [j i])) (recur j (inc i) (+ r (* 100 (inc j)) (inc i)))
        (= \] (get-in a [j i])) (recur j (inc i) (+ r (* 100 (inc j)) (inc i)))
        :else (recur j (inc i) r)))))

(assert (= 105 (calculate [(vec "...[]...")])))

(assert (= 10092 (->> test-movements
  parse-movements
  simulate
  calculate)))

(assert (= 1490942 (->> movements
  parse-movements
  simulate
  calculate)))

(u/vec-insertm [1 2 3 4] 1 [6 6])

(defn update-map [[r-map r-movements]]
  (loop [j 0 i 0 r r-map]
    (cond 
      (> j (count r)) [r r-movements]
      (> i (count (get r j))) (recur (inc j) 0 r)
      (= \# (get-in r [j i])) (recur j (+ i 2) (assoc r j (u/vec-insert (get r j) i \#)))
      (= \O (get-in r [j i])) (recur j (+ i 2) (-> r
                                                     (assoc j (u/vec-insert (get r j) i \[))
                                                     (assoc-in [j (inc i)] \])
                                                 ))
                                    
      (= \@ (get-in r [j i])) (recur j (+ i 2) (assoc r j (u/vec-insert (get r j) (inc i) \.)))
      (= \. (get-in r [j i])) (recur j (+ i 2) (assoc r j (u/vec-insert (get r j) i \.)))                                    
      :else (recur j (inc i) r))))


(defn can-move [d si sj r-map]
  (let [[oy ox] (direction d)]
    (if (or (= d \<) (= d \>))
      (loop [i (+ si oy) j (+ sj ox)]
                (cond 
                  (= \. (get-in r-map [i j])) true
                  (or 
                    (= \] (get-in r-map [i j]))
                    (= \[ (get-in r-map [i j]))) (recur (+ i oy oy) (+ j ox ox))
                  :else false))
      (cond 
        (= \# (get-in r-map [(+ si oy) (+ sj ox)])) false
        (= \[ (get-in r-map [(+ si oy) (+ sj ox)])) (and (can-move d (+ si oy) (+ sj ox) r-map) (can-move d (+ si oy) (+ sj ox 1) r-map))
        (= \] (get-in r-map [(+ si oy) (+ sj ox)])) (and (can-move d (+ si oy) (+ sj ox) r-map) (can-move d (+ si oy) (+ sj ox -1) r-map))
        (= \. (get-in r-map [(+ si oy) (+ sj ox)])) true))))

(defn move-box-right[si sj oy ox r]
  (loop [i si j sj r r]
    (let [b (get-in r [(+ i oy) (+ j ox)])] 
      (cond 
        (= \. (get-in r [(+ i oy oy oy) (+ j ox ox ox)])) (-> r  
                                                            (assoc-in [(+ si oy) (+ sj ox)] \.)
                                                            (assoc-in [(+ i oy oy) (+ j ox ox)] \[)
                                                            (assoc-in [(+ i oy oy oy) (+ j ox ox ox)] \]))

        (or (= \[ b) (= \] b)) (recur (+ i oy oy) (+ j ox ox) (-> r  
                                                                (assoc-in [(+ i oy oy) (+ j ox ox)] \[)
                                                                (assoc-in [(+ i oy oy oy) (+ j ox ox ox)] \])))))))

(defn move-box-left[si sj oy ox r]
  (loop [i si j sj r r]
    (let [b (get-in r [(+ i oy) (+ j ox)])] 
      (cond 
        (= \. (get-in r [(+ i oy oy oy) (+ j ox ox ox)])) (-> r  
                                                            (assoc-in [(+ si oy) (+ sj ox)] \.)
                                                            (assoc-in [(+ i oy oy) (+ j ox ox)] \])
                                                            (assoc-in [(+ i oy oy oy) (+ j ox ox ox)] \[))

        (or (= \[ b) (= \] b)) (recur (+ i oy oy) (+ j ox ox) (-> r  
                                                                (assoc-in [(+ i oy oy) (+ j ox ox)] \])
                                                                (assoc-in [(+ i oy oy oy) (+ j ox ox ox)] \[)))))))

(defn move-box-up-down [si sj oy ox r]
  (let [b (get-in r [(+ si oy) (+ sj ox)])]
    (cond 
      (= \[ b) (let [r' (->> r 
                          (move-box-up-down (+ si oy) (+ sj ox) oy ox)
                          (move-box-up-down (+ si oy) (+ sj ox 1) oy ox))
                     r' (-> r'
                          (assoc-in [(+ si oy oy) (+ sj ox)] \[)
                          (assoc-in [(+ si oy oy) (+ sj ox 1)] \])
                          (assoc-in [(+ si oy) (+ sj ox)] \.)
                          (assoc-in [(+ si oy) (+ sj ox 1)] \.))] r')
       
      (= \] b) (let [r' (->> r 
                          (move-box-up-down (+ si oy) (+ sj ox) oy ox)
                          (move-box-up-down (+ si oy) (+ sj ox -1) oy ox))
                     r' (-> r'
                          (assoc-in [(+ si oy oy) (+ sj ox)] \])
                          (assoc-in [(+ si oy oy) (+ sj ox -1)] \[)
                          (assoc-in [(+ si oy) (+ sj ox)] \.)
                          (assoc-in [(+ si oy) (+ sj ox -1)] \.))] r')
      :else r)))

(println (move-box-up-down 2 0 -1 0 [(vec "....") (vec "[]..") (vec "....")]))
(println (move-box-up-down 3 1 -1 0 [(vec "....") (vec "[][].") (vec ".[].") (vec "....")]))

(println (move-box-up-down 0 0 1 0 [(vec "....") (vec "[]..") (vec "....")]))
(println (move-box-up-down 0 1 1 0 [(vec "....") (vec ".[].") (vec "[][].") (vec ".....")]))

(move-box-right 0 0 0 1 [(vec ".[].")])
(move-box-right 0 0 0 1 [(vec ".[][].")])

(move-box-left 0 3 0 -1 [(vec ".[].")])
(move-box-left 0 5 0 -1 [(vec ".[][].")])

(defn move2 [d si sj r-map]
  (let [[oy ox] (direction d)
        cm (can-move d si sj r-map)
        rewrite-map (fn [i j r] 
                      (cond 
                        (= \# (get-in r-map [(+ si oy) (+ sj ox)])) r
                        (= \# (get-in r-map [(+ si oy) (+ sj ox)])) r
                        )) ]
    (if cm
      [(+ si oy) 
       (+ sj ox) 
       (cond 
         (= \. (get-in r-map [(+ si oy) (+ sj ox)])) (assoc-in r-map [(+ si oy) (+ sj ox)] \.)
         (= d \>) (move-box-right si sj oy ox r-map)
         (= d \<) (move-box-left si sj oy ox r-map)
         (= d \^) (move-box-up-down si sj oy ox r-map)
         (= d \v) (move-box-up-down si sj oy ox r-map))]
      [si sj r-map])))

(defn simulate2[[r-map r-movements]]
  (let[[i j] (find-start r-map)
       r-map (assoc-in r-map [i j] \.)]
    (loop [rm r-movements i i j j r-map r-map]
      (if (empty? rm) 
        r-map
        (let [f (first rm)
              [i' j' r-map'] (move2 f i j r-map)]
          (recur (next rm) i' j' r-map'))))))

(assert (= 9021 (->> test-movements
  parse-movements
  update-map
  simulate2
  calculate)))

(assert (= 1519202 (->> movements
  parse-movements
  update-map
  simulate2
  calculate)))



