(ns  aoc2024.d21
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clj-async-profiler.core :as prof])
   (:use     [utils]))

(def test-target (str/split (slurp "resources/aoc2024/d21_t") #"\n"))
(def target (str/split (slurp "resources/aoc2024/d21") #"\n"))

; +---+---+---+
; | 7 | 8 | 9 |
; +---+---+---+
; | 4 | 5 | 6 |
; +---+---+---+
; | 1 | 2 | 3 |
; +---+---+---+
;     | 0 | A |
;     +---+---+              
              
(def nkeypad {\7 [0 0] \8 [0 1] \9 [0 2]
              \4 [1 0] \5 [1 1] \6 [1 2]
              \1 [2 0] \2 [2 1] \3 [2 2]
                       \0 [3 1] \A [3 2]})

(count nkeypad)
(count dkeypad)


;     +---+---+
;     | ^ | A |
; +---+---+---+
; | < | v | > |
; +---+---+---+              
(def dkeypad {             \^ [0 1] \A [0 2]
                  \< [1 0] \v [1 1] \> [1 2]})


(defn parse-target [x]
  (let [[_ n] (re-matches #"(\d*)A" x)]
        [(Integer/parseInt n) (vec x)]))

(defn keypad>keypad [source distination] 
  (let [invert-distination (set/map-invert distination)
        apply-conj (partial apply conj)]
    (loop [s source r [] p \A]
      (if (empty? s)
        r
        (let [f (first s)
              [i1 j1] (distination p)
              [i2 j2] (distination f)
              
              is-nkeypad (= 11 (count distination))
              last-top-bottom (if (= (last r) \A) 
                                (or (= \^ (get r (- (count r) 2))) (= \v (get r (- (count r) 2))))
                                (or (= \^ (last r)) (= \v (last r))))
              r (cond 
                  (and is-nkeypad 
                    (or (= p \7) (= p \4) (= p \1)) 
                    (or (= f \0) (= f \A))) (-> r
                                              (apply-conj (repeat (- j2 j1) \>))
                                              (apply-conj (repeat (- i2 i1) \v)))
                  (and is-nkeypad 
                    (or (= f \7) (= f \4) (= f \1)) 
                    (or (= p \0) (= p \A))) (-> r
                                              (apply-conj (repeat (- i1 i2) \^))
                                              (apply-conj (repeat (- j1 j2) \<)))
                  (and (not is-nkeypad)
                    (= p \<) 
                    (or (= f \^) (= f \A))) (-> r
                                              (apply-conj (repeat (- j2 j1) \>))
                                              (apply-conj (repeat (- i1 i2) \^)))
                  (and (not is-nkeypad)
                    (= f \<) 
                    (or (= p \^) (= p \A))) (-> r
                                              (apply-conj (repeat (- i2 i1) \v))
                                              (apply-conj (repeat (- j1 j2) \<)))
                  ; last-top-bottom (-> r
                  ;          (apply-conj (if (> i1 i2) (repeat (- i1 i2) \^) (repeat (- i2 i1) \v)))
                  ;          (apply-conj (if (> j1 j2) (repeat (- j1 j2) \<) (repeat (- j2 j1) \>))))
                  :else (-> r
                           (apply-conj (if (> j1 j2) (repeat (- j1 j2) \<) (repeat (- j2 j1) \>)))
                           (apply-conj (if (> i1 i2) (repeat (- i1 i2) \^) (repeat (- i2 i1) \v)))))]
          (recur (next s) (conj r \A) f))))))


(defn calculate [[multiplier symbols]]
  (let [symbols (keypad>keypad symbols nkeypad)
        symbols (keypad>keypad symbols dkeypad)
        symbols (keypad>keypad symbols dkeypad)]
    (* multiplier (count symbols))))


(calculate [29 (vec "029A")])

(->> test-target
  (mapv parse-target)
  (mapv calculate)
  (apply +))

(->> target
  (mapv parse-target)
  (mapv calculate)
  (apply +))


96396