(ns  sicp.chapter1)

;1.1
10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(def a 3)

(def b (+ a 1))

(+ a b (* a b))

(if (and (> b a) (< b (* a b)))
  b
  a)

(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)

(+ 2 (if (> b a) b a))

(* (cond (> a b) a
         (< a b) b
         :else -1)
   (+ a 1))


;1.2
(def value (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))))

(assert (= (/ -37 150) value))

;1.3
(defn sum-of-squares [a b c] 
  (cond (and (> a c) (> b c)) (+ (* a a) (* b b))
        (and (> a b) (> c b)) (+ (* a a) (* c c))
        :else (+ (* b b) (* c c))))

(assert (= 13 (sum-of-squares 1 2 3)))
(assert (= 13 (sum-of-squares 3 2 1)))
(assert (= 13 (sum-of-squares 3 1 2)))

;1.4
(defn a-plus-abs-b [a b] 
  ((if (> b 0) + -) a b))

(assert (= 16 (a-plus-abs-b 6 -10)))

;1.5
;skipped

;1.6
;skipped