(ns matrix1
  "Taken from Dragan Djuric article on Linear Algebra.
  https://dragan.rocks/articles/17/Clojure-Linear-Algebra-Refresher-Vector-Spaces
  Exercises are from the textbook, LINEAR ALGEBRA WITH APPLICATIONS, 7th edition"
  (:require [uncomplicate.neanderthal
             [native :refer :all]
             [core :refer :all]]))

(def v1 (dv -1 2 5.2 0))
(def v2 (dv (range 22)))
(def v3 (dv -2 -3 1 0))
(def v4 (scal -1 v1))

(xpy v1 v3)

(scal 2.5 v1)

(axpy! 2.5 v1 v3)

;; Linear Combination
;;  au+bv+cw.  (+ (* 2 u) (* -3 v) (* 1 w))
(let [u (dv 2 5 -3)
      v (dv -4 1 9)
      w (dv 4 0 2)]
  (prn u v w)
  (axpy 2 u -3 v 1 w))

;; Dot Product
;; Dot Product is (+ (* u1 v1) (* u2 v2) (* u3 v3))
(let [u (dv 2 5 -3)
      v (dv -4 1 9)]
  (prn u v)
  (dot u v))

;; Norm
;; Norm is sqrt of (+ (u1 exp 2) (u2 exp 2) (u3 exp 3))
;; OR sqrt of FIRST ELEMENT SQUARED + 2ND ELEMENT SQUARED ... etc
;; Same as (Math/sqrt 35)
(let [u (dv 1 3 5)]
  (nrm2 u))

;; Angles between vectors
;; Divide the dot product of vectors by the norm of each
(let [u (dv 1 0 0)
      v (dv 1 0 1)]
  (/ (dot u v) (nrm2 u) (nrm2 v)))

;; Distance between points
;; Norm of the difference of linear combinations
(let [x (dv 4 0 -3 5)
      y (dv 1 -2 3 0)]
  (nrm2 (axpy -1 y x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Excercises
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4.1 Vector Space Rn
(def addition-and-scalar-multiplication-5
  {:a (scal 3        (dv 1 4))
   :b (scal -2       (dv -1 3))
   :c (scal (/ 1 2)  (dv 2 6))
   :d (scal (/ -1 2) (dv  2 4 2))
   :e (scal 3        (dv -1 2 3))
   :f (scal 4        (dv -1 2 3 -2))
   :g (scal -5       (dv 1 -4.3 2.5))
   :h (scal 3        (dv 3 0 4.2 -1))})

(def addition-and-scalar-multiplication-6
  (let [u (dv 1 2)
        v (dv 4 -1)
        w (dv -3 5)]
    {:a (axpy u v)
     :b (axpy 1 u 3 v)
     :c (axpy v u)
     :d (axpy 2 u 3 v -1 w)
     :e (axpy -3 u 4 v -2 w)}))

(def addition-and-scalar-multiplication-7
k  (let [u (dv 2 1 3)
        v (dv -1 3 2)
        w (dv 2 4 -2)]
    {:a (axpy u w)
     :b (axpy 2 u 1 v)
     :c (axpy 1 u 3 w)
     :d (axpy 5 u -2 v 6 w)
     :e (axpy 2 u -3 v -4 w)}))

(def addition-and-scalar-multiplication-8
  "Neanderthal represents both column and row vectors in the same way")

(def addition-and-scalar-multiplication-10
  {:a "Associative Propery"
   :b "Propery of the Negative Vector"
   :c "Distributive Propery"
   :d "Scalar multiplication by 1"})

;; 4.2 Dot Product, Norm, Angle, Distance
(def dot-product-1
  {:a (dot (dv 2 1)
           (dv 3 4))
   :b (dot (dv 1 -4)
           (dv 3 0))
   :c (dot (dv 2 0)
           (dv 0 -1))
   :d (dot (dv 5 -2)
           (dv -3 -4))})

(def dot-product-3
  (let [dot* (fn [a b]
               (dot (eval (cons dv a)) (eval (cons dv b))))]
    {:a (dot* [5 1] [2 -3])
     :b (dot* [-3 1 5] [2 0 4])
     :c (dot* [7 1 2 -4] [3 0 -1 5])
     :d (dot* [2 3 -4 1 6] [-3 1 -4 5 -1])
     :e (dot* [1 2 3 0 0 0] [0 0 0 -2 -4 9])}))

(def dot-product-4
  "Neanderthal represents both column and row vectors in the same way")

(def norms-5
  {:a (nrm2 (dv 1 2))
   :b (nrm2 (dv 3 -4))
   :c (nrm2 (dv 4 0))
   :d (nrm2 (dv -3 1))
   :e (nrm2 (dv 0 27))})

(def norms-7
  {:a (nrm2 (dv 5 2))
   :e (nrm2 (dv -3 0 1 4 2))
   :f (nrm2 (dv 0 0 0 7 0 0))})

(def normalize-9
  {:a (scal (/ (nrm2 (dv 1 3)))    (dv 1 3))
   :b (scal (/ (nrm2 (dv 2 -4)))   (dv 2 -4))
   :c (scal (/ (nrm2 (dv 1 2 3)))  (dv 1 2 3))
   :d (scal (/ (nrm2 (dv -2 4 0))) (dv -2 4 0))
   :e (scal (/ (nrm2 (dv 0 5 0)))  (dv 0 5 0))})

(def angles-between-vectors-13
  (let [ang (fn [a b]
              (let [u (eval (cons dv a))
                    v (eval (cons dv b))]
                (/ (dot u v) (nrm2 u) (nrm2 v))))]
    {:a (ang [4 -1] [2 3])
     :b (ang [3 -1 2] [4 1 1])
     :c (ang [2 -1 0] [5 3 1])
     :d (ang [7 1 0 0] [3 2 1 0])
     :e (ang [1 2 -1 3 1] [2 0 1 0 4])}))

(def orthagonality-16
  (let [orth (fn [a b]
               (dot (eval (cons dv a))
                    (eval (cons dv b))))]
    {:a (orth [3 -5] [5 3])
     :b (orth [1 2 -3] [4 1 2])
     :c (orth [7 1 0] [2 -14 3])
     :d (orth [5 1 0 2] [-3 7 9 4])
     :e (orth [1 -1 2 -5 9] [4 7 4 1 0])}))

(def distances-between-points-22
  (let [dist (fn [a b]
               (let [x (eval (cons dv a))
                     y (eval (cons dv b))]
                 (nrm2 (axpy -1 y x))))]
    {:a (dist [4 1] [2 -3])
     :b (dist [1 2 3] [2 1 0])
     :c (dist [-3 1 2] [4 -1 1])
     :d (dist [5 1 0 0] [2 0 1 3])
     :e (dist [-3 1 1 0 2] [2 1 4 1 -1])}))

;; 4.3 General Vector Spaces
