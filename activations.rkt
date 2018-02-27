#lang racket

(require math racket/trace)
(require csv)

(define (XXrow-string->numbers x)
  (list->array (map string->number x)))

(define (XXarray-string-->number m)
  (list->array (map row-string->numbers m)))

(define (row-string->numbers x)
  (map string->number x))

(define (array-string-->number m)
  (map row-string->numbers m))

(define (sigmoid x)
  (/ (+ 1.0 (exp (- x)))))

(define (matrix-sigmoid m)
  (matrix-map sigmoid m))

(define (relu x)
  (if (< x 0)
      0
      x))

(define (matrix-relu m)
  (matrix-map relu m))

(define (matrix-mul m n)
  (matrix-map * m n))


(define d (read-csv "model_data/tiny.csv"))

(define d2 (array-string-->number d))

(print d2)

(define mm (list*->matrix d2))

(print "\nmm:\n")

(print mm)

(define m2 (matrix-mul mm mm))

(print "\nm2:\n")
(print m2)

;;(define d2-m (matrix d2))

;;(print d2-m)
