#lang racket

(require math racket/trace)
(require csv)
(require racket/pretty)

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

(define (file->matrix fname)
  (let* ((x (read-csv fname))
         (y (array-string-->number x)))
    (list*->matrix y)))
        
(define w1 (file->matrix "model_data/w1.csv"))
(define w2 (file->matrix "model_data/w2.csv"))
(define w3 (file->matrix "model_data/w3.csv"))
(define b1 (file->matrix "model_data/b1.csv"))
(define b2 (file->matrix "model_data/b2.csv"))
(define b3 (file->matrix "model_data/b3.csv"))

(display "** weights and biases loaded **")

(print "\nb2:\n")

(pretty-print b2)

(define m2 (matrix* w2 w2))  ;; dot product

(display "\nm2:\n")
(display m2)


(let-values ([(nrows ncols) (matrix-shape w2)])
  (display "shape of w2:\n")
  (pretty-print nrows)
  (pretty-print ncols))


(let-values ([(nrows ncols) (matrix-shape b1)])
  (display "shape of b1:\n")
  (pretty-print nrows)
  (pretty-print ncols))

;;(define r1 (matrix-sigmoid m2))
;;(display "\nr1:\n")
;;(pretty-print r1)

;;(define d2-m (matrix d2))

;;(print d2-m)
