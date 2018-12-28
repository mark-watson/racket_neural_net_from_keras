#lang racket

;; Copyright 2018 by Mark Watson all rights reserved. Released under Apache 2 license.

(require math racket/trace)
(require csv)
(require racket/pretty)

(define (row-string->numbers x)
  (map string->number x))

(define (array-string-->number m)
  (map row-string->numbers m))

(define (sigmoid x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

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

(display "** weights loaded **")

(let-values ([(nrows ncols) (matrix-shape w2)])
  (display "shape of w2:\n")
  (pretty-print nrows)
  (pretty-print ncols))

(define (drop-last l) (reverse (cdr (reverse l))))

(define (evaluate x y)
  (let* ([layer1 (matrix-relu (matrix* x w1))]
         [layer2 (matrix-relu (matrix* layer1 w2))]
         [outputs (matrix-sigmoid (matrix* layer2 w3))]
         [yhat (first (first (array->list* outputs)))])
    (if (> yhat 0.6) 1 0)))


(define (tests)
  (let ([good 0]
        [bad 0]
        [samples
          (array->list*
           (file->matrix "model_data/test.csv"))])
    (for ([fs samples])
      (let* ([xs (drop-last fs)]
             [y (last fs)]
             [x (list*->matrix (list xs))])
        (if (= (evaluate x y) y)
            (set! good (+ 1 good))
            (set! bad (+ 1 bad)))))
    (display (list "number correct:" good))
    (display (list "number wrong:" bad))
    (display (list "accuracy:" (/ (* 100.0 good) (+ good bad))))))
    
(tests)
