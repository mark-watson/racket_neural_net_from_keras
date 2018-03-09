#lang racket

(require math racket/trace)
(require csv)
(require racket/pretty)

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

(define (matrix-add m n)
  (matrix-map + m n))

(define (file->matrix fname)
  (let* ((x (read-csv fname))
         (y (array-string-->number x)))
    (list*->matrix y)))
        
(define w1 (file->matrix "model_data/w1.csv"))
(define w2 (file->matrix "model_data/w2.csv"))
(define w3 (file->matrix "model_data/w3.csv"))

(display "** weights and biases loaded **")

;;(print "\nb2:\n")

;;(pretty-print b2)


;(define m2 (matrix* A B))  ;; dot product 2x2

;(display "\nm2:\n")
;(display m2)

(let-values ([(nrows ncols) (matrix-shape w2)])
  (display "shape of w2:\n")
  (pretty-print nrows)
  (pretty-print ncols))

(define (drop-last l) (reverse (cdr (reverse l))))

(define (evaluateXXX x y)
  (let* ([layer1 (matrix-relu (matrix* x w1))]
         [layer2 (matrix-relu (matrix* layer1 w2))]
         [outputs (matrix-sigmoid (matrix* layer2 w3))])
    (pretty-print outputs)
    (pretty-print y)))

(define (evaluate x y)
  (let* ([layer1 (matrix-relu (matrix* x w1))]
         [layer2 (matrix-relu (matrix* layer1 w2))]
         [outputs (matrix-sigmoid (matrix* layer2 w3))])
    (pretty-print outputs)
    (pretty-print y)))


(define (tests)
  (let ([samples
          (array->list*
           (file->matrix "model_data/testing.csv"))])
    (pretty-print samples)
    (for ([fs samples])
      ;;(pretty-print fs)
      ;;(pretty-print (list (drop-last fs)))
      ;;(pretty-print (list*->matrix (list (drop-last fs))))
      (let* ([xs (drop-last fs)]
             [y (last fs)]
             [x (list*->matrix (list xs))])
        (evaluate x y)))))
    
(tests)