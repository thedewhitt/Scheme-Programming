;; Erik Gonzalez-DeWhitt
;; ergonzal
;; Dawghouse Section 16779
;; I worked alone.

(import (c211 image))
(import (c211 matrix))

;; energy function from a12.ss
(define (energy img row col)
  (define safe-bright
    (lambda (img row col)
     (if (and (>= row 0) (>= col 0)
              (< row (image-rows img))
              (< col (image-cols img)))
         (brightness (image-ref img row col))
         0)))
  (let ([a (safe-bright img (sub1 row) (sub1 col))]
        [b (safe-bright img (sub1 row) col)]
        [c (safe-bright img (sub1 row) (add1 col))]
        [d (safe-bright img row (sub1 col))]
        [f (safe-bright img row (add1 col))]
        [g (safe-bright img (add1 row) (sub1 col))]
        [h (safe-bright img (add1 row) col)]
        [i (safe-bright img (add1 row) (add1 col))])
    (define (xenergy a d g c f i)
      (- (- (- (+ (+ a (* 2 d)) g) c) (* 2 f)) i))
    (define (yenergy a b c g h i)
      (- (- (- (+ (+ a (* 2 b)) c) g) (* 2 h)) i))
    (sqrt (+ (expt (xenergy a d g c f i) 2) (expt (yenergy a b c g h i) 2)))))

(define (brightness clr)
  (let ([a (color-ref clr 'red)]
        [b (color-ref clr 'green)]
        [c (color-ref clr 'blue)])
    (+ a b c)))

;; image-crop from a12.ss
(define (image-crop img columns)
  (make-image (image-rows img) (if (> columns (image-cols img))
                                   0
                                   (- (image-cols img) columns))
    (lambda (r c) (image-ref img r c))))

;; image-copy from a12.ss
(define (image-copy img)
  (make-image (image-rows img) (image-cols img)
    (lambda (x y) (image-ref img x y))))

;; Problem 1
;; (a) takes no arguments, returnign the chosen representaiton of infinity
(define infinity
  (lambda ()
    'oo))

;; (b) takes an item, x, returning #t if corresponds to our representation of
;;     infinity
(define (infinity? x)
  (if (equal? x (infinity))
      #t
      #f))

;; (c) takes two inputs, returning their sum, unless one or both of the inputs
;;     is infinity, in which case returns infinity
(define (:+ x y)
  (if (or (infinity? x) (infinity? y))
      (infinity)
      (+ x y)))

;; (d) a predicate taking two inputs, returning #t if the first is less than the
;;     second input
(define (:< x y)
  (cond
    [(or (and (infinity? x) (not (infinity? y))) (equal? x y)) #f]
    [(and (not (infinity? x)) (infinity? y))  #t]
    [else (< x y)]))

;; Problem 2
;; takes an image, list of column indices representing a vertical seam, a color,
;; and destructively mutates the image by replacing all pixels along the seam
;; with the specified highlight color
(define img
       (make-image
         200
         300
         (lambda (r c)
           (let ([d (sqrt (+ (* r r) (* c c)))])
             (if (even? (inexact->exact (floor (/ d 20))))
                 yellow
                 black)))))

(define (highlight-seam! img col-index clor)
  (let loop ([row 0])
    (if (not (= row (image-rows img)))
        (begin (image-set! img row (list-ref col-index row) clor)
          (loop (add1 row))))))

;; Problem 3
;; (a) takes an image, returning a matrix of the same dimensions, where each
;;     entry represents the energy of the corresponding pixel in the image
(define (make-energy-matrix img)
  (matrix-generator (image-rows img) (image-cols img)
    (lambda (r c)
      (energy img r c))))

;; (b) takes a procedure of one-argument and a matrix, returning a matrix of
;;     the same dimensions, where each element is the result of applying the
;;     procedure to the given entry
(define (matrix-map proc mtrx)
  (matrix-generator (matrix-rows mtrx) (matrix-cols mtrx)
    (lambda (r c)
      (proc (matrix-ref mtrx r c)))))

;; (c) takes a matrix, returning a seperate copy of the given matrix
(define (matrix-copy mtrx)
  (matrix-map (lambda (x) x) mtrx))

;; (d) takes a matrix, a row index, a column index, and a maximum width n. If
;;     the indices refer to a legal element in leftmost n columns, then the
;;     corresponding element is returned, otherwise return infinity
(define (cropped-matrix-ref mtrx row col n)
  (let ([m (matrix-rows mtrx)])
    (if (and (and (not (negative? row)) (not (negative? col)))
             (and (< row m) (< col n)))
        (matrix-ref mtrx row col)
        (infinity))))

;; Problem 4
;; -takes three costs, returning the best direction for the seam to take, based
;;  on given costs.
;; -a move to the left represented with the number -1, staying in the column 0,
;;  and moving to the right with 1
;; -each costs is either a number or infinity
;; -the best direction correponds to the smallest cost (if two costs are equal
;;  the rightmost one is chosen
(define (best-direction x y z)
  (if (:< x y)
      (if (:< x z)
          -1
          1)
      (if (:< y z)
          0
          1)))

;; Problem 5
;; takes a matrix of costs and a positive integer, representing the virtual
;; width, returnin the column corresponding to the smallest cost
;; - if there is more than one entry with the smallest cost, return the leftmost
;;   one
(define (seam-origin mtrx width)
  (let loop ([min 0] [index 1])
    (cond
      [(equal? index width) min]
      [(or (:< (matrix-ref mtrx 0 min) (matrix-ref mtrx 0 index))
           (equal? (matrix-ref mtrx 0 min) (matrix-ref mtrx 0 index)))
       (loop min (add1 index))]
      [else (loop index (add1 index))])))

;; Problem 6
;; Takes a seam matrix, sm, a starting column, c, and returns the seam
;; corresponding to the given column
(define m1 (matrix-generator 5 5 (lambda (r c) c)))

(define m2
  (matrix-generator 5 5 (lambda (r c) (mod (+ c 1) 5))))

(define m3
  (matrix-generator 5 5 (lambda (r c) (mod (- c 1) 5))))

(define m4
  (matrix-generator
    10
    20
    (lambda (r c) (mod (+ c (random 3) -1) 20))))

(define walk-seam
  (lambda (sm c)
    (let ([num-rows (matrix-rows sm)])
      (let loop ([r 0]
                 [c start])
        (if (= r num-rows)
            '()
            (cons c (loop (add1 r) (matrix-ref sm r c))))))))

;; Problem 7
;; takes a matrix and a positive integer n, returning a minimal cost
;; vertical seam restricted to the first n columns of the matrix
(define (least-energy-seam em n)
  (let ([cm (matrix-copy em)]
     [sm (make-matrix (matrix-rows em) (matrix-cols em))])
 (let ([num-rows (matrix-rows em)]
       [num-cols (matrix-cols em)])
   (let rloop ([r (- num-rows 2)])
     (unless (negative? r)
       (let cloop ([c 0])
         (when (< c n)
           (let ([bd (best-direction
                       (cropped-matrix-ref cm (add1 r) (sub1 c) n)
                       (cropped-matrix-ref cm (add1 r) c n)
                       (cropped-matrix-ref cm (add1 r) (add1 c) n))])
             (matrix-set! cm r c (+ (matrix-ref cm r c)
                                   (matrix-ref cm (add1 r)
                                     (+ c bd))))
             (matrix-set! sm r c (+ c bd))
             (cloop (+ c 1)))))
       (rloop (- r 1))))
   (walk-seam sm (seam-origin cm n)))))

;; Problem 8
(define (image-seam-carve! img seam n)
  (let ([rows (image-rows img)])
 (let rloop ([r 0][seam seam])
   (when (< r rows)
     (let sloop ([s (car seam)])
       (when (< s (- n 1))
         (image-set! img r s (image-ref img r (add1 s)))
         (sloop (add1 s))))
     (rloop (add1 r)(cdr seam))))))

;; Problem 9
(import (c211 image))
(define (image-copy img)
  (make-image (image-rows img) (image-cols img) (lambda (r c)
                                               (image-ref img r c))))

(define (content-aware-resize img-orig pixels)
  (let ([img (image-copy img-orig)])
 (let loop ([n (image-cols img)][p pixels])
   (if (zero? p)
       (image-crop img pixels)
       (begin
         (let ([ge (make-energy-matrix img)])
           (image-seam-carve! img (least-energy-seam ge n) n))
         (loop (sub1 n)(sub1 p)))))))