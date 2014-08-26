;; Erik Gonzalez-DeWhitt
;; ergonzal
;; Dawghouse Section 16779
;; I worked alone, with help from the messages.

(import (c211 tree))
(import (c211 image))
(define butterfly
  (read-image
    "/Users/erikgonzalez-dewhitt/Documents/IU/Junior/CSCI - C211/butterfly.jpg"
    ))

(define strip
       (list
         (make-image 50 75)
         (make-image 50 50 yellow)
         (make-image 100 100 green)
         (make-image 50 200 cyan)))

;; Problem 1
;; (a)
(define (monster-battle heads weapons)
  (define (monster-battle-helper head weapons)
    (cond
      [(zero? heads) '()]
      [(or (negative? heads) (null? weapons)) #f]
      [else (let ([yes  (cons (car weapons)
                          (monster-battle
                            (- heads (car weapons)) (cdr weapons)))]
                  [no (monster-battle heads (cdr weapons))])
              (cond
                [(and (equal? (cdr yes) #f) (equal? no #f)) #f]
                [(equal? (cdr yes) #f) no]
                [(equal? no #f) yes]
                [else (shorter yes no)]))]))
  (if (negative? heads)
      '()
      (monster-battle-helper head weapons)))

;; (b)
(define (tree-ref tr direct)
  (cond
    [(null? direct) (root-value tr)]
    [(equal? (car direct) 'left) (tree-ref (left-subtree tr) (cdr direct))]
    [else (tree-ref (right-subtree tr) (cdr direct))]))


;; (c) shorter
(define (shorter lst1 lst2)
  (define (shorter-helper lst1 lst2)
    (cond
      [(null? lst1) #t]
      [(null? lst2) #f]
      [else (shorter-helper (cdr lst1) (cdr lst2))]))
  (if (shorter-helper lst1 lst2)
      lst1
      lst2))


;; Problem 2
;; (a) Takes a vector, returning the middle element or the middle two, depending
;;     on if there is an odd number of elements or an even number, respectively
(define (middle vec)
  (let ([lngth (vector-length vec)])
    (cond
      [(zero? lngth) '()]
      [(even? lngth) (list (vector-ref vec (sub1 (quotient lngth 2)))
                           (vector-ref vec (quotient lngth 2)))]
      [else (list (vector-ref vec (quotient lngth 2)))])))

;; (b) Takes a vector and an index, returning a list of the elements immediately
;;     to the left and the right of the index element in the list
(define (neighbors vec index)
  (let ([lngth (vector-length vec)])
    (define (neighbors-help vec index)
      (cond
        [(= lngth 1) '()]
        [(zero? index) (list (vector-ref vec (add1 index)))]
        [(= index (sub1 lngth)) (list (vector-ref vec (sub1 index)))]
        [else (list (vector-ref vec (sub1 index))
                    (vector-ref vec (add1 index)))]))
    (neighbors-help vec index)))

;; Problem 3
;; Takes a string and a character, returning the number of occurences of the
;; character in the string
(define (char-count str char)
  (let ([index 0]
        [count 0]
        [end (string-length str)])
    (define (char-count-help str char index count)
      (cond
        [(= index end) count]
        [(char=? (string-ref str index) char)
         (char-count-help str char (add1 index) (add1 count))]
        [else (char-count-help str char (add1 index) count)]))
    (cond
      [(zero? end) 0]
      [else (let ([end (sub1 end)])
              (char-count-help str char index count))])))

;; Problem 4
;; Takes a vector, v1, a procedure taking two arguments, and a vector v2,
;; destructively updating v1 to contain the result of applying proc to the
;; corresponding elements of v1 and v2.
;; Mutates v1, does not return a new vector
(define (vector-combine! vec1 proc vec2)
  (let ([n (vector-length vec1)])
    (let loop ([i 0])
      (if (< i n)
          (begin
            (vector-set! vec1 i
              (proc (vector-ref vec1 i) (vector-ref vec2 i)))
            (loop (add1 i)))))))

;; Problem 5
;; (a) takes an image and returns a new image which is an exact copy of the
;;     given image
(define (image-copy img)
  (make-image (image-rows img) (image-cols img)
    (lambda (x y) (image-ref img x y))))

;; (b)
(define (image-transpose img)
  (make-image
    (image-cols img) (image-rows img) (lambda (r c)
                                        (image-ref img c r))))

;; (c)
(define (image-crop img columns)
  (make-image (image-rows img) (if (> columns (image-cols img))
                                   0
                                   (- (image-cols img) columns))
    (lambda (r c) (image-ref img r c))))

;; Problem 6
;; (a) Takes a non-empty list of images, returning the image formed by
;;     concatenating the given images from left to right.
(define (left-to-right lst)
  (cond
    [(null? (cdr lst)) (car lst)]
    [else (let ([first (car lst)]
                [rest (left-to-right (cdr lst))])
            (make-image
              (if (< (image-rows first) (image-rows rest))
                  (image-rows first)
                  (image-rows rest))
              (+ (image-cols first) (image-cols rest))
              (lambda (r c)
                (if (< c (image-cols first))
                    (image-ref first r c)
                    (image-ref rest r (- c (image-cols first)))))))]))

;; (b)
(define (top-to-bottom images)
  (define (top-to-bottom-helper img1 img2)
    (let ([rows1 (image-rows img1)]
          [rows2 (image-rows img2)])
    (make-image (+ rows1 rows2) (min (image-cols img1) (image-cols img2))
      (lambda (r c) (if (> r (sub1 rows1))
                       (image-ref img2 (- r rows1) c)
                       (image-ref img1 r c))))))
  (cond
    [(null? (cdr images)) (car images)]
    [else
     (top-to-bottom
       (cons (top-to-bottom-helper (car images) (cadr images))
         (cddr images)))]))

;; Problem 7

;; (a) takes a color and returns the sum of the three color channels
(define (brightness clr)
  (let ([a (color-ref clr 'red)]
        [b (color-ref clr 'green)]
        [c (color-ref clr 'blue)])
    (+ a b c)))

;; (b) takes an image and two integers, representing the row and column indices
;;     of some pixel on the image, returning the energy of the indicated energy
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

;; Problem 8
;; Takes a non-empty vector and an index, i, destructively mutating the vector
;; by carving out the ith element. All elements to the right of i are shifted
;; one space to the left and a zero placed at the right most element
(define (vector-carve! vec1 i)
  (let ([n (vector-length vec1)])
    (let ([end (sub1 n)])
      (let loop ([j 0])
        (if (< j i)
            (begin
              (vector-set! vec1 j (vector-ref vec1 j)) (loop (add1 j)))
            (if (< j end)
                (begin
                  (vector-set! vec1 j (vector-ref vec1 (add1 j)))
                  (loop (add1 j)))
                (vector-set! vec1 j 0)))))))

;; Problem 9
;; Takes an image and a column index, returning the sum of all the energies
;; in the specified column
(define (column-energy img col)
  (let ([n (image-rows img)])
    (let loop ([i 0])
      (if (< i n)
          (begin (+ (energy img i col) (loop (add1 i))))
          0))))

;; Problem 10
;; Takes a non-empty image, computing the column with minimal energy and
;; returning a two-element vector containing the column index and energy of the
;; column. In the event there are two minimal columns, choose the leftmost one
(define (least-energy-column img)
  (let ([n (image-cols img)])
    (let loop ([i 0] [j 1])
      (let ([col-i (column-energy img i)] [col-j (column-energy img j)])
        (if (< j n)
            (if (<= col-i col-j)
                (loop i (add1 j))
                (loop j (add1 j)))
            (if (> col-j col-i)
                (vector j col-j)
                (vector i col-i)))))))