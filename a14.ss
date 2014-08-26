;; Erik DeWhitt
;; ergonzal
;; Dawghouse Section 167779
;; I worked alone

(import (c211 image))
(import (c211 color))

(define blackmail-list '((al bonnie clyde)
                         (bonnie clyde)
                         (clyde tony floyd michael walt)
                         (walt saul jesse)
                         (eliot al don)
                         (floyd don jack sonny)))

;; Problem 1
;; (a)
(define (simple-can-reach? name1 name2 lst)
  (define (get-name contact)
    (car contact))
  (define (lookup-contact name lst)
    (cond
      [(null? lst) #f]
      [(equal? (get-name (car lst)) name) (car lst)]
      [else (lookup-contact name (cdr lst))]))
  (cond
    [(null? lst) #f]
    [(equal? #f (or (lookup-contact name1 lst) (lookup-contact name2 lst))) #f]
    [else #t]))

;; (b)
(define can-reach?
  (lambda (a b blist)
    (if (null? blist)
        #f
        (let loop ([n 0])
          (if (= n(length (lookup a blist)))
              #f
              (let ([le (length (lookup a blist))]
                    [lr (list-ref (lookup a blist) n)])
                (if (equal? lr b)
                    #t
                    (if (member? b (lookup lr blist))
                        #t
                        (if (null? (lookup lr blist))
                            (loop (add1 n))
                            (can-reach? lr b blist))))))))))


;; Problem 2
;; (a)
(define (image->string img)
  (let ([n (inexact->exact (log (image-rows img) 2))])
    (let loop ([r 0] [c 0] [n n])
      (if (solid-region? img r c n)
          (if (color-equal? (image-ref img r c) black)
              "10"
              "11")
          (let ([half (div (image-rows img) 2)])
            (string-append "0"
              (image->string
                (make-image half half (lambda (r c) (image-ref img r c))))
              (image->string (make-image half half
                               (lambda (r c) (image-ref img r (+ half c)))))
              (image->string (make-image half half
                               (lambda (r c) (image-ref img (+ half r) c))))
              (image->string (make-image half half
                               (lambda (r c)
                                 (image-ref img (+ half r) (+ half c)))))))))))

;; (b)
(define (string->image str n)
  (define (find-index str start-i n)
    (let loop ([i start-i] [n n])
      (cond
        [(= n 0) i]
        [(equal? #\1 (string-ref str i)) (loop (+ 2 i) (sub1 n))]
        [else (loop (find-index str (+ 1 i) 4) (sub1 n))])))
  (let ([a (expt 2 n)])
    (cond
      [(equal? (substring str 0 2) "10") (make-image a a)]
      [(equal? (substring str 0 2) "11") (make-image a a white)]
      [else (let* ([lngth (string-length str)]
                   [img1 (string->image (substring str 1 lngth) (sub1 n))]
                   [img2 (string->image
                           (substring str (find-index str 1 1) lngth) (sub1 n))]
                   [img3 (string->image
                           (substring str (find-index str 1 2) lngth) (sub1 n))]
                   [img4 (string->image
                           (substring str
                             (find-index str 1 3) lngth) (sub1 n))])
              (let ([half (div a 2)])
                (make-image a a
                  (lambda (r c)
                    (cond
                      [(and (< r half) (< c half))
                       (image-ref img1 r c)]
                      [(and (< r half) (>= c half))
                       (image-ref img2 r (- c half))]
                      [(and (>= r half) (< c half))
                       (image-ref img3 (- r half) c)]
                      [else (image-ref img4 (- r half) (- c half))])))))])))

;; Problem 3
;; (a) takes two colors, returning the result of blending in equal parts
(define (color-mix color1 color2)
  (define (average2 num1 num2)
    (quotient (+ num1 num2) 2))
  (color
    (average2 (color-ref color1 'red) (color-ref color2 'red))
    (average2 (color-ref color1 'green) (color-ref color2 'green))
    (average2 (color-ref color1 'blue) (color-ref color2 'blue))))

;; (b)


;; Problem 4
;; (a)
(define glow!
  (lambda (imf thres col)
    (make-image (image-rows img)(image-cols img)
      (lambda (r c)
        (let ([i (image-ref img r c)])
          (if (> (color-ref i 0) thres)
              col
              i))))))

;; (b)
(define (read-brain n path)
  (define (file count path)
    (if (<= count 9)
        (read-image (string-append path "/z" (number->string 0)
                      (number->string count) ".jpg"))
        (read-image (string-append path "/z" (number->string count) ".jpg"))))
  (let ([v (make-vector n 0)])
    (let loop ([count 0])
      (if (< count (- n 1))
          (begin
            (vector-set! v count (file count path))
            (loop (add1 count)))
          v))))

;; (c)
(define (composite vec)
  (let ([rows (image-rows (vector-ref vec 1))]
        [cols (image-cols (vector-ref vec 1))])
    (let loop ([new-img (make-image rows cols)]
               [index 0])
      (cond
        [(equal? index 21) new-img]
        [else (let ([img (vector-ref vec index)])
                (loop (make-image rows cols
                        (lambda (r c) (cond

                                   [(> (color-ref (image-ref img r c) 0) 50)
                                    (image-ref img r c)]

                                   [else (image-ref new-img r c)])))
                  (add1 index)))]))))

;; (d)

;; Problem 5
(define find-matching-paren
  (lambda (expr i)
    (if (char=? #\)(string-ref expr i))
        (let loop ([i (sub1 i)][skip 0])
          (cond
            [(and (zero? skip)(char=?(string-ref expr i)#\()) i]
            [(char=?(string-ref expr i)#\))(loop(sub1 i)(add1 skip))]
            [(char=?(string-ref expr i)#\))(loop(sub1 i)(sub1 skip))]
            [else (loop (sub1 i) skip)]))
        #f)))

;; Problem 6
(define (longest-path tr)
  (define height
    (lambda (tr)
      (cond
        [(empty-tree? tr) 0]
        [else (let ([left (add1 (height (left-subtree tr)))]
                    [right (add1 (height (right-subtree tr)))])
                (max left right))])))
  (define path-helper
    (lambda (tr)
      (let([rootval (root-value tr)])
        (cond
          [(empty-tree? tr)'()]
          [(and (empty-tree? (left-subtree tr)) (empty-tree?
                                                  (right-subtree tr))) '()]
          [(empty-tree? (left-subtree tr))
           (cons (root-value (right-subtree tr))'())]
          [(empty-tree? (right-subtree tr))
           (cons (root-value (left-subtree tr))'())]
          [else (let ([left (root-value (left-subtree tr))]
                      [right (root-value (right-subtree tr))])
                  (if (>=
                        (height (left-subtree tr)) (height (right-subtree tr)))
                      (cons left (path-helper (left-subtree tr)))
                      (cons right (path-helper (right-subtree tr)))))]))))
  (if (empty-tree? tr)
      '()
      (append(list(root-value tr))(path-helper tr))))

;; Problem 7
(define seam-cost
  (lambda (em c ls)
    (let loop ([r 0][c c][ls ls][add 0])
      (let ([ref (matrix-ref em r c)])
        (cond
          [(null? ls)(+ (matrix-ref em r c) add)]
          [(= (car ls)- 1)(loop (+ r 1)(- c 1)(cdr ls)(+ ref add))]
          [(= (car ls) 0)(loop (+ r 1) c (cdr ls)(+ ref add))]
          [else (loop (+ r 1)(+ c 1)(cdr ls)(+ ref add))])))))

;; Problem 8
;; (a)
(define cookies-in-column
  (lambda (grid c)
    (let ([rows (matrix-rows grid)])
      (let loop ([r 0][cookies 0])
        (if (< r rows)
            (if (equal? (matrix-ref grid r c) 'w)
                (loop (add1 r) cookies)
                (loop (add1 r)(+ (matrix-ref grid r c) cookies)))
            cookies)))))

;; (b)
(define (cookie-monster grid)
  (define (cookie-counter grid r c)
    (let ([rows (sub1 (matrix-rows grid))]
          [cols (sub1 (matrix-cols grid))]
          [cookies (matrix-ref grid r c)])
      (cond
        [(and (= r rows) (= c cols))
         (matrix-ref grid r c)]
        [(equal? cookies 'w) 0]
        [(= r rows) (+ cookies (cookie-counter grid r (+ c 1)))]
        [(= c cols) (+ cookies (cookie-counter grid (+ r 1) c))]
        [else (let ([right (cookie-counter grid r (+ c 1))]
                    [down (cookie-counter grid (+ r 1) c)])
                (+ cookies (max right down)))])))
  (cookie-counter grid 0 0))

;; (c)
(define (cookie-monster-path cgrid)
  (define (rid-number ls)

    (map (lambda (x) (if (number? x) 'right x)) ls))
  (define (count cgrid r c counter)
    (let ([rows (sub1 (matrix-rows cgrid))]
          [cols (sub1 (matrix-cols cgrid))]
          [ans (matrix-ref cgrid r c)])
      (cond
        [(and (= r rows) (= c cols)) (list counter)]
        [(equal? ans 'w) (list -1)]
        [(= r rows) (cons 'right (count cgrid r (+ c 1) (+ ans counter)))]
        [(= c cols) (cons 'down (count cgrid (+ r 1) c (+ ans counter)))]
        [else (let ([right (count cgrid r (+ c 1) (+ ans counter))]
                    [down (count cgrid (+ r 1) c (+ ans counter))])
               (if (< (list-ref right (- (length right) 1))
                     (list-ref down (- (length down) 1)))
                   (cons 'down down)
                   (cons 'right right)))])))
 (list (rid-number (count cgrid 0 0 0))))