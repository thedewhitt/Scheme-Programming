;; Erik Gonzalez-DeWhitt
;; ergonzal
;; Dawghouse Section 16779
;; I worked alone

(import (c211 image) (c211 tree))

(define www
  '((1 the cat sat on the mat)
    (2 the dog stood on the mat)
    (3 the cat stood while a dog sat)))

(define img (make-image 2 3 black))

(define num-tree
  (tree
    7
    (tree 5 (leaf 3) (leaf 6))
    (tree 9 (leaf 8) (empty-tree))))

;; Problem 1
;; (a) Takes a message, composed of a list of numbers, and returns the same
;;     message with a checksum valued placed at the end. if the checksum is
;;     mulitple digits, only the last digit is presented as the checksum

(define (affix-plain-checksum lst)
  (let ([sum 0])
    (define (affix-helper lst sum)
      (let ([sum (if (> sum 9)
                     (remainder sum 10)
                     sum)])
        (cond
          [(null? lst) (list sum)]
          [else (cons
                  (car lst)
                  (affix-helper (cdr lst) (+ sum (car lst))))])))
    (affix-helper lst sum)))

;; (b) Given a list of digits, where the last digit is a checksum value, returns
;;     a value of #t if the transmitted message and checksum match up, otherwise
;;     returns a value of #f

(define (verify-plain? lst)
  (let ([sum 0])
    (define (verify-plain?-helper lst sum)
      (let ([sum (if (> sum 9)
                     (remainder sum 10)
                     sum)])
        (cond
          [(null? (cdr lst)) (equal? (car lst) sum)]
          [else (verify-plain?-helper (cdr lst) (+ sum (car lst)))])))
    (verify-plain?-helper lst sum)))

;; (c) Returns a list, given a list of numbers, with the same values as well as
;;     the staircase checksum affixed to the end of the list

(define (affix-staircase-checksum lst)
  (let ([sum 0]
        [number 1])
    (define (staircase-helper lst sum number)
      (let ([sum (if (> sum 9)
                     (remainder sum 10)
                     sum)])
        (cond
          [(null? lst) (list sum)]
          [else (cons
                  (car lst)
                  (staircase-helper
                    (cdr lst) (+ sum (* (car lst) number)) (+ number 1)))])))
    (staircase-helper lst sum number)))

;; (d) Given a list of digits, returns a #t value if the staircase checksum
;;     value at the end of the list is correct for the rest of the list,
;;     otherwise returns #f

(define (verify-staircase? lst)
  (let ([sum 0]
        [number 1])
    (define (verify-staircase-helper lst sum number)
      (let ([sum (if (> sum 10)
                     (remainder sum 10)
                     sum)])
        (cond
          [(null? (cdr lst)) (equal? sum (car lst))]
          [else (verify-staircase-helper
                  (cdr lst) (+ sum (* number (car lst))) (+ number 1))])))
    (verify-staircase-helper lst sum number)))

;; Problem 2
;; Takes a list representing web pages, returning a simple index in which words,
;; returned in sublists with the numbers of the web pages they appear on (in
;; increasing order, are arranged alphabetically
(define (build-simple-index llst)
  (let ([words '()]
        [original '()]
        [pages '()])
    (define (find-words llst words)
      (cond
        [(null? llst) words]
        [else (find-words (cdr llst) (append (cdar llst) words))]))
    (define member?
      (lambda (x ls1)
        (cond
          [(null? ls1) #f]
          [(equal? x (car ls1)) #t]
          [else (member? x (cdr ls1))])))
    (define (sort-originals words original)
      (cond
        [(null? words)
         (map string->symbol (sort string<? (map symbol->string original)))]
        [(not (member? (car words) original))
         (sort-originals (cdr words) (cons (car words) original))]
        [else (sort-originals (cdr words) original)]))
    (define (find-pages word llst pages)
      (cond
        [(null? llst) pages]
        [(member? word (car llst))
         (cons (caar llst) (find-pages word (cdr llst) pages))]
        [else (find-pages word (cdr llst) pages)]))
    (map (lambda (x)
           (cons x (sort < (find-pages x llst pages))))
      (sort-originals (find-words llst words) original))))

;; Problem 3
;; (a) Similar to build-simple-index, but returns with the index location of
;;     each word appended to each page on which the word appears
(define (build-better-index llst)
  (let ([words '()]
        [original '()]
        [pages '()])
    (define (find-words llst words)
      (cond
        [(null? llst) words]
        [else (find-words (cdr llst) (append (cdar llst) words))]))
    (define member?
      (lambda (x ls1)
        (cond
          [(null? ls1) #f]
          [(equal? x (car ls1)) #t]
          [else (member? x (cdr ls1))])))
    (define (sort-originals words original)
      (cond
        [(null? words)
         (map string->symbol (sort string<? (map symbol->string original)))]
        [(not (member? (car words) original))
         (sort-originals (cdr words) (cons (car words) original))]
        [else (sort-originals (cdr words) original)]))
    (define (find-place word lst place)
      (cond
        [(null? lst) '()]
        [(equal? word (car lst))
         (cons place (find-place word (cdr lst) (+ place 1)))]
        [else (find-place word (cdr lst) (+ place 1))]))
    (define (find-pages word llst pages)
      (cond
        [(null? llst) pages]
        [(member? word (car llst))
         (append (map (lambda (x)
                        (list (caar llst) x))
                   (find-place word (car llst) 0))
           (find-pages word (cdr llst) pages))]
        [else (find-pages word (cdr llst) pages)]))
    (map (lambda (x)
           (cons x (find-pages x llst pages)))
      (sort-originals (find-words llst words) original))))

;; (b) match phrase query algorithm
(define (match-phrase-query phrase better-index)
  ;; gets the better-index information of specific words
  (define (lookup word index)
    (cond
      [(equal? (caar index) word) (cdar index)]
      [else (lookup word (cdr index))]))
  ;; gets the better-index information of a phrase,
  ;;   in the order of the phrase
  (define (lookup-phrase phrase better-index)
    (map (lambda (x)
           (lookup x better-index))
      phrase))
  ;; Takes a list of lists and a list, with page number and indexes,
  ;; returning the original lst if any item of llst follows, in page number
  ;; and index
  (define (compare lst llst)
    (let ([a (car lst)]
          [b (caar llst)]
          [c (cadr lst)]
          [d (cadar llst)])
      (cond
        [(null? llst) #f]
        [(and (= a b) (= c (sub1 d))) lst]
        [else (if (null? (cdr llst))
                  #f
                  (compare lst (cdr llst)))])))
  ;; maps the compare function on the elements of a llst and a second llst
  (define (compare+ llst1 llst2)
    (map (lambda (x)
           (compare x llst2))
      llst1))
  ;; eliminates false-values from a lst
  (define (false-elim lst)
    (cond
      [(null? lst) '()]
      [(equal? (car lst) #f) (false-elim (cdr lst))]
      [else (cons (car lst) (false-elim (cdr lst)))]))
  (let ([phrase (lookup-phrase phrase better-index)])
    (define (match-helper phrase)
      (cond
        [(null? (cdr phrase)) (car phrase)]
        [else
         (false-elim (compare+ (car phrase) (match-helper (cdr phrase))))]))
    (match-helper phrase)))

;; Problem 4
;; takes a nonnegative integer, n, and returns a list of n random
;; numbers between 0 and 999
(define (random-big-list n)
  (map (lambda (x)
         (random n))
    (iota n)))

;; binding lists created by random-big-list
(define ls1 (random-big-list 100))
(define ls2 (random-big-list 1000))
(define ls3 (random-big-list 5000))
(define ls4 (random-big-list 10000))

;; insertion-sort and related procedures from a9.ss
(define (insertion-sort rel? lst)
  (cond
    [(null? lst) lst]
    [else (insert-in-order rel? (car lst) (insertion-sort rel? (cdr lst)))]))

(define (insert-in-order rel? item lst)
  (cond
    [(null? lst) (list item)]
    [(rel? (car lst) item)
     (cons (car lst) (insert-in-order rel? item (cdr lst)))]
    [else (cons item lst)]))

;; mergesort and related procedures from a9.ss
(define (mergesort rel? lst)
  (let ([seqs (group-sorted-sequences rel? lst)])
    (define (mergesort-help rel? seqs)
      (cond
        [(null? seqs) '()]
        [(null? (cdr seqs)) (car seqs)]
        [else (mergesort-help rel? (merge-adjacent-sequences rel? seqs))]))
    (mergesort-help rel? seqs)))

(define (group-sorted-sequences rel? lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) (list lst)]
    [else (let ([ans (group-sorted-sequences rel? (cdr lst))])
            (cond
              [(rel? (car lst) (caar ans))
               (cons (cons (car lst) (car ans)) (cdr ans))]
              [else (cons (list (car lst)) ans)]))]))

(define (merge-adjacent-sequences rel? lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) lst]
    [else (cons (merge rel? (car lst) (cadr lst))
            (merge-adjacent-sequences rel? (cddr lst)))]))

(define (merge rel? lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [else (merge rel? (cdr lst1) (insert-in-order rel? (car lst1) lst2))]))

;; time-it procedure given in lecture
(define time-it
  (lambda (thunk)
    (let ([start (cpu-time)])
      (let ([value (thunk)])
        (- (cpu-time) start)))))

;; Amount of Time Until Completion of Various Sorting Procedures
;; Number of List                  Procedures
;;   Elements   |    insertion-sort  |  mergesort    |  sort
;;      100     |           1        |       2       |    0
;;     1000     |          70        |      98       |    0
;;     5000     |        1088        |    3199       |    2
;;    10000     |        4206        |   12805       |    3
;; *relational predicate of > used for all procedures
;; *all times reported in cps ms

;; From the given table, it is clear that the predefined sort procedure is the
;; most efficient. Taking several magnitudes less time to sort a list of ten
;; thousand random numbers, the sort procedure must be based off a more
;; efficient algorithm than either insertion-sort or mergesort. The time
;; difference between the other two procedures, when the lists are relatively
;; small, is almost insignificant but dramatically different the lists become
;; larger. This implies that the sorting algorithm used by the insertion-sort
;; procedure is more efficient than the algorithm used by the mergesort
;; procedure.

;; Problem 5
;; Takes a list of non-negative numbers and a target number, returning a value
;; of #t if there is some subset of the elements of the the list that sum to
;; the given target number

(define (subset-sum? lst number)
  (cond
    [(and (null? lst) (zero? number)) #t]
    [(null? lst) #f]
    [else (or (subset-sum? (cdr lst) (- number (car lst)))
              (subset-sum? (cdr lst) number))]))
;; the or statement at the end, assesses two situations, whether the first
;; number in the list is or isn't in the target sublist. if the target number is
;; ever reduced to zero and the list is null, a #t value is returned

;; Problem 6
;; Takes two positive integers representing the position of some square on the
;; board, returning the number of shortest paths between the indicated square
;; and the upper left corner
(define (count-shortest-paths x y)
  (cond
    [(and (equal? 1 x) (equal? y 1)) 0]
    [(or (equal? 1 x) (equal? 1 y)) 1]
    [else (let ([left (count-shortest-paths (- x 1) y)]
                [right (count-shortest-paths x (- y 1))])
            (+ left right))]))

;; Problem 7
;; Takes two equal-size images and returns their distance (number of mismatches
;; between corresponding pixels)
(define (image-distance img1 img2)
  (let ([img1 (image->list img1)]
        [img2 (image->list img2)])
    (let loop ([a img1] [b img2])
      (if (null? a)
          0
          (if (color-equal? (car a) (car b))
              (loop (cdr a) (cdr b))
              (add1 (loop (cdr a) (cdr b))))))))

;; Problem 8
;; Build the pictured trees, defined using the given labels
(define tr0 (tree 'a (leaf 5) (empty-tree)))

(define tr1 (tree 4 (empty-tree) (leaf 'b)))

(define tr2 (tree 'd (leaf 'c) (leaf 'e)))

(define tr3 (tree '+
              (leaf 7)
              (tree '-
                (tree '/
                  (leaf 'x)
                  (leaf 5))
                (leaf 'y))))

(define tr4 (tree 1
              (tree 2
                (empty-tree)
                (tree 3
                  (tree 4
                    (empty-tree)
                    (leaf 5))
                  (empty-tree)))
              (tree 2
                (tree 3
                  (empty-tree)
                  (leaf 4))
                (empty-tree))))

;; Problem 9
;; Takes a tree and returns the height of the tree, the longest chain of nodes
;; from the main base of the tree
(define (height tr)
  (let ([length 0])
    (define (height-helper tr length)
      (cond
        [(empty-tree? tr) length]
        [else (let ([lans (height-helper (left-subtree tr) (+ length 1))]
                    [rans (height-helper (right-subtree tr) (+ length 1))])
                (if (> lans rans)
                    lans
                    rans))]))
    (height-helper tr length)))

;; Problem 10
;; Takes an item and binary tree, searching for if the item is on any leaf
;; in the tree, ignoring any non-leaf root-value
(define (leaf-member? item tr)
  (cond
    [(empty-tree? tr) #f]
    [(and (leaf? tr) (equal? (root-value tr) item)) #t]
    [else (or (leaf-member? item (left-subtree tr))
              (leaf-member? item (right-subtree tr)))]))

;; Problem 11
;; Takes a tree, returning a list of values indicating a path along the tree,
;; terminating at a leaf
(define (random-walk tr)
  (let ([path '()])
    (define (random-walk-helper tr path)
      (cond
        [(empty-tree? tr) path]
        [(leaf? tr) (cons (root-value tr) path)]
        [(empty-tree? (left-subtree tr))
         (cons (root-value tr) (random-walk-helper (right-subtree tr) path))]
        [(empty-tree? (right-subtree tr))
         (cons (root-value tr) (random-walk-helper (left-subtree tr) path))]
        [else (if (zero? (random 2))
                  (cons (root-value tr)
                    (random-walk-helper (right-subtree tr) path))
                  (cons (root-value tr)
                    (random-walk-helper (left-subtree tr) path)))]))
    (random-walk-helper tr path)))

;; Problem 12
;; Takes two trees, returning a value of #t if the two trees share the same
;; shape
(define (same-shape? tr1 tr2)
  (cond
    [(not (equal? (empty-tree? tr1) (empty-tree? tr2))) #f]
    [(and (empty-tree? tr1) (empty-tree? tr2)) #t]
    [else (let ([lefttr1 (left-subtree tr1)]
                [lefttr2 (left-subtree tr2)])
            (if (same-shape? lefttr1 lefttr2)
                (let ([righttr1 (right-subtree tr1)]
                      [righttr2 (right-subtree tr2)])
                  (if (same-shape? righttr1 righttr2)
                      #t
                      #f))
                #f))]))


;; Problem 13
;; (a) Takes a predicate and list, returning two lists. The first of which is
;;     composed of elements of the original list that satisfy the predicate and
;;     the second composed of those elements that do not
(define (divide pred? lst)
  (let ([winners '()]
        [losers '()])
    (define (divide-helper pred? lst winners losers)
      (cond
        [(null? lst) (cons winners (list losers))]
        [(pred? (car lst))
         (divide-helper pred? (cdr lst) (cons (car lst) winners) losers)]
        [else (divide-helper pred? (cdr lst) winners (cons (car lst) losers))]))
    (divide-helper pred? lst winners losers)))

;; (b) Takes two lists and an item, returning a list composed of the elements
;;     of list one, followed by the item, and the elements of list two
(define (join lst1 item lst2)
  (cond
    [(null? lst1) (cons item lst2)]
    [else (cons (car lst1) (join (cdr lst1) item lst2))]))

;; (c) Takes a binary relationship and a list, returning the result of sorting
;;     the given lists by the given relationship
(define (quicksort rel? lst)
  (cond
    [(null? lst) lst]
    [(null? (cdr lst)) lst]
    [else (let ([left
                  (car (divide (lambda (x) (rel? x (car lst))) (cdr lst)))]
                [right
                  (cadr (divide (lambda (x) (rel? x (car lst))) (cdr lst)))])
            (join (quicksort rel? left) (car lst) (quicksort rel? right)))]))