#lang racket

(require "util.rkt")
(require math/matrix)

; Sample math from given input

(define S0 (matrix-transpose (matrix [[-618 -824 -621 1]
                                      [-537 -823 -458 1]
                                      [-447 -329 318 1]
                                      [404 -588 -901 1]])))
(define S1 (matrix-transpose (matrix [[686 422 578 1]
                                      [605 423 415 1]
                                      [515 917 -361 1]
                                      [-336 658 858 1]])))

(define T (matrix* S0 (matrix-inverse S1)))


; Find 4 points that are the same distance from each other in both S0 and S1
; Do the matrix math find T
; Use T move all the points in S1 to S0
; Repeat



; find-relative-distances : [List (list x y z)] -> [HashMap  ((list x y z), (list x y z)) -> d]
(define (find-relative-distances lop)
  (for*/fold ([distances (hash)])
             ([from lop]
              [to (member from lop)])
    (if (equal? from to)
        distances
        (hash-set distances (set from to) (distance from to)))))

(define (distance from to)
  (sqrt (foldr + 0 (map sqr (map - from to))))) ; remove sqrt if equality issues arise

; matching-points : [List (list x y z)] [List (list x y z)]  -> [List (list (list x y z) (list x y z))]?
(define (matching-points l1 l2)
  #;(define l1-dist (find-relative-distances l1))
  #;(define l2-dist (find-relative-distances l2))
  ; acc :  [HashMap (list x y z) -> (list x y z)] [Set (list (list x y z) (list x y z))] ->  [HashMap (list x y z) -> (list x y z)]?
  (define (acc mapping unused)
    (if (= 0 (set-count unused)) mapping
        (for/or ([pair unused])
          (let ([new-mapping (hash-set mapping (first pair) (second pair))])
            (if (valid? new-mapping)
                (acc new-mapping (set-remove unused pair))
                #f)))))
  (acc (hash) (list->set (all-pairs l1 l2))))

; valid?  : [HashMap (list x y z) -> (list x y z)] [HashMap  ((list x y z), (list x y z)) -> d] ^2 -> Boolean
(define (valid? mapping)
  (and (let* ([values (hash-values mapping)]
              [value-set (list->set values)])
         (= (length values) (set-count value-set))) ; Mapping is bijective
       (= (apply + (hash-values (find-relative-distances (hash-keys mapping))))
          (apply + (hash-values (find-relative-distances (hash-values mapping)))))))

; all-pairs [List X] [List Y] -> [List (list X Y)]
(define (all-pairs lox loy)
  (for*/list ([x lox]
              [y loy])
    (list x y)))

; parse-lop : -> [List (list X Y Z)]
(define (parse-lop)
  (read-lines (λ [line] (map string->number (string-split line ",")))))