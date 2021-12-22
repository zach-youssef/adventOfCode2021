#lang racket

(require "util.rkt")
(require math/matrix)
(require math/array)

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

; Or - lmao
; Apply rotations and translations like before cause fuck dis shit
(define all-rotations (list (matrix [[1 0 0] [0 1 0] [0 0 1]])
                            (matrix [[-1 0 0] [0 -1 0] [0 0 1]])
                            (matrix [[-1 0 0] [0 1 0] [0 0 -1]])
                            (matrix [[1 0 0] [0 -1 0]  [0 0 -1]])
                            
                            (matrix [[-1 0 0] [0 0 1] [0 1 0]])
                            (matrix [[1 0 0] [0 0 -1] [0 1 0]])
                            (matrix [[1 0 0] [0 0 1] [0 -1 0]])
                            (matrix [[-1 0 0] [0 0 -1] [0 -1 0]])
                            
                            (matrix [[0 -1 0] [1 0 0] [ 0 0 1]])
                            (matrix [[ 0 1 0] [-1 0 0] [ 0 0 1]])
                            (matrix [[0 1 0] [1 0 0] [0 0 -1]])
                            (matrix [[0 -1 0] [-1 0 0] [0 0 -1]])
                            
                            (matrix [[0 1 0] [0 0 1] [1 0 0]])
                            (matrix [[0 -1 0] [0 0 -1] [1 0 0]])
                            (matrix [[0 -1 0] [0 0 1] [-1 0 0]])
                            (matrix [[0 1 0] [0 0 -1] [-1 0 0]])

                            (matrix [[0 0 1] [1 0 0] [0 1 0]])
                            (matrix [[0 0 -1] [-1 0 0] [0 1 0]])
                            (matrix [[0 0 -1] [1 0 0] [0 -1 0]])
                            (matrix [[0 0 1] [-1 0 0] [0 -1 0]])
                            
                            (matrix [[0 0 -1] [0 1 0] [1 0 0]])
                            (matrix [[0 0 1] [0 -1 0] [1 0 0]])
                            (matrix [[0 0 1] [0 1 0] [-1 0 0]])
                            (matrix [[0 0 -1] [0 -1 0] [-1 0 0]])))

(struct dataset [beacons scanners] #:transparent)

; merge-dataset : Dataset Dataset -> Dataset?
(define (merge-dataset d1 d2)
  (for/or ([rot all-rotations])
    (let* ([rotate (λ [v] (matrix* rot v))]
           [b2 (set-map (dataset-beacons d2) rotate)]
           [distance-map (for*/fold ([map (hash)])
                                    ([a (dataset-beacons d1)]
                                     [b b2])
                           (count-map-add map (distance a b)))]
           [best-count (apply max (hash-values distance-map))])
      (if (>= best-count 12)
          (let ([best-displacement (findf (λ [k] (= (hash-ref distance-map k) best-count)) (hash-keys distance-map))])
            (dataset (set-union (dataset-beacons d1)
                                (list->set (map (λ [v] (matrix+ best-displacement v)) b2)))
                     (set-union (dataset-scanners d1)
                                (list->set (set-map (dataset-scanners d2)
                                                    (λ [v] (matrix+ (matrix* rot v) best-displacement)))))))
          #f))))

; merge-datasets : [List Dataset] -> Dataset
(define (merge-datasets datasets)
  (define (acc s0 datasets)
    (if (empty? datasets) s0
        (let ([merge-result (merge-dataset s0 (first datasets))])
          (if merge-result
              (acc merge-result (rest datasets))
              (acc s0 (append (rest datasets) (list (first datasets))))))))
  (acc (first datasets) (rest datasets)))

; Distance : 3x1Matrix 3x1Matrix -> 3x1Matrix
(define (distance v1 v2)
  (matrix- v1 v2))
                             
; read-datasets : -> [List Dataset]
(define (read-datasets)
  (let ([next (read-dataset)])
    (if (= 0 (set-count (dataset-beacons next)))
        '()
        (cons next (read-datasets)))))

; read-dataset : -> Dataset
(define (read-dataset)
  (let ([line (read-line)])
    (dataset (list->set (parse-lop))
             (set (col-matrix [0 0 0])))))

; parse-lop : -> [List 3x1Matrix]
(define (parse-lop)
  (read-lines string->vec))

; string->vec : String -> 3x1Matrix
(define (string->vec line)
  (let ([vals (map string->number (string-split line ","))])
    (col-matrix [(first vals) (second vals) (third vals)])))

#;(module+ main
  (display (set-count (dataset-beacons (merge-datasets (read-datasets))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; largest-manhattan : [List 3x1Matrix] -> Nat
(define (largest-manhattan points)
  (for*/fold ([lm -inf.0])
             ([a points]
              [b points])
      (max (manhattan a b) lm)))

; manhattan : 3x1Mat 3x1Mat -> Nat
(define (manhattan a b)
  (apply + (array->list (matrix-map abs (matrix- a b)))))

(module+ main
  (let ([merged-data (merge-datasets (read-datasets))])
    (begin
      (display "Beacons: ")
      (displayln (set-count (dataset-beacons merged-data)))
      (display "ScannerDist: ")
      (displayln (largest-manhattan (set->list (dataset-scanners merged-data)))))))