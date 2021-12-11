#lang racket

(require "util.rkt")

; Part 1 -----------------------------------------------------------

; parse-heatmap : [List [List Number]] -> [HashMap (Nat, Nat) -> Number]
(define (parse-heatmap input)
  (for*/fold ([heatmap (hash)])
             ([r (length input)]
              [c (length (list-ref input r))])
    (values (hash-set heatmap (cons r c) (list-ref (list-ref input r) c)))))

; read-heatmap : -> [HashMap (Nat, Nat) -> Number]
(define (read-heatmap)
  (parse-heatmap (read-lines (λ [line] (map string->number (map string (string->list line)))))))

; adjacent-points : (Nat, Nat) -> [List (Nat, Nat)]
(define (adjacent-points pos)
  (map (λ [dx dy] (cons (+ dx (car pos)) (+ dy (cdr pos)))) '(1 -1 0 0) '(0 0 -1 1)))

; find-lowpoints : [HashMap (Nat, Nat) -> Number] -> [List (Nat, Nat)]
; Returns the list of points that are lower than all adjacent points
(define (find-lowpoints heatmap)
  (for/fold ([lowpoints '()])
            ([pos (hash-keys heatmap)])
    (let ([val (hash-ref heatmap pos)]
          [adj-vals (map (λ [adj] (get-or-default heatmap adj +inf.0))
                         (adjacent-points pos))])
      (if (< val (apply min adj-vals))
          (values (cons pos lowpoints))
          (values lowpoints)))))

; lowpoint-score :  [HashMap (Nat, Nat) -> Number] -> Number
(define (lowpoint-score heatmap)
  (foldr + 0 (map (λ [x] (+ 1 (hash-ref heatmap x))) (find-lowpoints heatmap))))

#;(module+ main
  (display (lowpoint-score (read-heatmap))))

; Part 2 -----------------------------------------------------------

; find-basin : (Nat, Nat) [HashMap (Nat, Nat) -> Number] -> [Set (Nat, Nat)]
; Finds all the points in the basin the given point is a part of
(define (find-basin pos heatmap)
  ; basin/acc : (Nat, Nat) [Set (Nat, Nat)] -> [Set (Nat, Nat)]
  (define (basin/acc cur so-far)
    (if (or (set-member? so-far cur)
            (= 9 (get-or-default heatmap cur 9)))
        so-far
        (for/fold ([new-so-far (set-add so-far cur)])
                  ([adj (adjacent-points cur)])
          (values (basin/acc adj new-so-far)))))
  (basin/acc pos (set)))

; score-basins : [HashMap (Nat, Nat) -> Number] -> Number
(define (score-basins heatmap)
  ((λ [lox] (* (first lox) (second lox) (third lox)))
   (sort
    (map (λ [basin] (set-count basin))
         (map (λ [lowpoint] (find-basin lowpoint heatmap))
              (find-lowpoints heatmap)))
    (λ [a b] (> a b)))))

(module+ main
  (display (score-basins (read-heatmap))))