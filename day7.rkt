#lang racket

; Part 1 -----------------------------------------------------------

; fuel-required : [List Number] Number -> Number
; Calculates the amount of fuel required to move all subs to the given position
(define (fuel-required subs pos)
  (foldr + 0 (map (λ [x] (abs (- pos x))) subs)))

; find-min-naive : [List Number] [[List Number] Number -> Number] -> Number
(define (find-min-naive subs f)
  (for/fold [(best +inf.0)]
            [(pos (inclusive-range (apply min subs) (apply max subs)))]
    (let ([fuel (f subs pos)])
      (values (if (< fuel best) fuel best)))))

#;(module+ main
  (display (find-min-naive (map string->number (string-split (read-line) ",")) fuel-required)))

; Part 2 -----------------------------------------------------------

; triangle : Nat -> Nat
; Returns the corresponding triangle number
(define (triangle n) (/ (* n (+ n 1)) 2))

; fuel-required-v2 : [List Number] Number -> Number
(define (fuel-required-v2 subs pos)
  (foldr + 0 (map (λ [x] (triangle (abs (- pos x)))) subs)))

(module+ main
  (display (find-min-naive (map string->number (string-split (read-line) ",")) fuel-required-v2)))