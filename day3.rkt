#lang racket

(require "util.rkt")

; parse-binary : String -> [List Boolean]
(define (parse-binary line)
  (map (λ [x] (char=? #\1 x)) (string->list line)))

; transpose : [List [List X]] -> [List [List X]]
; Takes a list of lists of the same length and transposes the elements
; Ex: '((1 2 3) (4 5 6) (7 8 9)) -> '((1 4 7) (2 5 8) (3 6 9))
(define (transpose llox)
  (if (empty? (first llox)) '()
      (cons (map first llox) (transpose (map rest llox)))))

; most-common-bit : [List Boolean] -> Boolean
(define (most-common-bit lob)
  (for/fold ([ones 0]
             [zeros 0]
             #:result (> ones zeros))
            ([bit lob])
    (if bit
        (values (add1 ones) zeros)
        (values ones (add1 zeros)))))

; opposite : [List Boolean] -> [List Boolean]
(define (opposite lob)
  (map not lob))

; binary->decimal : [List Boolean] -> Number
(define (binary->decimal lob)
  (for/foldr ([power 1]
             [sum 0]
             #:result sum)
            ([bit lob])
    (values (* 2 power) (if bit (+ power sum) sum))))

; power-consumption : [List [List Boolean]] -> Number
(define (power-consumption input)
  (define gamma (map most-common-bit (transpose input)))
  (define epsilon (opposite gamma))
  (* (binary->decimal gamma) (binary->decimal epsilon)))

(module+ main
  (power-consumption (read-lines parse-binary)))