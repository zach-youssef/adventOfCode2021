#lang racket

(require "util.rkt")

; Part 1 -----------------------------------------------------------

; count-increases : [NE-List Number] -> Nat
; Takes a sequence of numbers and returns the number of instances
; Where A[i] < A[i+1]
(define (count-increases lon)
  (for/fold ([count 0]
             [last-elem (first lon)]
             #:result count)
            ([cur-elem (rest lon)])
    (values (if (> cur-elem last-elem) (add1 count) count)
            cur-elem)))

#;(module+ main
  (display (count-increases (read-lines string->number))))

; Part 2 -----------------------------------------------------------

; sliding-depths : [NEList Number] -> [NEList Number]
; Takes a list of measurements and computes the three-mesaurement sliding list of sums
(define (sliding-depths lon)
  (if (and (cons? lon)
           (cons? (cdr lon))
           (cons? (cddr lon)))
      (cons (+ (car lon)
               (cadr lon)
               (caddr lon))
            (sliding-depths (rest lon)))
      '()))

(module+ main
  (display (count-increases (sliding-depths (read-lines string->number)))))