#lang racket

(require "util.rkt")

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

(module+ main
  (display (count-increases (read-lines string->number))))