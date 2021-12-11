#lang racket

; sim-day : [List Number] -> [List Number]
(define (sim-day fish)
  (for/fold ([new-fish '()])
             ([fish fish])
    (values (if (= fish 0)
                (cons 8 (cons 6 new-fish))
                (cons (sub1 fish) new-fish)))))

; sim-days : [List Number] Nat -> [List Number]
(define (sim-days fish n)
  (for/fold ([new-fish fish])
            ([_ n])
    (values (sim-day new-fish))))

(module+ main
  (display (length (sim-days (map string->number (string-split (read-line) ",")) 80))))