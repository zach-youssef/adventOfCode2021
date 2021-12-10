#lang racket

(require "util.rkt")

; Part 1 -----------------------------------------------------------

; apply-movements : [List (list String Number)] -> (list Number Number)
; Takes a list of instructions and returns the horizontal position & depth afterwards
(define (apply-movements instructions)
  (for/fold ([pos 0]
             [depth 0]
             #:result (list pos depth))
            ([instr instructions])
    (match instr
      [(list "forward" n) (values (+ pos n) depth)]
      [(list "down" n) (values pos (+ depth n))]
      [(list "up" n) (values pos (- depth n))])))

; parse-command : String -> (list String Number)
; Parses the command out of the raw string
(define (parse-command line)
  (apply (λ [command n] (list command (string->number n)))
         (string-split line " ")))

#;(module+ main
  (display (apply * (apply-movements (read-lines parse-command)))))

; Part 2 -----------------------------------------------------------

; apply-movements-v2 : [List (list String Number)] -> (list Number Number)
; Same as before, but modified to keep track of aim
(define (apply-movements-v2 instructions)
  (for/fold ([pos 0]
             [depth 0]
             [aim 0]
             #:result (list pos depth))
            ([instr instructions])
    (match instr
      [(list "down" n) (values pos depth (+ aim n))]
      [(list "up" n) (values pos depth (- aim n))]
      [(list "forward" n) (values (+ pos n) (+ depth (* aim n)) aim)])))

(module+ main
  (display (apply * (apply-movements-v2 (read-lines parse-command)))))