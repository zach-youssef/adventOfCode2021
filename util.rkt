#lang racket

(provide read-lines)

; read-lines [String->X] -> [List X]
; Returns a value for each line of input from stdin.
; By default, returns the lines as strings
(define (read-lines [f identity])
  (let [(line (read-line))]
    (if (string? line)
        (cons (f line) (read-lines f))
        '())))