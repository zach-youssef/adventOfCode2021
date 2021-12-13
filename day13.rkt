#lang racket

(require "util.rkt")
(provide apply-folds read-dotmap read-folds)

; Part 1 -----------------------------------------------------------

; read-dotmap : -> [Set (Number, Number)]
(define (read-dotmap)
  (list->set (read-lines (λ [line] (apply cons (map string->number (string-split line ",")))))))

; read-folds : -> [List (list String Number)]
(define (read-folds)
  (read-lines (λ [line] (apply (λ [axis num] (list axis (string->number num)))
                               (string-split (string-replace line "fold along " "")
                                             "=")))))

; apply-x-fold : [Set (Number, Number)] Number -> [Set (Number, Number)]
(define (apply-x-fold dots x)
  (for/fold [(new-dots (set))]
            [(dot dots)]
    (values (if (> (car dot) x)
                (set-add new-dots (cons (- (* x 2) (car dot)) (cdr dot))) ; x - (d-x) = 2x - d
                (set-add new-dots dot)))))

#;(module+ main
  (display (set-count (apply-x-fold (read-dotmap) (cadr (first (read-folds)))))))

; Part 2 -----------------------------------------------------------

; apply-y-fold : [Set (Number, Number)] Number -> [Set (Number, Number)]
(define (apply-y-fold dots y)
  (for/fold [(new-dots (set))]
            [(dot dots)]
    (values (if (> (cdr dot) y)
                (set-add new-dots (cons (car dot) (- (* y 2) (cdr dot))))
                (set-add new-dots dot)))))

; apply-folds: [Set (Number, Number)] [List (list String Number)] -> [Set (Number, Number)]
(define (apply-folds dots folds)
  (for/fold [(new-dots dots)]
            [(fold folds)]
    (values (match fold
              [(list "x" n) (apply-x-fold new-dots n)]
              [(list "y" n) (apply-y-fold new-dots n)]))))

; display-dots [Set (Number, Number)] -> Void
(define (display-dots dots)
  (for  [(x (add1 (apply max (set-map dots car))))]
    (for [(y (add1 (apply max (set-map dots cdr))))]
      (display (if (set-member? dots (cons x y)) "#" ".")))
    (display "\n")))

(module+ main
  (display-dots (apply-folds (read-dotmap) (read-folds))))