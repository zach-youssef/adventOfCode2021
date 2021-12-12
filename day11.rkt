#lang racket

(require "util.rkt")

; Part 1 -----------------------------------------------------------

; octo-step : [HashMap (Nat, Nat) -> Number] -> (list [HashMap (Nat, Nat) -> Number] Nat)
(define (octo-step octos)
  (flash-octos (increment-octos octos)))

; increment-octos : [HashMap (Nat, Nat) -> Number] -> [HashMap (Nat, Nat) -> Number]
; Increases the value of every octopus by 1
(define (increment-octos octos)
  (for/fold [(new-octos (hash))]
            [(pos (hash-keys octos))]
    (values (hash-set new-octos pos (add1 (hash-ref octos pos))))))

; flash-octos : [HashMap (Nat, Nat) -> Number] -> (list [HashMap (Nat, Nat) -> Number] Nat)
; Returns the resulting map and the number of flashes that occured
(define (flash-octos octos)
  (let ([flash (powered-octo octos)])
    (if (not flash) (list (elim-infs octos) 0)
        (for/fold [(new-octos (hash-set octos flash -inf.0))
                   #:result (apply (λ [o n] (list o (add1 n))) (flash-octos new-octos))]
                  [(adj (adjacent-octos octos flash))]
          (values (hash-set new-octos
                            adj
                            (add1 (hash-ref octos adj))))))))
          

; powered-octo : [HashMap (Nat, Nat) -> Number] -> (Nat, Nat)?
; Returns the first octopus with power over 9, if it exists
(define (powered-octo octos)
  (for/or [(pos (hash-keys octos))]
    (if (> (hash-ref octos pos) 9) pos #f)))

; adjacent-octos : [HashMap (Nat, Nat) -> Number] (Nat, Nat) -> [List (Nat, Nat)]
(define (adjacent-octos octos pos)
  (filter (λ [k] (hash-has-key? octos k))
          (map (λ [dx dy] (cons (+ dx (car pos)) (+ dy (cdr pos))))
               '(1 -1 0 0 1 -1 1 -1)
               '(0 0 -1 1 1 -1 -1 1))))

; elim-infs : [HashMap (Nat, Nat) -> Number] -> [HashMap (Nat, Nat) -> Number]
(define (elim-infs octos)
  (for/fold ([new-octos octos])
            ([octo (hash-keys octos)])
    (values (if (= (hash-ref octos octo) -inf.0)
                (hash-set new-octos octo 0)
                new-octos))))

; octo-steps : [HashMap (Nat, Nat) -> Number] Nat -> Nat
; Applies the given number of steps and returns the number of flashes that occured
(define (octo-steps octos steps)
  (for/fold [(new-octos octos)
             (flashes 0)
             #:result flashes]
            [(_ steps)]
    (let ([step (octo-step new-octos)])
      (values (car step) (+ flashes (cadr step))))))

#;(module+ main
  (display (octo-steps (read-num-grid) 100)))

; Part 2 -----------------------------------------------------------

; find-sync-flash : [HashMap (Nat, Nat) -> Number] -> Nat
; Returns the step at whcih all octopuses flash
(define (find-sync-flash octos)
  (let [(next (octo-step octos))]
    (if (= (cadr next) (length (hash-keys (car next))))
        1
        (+ 1 (find-sync-flash (car next))))))

(module+ main
  (display (find-sync-flash (read-num-grid))))