#lang racket

(require "util.rkt")

(require data/heap)

; succ : (Nat Nat) (Nat Nat) -> [List (Nat Nat)]
; Returns the adjacent points in the grid (succesors in the graph)
(define (succ state goal)
  (filter (λ [state] (and (<= (car state) (car goal))
                          (<= (cdr state) (cdr goal))
                          (>= (car state) 0)
                          (>= (cdr state) 0)))
          (map (λ [dx dy] (cons (+ dx (car state)) (+ dy (cdr state)))) '(1 -1 0 0) '(0 0 -1 1))))
  

#;(module+ main
  (let* ([risk (read-num-grid)]
         [goal (cons (apply max (map car (hash-keys risk)))
                     (apply max (map cdr (hash-keys risk))))]
         [h (λ [pos] (hash-ref risk pos))])
    (astar (cons 0 0) 0
           (make-heap (λ [a b] (if (= (cadr a) (cadr b))
                                   (< (caddr a) (caddr b))
                                   (< (cadr a) (cadr b)))))
           (set) goal h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-risk : [HashMap (Nat, Nat) -> Number] (Nat, Nat) -> [(Nat, Nat) -> Number]
; Returns a function that calculates the risk at a point
; Based on the input risk grid and the dimensions of the total cave system
; (encapsualted in the goal coordinates)
(define (get-risk risk goal)
  (λ [pos]
    ((λ [n] (if (= 0 n) 9 n))
     (modulo
      (+ (hash-ref risk (cons (modulo (car pos) (/ (add1 (car goal)) 5))
                              (modulo (cdr pos) (/ (add1 (cdr goal)) 5))))
         (floor (/ (car pos) (/ (add1 (car goal)) 5)))
         (floor (/ (cdr pos) (/ (add1 (cdr goal)) 5))))
      9))))

; calc-goal : [HashMap (Nat, Nat) -> Number]
; Determines the coordinate of the bottom right corner of the expanded cave system
(define (calc-goal risk)
  (let ([f (λ [a] (sub1 (* 5 (add1 (apply max (map a (hash-keys risk)))))))])
  (cons (f car)
        (f cdr))))

; dist : (Nat Nat) (Nat Nat) -> Nat
; Returns the manhattan distance betwen two points
(define (dist a b)
  (+ (abs (- (car b) (car a)))
     (abs (- (cdr b) (cdr a)))))

; astar : (Nat Nat) Nat [Heap (list (Nat, Nat) Nat Nat)] [Set (Nat Nat)] (Nat Nat) [(Nat Nat) -> Nat] -> Nat
; Returns the total risk of the safest path to the goal from the given position
(define (astar pos cost queue seen goal h)
  (cond
    [(and (= (car pos) (car goal))
          (= (cdr pos) (cdr goal)))  cost]
    [(set-member? seen pos) (let* ([next (heap-min queue)]
                                   [update (heap-remove-min! queue)])
                              (astar (car next) (cadr next) queue seen goal h))]
    [else (let* ([loop (for ([next (filter (λ [s] (not (set-member? seen s))) (succ pos goal))])
                         (heap-add! queue (list next (+ (h next) cost) (dist next goal))))]
                 [next (heap-min queue)]
                 [update (heap-remove-min! queue)])
            (astar (car next) (cadr next) queue (set-add seen pos) goal h))]))

(module+ main
  (let* ([risk (read-num-grid)]
         [goal (calc-goal risk)]
         [h (get-risk risk goal)])
    (astar (cons 0 0) 0
           (make-heap (λ [a b] (if (= (cadr a) (cadr b))
                                   (< (caddr a) (caddr b))
                                   (< (cadr a) (cadr b)))))
           (set) goal h)))