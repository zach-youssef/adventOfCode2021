#lang racket

(require "util.rkt")

(require data/heap)
(require racket/hash)

; astar : (Nat Nat) [List (Nat Nat)] [HashMap (Nat Nat) -> Nat] [Set (Nat Nat)] [Heap (list (Nat, Nat) Nat [List (Nat, Nat)])] -> [List (Nat, Nat)]
(define (astar cur path risk seen queue goal)
  (cond
    [(and (= (car goal) (car cur))
          (= (cdr goal) (cdr cur))) (cons goal path)]
    #;[(set-member? seen cur) (let* ([next (heap-min queue)]
                                   [update (heap-remove-min! queue)])
                              (astar (first next) (third next) risk seen queue goal))]
    [else (let* ([loop (for ([next (succ cur goal)])
                        (if (not (set-member? seen next))
                            (heap-add! queue (list next (hval next risk goal path) (cons cur path)))
                            'else))]
                 [next (heap-min queue)]
                 [update (heap-remove-min! queue)])
            (astar (first next) (third next) risk (set-add seen cur) queue goal))]))

(define (succ state goal)
  (filter (λ [state] (and (<= (car state) (car goal))
                          (<= (cdr state) (cdr goal))
                          (>= (car state) 0)
                          (>= (cdr state) 0)))
          (map (λ [dx dy] (cons (+ dx (car state)) (+ dy (cdr state)))) '(1 -1 0 0) '(0 0 -1 1))))
  
(define (hval state risk goal path)
  (+ (score-path path risk)
     (hash-ref risk state)))

(define (path grid)
  (astar (cons 0 0) '() grid (set)
         (make-heap (λ [a b]  (<= (second a) (second b))))
         (cons (apply max (map car (hash-keys grid)))
               (apply max (map cdr (hash-keys grid))))))

(define (score-path path risk)
  (- (foldr (λ [state sum] (+ (hash-ref risk state) sum)) 0 path)
     (hash-ref risk (cons 0 0))))

#;(module+ main
  (let* [(grid (read-num-grid))
         (p (path grid))]
    (display (score-path p grid))
    (display "\n")
    #;(print-path grid p)))

(define (print-path grid path)
  (for [(r (add1 (apply max (map car (hash-keys grid)))))]
    (for [(c (add1 (apply max (map cdr (hash-keys grid)))))]
      (let ([risk (hash-ref grid (cons r c))])
        (if (set-member? path (cons r c))
            (display "_")
            (display risk))))
    (display "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (increase-scan grid)
  (let ([rows (add1 (apply max (map car (hash-keys grid))))]
        [cols (add1 (apply max (map cdr (hash-keys grid))))])
  (for*/fold ([new-grid (hash)])
            ([r 5]
             [c 5])
    (foldr hash-union new-grid
           (hash-map grid
                     (λ [pos risk] (hash (cons (+ (* r rows) (car pos))
                                               (+ (* c cols) (cdr pos)))
                                         ((λ [n] (if (= 0 n) 9 n))
                                          (modulo (+ r c risk) 9)))))))))

(module+ main
  (let* [(grid (increase-scan (read-num-grid)))
         (p (path grid))]
    (display (score-path p grid))
    (display "\n")
    #;(print-path grid p)))