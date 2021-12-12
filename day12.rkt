#lang racket

(require "util.rkt")

; Part 1 -----------------------------------------------------------

; read-graph : -> [HashMap String -> [Set String]]
(define (read-graph)
  (let ([entries (read-lines (λ [line] (string-split line "-")))])
    (for/fold ([graph (hash)])
              ([entry entries])
      (values (multimap/insert (multimap/insert graph (car entry) (cadr entry)) (cadr entry) (car entry))))))

; find-paths : [HashMap String -> [Set String]] -> [List [List String]]
; Finds all paths through the graph that travel through "small" caves at most once
(define (find-paths graph)
  ; find-paths/acc : String [List String] [Set String] -> [List [List String]]
  (define (find-paths/acc cur so-far small-seen)
    (cond
      [(string=? cur "end") (list (cons cur so-far))]
      [(and (small? cur) (set-member? small-seen cur)) '()]
      [else (let ([new-seen (if (small? cur) (set-add small-seen cur) small-seen)])
              (for/fold ([paths '()])
                        ([next (get-or-default graph cur (set))])
                (values (append paths
                                (find-paths/acc next (cons cur so-far) new-seen)))))]))
  (find-paths/acc "start" '() (set)))

; small? : String -> Boolean
; Returns if the given node name represents a small cave (all lowercase)
(define (small? node)
  (string=? node (string-downcase node)))

#;(module+ main
  (display (length (find-paths (read-graph)))))

; Part 2 -----------------------------------------------------------

; find-paths-v2 : [HashMap String -> [Set String]] -> [List [List String]]
; Finds all paths that visit a single small cave at most twice, and other small caves at most once
(define (find-paths-v2 graph)
  ; find-paths/acc : String [List String] [HashMap String -> Nat] -> [List [List String]]
  (define (find-paths/acc cur so-far visit-count)
    (cond
      [(string=? cur "end") (list (cons cur so-far))]
      [(not (can-visit? cur visit-count)) '()]
      [else (let ([new-seen (count-map-add visit-count cur)])
              (for/fold ([paths '()])
                        ([next (get-or-default graph cur (set))])
                (values (append paths
                                (find-paths/acc next (cons cur so-far) new-seen)))))]))
  (find-paths/acc "start" '() (hash)))


; can-visit? : String [HashMap String-> Nat] -> Boolean
; Returns wether a cave can be visited. Will always be true for large caves.
(define (can-visit? node visit-count)
  (cond
    [(not (small? node)) #t]
    [(string=? node "start") (not (hash-has-key? visit-count node))]
    [(small? node) (or (not (hash-has-key? visit-count node))
                       (and (= 1 (hash-ref visit-count node))
                            (not (small-visited-twice? visit-count))))]))
  
  #;(or (not (small? node))
      (and (member node '("start" "end"))
           (not (hash-has-key? visit-count node)))
      (and (small? node)
           (not (member node '("start" "end")))
           (or (not (hash-has-key? visit-count node))
               (and (= 1 (hash-ref visit-count node))
                    (not (small-visited-twice? visit-count))))))

; small-visited-twice? [HashMap String->Nat] -> Boolean
; Returns true if a small cave has been visited twice
(define (small-visited-twice? visit-count)
  (for/or [(node (hash-keys visit-count))]
    (and (small? node) (>= (hash-ref visit-count node) 2))))
           
(module+ main
  (display (length (find-paths-v2 (read-graph)))))         