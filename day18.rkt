#lang racket

; A SnailFishNumber (SNF) is one of
; Number
; (list SNF SNF)

; A PathStep is one of
; 'L
; 'R

(struct e [path left right] #:transparent)
(struct s [path] #:transparent)
; A ReductionStep is one of
; (s [List PathStep Number Number])
; (e [List PathStep])
; #f


; sum-stdin : -> SNF
; Reads SnailFishNumbers from stdin and sums them together, reducing along the way
(define (sum-stdin)
  (define (loop snf)
    (let ([next (read)])
      (if (eof-object? next)
          snf
          (loop (reduce (add snf next))))))
  (loop (read)))

; add : SNF SNF -> SNF
(define (add snf1 snf2)
  (list snf1 snf2))

; magnitude : SNF -> Number
(define (magnitude snf)
  (match snf
    [(list left right) (+ (* 3 (magnitude left))
                          (* 2 (magnitude right)))]
    [n n]))

#;(module+ main
  (display (magnitude (sum-stdin))))

; reduce : SNF -> SNF
(define (reduce snf)
  (let ([step (needs-reduction? snf)])
    (cond
      [(e? step) (reduce (apply-explosion snf step))]
      [(s? step) (reduce (apply-split snf step))]
      [else snf])))

; needs-reduction? : SNF -> ReductionStep
(define (needs-reduction? snf)
  (define (acc snf depth path)
    (cond
      [(and (list? snf) (= depth 4)) (e (reverse path) (car snf) (cadr snf))]
      [(list? snf) (let ([left (acc (car snf) (add1 depth) (cons 'L path))])
                     (if (e? left) left
                         (let ([right (acc (cadr  snf) (add1 depth) (cons 'R path))])
                           (if (e? right) right
                               (or left right)))))]
      [(and (number? snf) (>= snf 10)) (s (reverse path))]
      [(number? snf) #f]))
  (acc snf 0 '()))

; apply-explosion : SNF E -> SNF                    
; 3 steps:
; - Replace value at path with 0
; - Add left to leftmost value, if it exists
; - Add right to rightmost value, if it exists
(define (apply-explosion snf e)
  (let* ([path (e-path e)]
         [zeroed (modify-at-path snf path (λ [_] 0))]
         [left-path (path-to-adj zeroed path 'L)]
         [lefted (if left-path
                     (modify-at-path zeroed left-path (λ [n] (+ n (e-left e))))
                     zeroed)]
         [right-path (path-to-adj zeroed path 'R)])
    (if right-path
        (modify-at-path lefted right-path (λ [n] (+ n (e-right e))))
        lefted)))

; apply-split : SNF S -> SNF
(define (apply-split snf s)
  (modify-at-path snf (s-path s) (λ [n] (list (exact-floor (/ n 2))
                                              (exact-ceiling (/ n 2))))))

; modify-at-path : SNF [List PathStep] [SNF -> SNF] -> SNF?
; Applies the given function to the SNF at the given path
; Returns false if the path is not valid
(define (modify-at-path snf path f)
  (cond
    [(and (number? snf) (cons? path)) #f]
    [(empty? path) (f snf)]
    [(and (list? snf)(list? path)) (match (first path)
                                     ['L (list (modify-at-path (car snf) (rest path) f) (cadr snf))]
                                     ['R (list (car snf) (modify-at-path (cadr snf) (rest path) f))])]))

; path-to-adj : SNF [List PathStep] PathStep -> [List PathStep]?
(define (path-to-adj snf path dir)
  (let ([path-start (path-to-adj-parent path dir)])
    (and path-start
         (proceed-to-terminal snf (append path-start (list dir)) (opposite dir)))))

; path-to-adj-parent : [List PathStep] PathStep -> [List PathStep]?
; Returns the first parent node that the original path shares with a number to its left (if it exists)
(define (path-to-adj-parent path dir)
  (define (loop path)
  (cond
    [(empty? path) #f]
    [(cons? path) (if (symbol=? (first path) dir)
                      (loop (rest path))
                      (rest path))]))
  (let ([result (loop (reverse path))])
    (and result (reverse result))))

; opposite : PathStep -> PathStep
(define (opposite dir)
  (match dir
    ['L 'R]
    ['R 'L]))

; path-step->op : PathStep -> [SNF -> SNF]
(define (path-step->op dir)
  (match dir
    ['L car]
    ['R cadr]))

; apply-path : SNF [List PathStep] -> SNF?
(define (apply-path snf path)
  (cond
    [(empty? path) snf]
    [(and (cons? path) (number? snf)) #f]
    [(and (cons? path) (list? snf)) (apply-path ((path-step->op (first path)) snf) (rest path))]))

; proceed-to-terminal : SNF [List PathStep] PathStep -> [List PathStep]
; Add dir to path until a number/terminal is reached
(define (proceed-to-terminal snf path dir)
  (define (loop snf)
    (cond [(number? snf) '()]
          [(list? snf) (cons dir (loop ((path-step->op dir) snf)))]))
  (append path (loop (apply-path snf path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; read-all-snf : -> [List SNF]
(define (read-all-snf)
  (let ([snf (read)])
    (if (eof-object? snf)
        '()
        (cons snf (read-all-snf)))))

; largest-sum : [List SNF] -> Nat
(define (largest-sum snfs)
  (for*/fold ([best -inf.0])
             ([a snfs]
              [b snfs])
    (if (equal? a b)
        best
        (max (magnitude (reduce (add a b)))
             best))))

(module+ main
  (largest-sum (read-all-snf)))