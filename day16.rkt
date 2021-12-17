#lang racket

(require "util.rkt")
(require "day3.rkt")
(require racket/generator)

; hexchar->binary : 1String -> [List Boolean]
(define (hexchar->binary hex)
  (map (λ [bit] (= bit 1))
       (match hex
         ["0" '(0 0 0 0)]
         ["1" '(0 0 0 1)]
         ["2" '(0 0 1 0)]
         ["3" '(0 0 1 1)]
         ["4" '(0 1 0 0)]
         ["5" '(0 1 0 1)]
         ["6" '(0 1 1 0)]
         ["7" '(0 1 1 1)]
         ["8" '(1 0 0 0)]
         ["9" '(1 0 0 1)]
         ["A" '(1 0 1 0)]
         ["B" '(1 0 1 1)]
         ["C" '(1 1 0 0)]
         ["D" '(1 1 0 1)]
         ["E" '(1 1 1 0)]
         ["F" '(1 1 1 1)])))

; hexstring->binary : String -> [List Boolean]
(define (hexstring->binary hexstr)
  (foldr append '()
         (map hexchar->binary
              (map string (string->list hexstr)))))

(struct literal [version value] #:transparent)
(struct operator [version type-id sub-packets] #:transparent)
; A Packet is one of
; - a (literal Number Number)
; - an (operator Number Number [List Packet])

;read-n : Generator Nat -> [List Boolean]
; Reads n elements from a generator into a list
(define (read-n g nat)
  (build-list nat (λ [_] (g))))

; parse-packet : [Generator Boolean] -> Packet?
; Parses a packet structure from the given binary data
; Returns false if the binary generator has run out
(define (parse-packet binary)
  (let ([start (binary)])
    (if (void? start) #f
        (let ([version (binary->decimal (cons start (read-n binary 2)))]
              [type-id (binary->decimal (read-n binary 3))])
          (if (= type-id 4)
              (parse-literal version binary)
              (parse-operator version type-id binary))))))

; parse-literal : Number [Generator Boolean] -> Packet
(define (parse-literal version binary)
  (letrec ([read-bits (λ [] (let ([more? (binary)])
                              (append (read-n binary 4)
                                      (if more? (read-bits) '()))))])
    (literal version (binary->decimal (read-bits)))))

; parse-operator : Number Number [Generator Boolean] -> Packet
(define (parse-operator version type-id binary)
  (operator version type-id
            (if (binary) ; switch off of length type
                (read-sub-by-count binary)
                (read-sub-by-length binary))))

; read-sub-by-count : [Generator Boolean] -> [List Packet]
(define (read-sub-by-count binary)
  (let ([count (binary->decimal (read-n binary 11))])
    (build-list count (λ [_] (parse-packet binary)))))

; read-sub-by-length : [Generator Boolean] -> [List Packet]
(define (read-sub-by-length b)
  (define (loop binary)
    (let ([packet (parse-packet binary)])
      (if packet
          (cons packet (loop binary))
          '())))
  (let* ([len (binary->decimal (read-n b 15))]
         [binary (sequence->generator (read-n b len))])
    (loop binary)))
    
; sum-versions : Packet -> Number
(define (sum-versions packet)
  (cond
    [(literal? packet) (literal-version packet)]
    [(operator? packet) (foldr (λ [p n] (+ (sum-versions p) n))
                               (operator-version packet)
                               (operator-sub-packets packet))]))
        
#;(module+ main
  (sum-versions (parse-packet (sequence->generator (hexstring->binary (read-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; eval-packet : Packet -> Number
(define (eval-packet packet)
  (cond
    [(literal? packet) (literal-value packet)]
    [(operator? packet) (eval-operator packet)]))

; eval-operator : Operator -> Number
(define (eval-operator op)
  (match op
    [(operator _ 0 children) (foldr + 0 (map eval-packet children))]
    [(operator _ 1 children) (foldr * 1 (map eval-packet children))]
    [(operator _ 2 children) (apply min (map eval-packet children))]
    [(operator _ 3 children) (apply max (map eval-packet children))]
    [(operator _ 5 (list left right)) (if (> (eval-packet left) (eval-packet right)) 1 0)]
    [(operator _ 6 (list left right)) (if (< (eval-packet left) (eval-packet right)) 1 0)]
    [(operator _ 7 (list left right)) (if (= (eval-packet left) (eval-packet right)) 1 0)]))

(module+ main
  (eval-packet (parse-packet (sequence->generator (hexstring->binary (read-line))))))