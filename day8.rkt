#lang racket

(require "util.rkt")
(require racket/hash)

; Part 1 -----------------------------------------------------------

(define 2-vals (set "c" "f"))
(define 3-vals (set "a" "c" "f"))
(define 4-vals (set "b" "c" "d" "f"))
(define 8-vals (set "a" "b" "c" "d" "e" "f" "g"))

(define options (hash 2 2-vals
                     3 3-vals
                     4 4-vals
                     7 8-vals))

; parse-line : String -> (list [List [List 1String]] [List [List [1String]])
(define (parse-line line)
  (map (λ [part] (map (λ [x] (map string (string->list x))) (string-split part " "))) (string-split line " | ")))
        
#;(module+ main
  (define input-data (read-lines parse-line))
  (foldr + 0 (map (λ [group] (length (filter (λ [display] (hash-has-key? options (length display))) group))) (map cadr input-data))))

; Part 2 -----------------------------------------------------------

(define INIT-MAP
  (for/fold ([map (hash)])
            ([val '("a" "b" "c" "d" "e" "f" "g")])
    (values (hash-set map val 8-vals))))

; combine-rulesets : [HashMap 1String -> [Set 1String]] [HashMap 1String -> [Set 1String]] -> [HashMap 1String -> [Set 1String]]
(define (combine-rulesets a b)
  (hash-union a b
              #:combine/key (λ [k v1 v2] (set-intersect v1 v2))))

; gen-options : [List [List 1String]] -> [HashMap 1String -> [Set 1String]]
; Creates the set of constrains for the eventual CSP search
(define (gen-options notes)
  (for*/fold ([mapping INIT-MAP])
            ([note notes]
             [val note])
    (values (hash-set mapping val (set-intersect (hash-ref mapping val) (get-or-default options (length note) 8-vals))))))

; gen-options : [List [List 1String]] -> [HashMap 1String -> [Set 1String]]
; Adds more creative rules to fix ambiguities in the rules from the unique numbers
(define (extra-rules notes)
  (foldr combine-rulesets (hash)
         (append
          ; If a letter appears < 3 times in the 6-segment words, it can't be f
          (for*/fold ([count-map (hash)]
                      #:result (hash-map count-map (λ [l count] (if (< count 3) (hash l (set "a" "b" "c" "d" "e" "g")) (hash)))))
                     ([6-word (filter (λ [word] (= 6 (length word))) notes)]
                      [letter 6-word])
            (values (count-map-add count-map letter)))
          ; If a number appears in > 1 times in the 5-segment words, it can't be e or b
          (for*/fold ([count-map (hash)]
                      #:result (hash-map count-map (λ [l count] (if (> count 1) (hash l (set "a" "c" "d" "f" "g")) (hash)))))
                     ([5-word (filter (λ [word] (= 5 (length word))) notes)]
                      [letter 5-word])
            (values (count-map-add count-map letter))))))
          


; csp : [HashMap 1String -> [Set 1String]] -> [HashMap 1String -> 1String]
; Solves the CSP to return a mapping for each char
(define (csp rules)
  ; Iteration order appears to affect things
  (define letters (hash 1 "a"
                        0 "b"
                        2 "c"
                        3 "d"
                        4 "e"
                        5 "f"
                        6 "g"))
  ; csp/acc : [HashMap 1String -> 1String] [Set 1String] Nat -> [HashMap 1String -> 1String]?
  (define (csp/acc out used n)
    (if (>= n 7) out
        (let* ([cur (hash-ref letters n)]
               [available (set-subtract (hash-ref rules cur) used)])
          (for/or ([option available])
            (csp/acc (hash-set out cur option)
                     (set-add used option)
                     (add1 n))))))
  (csp/acc (hash) (set) 0))

(define 7SEG (hash
              (set "a" "b" "c" "e" "f" "g") "0"
              (set "c" "f") "1"
              (set "a" "c" "d" "e" "g") "2"
              (set "a" "c" "d" "f" "g") "3"
              (set "b" "c" "d" "f") "4"
              (set "a" "b" "d" "f" "g") "5"
              (set "a" "b" "d" "e" "f" "g") "6"
              (set "a" "c" "f") "7"
              (set "a" "b" "c" "d" "e" "f" "g") "8"
              (set "a" "b" "c" "d" "f" "g") "9"))

; solve-number : [List [List 1 String]] [List [List 1 String]] -> PosInt
(define (solve-number notes output)
  (define mapping (csp (combine-rulesets (gen-options notes) (extra-rules notes))))
  (string->number (foldr string-append ""
                         (map (λ [display] (hash-ref 7SEG
                                                     (list->set (map (λ [segment]
                                                                       (hash-ref mapping segment))
                                                                     display))))
                              output))))

(module+ main
  (define input-data (read-lines parse-line))
  (display (foldr + 0 (map (λ [data] (apply solve-number data)) input-data))))