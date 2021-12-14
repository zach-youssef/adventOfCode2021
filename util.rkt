#lang racket

(provide read-lines
         count-map-add
         get-or-default
         read-num-grid
         multimap/insert
         list->count-map)

; read-lines : [String->X] -> [List X]
; Returns a value for each line of input from stdin.
; By default, returns the lines as strings
(define (read-lines [f identity])
  (let [(line (read-line))]
    (if (and (string? line) (not (string=? line "")))
        (cons (f line) (read-lines f))
        '())))

; count-map-add : [HashMap X -> Nat] X -> [HashMap X -> Nat]
; Increments the value at X, or sets it to 1 if not presence
(define (count-map-add map key)
  (if (hash-has-key? map key)
      (hash-set map key (add1 (hash-ref map key)))
      (hash-set map key 1)))

; get-or-default : [HashMap X->Y] X Y -> Y
(define (get-or-default map key default)
  (if (hash-has-key? map key)
      (hash-ref map key)
      default))

; read-num-grid : -> [HashMap (Nat, Nat) -> Number]
(define (read-num-grid)
  (parse-heatmap (read-lines (λ [line] (map string->number (map string (string->list line)))))))

; parse-heatmap : [List [List Number]] -> [HashMap (Nat, Nat) -> Number]
(define (parse-heatmap input)
  (for*/fold ([heatmap (hash)])
             ([r (length input)]
              [c (length (list-ref input r))])
    (values (hash-set heatmap (cons r c) (list-ref (list-ref input r) c)))))

; multimap/insert : [HashMap X -> [Set Y]] X Y -> [HashMap X -> [Set Y]]
(define (multimap/insert map key val)
  (if (hash-has-key? map key)
      (hash-set map key (set-add (hash-ref map key) val))
      (hash-set map key (set val))))

; list->count-map : [List X] ->  [HashMap  X->Number]
(define (list->count-map lox)
  (for/fold [(map (hash))]
            [(e lox)]
    (count-map-add map e)))
