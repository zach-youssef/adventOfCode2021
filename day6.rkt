#lang racket

(require racket/hash)

; Part 1 -----------------------------------------------------------

; sim-day : [List Number] -> [List Number]
(define (sim-day fish)
  (for/fold ([new-fish '()])
             ([fish fish])
    (values (if (= fish 0)
                (cons 8 (cons 6 new-fish))
                (cons (sub1 fish) new-fish)))))

; sim-days : [List Number] Nat -> [List Number]
(define (sim-days fish n)
  (for/fold ([new-fish fish])
            ([_ n])
    (values (sim-day new-fish))))

#;(module+ main
  (display (length (sim-days (map string->number (string-split (read-line) ",")) 80))))

; Part 2 -----------------------------------------------------------

; Idea - in order to be more peformant, we can memoize how many fish there are starting from
; each single fish value after a certain number of days

; '(0 1) x 2 days = '(0) x 2 days + '(1) x 2 days = '(5 7) + '(6 8) = 4 fish

; sim-memo : Number Number [HashMap (Number, Number) -> Number] -> (list Number [HashMap (Number, Number) -> Number])
; Takes a fish and a map from (Fish, Days) -> AmountofFish and returns the number of fish,
; plus the map (potentially with more data filled in)
(define (sim-memo fish days map)
  (cond
    [(= days 0) (list 1 map)]
    [(hash-has-key? map (cons fish days)) (list (hash-ref map (cons fish days)) map)]
    [else (for/fold ([m map]
                     [count 0]
                     #:result (list count (hash-set m (cons fish days) count)))
                    ([f (sim-day (list fish))])
            (let ([total (sim-memo f (sub1 days) m)])
              (values (hash-union m (cadr total) #:combine/key (λ [_k v _v] v)) (+ count (car total)))))]))

(module+ main
  (display (for/fold ([m (hash)]
                      [count 0]
                      #:result count)
                     ([fish (map string->number (string-split (read-line) ","))])
             (let ([sub-total (sim-memo fish 256 m)])
               (values (hash-union m (cadr sub-total) #:combine/key (λ [_k v _v] v)) (+ count (car sub-total)))))))