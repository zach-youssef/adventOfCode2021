#lang racket

(require "util.rkt")
(require srfi/13)
(require racket/generator)

(define ROLL-COUNT 0)
(define DIE (infinite-generator
             (let loop ([n 1])
               (if (> n 100)
                   (loop 1)
                   (begin
                     (set! ROLL-COUNT (add1 ROLL-COUNT))
                     (yield n)
                     (loop (add1 n)))))))

; returns score of losing player
(define (game-loop pos1 score1 pos2 score2 [p2turn #f])
  (if (>= (max score1 score2) 1000)
      (min score1 score2)
  (let ([dist (apply + (build-list 3 (λ [_] (DIE))))])
    (if p2turn
        (let ([new-pos (move pos2 dist)])
          (game-loop pos1 score1 new-pos (+ score2 new-pos) #f))
        (let ([new-pos (move pos1 dist)])
          (game-loop new-pos (+ score1 new-pos) pos2 score2 #t))))))

(define (move start dist)
  (let ([dest (modulo (+ start dist) 10)])
    (if (= 0 dest) 10 dest)))

(define (read-start-positions)
  (read-lines (λ [line] (string->number (substring line (+ 2 (string-contains line ":")))))))

#;(module+ main
  (let ([losing-score (apply (λ [pos1 pos2] (game-loop pos1 0 pos2 0))
                             (read-start-positions))])
    (* ROLL-COUNT losing-score)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ROLL-MAP (for*/fold ([map (hash)])
                            ([a 3][b 3][c 3])
                   (count-map-add map (+ a b c 3))))

; dirac-loop: Nat Nat Nat Nat Nat -> (list Nat Nat)
; Returns (# universes where p1 wins, # universes where p2 wins)
(define (dirac-loop pos1 score1 pos2 score2 universes [p2turn #f])
  (cond
    [(>= score1 21) (list universes 0)]
    [(>= score2 21) (list 0 universes)]
    [else (for/fold ([result '(0 0)])
                    ([dist (hash-keys ROLL-MAP)])
            (map + result
                 (if p2turn
                     (let ([new-pos (move pos2 dist)])
                       (dirac-loop pos1 score1 new-pos (+ score2 new-pos) (* universes (hash-ref ROLL-MAP dist))))
                     (let ([new-pos (move pos1 dist)])
                       (dirac-loop new-pos (+ score1 new-pos) pos2 score2 (* universes (hash-ref ROLL-MAP dist)) #t)))))]))

(module+ main
  (apply (λ [pos1 pos2] (dirac-loop pos1 0 pos2 0 1))
         (read-start-positions)))