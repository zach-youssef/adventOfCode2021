#lang racket

; Part 1 -----------------------------------------------------------

; A Bingo is a (bingo [HashMap Number -> (Number, Number)] [Set (Number, Number)]
; Where values represents a map from a number on the board to its position
; and marked is the set of positions that are "marked"
(struct bingo [vals marked] #:transparent)

; read-bingo : -> Bingo
; Reads a completely unmarked bingo board from standard in
(define (read-bingo)
  (bingo (for*/fold [(vals (hash))]
                   [(row 5)
                    (col 5)]
          (values (hash-set vals (read) (cons row col))))
         (set)))

; board-score : Bingo Number -> Number
(define (board-score bingo last-called)
  (* last-called
     (foldr + 0
            (hash-map (bingo-vals bingo)
                      (λ [k v] (if (set-member? (bingo-marked bingo) v)
                                   0 k))))))

; winning-board? : Bingo -> Boolean
(define (winning-board? bingo)
  (for/or ([i 5])
    (or (for/and ([j 5])
          (set-member? (bingo-marked bingo) (cons i j)))
        (for/and ([j 5])
          (set-member? (bingo-marked bingo) (cons j i))))))

; mark-number : Bingo Number -> Bingo
; Returns a bingo board after the given number has been called
(define (mark-number board num)
  (if (hash-has-key? (bingo-vals board) num)
      (bingo (bingo-vals board)
             (set-add (bingo-marked board)
                      (hash-ref (bingo-vals board)
                                num)))
      board))

; winning-score : [List Number] [List Bingo] -> Number
; Returns the score of the bingo board that wins first
; based on the given sequence of numbers
(define (winning-score seq boards)
  (let* [(marked-boards (map (λ [b] (mark-number b (first seq))) boards))
         (winning-board (for/or [(board marked-boards)]
                          (if (winning-board? board)
                              board #f)))]
    (if winning-board
        (board-score winning-board (first seq))
        (winning-score (rest seq) marked-boards))))

; read-bingos : -> [List Bingo]
; Reads bingo boards separated by a newline until eof
; The first bingo board should be preceded by a newline,
; the last one should be immediately followed by eof
(define (read-bingos)
  (let [(line (read-line))]
    (if (string? line)
        (cons (read-bingo) (read-bingos))
        '())))

(module+ main
  (define sequence (map string->number (string-split (read-line) ",")))
  (display (winning-score sequence (read-bingos))))