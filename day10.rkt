#lang racket

(require "util.rkt")

; Part 1 -----------------------------------------------------------

; parse-chunk : String -> [List 1String]
(define (parse-chunk line)
  (map string (string->list line)))

; is-corrupted : [List 1String] -> 1String?
; Returns false or the first incorrect closing symbol
(define (is-corrupted chunk)
  ; acc : [List 1String] [List 1String]
  (define (acc input stack)
    (and (not (empty? input))
         (match (list (first input) (first stack))
           [(list "(" _) (acc (rest input) (cons ")" stack))]
           [(list "[" _) (acc (rest input) (cons "]" stack))]
           [(list "{" _) (acc (rest input) (cons "}" stack))]
           [(list "<" _) (acc (rest input) (cons ">" stack))]
           [(list ")" ")") (acc (rest input) (rest stack))]
           [(list "]" "]") (acc (rest input) (rest stack))]
           [(list "}" "}") (acc (rest input) (rest stack))]
           [(list ">" ">") (acc (rest input) (rest stack))]
           [_ (first input)])))
  (acc chunk (list "#")))

;score-corruption : [List [List 1String]] -> Number
(define (score-correction chunks)
  (for/fold [(total 0)]
            [(chunk chunks)]
    (values (+ total
               (match (is-corrupted chunk)
                 [")" 3]
                 ["]" 57]
                 ["}" 1197]
                 [">" 25137]
                 [_ 0])))))

#;(module+ main
  (display (score-correction (read-lines parse-chunk))))

; Part 2 -----------------------------------------------------------

; solve-line : [List 1String] -> [Or 1String [List 1String]]
; Returns the first error char if corrupted, or the autocomplete if incomplete
(define (solve-line chunk)
  ; acc : [List 1String] [List 1String] -> [Or 1String [List 1String]]
  (define (acc input stack)
    (if (empty? input)
        (filter (λ [x] (not (string=? x "#"))) stack)
        (match (list (first input) (first stack))
          [(list "(" _) (acc (rest input) (cons ")" stack))]
          [(list "[" _) (acc (rest input) (cons "]" stack))]
          [(list "{" _) (acc (rest input) (cons "}" stack))]
          [(list "<" _) (acc (rest input) (cons ">" stack))]
          [(list ")" ")") (acc (rest input) (rest stack))]
          [(list "]" "]") (acc (rest input) (rest stack))]
          [(list "}" "}") (acc (rest input) (rest stack))]
          [(list ">" ">") (acc (rest input) (rest stack))]
          [_ (first input)])))
  (acc chunk '("#")))

; score-autocompletion : [List 1String] -> Number
(define (score-autocompletion solution)
  (for/fold [(score 0)]
            [(char solution)]
    (values (+ (* score 5)
               (match char
                 [")" 1]
                 ["]" 2]
                 ["}" 3]
                 [">" 4])))))


; middle-score : [List Nat] -> Nat
; Find the middle number in a list of nats
(define (middle-score scores)
  (list-ref (sort scores <)
            (exact-floor (/ (length scores) 2))))

(module+ main
  (display
   (middle-score
    (map score-autocompletion
         (filter list?
                 (map solve-line
                      (read-lines parse-chunk)))))))