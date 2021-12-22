#lang racket

(require "util.rkt")
(require "day3.rkt")

; read-inputs : -> (list [List Boolean] [Set (Nat, Nat) -> Boolean])
(define (read-inputs)
  (list (map (λ [x] (char=? x #\#)) (string->list (read-line)))
        (let ([skip (read-line)])
          (read-hash-grid))))

; read-hash-grid : -> [Set (Nat, Nat) -> Boolean]
(define (read-hash-grid)
  (list->set
   (map car
        (filter cdr
                (hash->list
                 (parse-grid (read-lines (λ [line] (map (λ [x] (char=? x #\#))
                                                        (string->list line))))))))))

(define SCAN-REG '((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1)))

(define (enhance algo image [inf #f])
  (let* ([min-row (apply min (set-map image car))]
         [min-col (apply min (set-map image cdr))]
         [max-row (apply max (set-map image car))]
         [max-col (apply max (set-map image cdr))]
         [out-of-bounds? (λ [pos] (or (> (car pos) max-row)
                                      (< (car pos) min-row)
                                      (> (cdr pos) max-col)
                                      (< (cdr pos) min-col)))])
    (for*/fold ([new-image (set)])
               ([r (range (sub1 min-row) (+ 2 max-row))]
                [c (range (sub1 min-col) (+ 2 max-col))])
      (let ([index-b (map (λ [d] (let ([lookup (cons (+ (car d) r)
                                                     (+ (cadr d) c))])
                                   (or (set-member? image lookup)
                                       (and inf (out-of-bounds? lookup)))))
                          SCAN-REG)])
        (if (list-ref algo (binary->decimal index-b))
            (set-add new-image (cons r c))
            new-image)))))

(define (print-image image)
  (let ([min-row (sub1 (apply min (set-map image car)))]
        [min-col (sub1 (apply min (set-map image cdr)))]
        [max-row (add1 (apply max (set-map image car)))]
        [max-col (add1 (apply max (set-map image cdr)))])
    (for ([r (range min-row (add1 max-row))])
      (for ([c (range min-col (add1 max-col))])
        (display (if (set-member? image (cons r c)) "#" ".")))
      (displayln ""))))

(define (enhance-n algo image amount)
  (define (loop image n)
    (if (= n amount) image
        (loop (enhance algo image (and (first algo) (= (modulo n 2) 1)))
              (add1 n))))
  (loop image 0))

#;(module+ main
  (let ([image (apply (λ [algo image] (enhance-n algo image 2)) (read-inputs))])
    (begin
      #;(print-image image)
      (displayln (set-count image)))))

(module+ main
  (let ([image (apply (λ [algo image] (enhance-n algo image 50)) (read-inputs))])
    (begin
      #;(print-image image)
      (displayln (set-count image)))))