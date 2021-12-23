#lang racket
(require "util.rkt")

(struct step [on? xmin xmax ymin ymax zmin zmax] #:transparent)
; read-steps: -> [List Step]
(define (read-steps)
  (read-lines (λ [line] (match (string-split
                                (string-replace
                                 (string-replace
                                  (string-replace
                                   (string-replace
                                    (string-replace line ".." " ")
                                    "," " ")
                                   "x=" "")
                                  "y=" "")
                                 "z=" "")
                                " ")
                          [(cons "on" vals) (apply (λ [a b c d e f] (step #t a b c d e f)) (map string->number vals))]
                          [(cons "off" vals) (apply (λ [a b c d e f] (step #f a b c d e f)) (map string->number vals))]))))

; step-applies? : Step Nat Nat Nat -> Boolean
(define (step-applies? step x y z)
  (and (>= x (step-xmin step))
       (<= x (step-xmax step))
       (>= y (step-ymin step))
       (<= y (step-ymax step))
       (>= z (step-zmin step))
       (<= z (step-zmax step))))

; count-on : [List Step] -> Nat
(define (count-on steps xmin xmax ymin ymax zmin zmax)
  (for*/fold ([count 0])
             ([x (range xmin (add1 xmax))]
              [y (range ymin (add1 ymax))]
              [z (range zmin (add1 zmax))])
    (if
     (last (cons #f (map step-on? (filter (λ [step] (step-applies? step x y z)) steps))))
     (add1 count)
     count)))

#;(module+ main
  (count-on (read-steps)
            -50 50
            -50 50
            -50 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct cuboid [xmin xmax ymin ymax zmin zmax])

; intersect? : Cuboid Cuboid -> Boolean
(define (intersect a b)
  (and (> (cuboid-xmax a) (cuboid-xmin b))
       (< (cuboid-xmin a) (cuboid-xmax b))
       (> (cuboid-ymax a) (cuboid-ymin b))
       (< (cuboid-ymin a) (cuboid-ymax b))
       (> (cuboid-zmax a) (cuboid-zmin b))
       (< (cuboid-zmin a) (cuboid-zmax b))))

; corners : Cuboid -> [List (list x y z)]
(define (corners cuboid)
  (define (loop accs)
    (if (empty? (rest accs)) (list (list ((caar accs) cuboid))
                                   (list ((cadar accs) cuboid)))
        (foldr (λ [point list] (cons (cons ((caar accs) cuboid) point)
                                     (cons (cons ((cadar accs) cuboid) point) list)))
               '() (loop (rest accs)))))
    (loop (list (list cuboid-xmin cuboid-xmax) (list cuboid-ymin cuboid-ymax) (list cuboid-zmin cuboid-zmax))))

; contains-point? : Cuboid [List X Y Z] -> Boolean
(define (contains-point cuboid point)
  (apply (λ [x y z] (and (>= x (cuboid-xmin cuboid))
                         (<= x (cuboid-xmax cuboid))
                         (>= y (cuboid-ymin cuboid))
                         (<= y (cuboid-ymax cuboid))
                         (>= z (cuboid-zmin cuboid))
                         (<= z (cuboid-zmax cuboid))))
         point))
           
; split : Cuboid Cuboid -> [List Cuboid]
; Breaks the first cuboid into mulitiple pieces. The resulting pieces do not intersect the second cuboid.

#;(module+ main
  (let ([steps (read-steps)])
    steps))
