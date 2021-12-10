#lang racket

(require "util.rkt")

; Part 1 -----------------------------------------------------------

(struct segment [x1 y1 x2 y2] #:transparent)
 
; parse-segment : String -> Segment
(define (parse-segment line)
  (apply segment
         (map string->number
              (string-split (string-replace line " -> " ",") ","))))

; dir : Number -> Number
; Returns 1, 0, or -1 based on if the number is positive, negative, or zero
(define (dir n)
  (if (= n 0) 0 (/ n (abs n))))

; points-covered : Segment -> [List (list Nat, Nat)]
; Returns the list of integer points covered by the given line segment
; (includes the end points)
(define (points-covered seg)
  (local [(define dx (dir (- (segment-x2 seg)
                             (segment-x1 seg))))
          (define dy (dir (- (segment-y2 seg)
                             (segment-y1 seg))))
          (define (acc cur)
            (if (and (= (car cur)
                        (segment-x2 seg))
                     (= (cadr cur)
                        (segment-y2 seg)))
                (list cur)
                (cons cur (acc (list (+ dx (car cur)) (+ dy (cadr cur)))))))]
    (acc (list (segment-x1 seg) (segment-y1 seg)))))

; normal? : Segment -> Boolean
; Returns true if the line is either vertical or horizontal
(define (normal? seg)
  (or (= (segment-x1 seg)
         (segment-x2 seg))
      (= (segment-y1 seg)
         (segment-y2 seg))))

; gen-heatmap : [List Segment] -> [HashMap (list Nat, Nat) -> Nat]
; returns a map from point to how many lines cover that point
(define (gen-heatmap segs)
  (for*/fold ([map (hash)])
             ([seg segs]
              [point (points-covered seg)])
    (values (count-map-add map point))))

; danger-points : [HashMap (list Nat, Nat) -> Nat] -> [List Point]
; Returns the points in the given heatmap where 2 or more lines are present
(define (danger-points heatmap)
  (filter identity (hash-map heatmap
                             (λ [k v] (if (>= v 2) k #f)))))

#;(module+ main
  (display (length (danger-points (gen-heatmap (filter normal? (read-lines parse-segment)))))))

; Part 2
(module+ main
  (display (length (danger-points (gen-heatmap (read-lines parse-segment))))))