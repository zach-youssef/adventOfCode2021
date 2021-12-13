#lang racket

(require "../day13.rkt")
(require 2htdp/image)
(require 2htdp/universe)

; draw-dots : [Set (Number, Number)] -> Image
(define (draw-dots dots)
  (define WINDOW-SIZE 1000)
  (define DOT-SIZE (/ WINDOW-SIZE (add1 (max (apply max (set-map dots car)) (apply max (set-map dots cdr))))))
  (for/fold [(image (rectangle WINDOW-SIZE WINDOW-SIZE 'solid 'white))]
            [(dot dots)]
    (values (place-image (rectangle DOT-SIZE DOT-SIZE 'solid 'black)
                         (+ (* DOT-SIZE (car dot)) (/ DOT-SIZE 2))
                         (+ (* DOT-SIZE (cdr dot)) (/ DOT-SIZE 2))
                         image))))

; interpolate-fold : [Set (Number, Number)] (list String Number) Number -> [List [Set Number]]
; Calculates a list of "frames" of dot positions
(define (interpolate-fold dots fold num-frames)
  (let [(delta-map (for/fold [(delta (hash))]
                             [(dot dots)]
                     (hash-set delta dot (match fold 
                                           [(list "x" x) (if (> (car dot) x)
                                                             (list (/ (* 2 (- x (car dot))) num-frames) 0)
                                                             (list 0 0))]
                                           [(list "y" y) (if (> (cdr dot) y)
                                                             (list 0 (/ (* 2 (- y (cdr dot))) num-frames))
                                                             (list 0 0))]))))]
    (for/list [(i (add1 num-frames))]
      (list->set (set-map dots (λ [dot] (apply (λ [dx dy] (cons (+ (* dx i) (car dot))
                                                                (+ (* dy i) (cdr dot))))
                                               (hash-ref delta-map dot))))))))

(struct state [dots folds frames])

(module+ main
  (big-bang (state (read-dotmap) (read-folds) '())
    [on-tick (λ [s] (cond
                      [(and (empty? (state-frames s)) (cons? (state-folds s))) (state (state-dots s)
                                                                                      (rest (state-folds s))
                                                                                      (interpolate-fold (state-dots s)
                                                                                                        (first (state-folds s))
                                                                                                        30))]
                      [(cons? (state-frames s)) (state (first (state-frames s))
                                                       (state-folds s)
                                                       (rest (state-frames s)))]
                      [else s]))]
    [to-draw (λ [s] (draw-dots (state-dots s)))]))