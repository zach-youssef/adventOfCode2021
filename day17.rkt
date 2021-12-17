#lang racket

; We are looking for highest - so we can look at just y and know that its both positive, and can increase only by 1!
; So we can start at y = 0, see if it gets into the Y range, and find where it is optimal
; X is irrelevant for part 1


(define (max-height v)
  (define (acc last v)
    (let ([cur (+ last v)])
      (if (< cur last)
          last
          (acc cur (sub1 v)))))
  (acc 0 v))

(define (all-heights v t)
  (for/fold ([path '(0)]
             [pos 0]
             [v v]
             #:result (reverse path))
            ([i t])
    (let ([new-pos (+ pos v)])
      (values (cons new-pos path)
              new-pos
              (sub1 v)))))

(define (all-horiz v t)
  (for/fold ([path '(0)]
             [pos 0]
             [v v]
             #:result (reverse path))
            ([i t])
    (let ([new-pos (+ pos v)])
      (values (cons new-pos path)
              new-pos
              (if (zero? v) v (sub1 v))))))

; We know y-velocity can't go higher when :
; step n height > target max
; and step n+1 height < target min (we passed the area, too fast)
(define (hits-target? v target-min target-max)
  (define (acc last-h v)
    (let ([h (+ last-h v)])
      (cond
        [(and (> last-h target-max) (<= h target-max)) (>= h target-min)]
        [else (acc h (sub1 v))])))
  (acc 0 v))

; read-target-area : -> [List Integer]
(define (read-target-area)
  (map string->number
       (string-split (string-replace
                      (string-replace (read-line)
                                      "target area: x=" "")
                      ", y=" "..") "..")))


; Ugly brute force because I don't get why my thing doesnt work
(define (best-height xmin xmax ymin ymax)
  (for/fold ([best-height 0]
              [best-v 0])
             ([v 500])
    (let ([height (max-height v)])
      (if (and (> height best-height)
               (hits-target? v ymin ymax))
          (values height v)
          (values best-height best-v)))))        

(module+ main
  (apply best-height (read-target-area)))
