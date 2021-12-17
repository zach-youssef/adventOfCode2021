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


(define (best-height xmin xmax ymin ymax)
  (for/fold ([best-height 0]
              [best-v 0])
             ([v 500])
    (let ([height (max-height v)])
      (if (and (> height best-height)
               (hits-target? v ymin ymax))
          (values height v)
          (values best-height best-v)))))        

#;(module+ main
  (apply best-height (read-target-area)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (hits-target-2? vx vy xmin xmax ymin ymax)
  (define (acc last-x last-y vx vy)
    (let ([x (+ last-x vx)]
          [y (+ last-y vy)])
      (cond
        [(and (>= x xmin)
              (<= x xmax)
              (>= y ymin)
              (<= y ymax)) #t] ; we hit the target
        [(or (and (= vx 0)
                  (or (< x xmin)
                      (> x xmax)))
             (< y ymin)) #f] ; we won't hit the target
        [else (acc x y (if (zero? vx) 0 (sub1 vx)) (sub1 vy))]))) ; we might hit the target
  (acc 0 0 vx vy))

(define (count-hit-target xmin xmax ymin ymax)
  (for*/fold ([valid 0])
             ([vx (add1 xmax)]
              [vy (range (sub1 ymin) 101)]) ; using 101 since we know 100 is the highest that actually hits the target
    (if (hits-target-2? vx vy xmin xmax ymin ymax)
        (add1 valid)
        valid)))

(module+ main
  (apply count-hit-target (read-target-area)))