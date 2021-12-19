#lang racket

; Brute force to "merge" data from scanners n and n+1:
; For each possible "orientation" of scanner n+1
;     For each possible pairing of beacons from n and n+1
;        If 12 or more pairs have the same dx, dy, and dz
;            "Merge" the datasets by applying that d(x,y,z) to each beacon and scanner in n+1
;        Else
;            Keep looping

(struct dataset [beacons scanners])
; A Position is a (list Int Int Int)
; Representing an X, Y, Z coordinate

; A Dataset is a (dataset [Set Position] [Set Position])
; Representing beacons and scanners on the same coordinate system

(define (flip f) (λ [lon] (- (f lon))))

(define (flip-perms lof)
  (if (empty? (rest lof)) (list (list (first lof)) (list (λ [lon] (- ((first lof) lon)))))
      (append (map (λ [llof] (cons (first lof) llof)) (flip-perms (rest lof)))
              (map (λ [llof] (cons (flip (first lof)) llof)) (flip-perms (rest lof))))))

(define ALL-ORIENTATIONS (foldr (λ [l ll] (append (flip-perms l) ll))
                                '()
                                (list (list first second third)
                                      (list first third second)
                                      (list second first third)
                                      (list second third first)
                                      (list third first second)
                                      (list third second first))))

(define (apply-orient orientation pos)
  (map (λ [f] (f pos)) orientation))

(define (merge d1 d2)
  (for/or ([orientation ALL-ORIENTATIONS])
    (let* ([rotate (λ [pos] (apply-orient orientation pos))]
          [d-rotate (dataset (list->set (set-map (dataset-beacons d2) rotate))
                             (list->set (set-map (dataset-scanners d2) rotate)))]
          [displacement (find-displacement d1 d-rotate)])
      (and displacement
           (dataset (set-union (dataset-beacons d1)
                               (list->set (set-map (dataset-beacons d-rotate) (λ [b] (apply-displacement b displacement)))))
                    (set-union (dataset-scanners d1)
                               (list->set (set-map (dataset-scanners d-rotate) (λ [s] (apply-displacement s displacement))))))))))

(define (find-displacement d1 d2)
  #f)

(define (apply-displacement pos displacement) #f)

