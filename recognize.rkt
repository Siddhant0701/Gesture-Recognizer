;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Siddhant Mahajan (20889590)
;; CS 135 Fall 2020
;; Assignment 04, Problem 03
;; ***************************************************
;;

;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary
;;    (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than 
;;             the respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;; *********************
;; Problem 3a
;; *********************



;; Problem 3ai


;; (get-x pnt) produces the x-coordinate of a Point
;; Examples:
(check-expect (get-x (list 2 3)) 2)
(check-expect (get-x (list 7 1)) 7)

;; get-x: Point -> Num
(define (get-x pnt)
  (first pnt))


;; (get-y pnt) produces the y-coordinate of a Point
;; Examples:
(check-expect (get-y (list 2 3)) 3)
(check-expect (get-y (list 7 1)) 1)

;; get-y: Point -> Num
(define (get-y pnt)
  (second pnt))



;; Problem 3aii

;; Test Gestures
(define g1 (list (list 1 2) (list 3 4) (list 5 6)))
(define g2 (list (list 7 1) (list 1 4) (list 3 2)))
(define mygest (list (list 100 0) (list 200 100) (list 100 200)
                     (list 0 100) (list 100 50)))



;; (translate-gesture ges x-offset y-offset) produces a translated 
;;      Gesture after applying translations, x-offset and y-offset to
;;      each Point in a Gesture, ges

;; Examples:
(check-expect (translate-gesture g1 1 1)
              (list (list 2 3) (list 4 5) (list 6 7)))
(check-expect (translate-gesture g2 2 -2)
              (list (list 9 -1) (list 3 2) (list 5 0)))


;; translate-gesture: Gesture -> Gesture
(define (translate-gesture ges x-offset y-offset)
  (cond [(empty? ges) empty]
        [(cons? ges) (cons
                      (cons (+ (get-x (first ges)) x-offset)
                            (cons (+ (get-y (first ges)) y-offset)
                                  empty))
                      (translate-gesture (rest ges)
                                         x-offset y-offset))]))


;; Problem 3aiii


;; (scale-gesture ges x-scale y-scale) produces a scaled 
;;      Gesture after applying the individual scales, x-scale and
;;      y-scale to each Point in a Gesture, ges

;; Examples:
(check-expect (scale-gesture g1 2 1)
              (list (list 2 2) (list 6 4) (list 10 6)))
(check-expect (scale-gesture g2 3 2)
              (list (list 21 2) (list 3 8) (list 9 4)))


;; scale-gesture: Gesture -> Gesture
;; Requires:      x-scale > 0
;;                y-scale > 0
(define (scale-gesture ges x-scale y-scale)
  (cond [(empty? ges) empty]
        [(cons? ges) (cons
                      (cons (* (get-x (first ges)) x-scale)
                            (cons (* (get-y (first ges)) y-scale)
                                  empty))
                      (scale-gesture (rest ges)
                                     x-scale y-scale))]))



;; Problem 3aiv


;; (get-b-box ges) produces the BoundingBox for a Gesture, ges
;; Examples:
(check-expect (get-b-box g1) (list (list 1 2) (list 5 6)))
(check-expect (get-b-box g2) (list (list 1 1) (list 7 4)))


;; get-b-box: Gesture -> BoundingBox
;; Requires: ges is non-empty
(define (get-b-box ges)
  (list (list (min-x ges) (min-y ges))
        (list (max-x ges) (max-y ges))))





;; (min-x ges) produces the lowest x-coordinate in a Gesture
;; Examples:
(check-expect (min-x g1) 1)
(check-expect (min-x g2) 1)

;; min-x: ne-Gesture -> Num
(define (min-x ges)
  (cond [(empty? (rest ges)) (get-x (first ges))]
        [else (min (get-x (first ges)) 
                   (min-x (rest ges)))]))




;; (min-y ges) produces the lowest y-coordinate in a Gesture
;; Examples:
(check-expect (min-y g1) 2)
(check-expect (min-y g2) 1)

;; min-y: ne-Gesture -> Num
(define (min-y ges)
  (cond [(empty? (rest ges)) (get-y (first ges))]
        [else (min (get-y (first ges)) 
                   (min-y (rest ges)))]))



;; (max-x ges) produces the highest x-coordinate in a Gesture
;; Examples:
(check-expect (max-x g1) 5)
(check-expect (max-x g2) 7)

;; max-x: ne-Gesture -> Num
(define (max-x ges)
  (cond [(empty? (rest ges)) (get-x (first ges))]
        [else (max (get-x (first ges)) 
                   (max-x (rest ges)))]))



;; (max-y ges) produces the highest y-coordinate in a Gesture
;; Examples:
(check-expect (max-y g1) 6)
(check-expect (max-y g2) 4)

;; max-y: ne-Gesture -> Num
(define (max-y ges)
  (cond [(empty? (rest ges)) (get-y (first ges))]
        [else (max (get-y (first ges)) 
                   (max-y (rest ges)))]))






;; *********************
;; Problem 3b
;; *********************


;; Problem 3bi

;; (gesture-length ges) produces the length of a given Gesture, ges
;; Examples:
(check-expect (gesture-length empty) 0)
(check-expect (gesture-length (list (list 1 2))) 0)
(check-within (gesture-length (list (list 2 7) (list 3 9))) 2.23 0.01)


;; gesture-length: Gesture -> Num
(define (gesture-length ges)
  (cond [(or (empty? ges) (empty? (rest ges))) 0]
        [(cons? ges) (+ (sqrt
                         (+ (sqr (- (get-x (first ges))
                                    (get-x (first (rest ges))))) 
                            (sqr (- (get-y (first ges))
                                    (get-y (first (rest ges)))))))
                        (gesture-length (rest ges)))]))

;; Tests:
(check-within (gesture-length g1) 5.65 0.01)
(check-within (gesture-length (list (list 1 0) (list 2 0) (list 3 0)))
              2 0.01)
(check-within (gesture-length (list (list 1 1) (list 1 2) (list 1 3)))
              2 0.01)
(check-within (gesture-length g2) 9.54 0.01)





;; Problem 3bii


;; (get-points g lon) produces a sub Gesture from a given Gesture, g 
;;      and indexes from a list of numbers, lon
;; Examples:
(check-expect (get-points g1 empty) empty)
(check-expect (get-points g1 (list 0 2)) (list (list 1 2) (list 5 6)))

;; get-points: Gesture (listof Nat) -> Gesture
;; Requires: (length g) > (Maximum value in lon)
;;            g is non-empty
(define (get-points g lon)
  (cond [(empty? lon) empty]
        [(= (first lon) 0)
         (cons (first g) (get-points g (rest lon)))]
        [else (get-points (rest g) (sub1-list lon))]))

;; Tests:
(check-expect (get-points mygest (list 0 0 2 4 4))
              (list (list 100 0) (list 100 0) (list 100 200)
                    (list 100 50) (list 100 50)))
(check-expect (get-points (list (list 1 1) (list 2 2)) (list 0 0 0 0))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (get-points (list (list 1 1) (list 2 2) (list 3 3))
                          (list 0 1 2))
              (list (list 1 1) (list 2 2) (list 3 3)))



;; (sub1-list lon) produces a list of numbers from a given list of
;;      numbers, lon where each element is one less than the element
;;      in the original list
;; Examples:
(check-expect (sub1-list empty) empty)
(check-expect (sub1-list (list 2 3)) (list 1 2))

;; sub1-list:  (listof Num) -> (listof Num)
(define (sub1-list lon)
  (cond [(empty? lon) empty]
        [(cons? lon) (cons (sub1 (first lon))
                           (sub1-list (rest lon)))]))

;; Tests:
(check-expect (sub1-list (list 3 7 10 9)) (list 2 6 9 8))
(check-expect (sub1-list (list 0 -1)) (list -1 -2))
(check-expect (sub1-list (list 2.1 3.1)) (list 1.1 2.1))







;; *********************
;; Problem 3c
;; *********************


;; Problem 3ci

;; (five-sample gesture) produces a sampling of gesture which has 5 
;;      points: the first, n/4th, n/2th, 3n/4th, and last point where
;;      n is the number of points in gesture
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1)
                    (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)
                                 (list 5 5) (list 6 6)
                                 (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5)
                    (list 7 7) (list 8 8)))



;; five-sample: Gesture -> Gesture
;; Requires: gesture is non-empty
(define (five-sample gesture)
  (get-points gesture (list 0 (floor (* 0.25 (length gesture)))
                            (floor (* 0.5 (length gesture)))
                            (floor (* 0.75 (length gesture)))
                            (sub1 (length gesture)))))



;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2)
                                 (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3)
                    (list 4 4) (list 4 4)))

(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1)
                    (list 1 1) (list 1 1)))

(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3)
                                 (list 4 4) (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3)
                    (list 4 4) (list 5 5)))

(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) 
                                 (list 4 4) (list 5 5) (list 6 6)
                                 (list 7 7)))
              (list (list 1 1) (list 2 2) (list 4 4)
                    (list 6 6) (list 7 7)))




;; Problem 3cii

;; (move-and-scale gesture x-scale y-scale) moves gesture to (0,0) 
;;      and scales it by (x-scale) and (y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1)
              (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; Requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture
   (translate-gesture gesture
                      (- (get-x (first (get-b-box gesture))))
                      (- (get-y (first (get-b-box gesture)))))
   x-scale y-scale))



;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))

(check-expect (move-and-scale (list (list 0 0) (list 10 8)) 7 4)
              (list (list 0 0) (list 70 32)))





;; Problem 3ciii

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;; (normalize-gesture gesture) normalizes gesture to (0,0) and a
;;      standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50)
                                       (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; Requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
  (cond [(< (width gesture) min-width)
         (move-and-scale gesture 1 (/ norm-size (height gesture)))]
        [(< (height gesture) min-height)
         (move-and-scale gesture (/ norm-size (width gesture)) 1)]
        [else
         (move-and-scale gesture (/ norm-size (width gesture))
                         (/ norm-size (height gesture)))]))



;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)






;; (width ges) produces the width of a gesture, ges
;; Examples:
(check-expect (width g1) 4)
(check-expect (width g2) 6)

;; width: Getsure -> Num
;; Requires: ges is not empty
(define (width ges)
  (- (get-x (second (get-b-box ges)))
     (get-x (first (get-b-box ges)))))

;; Tests:
(check-expect (width (list (list 1 0) (list 2 0) (list 3 0))) 2)
(check-expect (width (list (list 0 0) (list 0 1) (list 0 2))) 0)





;; (height ges) produces the height of a gesture, ges
;; Examples:
(check-expect (height g1) 4)
(check-expect (height g2) 3)

;; height: Getsure -> Num
;; Requires: ges is not empty
(define (height ges)
  (- (get-y (second (get-b-box ges)))
     (get-y (first (get-b-box ges)))))

;; Tests:
(check-expect (height (list (list 1 0) (list 2 0) (list 3 0))) 0)
(check-expect (height (list (list 0 0) (list 0 1) (list 0 2))) 2)






;; Problem 3civ

;; (geometric-5match gesture1 gesture2) produces the average distance
;;      between points in sub-sampled gesture1 and gesture2 after
;;      sub-sampling them with 5 points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40)))
              16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; Requires: gesture1 and gesture2 are each not both vertical
;;           and horizontal
;;
;;           gesture1 and gesture2 are both not empty        
(define (geometric-5match gesture1 gesture2)
  (* 1/5
     (sum-of-distances (normalize-gesture (five-sample gesture1))
                       (normalize-gesture (five-sample gesture2)))))


;; Tests:
(check-within (geometric-5match (second (fourth templates))
                                (second (fourth templates))) 0 0.1)




;; (sum-of-distances ges1 ges2) produces the sum of distances between
;;      the corresponding points of two gestures, ges1 and ges2
;; Examples:
(check-within (sum-of-distances g1 g2) 12.55 0.01)

;; sum-of-distances: Gesture Gesture -> Num
(define (sum-of-distances ges1 ges2)
  (cond[(empty? ges1) 0]
       [else (+ (gesture-length (list (first ges1) (first ges2)))
                (sum-of-distances (rest ges1) (rest ges2)))]))

;; Tests:
(check-within (sum-of-distances (list (list 0 0) (list 1 2))
                                (list (list 1 1) (list 3 5)))
              5.02 0.01)

(check-within (sum-of-distances (list (list 0 1) (list 0 2))
                                (list (list 1 0) (list 2 0)))
              4.24 0.01)

(check-within (sum-of-distances (list (list 0 1) (list 0 2))
                                (list (list 0 5) (list 0 8)))
              10 0.01)

(check-within (sum-of-distances (list (list 9 1) (list 10 1))
                                (list (list 1 0) (list 2 0)))
              16.12 0.01)




;; Problem 3cv

;; (five-point-rec candidate template-library) produces the symbol in
;;      template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;; five-point-rec: Gesture TL -> Sym
;; Requires: candidate is not both vertical and horizontal
;;           candidate is not empty
(define (five-point-rec candidate template-library)
  (first (min-element (distance-list candidate template-library))))


;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)




;; (distance-list ges tl) produces a list of the symbol and
;;      the average distance between points of a Gesture, ges and
;;      all the gestures in a TemplateLibrary, tl
;; Examples:
(check-within (distance-list g1 (list (list 'g1 g1)))
              (list (list 'g1 0)) 0.01)
(check-within (distance-list g1 (list (list 'g2 g2)))
              (list (list 'g2 75.74)) 0.01)



;; distance-list: Gesture TL -> (listOf (list Sym Num))
(define (distance-list ges tl)
  (cond [(empty? tl) empty]
        [else (cons  (cons (first (first tl))
                           (cons (geometric-5match
                                  ges
                                  (first (rest (first tl)))) empty))
                     (distance-list ges (rest tl)))]))


;; Tests:
(check-within (distance-list g1 (list (list 'g1 g1) (list 'g2 g2)))
              (list (list 'g1 0) (list 'g2 75.74)) 0.01)

(check-within (distance-list (second (first templates))
                             (list (first templates)
                                   (second templates)))
              (list (list 'a 0) (list 'b 133.25)) 0.01)







;; (min-element lst) produces the element of the list, lst which has
;;      the lowest numeric element
;; Examples:
(check-expect (min-element  (list (list 'g1 0))) (list 'g1 0))
(check-expect (min-element  (list (list 'g2 15.875)))
              (list 'g2 15.875))


;; min-element: (ne-listof (list Sym Num)) -> (list Sym Num)
(define (min-element lst)
  (cond[(empty? (rest lst)) (first lst)]
       [(< (second (first lst)) (second (second lst)))
        (min-element (cons (first lst) (rest (rest lst))))]
       [(> (second (first lst)) (second (second lst)))
        (min-element (rest lst))]))


;; Tests:
(check-expect (min-element  (list (list 'g2 15.875) (list 'g1 0)))
              (list 'g1 0))
(check-expect (min-element  (list (list 'a 12) (list 'b 3)))
              (list 'b 3))
(check-expect (min-element  (list (list 'a 3) (list 'b 4)))
              (list 'a 3))
(check-expect (min-element  (list (list 'a 12) (list 'b 3)
                                  (list 'c 1) (list 'd 0.1)))
              (list 'd 0.1))




;; *********************
;; Problem 3d
;; *********************

;; (sub-sample gesture k) produces a sampling of a Gesture, gesture
;;      with k points chosen at equal intervals from the total points.
;; Examples:
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 3)
              (list (list 1 1) (list 2 2) (list 2 2)))

(check-expect (sub-sample (list (list 1 1) (list 2 2)
                                (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6)
                                (list 7 7) (list 8 8)) 4)
              (list (list 1 1) (list 3 3)
                    (list 6 6) (list 8 8)))



;; sub-sample: Gesture -> Gesture
;; Requires: gesture is non-empty
;;              k > 2
(define (sub-sample gesture k)
  (get-points gesture (sampling-selection k 0 (length gesture))))



;; Tests:
(check-expect (sub-sample (list (list 1 3) (list 5 7) (list 8 11)
                                (list 6 1) (list  4 1))3)
              (list (list 1 3) (list 8 11) (list 4 1)))

(check-expect (sub-sample  (list (list 1 3) (list 5 7) (list 8 11)
                                 (list 6 1) (list  4 1)) 5)
              (list (list 1 3) (list 5 7) (list 8 11)
                    (list 6 1) (list  4 1)))

(check-expect (sub-sample  (list (list 1 3) (list 5 7) (list 8 11)
                                 (list 6 1) (list  4 1)) 4)
              (list (list 1 3) (list 5 7) (list 6 1) (list 4 1)))

(check-expect (sub-sample (list (list 1 1) (list 2 2)
                                (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6)) 5)
              (list (list 1 1) (list 2 2) (list 4 4) 
                    (list 5 5) (list 6 6)))






;; (sampling-selection k temp len) produces a list of distributed
;;      (k-temp) indexes that are required to create a sample of a
;;      gesture of length, len.
;; Examples:
(check-expect (sampling-selection 10 0 10)
              (list 0 1 2 3 4 5 6 7 8 9))
(check-expect (sampling-selection 7 0 15)
              (list 0 2 5 7 10 12 14))


;; sampling-selection: Nat Nat Nat -> (listof Nat)
;; Requires: k > 2
(define (sampling-selection k temp len)
  (cond [(= temp (sub1 k)) (cons (sub1 len) empty)]
        [else (cons (floor (* (/ temp (- k 1)) len))
                    (sampling-selection k (add1 temp) len))]))


;; Tests:
(check-expect (sampling-selection 5 0 3)
              (list 0 0 1 2 2))
(check-expect (sampling-selection 6 0 10)
              (list 0 2 4 6 8 9))
(check-expect (sampling-selection 7 0 10)
              (list 0 1 3 5 6 8 9))
(check-expect (sampling-selection 4 0 7)
              (list 0 2 4 6))
(check-expect (sampling-selection 5 0 1)
              (list 0 0 0 0 0))






;; (geometric-match gesture1 gesture2 k) produces the average distance
;;      between points in sub-sampled gesture1 and gesture2 after
;;      sub-sampling them with k points
;; Examples:

(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40)) 3)
              8.98 0.01)

(check-within (geometric-match (second (fourth templates))
                               (second (fourth templates)) 12)
              0 0.01)


;; geometric-match: Gesture Gesture Nat -> Num
;; Requires: gesture1 and gesture2 are each not both vertical
;;           and horizontal
;;
;;           gesture1 and gesture 2 are both not empty
;;
;;           k > 2
         
(define (geometric-match gesture1 gesture2 k)
  (* (/ 1 k)
     (sum-of-distances (normalize-gesture
                        (sub-sample gesture1 k))
                       (normalize-gesture
                        (sub-sample gesture2 k)))))









;; (k-point-rec candidate template-library k) produces the symbol in
;;      template-library closest to candidate
;; Examples:
(check-expect (k-point-rec testd templates 7) 'd)
(check-expect (k-point-rec testk templates 3) 'k)
(check-expect (k-point-rec testa templates 50) 'a)


;; k-point-rec: Gesture TL Nat -> Sym
;; Requires: candidate is not both vertical and horizontal
;;           candidate is not empty
;;           k > 2
(define (k-point-rec candidate template-library k)
  (first (min-element
          (k-distance-list candidate template-library k))))






;; (k-distance-list ges tl k) produces a list of the symbol and
;;      the average distance between points of a Gesture, ges and
;;      all the gestures in a TemplateLibrary, tl
;; Examples:
(check-within (k-distance-list g1 (list (list 'g1 g1)) 6)
              (list (list 'g1 0)) 0.01)
(check-within (k-distance-list g1 (list (list 'g2 g2)) 20)
              (list (list 'g2 78.78)) 0.01)
(check-within (k-distance-list g1 empty 20)
              empty 0.01)



;; k-distance-list: Gesture TL Nat-> (listOf (list Sym Num))
;; Requires:     k > 2
(define (k-distance-list ges tl k)
  (cond [(empty? tl) empty]
        [else (cons (cons (first (first tl))
                          (cons (geometric-match
                                 ges
                                 (second (first tl)) k) empty))
                    (k-distance-list ges (rest tl) k))]))


;; Tests:
(check-within (k-distance-list g1 (list (list 'g1 g1) (list 'g2 g2))
                               12)
              (list (list 'g1 0) (list 'g2 79.79)) 0.01)

(check-within (k-distance-list (second (first templates))
                               (list (first templates)
                                     (second templates)) 3)
              (list (list 'a 0) (list 'b 173.56)) 0.01)






;; *********************
;; *********************
;; Problem 4
;; *********************
;; *********************


;; (spatial-sub-sample ges1 k) produces a sub-sampled Gesture of
;;      ges with k points based on the distance between the
;;      points.
;; Examples:
(check-within (spatial-sub-sample (list (list 0 0) (list 200 200))  5)
              (list (list 0 0) (list 50 50) (list 100 100)
                    (list 150 150) (list 200 200)) 0.01)

(check-within (spatial-sub-sample (list (list 0 0) (list 50 100))  3)
              (list (list 0 0) (list 25 50) (list 50 100)) 0.01)



;; spatial-sub-sample: Gesture Nat Nat -> Gesture
;; Requires: k > 2
(define (spatial-sub-sample ges k)
  (cons (first ges) (get-spatial-sample
                     (rest ges)
                     (/ (gesture-length ges) (- k 1))
                     (first ges) (sub1 k))))


;; Tests:
(check-within (spatial-sub-sample
               (list (list 50 100) (list 250 100)
                     (list 250 300) (list 50 300) (list 50 100)) 5)
              (list (list 50 100) (list 250 100)
                    (list 250 300) (list 50 300) (list 50 100)) 0.01)

(check-within (spatial-sub-sample
               (list (list 50 100) (list 250 100)
                     (list 250 300) (list 50 300) (list 50 100)) 4)
              (list (list 50 100) (list 250 166.66)
                    (list 116.66 300) (list 50 100)) 0.01)

(check-within (spatial-sub-sample (list (list 0 0) (list 40 40)
                                        (list 80 80) (list 120 120)
                                        (list 160 160) (list 200 200))
                                  5)
              (list (list 0 0) (list 50 50) (list 100 100) 
                    (list 150 150) (list 200 200)) 0.01)

(check-within (spatial-sub-sample (list (list 0 0) (list 2 2)
                                        (list 4 0) (list 0 0)) 3)
              (list(list 0 0) (list 3.41 0.58) (list 0 0)) 0.01)

(check-within (spatial-sub-sample (list (list 0 0) (list 2 2)
                                        (list 4 0) (list 0 0)) 4)
              (list
               (list 0 0) (list 2.28 1.72)
               (list 3.22 0) (list 0 0)) 0.01)





;; (get-spatial-sample ges dss last-pnt k) produces a list of points
;;      that appear on a Gesture, ges based on the number of points
;;      remaining, k, the last point chosen, last-pnt and the distance
;;      at which sub-sampled points need to be selected, dss
;; Examples:
(check-within (get-spatial-sample (list (list 200 200))
                                  (* 50 (sqrt 2)) (list 0 0) 4)
              (list (list 50 50) (list 100 100)
                    (list 150 150) (list 200 200)) 0.01)

(check-within (get-spatial-sample (list (list 50 100))
                                  (* 25 (sqrt 5)) (list 0 0) 2)
              (list(list 25 50) (list 50 100)) 0.01)



;; get-spatial-sample: Gesture Num Point Nat -> Gesture
(define (get-spatial-sample ges dss last-pnt k)
  (cond [(= k 1) (cons (first
                        (get-points ges (list (- (length ges) 1))))
                       empty)]

        [(< dss (pnt-distance last-pnt (first ges)))
         (cons (next-pnt ges dss last-pnt)
               (get-spatial-sample ges dss
                                   (next-pnt ges dss last-pnt)
                                   (sub1 k)))]
        
        [(= dss (pnt-distance last-pnt (first ges)))
         (cons (first ges)
               (get-spatial-sample (rest ges) dss
                                   (first ges) (sub1 k)))]
        
        [(> dss (pnt-distance last-pnt (first ges)))
         (cons (next-pnt (rest ges)
                         (- dss (pnt-distance last-pnt (first ges)))
                         (first ges))
               (get-spatial-sample
                (rest ges) dss
                (next-pnt (rest ges)
                          (- dss (pnt-distance
                                  last-pnt (first ges)))
                          (first ges)) (sub1 k)))]
        [else (cons (first ges)
               (get-spatial-sample (rest ges) dss
                                   (first ges) (sub1 k)))]))


;; Tests:
(check-within (get-spatial-sample
               (list (list 250 100)
                     (list 250 300) (list 50 300) (list 50 100))
               200 (list 50 100) 4)
              (list (list 250 100) (list 250 300)
                    (list 50 300) (list 50 100)) 0.01)

(check-within (get-spatial-sample
               (list (list 250 100)
                     (list 250 300) (list 50 300) (list 50 100))
               800/3 (list 50 100) 3)
              (list (list 250 166.66)
                    (list 116.66 300) (list 50 100)) 0.01)






;; (pnt-distance pnt1 pnt2) produces the distance between two points,
;;      pnt1 and pnt2
;; Examples:
(check-within (pnt-distance (list 0 0) (list 0 0)) 0 0.01)
(check-within (pnt-distance (list 1 1) (list 1 1)) 0 0.01)
(check-within (pnt-distance (list 0 0) (list 1 1)) 1.41 0.01)
(check-within (pnt-distance (list 0 2) (list 0 4)) 2 0.01)
(check-within (pnt-distance (list 1 1) (list 6 1)) 5 0.01)

;; pnt-distance: Point Point -> Num
(define (pnt-distance pnt1 pnt2)
  (sqrt (+ (sqr (- (get-x pnt1) (get-x pnt2)))
           (sqr (- (get-y pnt1) (get-y pnt2))))))

;; Tests:
(check-within (pnt-distance (list 0 0) (list 6 8)) 10 0.01)
(check-within (pnt-distance (list 1 2) (list 3 5)) 3.61 0.01)
(check-within (pnt-distance (list 4 1) (list 2 3)) 2.82 0.01)






;; (next-pnt ges dss last-pnt) produces the next point for sampling
;;      a Gesture, ges where dss is the distance between the
;;      last Point, last-pnt and the new requires Point
;; Examples:
(check-within (next-pnt (list (list 200 200))
                        (* 100 (sqrt 2)) (list 0 0))
              (list 100 100) 0.01)

(check-within (next-pnt (list (list 6 8))
                        5 (list 0 0))
              (list 3 4) 0.01)


;; next-pnt: Gesture Num Point -> Point
(define (next-pnt ges dss last-pnt)
  (list
   (+ (get-x last-pnt)
      (* (/ dss (pnt-distance last-pnt (first ges)))
         (- (get-x (first ges))
            (get-x last-pnt))))
   (+ (get-y last-pnt)
      (* (/ dss (pnt-distance last-pnt (first ges)))
         (- (get-y (first ges))
            (get-y last-pnt))))))

;; Tests:
(check-within (next-pnt (list (list 6 8))
                        10 (list 0 0))
              (list 6 8) 0.01)
(check-within (next-pnt (list (list 2 7))
                        0 (list 0 0))
              (list 0 0) 0.01)







;; (spatial-geometric-match gesture1 gesture2 k) produces the average 
;;      distance between points in sub-sampled gesture1 
;;      and gesture2 after spatial-sub-sampling them with k points
;; Examples:

(check-within (spatial-geometric-match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40)) 7)
              0 0.01)

(check-within (spatial-geometric-match (second (fourth templates))
                                       (second (fourth templates)) 12)
              0 0.01)


;; spatial-geometric-match: Gesture Gesture Nat -> Num
;; Requires: gesture1 and gesture2 are each not both vertical
;;           and horizontal and both are not empty
;;
;;           k > 2
         
(define (spatial-geometric-match gesture1 gesture2 k)
  (* (/ 1 k)
     (sum-of-distances (normalize-gesture
                        (spatial-sub-sample gesture1 k))
                       (normalize-gesture
                        (spatial-sub-sample gesture2 k)))))





;; Template excluding letter 'c' and 'u'
(define templatesv1 (get-points templates (list 0 1 3 4 5 6 7 8
                                                       9 10 11 12 13
                                                       14 15 16 17 19
                                                       20 21 22 23 24
                                                       25)))

;; (spatial-rec candidate template-library k) produces the symbol in
;;      template-library closest to candidate
;; Examples:
(check-expect (spatial-rec testd templatesv1 5) 'd)
(check-expect (spatial-rec testk templatesv1 5) 'k)
(check-expect (spatial-rec testa templatesv1 10) 'a)

;; spatial-rec: Gesture TL Nat -> Sym
;; Requires: candidate is not both vertical and horizontal
;;           candidate is not empty
;;           k > 2
(define (spatial-rec candidate template-library k)
  (first (min-element
          (spatial-distance-list candidate template-library k))))







;; (spatial-distance-list ges tl k) produces a list of the symbol and
;;      the average distance between points of a Gesture, ges and
;;      all the gestures in a TemplateLibrary, tl
;; Examples:
(check-within (spatial-distance-list g1 (list (list 'g1 g1)) 6)
              (list (list 'g1 0)) 0.01)
(check-within (spatial-distance-list g1 (list (list 'g2 g2)) 6)
              (list (list 'g2 51.86)) 0.01)
(check-within (spatial-distance-list g1 empty 20)
              empty 0.01)



;; spatial-distance-list: Gesture TL Nat-> (listOf (list Sym Num))
;; Requires:     k > 2
(define (spatial-distance-list ges tl k)
  (cond [(empty? tl) empty]
        [else (cons (cons (first (first tl))
                          (cons (spatial-geometric-match
                                 ges
                                 (second (first tl)) k) empty))
                    (spatial-distance-list ges (rest tl) k))]))


;; Tests:
(check-within (spatial-distance-list g1 (list (list 'g1 g1)
                                              (list 'g2 g2)) 10)
              (list (list 'g1 0) (list 'g2 44.5)) 0.01)

(check-within (spatial-distance-list (second (first templates))
                                     (list (first templates)
                                           (second templates)) 10)
              (list (list 'a 0) (list 'b 106.58)) 0.01)