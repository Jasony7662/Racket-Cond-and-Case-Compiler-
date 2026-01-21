#lang racket
(provide test)
(require rackunit)

(define (test run)
  (begin ;; Abscond
    (check-equal? (run 7) 7)
    (check-equal? (run -8) -8))

  (begin ;; Blackmail
    (check-equal? (run '(add1 (add1 7))) 9)
    (check-equal? (run '(add1 (sub1 7))) 7))

  (begin ;; Con
    (check-equal? (run '(if (zero? 0) 1 2)) 1)
    (check-equal? (run '(if (zero? 1) 1 2)) 2)
    (check-equal? (run '(if (zero? -7) 1 2)) 2)
    (check-equal? (run '(if (zero? 0)
                            (if (zero? 1) 1 2)
                            7))
                  2)
    (check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                            (if (zero? 1) 1 2)
                            7))
                  7))

  (begin ;; Dupe
    (check-equal? (run #t) #t)
    (check-equal? (run #f) #f)
    (check-equal? (run '(if #t 1 2)) 1)
    (check-equal? (run '(if #f 1 2)) 2)
    (check-equal? (run '(if 0 1 2)) 1)
    (check-equal? (run '(if #t 3 4)) 3)
    (check-equal? (run '(if #f 3 4)) 4)
    (check-equal? (run '(if  0 3 4)) 3)
    (check-equal? (run '(zero? 4)) #f)
    (check-equal? (run '(zero? 0)) #t))

 (begin ;;dupe++
       ;; Prim Tests
    (check-equal? (run '(abs -2)) 2)
    (check-equal? (run '(abs 12)) 12)
    (check-equal? (run '(- -3)) 3)
    (check-equal? (run '(- 6)) -6)
    (check-equal? (run '(not #f)) #t)
    (check-equal? (run '(not #t)) #f)
    (check-equal? (run '(not 1)) #f)
    (check-equal? (run '(if (not(not #f)) (abs -6) (- -6))) 6)
    (check-equal? (run '(- (abs -6))) -6)
    (check-equal? (run '(abs (- 6))) 6)
    (check-equal? (run '(abs (add1(add1 -5)))) 3)
    (check-equal? (run '(- (sub1 (sub1 (abs -3))))) -1)
    (check-equal? (run '(if (zero? 0) (not(not (zero? 3))) #t)) #f)
    ;;Cond Test
    (check-equal? (run '(cond [(zero? 0) 1] [else 2])) 1)
    (check-equal? (run '(cond [(zero? 1) 1] [else 2])) 2)
    (check-equal? (run '(cond [(not #t) 3] [7 4] [else 5])) 4)
    (check-equal? (run '(cond [(not #t) 3] [7 (abs -7)] [else 5])) 7)
    (check-equal? (run '(cond [(not (not #t)) 3] [7 #f] [else #t])) 3)
    (check-equal? (run '(cond [(not (not #f)) 3] [#f #f] [else #t])) #t)
    (check-equal? (run '(if(cond [(not (not #t)) 3] [7 #f] [else #t])5 6)) 5)
    ;;case test
    (check-equal? (run '(case (add1 3) [else 2])) 2)
    (check-equal? (run '(case 4 [(4) 1] [else 2])) 1)
    (check-equal? (run '(case 7 [(4 5 6) 1] [else 2])) 2)
    (check-equal? (run '(case (not #t) [(4 5 6) 1] [(#t #f) (not(not #f))] [else 2])) #f)
    (check-equal? (run '(case (- -5) [(4 5 6) (add1 7)] [else 2])) 8)
    (check-equal? (run '(case (not 1) [(4 5 6) 1] [(#t #f) (not(not #f))] [else 2])) #f)
    (check-equal? (run '(case #t [(4 5 6) 1] [(#t #f) 7] [else 2])) 7)))