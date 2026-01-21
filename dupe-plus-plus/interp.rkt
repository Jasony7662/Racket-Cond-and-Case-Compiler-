#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; Expr -> Value
(define (interp e)
  (match e
    [(Lit d) d]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    [(Cond eqs eas el) (interp-cond eqs eas el)]
    
    [(Case e ds es el) (interp-case (interp e) ds es el)]))


(define (interp-cond qs as e)
 (match qs
   ['() (interp e)]
   [(cons h t) (match as
                 ['() (interp e)]
                 [(cons h2 t2)(if
                               (interp h)
                               (interp h2)
                               (interp-cond t t2 e))])]))

(define (interp-case e ds es el)
  (match ds
    ['() (interp el)]
    [(cons list_of_datum rest)(match es
                                ['() (interp el)]
                                [(cons express tail)
                                 (letrec ([datum_loop (lambda (ds2)
                                                        (match ds2
                                                          ['() (interp-case e rest tail el)]
                                                          [(cons n t2)(if (equal? e n)(interp express)(datum_loop t2))]))])
                                  (datum_loop list_of_datum))])]))


