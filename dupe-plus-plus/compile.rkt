#lang racket
(provide compile
         compile-e)

(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast a86/registers)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit d) (compile-value d)]
    [(Prim1 p e) (compile-prim1 p e)]
    [(If e1 e2 e3)
     (compile-if e1 e2 e3)]
    [(Cond eqs eas el) (compile-cond eqs eas el)]
    [(Case e ds es el)   (let ((case_end (gensym 'end)))
                           (seq(compile-e e) ;; puts the matching expression in rax
                               (compile-case ds es el case_end) ;; gonna hand in the symbol so I can use it in the function to end
                           (Label case_end)))]))

;; Expr Listof(Listof datum) Listof Expr Expr
(define (compile-case ds es el case_end)
    (match ds
    ['() (compile-e el)]
      [(cons list_of_datum rest)(match es ;;grabs first list of Datum
                                ['() (compile-e el)]
                                [(cons express tail) (let ((l1 (gensym 'case)) ;;grabs matching expr
                                 (l2 (gensym 'case)))
                                 (letrec ([datum_loop (lambda (ds2) ;;recurses through the list of datum
                                                        (match ds2
                                                          ['() (Jmp l2)] ;; jump to l2 (meaning the last value in the datum list is used) so we need to jump to l2 to  
                                                          [(cons n t2)
                                                           (seq
                                                            (Mov r9 (value->bits n)) ;; puts n in r9 ;;tracing notes* 4 should go into r9
                                                            (Cmp rax r9) ;; matches e with n ;;tracing notes* 4 = 4
                                                            (Je l1) ;;tracing notes*  we jump to l1
                                                            (datum_loop t2))]))])
                                  (seq
                                      (datum_loop list_of_datum)
                                      (Jmp l2) ;;go straight to the next pair
                                      (Label l1) ;;this is the, I found a match brance ;;tracing notes* 4 should go into r9
                                      (compile-e express) ;; compile the expression associated with this list of thing and should put the result of expr in rax ;;tracing notes* then this is gonnnnaaaa beeeeee 1
                                      (Jmp case_end) ;; then jumps to case_end ;;tracing notes* we leave and go to the end outside of this function
                                      (Label l2) ;; this is the no match branch
                                      (compile-case rest tail el case_end) ;; goes to the next listof Datum and expr pair
                                      )))])]))

;; Listof Expr Listof Expr Expr -> Asm
(define (compile-cond qs as el)
  (match qs
    ['() (compile-e el)]
    [(cons h1 t1)
     (match as
       ['() (compile-e el)]
       [(cons h2 t2) (compile-if h1 h2 (Cond t1 t2 el))])]))


;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

