#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(PrimN p es)
     (match (interp*-env es r)
       ['err 'err]
       [vs (interp-primN p vs)])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    [(Cond cs e) (interp-cond cs (interp-env e r) r)]
    [(Case ev cs el) (interp-case (interp-env ev r) cs (interp-env el r) r)]
    [(Let xs es body)
     (if (eq? (interp*-env es r) 'err)
         'err
         (interp-env body (ext-multiple r xs (interp*-env es r))))]
    [(Let* xs es e) (interp-let* xs es e r)]))

;; Let* -> Answer
(define (interp-let* xs es e r)
  (match xs
    ['() (interp-env e r)]
    [(cons x rest)
     (match (interp-env (car es) r)
       ['err 'err]
       [v (interp-let* rest (cdr es) e (ext r x v))])]))


;; [Listof Id] [Listof Value] -> Env 
(define (ext-multiple r xs vs)
  (match xs
    ['() (if (eq? vs '()) r 'err)]
    [(cons y ys)
     (match vs
      ['() 'err]
      [(cons v vss) (ext-multiple (ext r y v) ys vss)])]))

;; Cond -> Answer
(define (interp-cond cs e r)
  (match cs
    [(cons (Clause e1 e2) xs)
     (match (interp-env e1 r)
       ['err 'err]
       [v (if v (interp-env e2 r) (interp-cond xs e r))])]
    ['() e]))

;; Case -> Answer
(define (interp-case e xs el r)
  (match xs
    [(cons (Clause cs expr) ys)  (if (eq? (memq e cs) #f) (interp-case e ys el r) (interp-env expr r))]
    ['() el]))

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    ['() (error "undefined variable:" x)]
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))
