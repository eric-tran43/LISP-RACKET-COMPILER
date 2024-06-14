#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r9  'r9)  ; scratch
(define rbx 'rbx) ; scratch

;; type CEnv = (Listof ID)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Global 'entry)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        pad-stack
        (Call 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(PrimN p es)       (compile-primN p es c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Cond cs el)       (compile-cond cs el c)]
    [(Case ev cs el)    (compile-case ev cs el c)]
    [(Let xs es e) (compile-let xs es e c)]
    [(Let* xs es e) (compile-let* xs es e c)]))

(define (compile-let* xs es body c)
  (match (list xs es)
    [(list '() '()) (compile-e body c)] ;; no bindings
    [(list (cons x xs-rest) (cons e es-rest))
     (seq (compile-e e c)
          (Push rax)
          (compile-let* xs-rest es-rest body (cons x c))
          (Add rsp 8))]))

(define (compile-let-helper xs es body c)
    (match (list xs es)
      [(list '() '()) (compile-e body c)] ;; no bindings
      [(list (cons x xs-rest) (cons e es-rest))
       (seq (compile-e e c)
            (Push rax)
            (compile-let-helper xs-rest es-rest body (cons x c))
            (Add rsp 8))]))

;; [Listof ID] [Listof Expr] Expr CEnv -> Asm
(define (compile-let xs es body c) (compile-let-helper xs es body c))


;; OpN Expr [Listof Expr] CEnv -> Asm
(define (compile-primN p es c)
  (let ((l0 (gensym 'primNend)))
    (seq (compile-e* es c) 
         (compile-opN p es c l0)
         (Label l0))))

;; [Listof CondClause] Expr CEnv Label -> Asm
(define (compile-cond-clauses cs e c end-label)
  (match cs
    ['() (compile-e e c)]
    [(cons (Clause e1 e2) rest)
     (let ((l0 (gensym 'next)))
       (seq (compile-e e1 c)
            (Cmp rax val-false)
            (Je l0)
            (compile-e e2 c)
            (Jmp end-label)
            (Label l0)
            (compile-cond-clauses rest e c end-label)))]))

;; [Listof CondClauses] Expr CEnv -> Asm
(define (compile-cond cs e c)
  (let ((end-label (gensym 'condend)))
    (seq (compile-cond-clauses cs e c end-label)
         (Label end-label))))


;; [Listof Expr] CEnv -> Asm
(define (compile-datum-lst ds c)
  (match ds
    ['() (seq (Mov rax val-false))]
    [(cons d dss) (let ((found (gensym 'datum)))
                    (seq (Mov r9 (value->bits d))
                         (Cmp r9 rbx)
                         (Je found)
                         (compile-datum-lst dss c)
                         (Label found)))]))

(define (compile-case-clauses cs e c end-label)
  (match cs
    ['() (compile-e e c)]
    [(cons (Clause ds e2) rest)
     (let ((l0 (gensym 'next)))
       (seq (compile-datum-lst ds c)
            (Cmp rax val-false)
            (Je l0)
            (compile-e e2 c)
            (Jmp end-label)
            (Label l0)
            (compile-case-clauses rest e c end-label)))]))

;; [Listof CaseClauses] Expr Expr CEnv -> Asm
(define (compile-case ev cs e c)
  (let ((end-label (gensym 'caseend)))
    (seq (compile-e ev c)
         (Mov rbx rax)
         (compile-case-clauses cs e c end-label)
         (Label end-label))))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))
