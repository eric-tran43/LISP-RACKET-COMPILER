#lang racket
(provide (all-defined-out))

;; Op0 -> Answer
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; Op1 Value -> Answer
(define (interp-prim1 op v)
  (match op
    ['add1          (if (integer? v) (add1 v) 'err)]
    ['sub1          (if (integer? v) (sub1 v) 'err)]
    ['zero?         (if (integer? v) (zero? v) 'err)]
    ['char?         (char? v)]
    ['char->integer (if (char? v) (char->integer v) 'err)]
    ['integer->char (if (codepoint? v) (integer->char v) 'err)]
    ['eof-object?   (eof-object? v)]
    ['write-byte    (if (byte? v) (write-byte v) 'err)]
    ['-             (if (integer? v) (- 0 v) 'err)]
    ['abs           (if (integer? v) (abs v) 'err)]
    ['integer?      (integer? v)]
    ['boolean?      (boolean? v)]
    [_ 'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 op v1 v2)
  (match op
    ['< (if (and (integer? v1) (integer? v2)) (< v1 v2) 'err)]
    ['= (if (and (integer? v1) (integer? v2)) (= v1 v2) 'err)]
    ['- (if (and (integer? v1) (integer? v2)) (- v1 v2) 'err)]))


;; helper function which sums a list using an acc
;; returns an 'err if element is not of type Int
(define (nary-plus lst acc)
       (match lst
         ['() acc]
         [(cons v rest) (if (integer? v) (nary-plus rest (+ acc v)) 'err)]))

;; OpN [Listof Value] -> Answer
(define (interp-primN op vs)
  (match op
    ['+ (nary-plus vs 0)]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
