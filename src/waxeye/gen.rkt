;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang racket/base
(require waxeye/ast)
(provide (all-defined-out))

(define *eof-check* #t)
(define *expression-level* '())
(define *file-header* #f)
(define *module-name* #f)
(define *name-prefix* #f)
(define *start-index* 0)
(define *start-name* "")


(define (eof-check! val)
  (set! *eof-check* val))


(define (file-header! val)
  (set! *file-header* val))


(define (module-name! val)
  (set! *module-name* val))


(define (name-prefix! val)
  (set! *name-prefix* val))


(define (start-index! val)
  (set! *start-index* val))


(define (start-name! val)
  (set! *start-name* val))


(define (start-nt! name grammar)
  ; This method exists in racket/list since Racket v6.7,
  ; but we're compatible with the Racket version in the latest Ubuntu LTS (v6.4 as of this comment).
  (define (index-of ls v)
    (let loop ([ls ls]
               [i 0])
      (cond [(null? ls) #f]
            [(equal? (car ls) v) i]
            [else (loop (cdr ls) (add1 i))])))
  (set! *start-name* name)
  (if (equal? name "")
      (start-name! (get-non-term (car (get-defs grammar))))
      (let ([si (index-of (map get-non-term (get-defs grammar)) name)])
        (if si
            (start-index! si)
            (error 'waxeye (format "Can't find definition of starting non-terminal: ~a" *start-name*))))))


(define (push-exp-level level)
  (set! *expression-level* (cons level *expression-level*)))


(define (pop-exp-level)
  (let ((top (car *expression-level*)))
    (set! *expression-level* (cdr *expression-level*))
    top))


(define (peek-exp-level)
  (car *expression-level*))


(define (get-non-terms grammar)
  (map get-non-term (ast-c grammar)))


(define (get-non-term def)
  (list->string (ast-c (car (ast-c def)))))


(define (get-defs grammar)
  (ast-c grammar))


(define (get-arrow def)
  (ast-t (cadr (ast-c def))))


(define (get-alternation def)
  (caddr (ast-c def)))
