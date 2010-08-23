;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
gen
mzscheme

(require (lib "ast.ss" "waxeye")
         (only (lib "1.ss" "srfi") list-index))
(provide (all-defined))

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
  (set! *start-name* name)
  (if (equal? *start-name* "")
      (start-name! (get-non-term (car (get-defs grammar))))
      (let ((si (list-index (lambda (a)
                              (equal? a *start-name*))
                            (map get-non-term (get-defs grammar)))))
        (if si
            (start-index! si)
            (error 'waxeye "Can't find definition of starting non-terminal: ~a" *start-name*)))))


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

)
