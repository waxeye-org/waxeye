;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang racket/base
(require (only-in racket/system system))
(provide ^ $ ++ cd cd$ run-cmd run-make target)

(define *target-table* (make-hasheq))
(define *dep-table* (make-hasheq))

(define ++ string-append)

(define-syntax target
  (syntax-rules ()
    ((_ name (deps ...) code ...)
     ;; bind target name to code
     (hash-set! *target-table*
                      'name
                      (lambda ()
                        ;; run dependencies
                        (for-each run-target '(deps ...))
                        ;; run code
                        code ...)))))


(define (run-target t)
  (let ((t-code (hash-ref *target-table* t #f)))
    (if t-code
        (unless (hash-ref *dep-table* t #f)
                (hash-set! *dep-table* t #t)
                (apply t-code '()))
        (error 'make (++ "target doesn't exist - " (symbol->string t))))))


(define (run-make)
  (let ((args (map string->symbol (vector->list (current-command-line-arguments)))))
    ;; if no make target was specified
    (if (null? args)
        ;; print all possible targets
        (begin
          (displayln "possible targets:")
          (for-each displayln (sort (map symbol->string (hash-map *target-table* (lambda (k v) k))) string<?)))
        ;; otherwise run targets
        (for-each run-target args))))


(define (run-cmd prog args)
  (define (as-string s)
    (cond
     ((symbol? s) (symbol->string s))
     ((char? s) (list->string (list s)))
     ((number? s) (number->string s))
     (else s)))
  (let ((cmd (++ (as-string prog)
                 (foldr (lambda (a b)
                          (++ " " (as-string a) b))
                        ""
                        args))))
    (displayln cmd)
    (system cmd)))


(define-syntax $
  (syntax-rules ()
    ((_ prog arg ...)
     (run-cmd 'prog (list arg ...)))))


(define-syntax ^
  (syntax-rules ()
    ((_ prog arg ...)
     (run-cmd 'prog '(arg ...)))))


(define-syntax cd$
  (syntax-rules ()
    ((_ dir code ...)
     (parameterize ((current-directory (let ((d dir))
                                         (if (symbol? d)
                                             (symbol->string d)
                                             d))))
                   code ...))))


(define-syntax cd
  (syntax-rules ()
    ((_ dir code ...)
     (cd$ 'dir code ...))))
