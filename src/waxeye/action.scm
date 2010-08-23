;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
action
mzscheme

(require (lib "ast.ss" "waxeye") "gen.scm")
(provide (all-defined))

(define *action-list* '())

(define (collect-actions grammar)
  (define (visit-action exp)
    (set! *action-list* (cons exp *action-list*)))

  (define (visit-alternation exp)
    (for-each visit-sequence (ast-c exp)))

  (define (visit-sequence exp)
    (for-each visit-unit (ast-c exp)))

  (define (visit-unit exp)
    (let* ((el (ast-c exp)) (el-len (length el)))
      (visit-exp (list-ref el (- el-len 1)))))

  (define (visit-exp exp)
    (let ((type (ast-t exp)))
      (case type
       ((action) (visit-action exp))
       ((alternation) (visit-alternation exp))
       ((sequence) (visit-sequence exp))
       ((unit) (visit-unit exp)))))

  (define (get-def-actions def)
    (visit-alternation (caddr (ast-c def))))

  (for-each get-def-actions (get-defs grammar))
  (set! *action-list* (reverse *action-list*)))

)
