;;; Waxeye Parser Generator
;;; www.waxeye.org
;;; Copyright (C) 2008 Orlando D. A. R. Hill
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is furnished to do
;;; so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.


(module
transform
mzscheme

(require (lib "ast.ss" "waxeye") "action.scm" "expand.scm" "gen.scm" "util.scm")
(provide (all-defined))


;; The hash table for the names of the non-terminals
(define nt-names (make-hash-table 'equal))


;; Transforms the grammar and performs sanity checks
(define (transform-grammar g)
  (and (check-not-empty g)
       (collect-actions g)
       (collect-nt-names g)
       (check-refs g)
       (expand-grammar g)))


(define (check-not-empty g)
  (when (null? (ast-c g))
        (error 'check-not-empty "grammar is empty")))


(define (collect-nt-names g)
  (let ((ok #t))
    (for-each (lambda (a)
                (let* ((name (get-non-term a)) (found (hash-table-get nt-names name #f)))
                  (if found
                      (begin
                        (set! ok #f)
                        (error 'check-duplicate "duplicate definition of non-terminal: ~a" name))
                      (hash-table-put! nt-names name name))))
              (ast-c g))
    ok))


;; Checks that referenced non-terminals have been defined
(define (check-refs grammar)
  (define (visit-nt exp)
    (let ((name (list->string (ast-c exp))))
      (unless (hash-table-get nt-names name #f)
              (error 'waxeye "undefined reference to non-terminal: ~a" name))))

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
       ((alternation) (visit-alternation exp))
       ((identifier) (visit-nt exp))
       ((sequence) (visit-sequence exp))
       ((unit) (visit-unit exp)))))

  (define (check-nt-refs def)
    (visit-alternation (caddr (ast-c def))))

  (for-each check-nt-refs (get-defs grammar)))

)
