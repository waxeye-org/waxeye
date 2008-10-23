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
make
mzscheme

(require (lib "process.ss"))
(provide target run run-make system)

(define *target-table* (make-hash-table))
(define *dep-table* (make-hash-table))

(define-syntax target
  (syntax-rules ()
    ((_ name (deps ...) code ...)
     (hash-table-put! *target-table* 'name (lambda () (for-each run-target '(deps ...)) code ...)))))

(define (run-target t)
  (let ((t-code (hash-table-get *target-table* t #f)))
    (if t-code
        (unless (hash-table-get *dep-table* t #f)
                (hash-table-put! *dep-table* t #t)
                (apply t-code ()))
        (error 'make (string-append "target doesn't exist - " (symbol->string t))))))

(define (run-make)
  (for-each (lambda (a)
              (run-target (string->symbol a)))
            (vector->list (current-command-line-arguments))))

(define-syntax run
  (syntax-rules ()
    ((_ prog arg ...) (let ((cmd (string-append (symbol->string 'prog) " " arg ...)))
                        (display cmd)
                        (newline)
                        (system cmd)))))

)
