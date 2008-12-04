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
tester
scheme

(require (lib "ast.ss" "waxeye") "gen.scm" "interp.scm" "scheme.scm" (only-in "util.scm" display-ln))
(provide tester)


(define *num-pass* 0)
(define *num-fail* 0)


(define (tester grammar tests)
  (define read-tests
    (lambda (i)
      (let ((test (read i)))
        (unless (eof-object? test)
                (start-nt! (symbol->string (car test)) grammar)
                (run-test-iter (dynamic-parser grammar) (cdr test))
                (read-tests i)))))
  (eof-check! #t)
  (set! *num-pass* 0)
  (set! *num-fail* 0)
  (call-with-input-file tests read-tests)
  (display-ln "Waxeye Grammar Tester")
  (display "------------------------------------------------------------------------------\n")
  (let* ((t-count (+ *num-pass* *num-fail*))
         (cl (string->list (number->string (exact->inexact (/ (* *num-pass* 100) t-count)))))
         (cent (list->string (take cl (min (length cl) 5)))))
    (display (format "passed ~a | failed ~a | success ~a%\n" *num-pass* *num-fail* cent)))
  (display "------------------------------------------------------------------------------\n"))


(define (run-test-iter parser pairs)
  (unless (null? pairs)
          (run-test parser (car pairs) (cadr pairs))
          (run-test-iter parser (cddr pairs))))


(define (run-test parser input expect)
  (let ((result (parser input)))
    (if (cond
         ((ast? result)
          (or (equal? expect 'pass) (is-expected? result expect)))
         ((parse-error? result)
          (equal? expect 'fail))
         ((equal? result #t)
          (equal? expect 'pass)))
        (set! *num-pass* (+ *num-pass* 1))
        (begin
          (set! *num-fail* (+ *num-fail* 1)) 
          (report-error input expect result)))))


(define (report-error input expect actual)
  (display-ln "Error! @ " *start-name*)
  (display-ln "input    = " input)
  (display-ln "expected = " expect)
  (display "actual   = ")
  (if (ast? actual)
      (display-ln (ast->string-sexpr actual))
      (display-ln (if (parse-error? actual)
                   'fail
                   'pass)))
  (newline))


(define (is-expected? result expect)
  (cond
   ((and (ast? result) (list? expect))
    (let ((type (car expect)) (child (cdr expect)))
      (or (equal? type '*)
          (and (equal? (ast-t result) type) (children-match? (ast-c result) child)))))
   ((and (char? result) (char? expect) (char=? result expect)))
   (else #f)))


(define (children-match? res expect)
  (if (null? res)
      (or (null? expect) (equal? (car expect) '*))
      (and (not (null? expect))
           (or (equal? (car expect) '*)
               (and (is-expected? (car res) (car expect))
                    (children-match? (cdr res) (cdr expect)))))))

)
