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
code
mzscheme

(require (only "util.scm" string-concat))

(provide (all-defined))

(define *code-output-list* '())
(define *code-indent-level* 0)
(define *code-indent-unit* "    ")


(define (code-indent-unit! val)
  (set! *code-indent-unit* val))


(define (clear-output)
  (set! *code-output-list* '()))


(define (dump-output path)
  (call-with-output-file path display-code-port 'replace))


(define (display-code)
  (display-code-port (current-output-port)))


(define (display-code-port port)
  (define (display-code-iter list)
    (unless (null? list)
            (display-code-iter (cdr list))
            (display (car list) port)))

  (display-code-iter *code-output-list*))


(define-syntax code-iu
  (syntax-rules ()
    ((_ a ...) (begin (code-indent) a ... (code-unindent)))))


(define-syntax code-iu-num
  (syntax-rules ()
    ((_ num a ...) (begin (code-indent-num num) a ... (code-unindent-num num)))))


(define-syntax code-brace
  (syntax-rules ()
    ((_ a ...) (begin (code-isn "{") (code-iu a ...) (code-isn "}")))))


(define-syntax code-sep
  (syntax-rules ()
    ((_ a) a)
    ((_ a b ...)
     (begin
       a
       (code-n)
       (code-sep b ...)))))


(define (code-space . data)
  (for-each (lambda (a)
              (code-s a)
              (code-s " "))
            data))


(define (code-indent)
  (code-indent-num 1))


(define (code-indent-num num)
  (set! *code-indent-level* (+ *code-indent-level* num)))


(define (code-unindent)
  (code-unindent-num 1))


(define (code-unindent-num num)
  (set! *code-indent-level* (- *code-indent-level* num)))


(define (code-i)
  (let loop ((i 0))
    (when (< i *code-indent-level*)
          (set! *code-output-list* (cons *code-indent-unit* *code-output-list*))
          (loop (+ i 1)))))


(define (code-n)
  (set! *code-output-list* (cons "\n" *code-output-list*)))


(define (code-s s)
  (unless (equal? s "")
          (set! *code-output-list* (cons s *code-output-list*))))


(define (code-sn s)
  (code-s s)
  (code-n))


(define (code-es . list)
  (for-each code-s list))


(define (code-esn . list)
  (for-each code-sn list))


(define (code-is s)
  (unless (equal? s "")
          (code-i)
          (set! *code-output-list* (cons s *code-output-list*))))


(define (code-isn s)
  (code-is s)
  (code-n))


(define (code-eisn . list)
  (for-each code-isn list))


(define (code-p fn list)
  (fn (string-concat list)))


(define (code-psn . list)
  (code-p code-sn list))


(define (code-pisn . list)
  (code-p code-isn list))


(define-syntax code-paren
  (syntax-rules ()
    ((_ a ...) (begin (code-s "(") a ... (code-s ")")))))

)
