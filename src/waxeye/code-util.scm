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
code-util
mzscheme

(provide (all-defined))

(define (camel-case-lower s)
  (let ((sl (string->list s)))
    (if (null? sl)
        ""
        (list->string (cons (char-downcase (car sl)) (cdr sl))))))


(define (camel-case-upper s)
  (let ((sl (string->list s)))
    (if (null? sl)
        ""
        (list->string (cons (char-upcase (car sl)) (cdr sl))))))


(define (string->upper s)
  (list->string (map char-upcase (string->list s))))


(define (escape-for-java-char? ch)
  (or (equal? ch #\\) (equal? ch #\')))


(define (escape-java-string s)
  (define (escape-java-string-iter sl)
    (if (null? sl)
        '()
        (if (equal? (car sl) #\")
            (cons #\\ (cons #\" (escape-java-string-iter (cdr sl))))
            (cons (car sl) (escape-java-string-iter (cdr sl))))))

  (list->string (escape-java-string-iter (string->list s))))

)
