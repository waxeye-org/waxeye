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
dot
mzscheme

(require "util.scm")


(define (display-dot name state)
  (let ((visited (make-hash-table)))
    (hash-table-put! visited state "match")
    (display-ln "digraph " name " {")
    (display-state visited state)
    (display-ln "\"match\" [ label = \"match\" ];")
    (display-ln "}")))


(define (display-state visited state)
  (define (get-state-name table state)
    (let ((val (hash-table-get table state #f)))
      (if val
          val
          (begin
            (let ((v2 (gensym)))
              (hash-table-put! table state v2)
              v2)))))
  (unless (state-match state)
          (display-ln "\"" (get-state-name visited state) "\""
                      "[ label = \"\" ];")
          (for-each (lambda (a)
                      (display-ln "\"" (get-state-name visited state) "\""
                                  "->"
                                  "\"" (get-state-name visited (cdr a)) "\""
                                  "[ label = \"" (car a) "\" ];")
                      (display-state visited (cdr a)))
                    (state-edges state))))


)
