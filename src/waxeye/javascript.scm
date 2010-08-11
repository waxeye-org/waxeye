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
javascript
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
(provide gen-javascript)


(define (javascript-comment lines)
  (comment-bookend "/*" " *" " */" lines))

(define (gen-javascript grammar path)
  (indent-unit! 4)
  (dump-string (gen-parser grammar)
               (string-append path (if *name-prefix*
                                       (string-append (camel-case-lower *name-prefix*) "_parser.js")
                                       "parser.js"))))


(define (gen-trans a)
  (define (gen-char t)
    (format "\"~a~a\""
            (if (escape-for-java-char? t) "\\" "")
            (cond
             ((equal? t #\") "\\\"")
             ((equal? t #\linefeed) "\\n")
             ((equal? t #\tab) "\\t")
             ((equal? t #\return) "\\r")
             (else t))))
  (define (gen-char-class-item a)
    (if (char? a)
        (gen-char a)
        (format "[~a, ~a]"
                (char->integer (car a))
                (char->integer (cdr a)))))
  (cond
   ((symbol? a) "-1") ;; use -1 for wild card
   ((list? a)
    (format "[~a~a]"
            (gen-char-class-item (car a))
            (string-concat (map (lambda (b)
                                  (string-append ", " (gen-char-class-item b)))
                                (cdr a)))))
   ((char? a) (gen-char a))
   (else a)))


(define (gen-edge a)
  (format "new waxeye.Edge(~a, ~a, ~a)"
          (gen-trans (edge-t a))
          (edge-s a)
          (bool->s (edge-v a))))


(define (gen-edges d)
  (gen-array gen-edge (list->vector d)))


(define (gen-state a)
  (format "new waxeye.State(~a, ~a)"
          (gen-edges (state-edges a))
          (bool->s (state-match a))))


(define (gen-states d)
  (gen-array gen-state d))


(define (gen-mode a)
  (let ((type (fa-type a)))
    (cond
     ((equal? type '&) "POS")
     ((equal? type '!) "NEG")
     (else
      (case (fa-mode a)
        ((voidArrow) "VOID")
        ((pruneArrow) "PRUNE")
        ((leftArrow) "LEFT"))))))


(define (gen-fa a)
  (format "new waxeye.FA(\"~a\", ~a, waxeye.FA.~a)"
          (let ((type (camel-case-lower (symbol->string (fa-type a)))))
            (if (or (equal? type "!") (equal? type "&"))
                ""
                type))
          (gen-states (fa-states a))
          (gen-mode a)))


(define (gen-fas d)
  (gen-array gen-fa d))


(define (gen-array fn data)
  (let ((ss (vector->list data)))
    (format "[~a]"
            (indent (if (null? ss)
                        ""
                        (string-append (fn (car ss))
                                       (string-concat (map (lambda (a)
                                                             (string-append ",\n" (ind) (fn a)))
                                                           (cdr ss)))))))))


(define (gen-parser grammar)
  (let ((parser-name (if *name-prefix*
                         (string-append (camel-case-upper *name-prefix*) "Parser")
                         "Parser")))
    (define (gen-parser-class)
      (format "\n~avar ~a = (function() {\n~a \n~a})();\n"
              (ind) 
              parser-name
              (indent (format "
~avar parser = function() { return this; };
~aparser.prototype = new waxeye.WaxeyeParser(~a, ~a, ~a);
~areturn parser;
"
                              (ind)
                              (ind)
                              *start-index*
                              (bool->s *eof-check*)
                              (gen-fas (make-automata grammar))
                              (ind)))
              (ind)))

    (define (gen-nodejs-imports)
      (indent (format "
var waxeye = waxeye;
if (typeof module !== 'undefined') {
~a// require from Node.js module system
~awaxeye = require('waxeye');
}
" (ind) (ind))))


    (define (gen-nodejs-exports)
      (indent (format "
// Add to Node.js module system
if (typeof module !== 'undefined') {
~amodule.exports.~a = ~a;
}
" (ind) parser-name parser-name)))

(format "~a~a~a~a"
        (if *file-header*
            (javascript-comment *file-header*)
            (javascript-comment *default-header*))
        (gen-nodejs-imports)
        (gen-parser-class)
        (gen-nodejs-exports))))

)

