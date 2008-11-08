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
ruby
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         "code.scm" "dfa.scm" "gen.scm" (only "scheme.scm" code-scheme-comment-base) "version.scm")
(provide gen-ruby)


(define (code-bool b)
  (code-s (if b
              "true"
              "false")))


(define (gen-ruby grammar path)
  (code-indent-unit! "  ")
  (gen-parser grammar)
  (dump-output (string-append path (if *name-prefix*
                                       (string-append (camel-case-lower *name-prefix*) "_parser.rb")
                                       "parser.rb")))
  (clear-output))


(define (code-comment . lines)
  (code-scheme-comment-base "#" lines))


(define (code-header-comment . lines)
  (code-scheme-comment-base "#" lines))


(define (gen-trans a)
(define (gen-char t)
  (code-s "\"")
  (when (escape-for-java-char? t)
        (code-s "\\"))
  (code-s (cond
           ((equal? t #\") "\\\"")
           ((equal? t #\linefeed) "\\n")
           ((equal? t #\tab) "\\t")
           ((equal? t #\return) "\\r")
           (else t)))
  (code-s "\""))
  (define (gen-char-class-item a)
    (if (char? a)
        (gen-char a)
        (begin
          (code-s (char->integer (car a)))
          (code-s "..")
          (code-s (char->integer (cdr a))))))
  (cond
   ((symbol? a) (code-s (format ":_~a" a)))
   ((list? a)
    (code-s "[")
    (gen-char-class-item (car a))
    (for-each (lambda (b)
                (code-s ", ")
                (gen-char-class-item b))
              (cdr a))
    (code-s "]"))
   ((char? a) (gen-char a))
   (else (code-s a))))


(define (gen-edge a)
  (code-s "Waxeye::Edge.new")
  (code-paren
   (gen-trans (edge-t a))
   (code-s ", ")
   (code-s (edge-s a))
   (code-s ", ")
   (code-bool (edge-v a))))


(define (gen-edges d)
  (gen-array gen-edge (list->vector d)))


(define (gen-state a)
  (code-s "Waxeye::State.new")
  (code-paren
   (gen-edges (state-edges a))
   (code-s ", ")
   (code-bool (state-match a))))


(define (gen-states d)
  (gen-array gen-state d))


(define (gen-fa a)
  (code-s "Waxeye::Automaton.new")
  (code-paren
   (code-s ":")
   (let ((type (camel-case-lower (symbol->string (fa-type a)))))
     (cond
      ((equal? type "!") (code-s "_not"))
      ((equal? type "&") (code-s "_and"))
      (else (code-s type))))
   (code-s ", ")
   (gen-states (fa-states a))
   (code-s ", :")
   (code-s (case (fa-mode a)
             ((voidArrow) "void")
             ((pruneArrow) "prune")
             ((leftArrow) "left")))))


(define (gen-fas d)
  (gen-array gen-fa d))


(define (gen-array fn data)
  (let ((ss (vector->list data)))
    (code-s "[")
    (code-iu
    (unless (null? ss)
            (fn (car ss))
            (for-each (lambda (a)
                        (code-s ",\n")
                        (code-i)
                        (fn a))
                      (cdr ss))))
    (code-s "]")))


(define (gen-parser grammar)
  (let ((parser-name (if *name-prefix*
                         (string-append (camel-case-upper *name-prefix*) "Parser")
                         "Parser")))
    (define (gen-parser-class)
       (code-is "class ")
       (code-s parser-name)
       (code-sn " < Waxeye::WaxeyeParser")
       (code-iu
        (code-is "@@start = ")
        (code-sn *start-index*)
        (code-is "@@eof_check = ")
        (code-bool *eof-check*)
        (code-n)
        (code-is "@@line_counting = :")
        (code-sn *line-counting*)
        (code-is "@@tab_width = ")
        (code-sn *tab-width*)
        (code-is "@@automata = ")
        (gen-fas (make-automata grammar))
        (code-n)
        (code-n)
        (code-isn "def initialize()")
        (code-iu (code-isn "super(@@start, @@eof_check, @@line_counting, @@tab_width, @@automata)"))
        (code-isn "end"))
       (code-isn "end"))

    (if *file-header*
        (map code-header-comment *file-header*)
        (code-header-comment (string-append "Generated by the Waxeye Parser Generator - version " *version*) "www.waxeye.org"))
    (code-n)

    (code-sn "require 'waxeye'")
    (code-n)

    (if *module-name*
        (begin
          (code-s "module ")
          (code-sn *module-name*)
          (code-n)
          (code-iu
           (gen-parser-class))
          (code-sn "end"))
        (gen-parser-class))))

)
