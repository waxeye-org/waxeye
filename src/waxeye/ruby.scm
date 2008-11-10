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
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
(provide gen-ruby)


(define (code-bool b)
  (if b "true" "false"))


(define (gen-ruby grammar path)
  (indent-unit! 2)
  (dump-string (gen-parser grammar)
               (string-append path (if *name-prefix*
                                       (string-append (camel-case-lower *name-prefix*) "_parser.rb")
                                       "parser.rb"))))


(define (ruby-comment lines)
  (comment-base "#" lines))


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
        (format "~a..~a"
                (char->integer (car a))
                (char->integer (cdr a)))))
  (cond
   ((symbol? a) (format ":_~a" a))
   ((list? a)
    (format "[~a~a]"
            (gen-char-class-item (car a))
            (string-concat (map (lambda (b)
                                  (string-append ", " (gen-char-class-item b)))
                                (cdr a)))))
   ((char? a) (gen-char a))
   (else a)))


(define (gen-edge a)
  (format "Waxeye::Edge.new(~a, ~a, ~a)"
          (gen-trans (edge-t a))
          (edge-s a)
          (code-bool (edge-v a))))


(define (gen-edges d)
  (gen-array gen-edge (list->vector d)))


(define (gen-state a)
  (format "Waxeye::State.new(~a, ~a)"
          (gen-edges (state-edges a))
          (code-bool (state-match a))))


(define (gen-states d)
  (gen-array gen-state d))


(define (gen-fa a)
  (format "Waxeye::Automaton.new(:~a, ~a, :~a)"
          (let ((type (camel-case-lower (symbol->string (fa-type a)))))
            (cond
             ((equal? type "!") "_not")
             ((equal? type "&") "_and")
             (else type)))
          (gen-states (fa-states a))
          (case (fa-mode a)
            ((voidArrow) "void")
            ((pruneArrow) "prune")
            ((leftArrow) "left"))))


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
       (format "~aclass ~a < Waxeye::WaxeyeParser\n~a~aend\n"
               (ind)
               parser-name
               (indent (format
"~a@@start = ~a
~a@@eof_check = ~a
~a@@line_counting = :~a
~a@@tab_width = ~a
~a@@automata = ~a

~adef initialize()
~a
~aend
"
(ind)
*start-index*
(ind)
(code-bool *eof-check*)
(ind)
*line-counting*
(ind)
*tab-width*
(ind)
(gen-fas (make-automata grammar))
(ind)
(indent (format "~asuper(@@start, @@eof_check, @@line_counting, @@tab_width, @@automata)" (ind)))
(ind)))
               (ind)
               ))

    (format "~a\nrequire 'waxeye'\n\n~a"
            (if *file-header*
                (ruby-comment *file-header*)
                (ruby-comment *default-header*))
            (if *module-name*
                (format "module ~a\n~aend\n"
                        *module-name*
                        (indent (gen-parser-class)))
                (gen-parser-class)))))

)
