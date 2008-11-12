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
c
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
(provide gen-c)


(define *c-prefix* "")
(define *c-parser-name* "")
(define *c-type-name* "")
(define *c-header-name* "")
(define *c-source-name* "")


(define (gen-c-names)
  (set! *c-prefix* (if *name-prefix*
                       (string-append (camel-case-lower *name-prefix*) "_")
                       ""))
  (set! *c-parser-name* (string-append *c-prefix* "parser"))
  (set! *c-type-name* (string-append *c-prefix* "node_type"))
  (set! *c-header-name* (string-append *c-parser-name* ".h"))
  (set! *c-source-name* (string-append *c-parser-name* ".c")))


(define (gen-c grammar path)
  (indent-unit! 4)
  (gen-c-names)
  (dump-string (gen-header grammar) (string-append path *c-header-name*))
  (dump-string (gen-parser grammar) (string-append path *c-source-name*)))


(define (c-comment lines)
  (comment-bookend "/*" " *" " */" lines))


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
          (bool->s (edge-v a))))


(define (gen-edges d)
  (gen-array gen-edge (list->vector d)))


(define (gen-state a)
  (format "Waxeye::State.new(~a, ~a)"
          (gen-edges (state-edges a))
          (bool->s (state-match a))))


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


(define (c-header-comment)
  (if *file-header*
      (c-comment *file-header*)
      (c-comment *default-header*)))


(define (gen-header grammar)
  (let ((non-terms (get-non-terms grammar))
        (parser-name (if *name-prefix*
                         (string-append (camel-case-upper *name-prefix*) "parser")
                         "parser")))
    (format "~a
#ifndef ~a_H_
#define ~a_H_

#include \"waxeye.h\"

enum ~a {
~a
};

#ifndef ~a_C_

extern struct parser_t* ~a_new();

#endif /* ~a_C_ */
#endif /* ~a_H_ */
"
            (c-header-comment)
            (string->upper *c-parser-name*)
            (string->upper *c-parser-name*)
            *c-type-name*
            (indent
             (string-append
              (ind)
              (string->upper (car non-terms))
              (string-concat
               (map (lambda (a)
                      (string-append "," (ind) (string->upper a)))
                    (cdr non-terms)))))
            (string->upper *c-parser-name*)
            *c-parser-name*
            (string->upper *c-parser-name*)
            (string->upper *c-parser-name*))))


(define (gen-parser grammar)
  (let ((automata (make-automata grammar)))
    (format "~a
#define ~a_C_
#include \"~a\"

struct parser_t* ~a_new() {
~a
}
"
            (c-header-comment)
            (string->upper *c-parser-name*)
            *c-header-name*
            *c-parser-name*
            (indent
             (format "~aconst size_t start = ~a;
~aconst bool eof_check = ~a;
~aconst size_t num_automata = ~a;
~aconst fa_t *automata = NULL;/*~a*/

~areturn parser_new(start, automata, num_automata, eof_check);"
                     (ind)
                     (number->string *start-index*)
                     (ind)
                     (bool->s *eof-check*)
                     (ind)
                     (number->string (vector-length automata))
                     (ind)
                     (gen-fas automata)
                     (ind))))))

)
