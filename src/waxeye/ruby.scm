;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
ruby
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
(provide gen-ruby)


(define (gen-ruby grammar path)
  (indent-unit! 2)
  (let ((file-path (string-append path (if *name-prefix*
                                           (string-append (string-downcase *name-prefix*) "_parser.rb")
                                           "parser.rb"))))
    (dump-string (gen-parser grammar) file-path)
    (list file-path)))


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
  (format "Waxeye::FA.new(:~a, ~a, :~a)"
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

(define (gen-require)
  (indent (format "
begin
  require 'waxeye'
rescue LoadError
  require 'rubygems'
  require 'waxeye'
end
")))


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
~a@@automata = ~a

~adef initialize()
~a
~aend
"
(ind)
*start-index*
(ind)
(bool->s *eof-check*)
(ind)
(gen-fas (make-automata grammar))
(ind)
(indent (format "~asuper(@@start, @@eof_check, @@automata)" (ind)))
(ind)))
               (ind)
               ))

    (format "~a~a\n~a"
            (if *file-header*
                (script-comment *file-header*)
                (script-comment *default-header*))
            (gen-require)
            (if *module-name*
                (format "module ~a\n~aend\n"
                        *module-name*
                        (indent (gen-parser-class)))
                (gen-parser-class)))))

)
