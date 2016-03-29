;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

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
  (let ((file-path (string-append path (if *name-prefix*
                                           (string-append (string-downcase *name-prefix*) "_parser.js")
                                           "parser.js"))))
    (dump-string (gen-parser grammar) file-path)
    (list file-path)))

(define (gen-trans a)
  (define (gen-char t)
    (format "'~a~a'"
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
                (gen-char (car a))
                (gen-char (cdr a)))))
  (cond
   ((symbol? a) "-1") ;; use -1 for wild card
   ((list? a) (gen-array gen-char-class-item a))
   ((char? a) (gen-char a))
   (else a)))

(define (gen-exp a)
  (format "{type:'~a', args:~a}"
    (case (ast-t a)
      [(wildCard) "ANY"]
      [(identifier) "NT"]
      [(void) "VOID"]
      [(literal) (if (<= (length (ast-c a)) 1)
        "CHAR"
        "SEQ")]
      [(charClass) "CHAR_CLASS"]
      [(and) "AND"]
      [(not) "NOT"]
      [(optional) "OPT"]
      [(alternation) "ALT"]
      [(sequence) "SEQ"]
      [(closure) "STAR"]
      [(plus) "PLUS"]
      [else (format "unknown:~a" (ast-t a))]
    )
    (case (ast-t a)
      [(wildCard) "[]"]
      [(identifier) (format "['~a']" (list->string (ast-c a)))]
      [(void) (gen-array gen-exp (ast-c a))]
      [(literal) (if (<= (length (ast-c a)) 1)
        (gen-trans (ast-c a))
        (gen-array gen-exp (map (lambda (b)
          (make-ast 'literal (cons b '()) '()))
          (ast-c a)) ))]
      [(charClass) (gen-trans (ast-c a))]
      [(and) (gen-array gen-exp (ast-c a))]
      [(not) (gen-array gen-exp (ast-c a))]
      [(optional) (gen-array gen-exp (ast-c a))]
      [(alternation) (gen-array gen-exp (ast-c a))]
      [(sequence) (gen-array gen-exp (ast-c a))]
      [(closure) (gen-array gen-exp (ast-c a))]
      [(plus) (gen-array gen-exp (ast-c a))]
      [else (format "unknown:~a" (ast-t a))]
    )))

(define (gen-def a)
  (format "'~a' : { mode : waxeye.Modes.~a, exp : ~a }"
      ; non-term name
      (list->string (ast-c (list-ref (ast-c a) 0)))
      ; mode
      (case (ast-t (list-ref (ast-c a) 1))
        ((voidArrow) "VOIDING")
        ((pruneArrow) "PRUNING")
        ((leftArrow) "NORMAL"))
      ; right-hand side expression
      (gen-exp (list-ref (ast-c a) 2))
  ))

(define (gen-defs a)
  (gen-map gen-def (ast-c a)))

(define (gen-map fn data)
    (format "{~a}"
            (indent (if (null? data)
                        ""
                        (string-append (fn (car data))
                                       (string-concat (map (lambda (a)
                                                             (string-append ",\n" (ind) (fn a)))
                                                           (cdr data))))))))
(define (gen-array fn data)
    (format "[~a]"
            (indent (if (null? data)
                        ""
                        (string-append (fn (car data))
                                       (string-concat (map (lambda (a)
                                                             (string-append ",\n" (ind) (fn a)))
                                                           (cdr data))))))))


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
~aparser.prototype = new waxeye.WaxeyeParser(
~a~a~a
~a~a, '~a');
~areturn parser;
"
                              (ind)
                              (ind)
                              (ind) (ind) (gen-defs grammar)
                              (ind) (ind) *start-name*
                              (ind)))
              (ind)))


    (define (gen-nodejs-imports)
      (indent (format "
var waxeye = waxeye;
if (typeof module !== 'undefined') {
~a// require from module system
~awaxeye = require('waxeye');
}
" (ind) (ind))))


    (define (gen-nodejs-exports)
      (indent (format "
// Add to module system
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
