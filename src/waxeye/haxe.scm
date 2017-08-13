;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
haxe
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         (only (lib "list.ss" "mzlib") filter)
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
(provide gen-haxe)


(define *haxe-parser-name* "")


(define (haxe-comment lines)
  (comment-bookend "/*" " *" " */" lines))


(define (haxe-doc . lines)
  (comment-bookend "/**" " *" " */" lines))


(define (haxe-header-comment)
  (if *file-header*
      (haxe-comment *file-header*)
      (haxe-comment *default-header*)))


(define (gen-haxe-names)
  (set! *haxe-parser-name* (if *name-prefix*
                               (string-append *name-prefix* "Parser")
                               "Parser")))


(define (gen-haxe grammar path)
  (gen-haxe-names)
  (let ((parser-file (string-append path *haxe-parser-name* ".hx")))
    (dump-string (haxe-parser grammar) parser-file)
    (list parser-file)))


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
  (format "new Exp(~a, ~a)"
    (case (ast-t a)
      [(wildCard) "ExpType.ANY"]
      [(identifier) "ExpType.NT"]
      [(void) "ExpType.VOID"]
      [(literal) (if (<= (length (ast-c a)) 1)
          "ExpType.CHAR"
          "ExpType.SEQ")]
      [(charClass) "ExpType.CHAR_CLASS"]
      [(and) "ExpType.AND"]
      [(not) "ExpType.NOT"]
      [(optional) "ExpType.OPT"]
      [(alternation) "ExpType.ALT"]
      [(sequence) "ExpType.SEQ"]
      [(closure) "ExpType.STAR"]
      [(plus) "ExpType.PLUS"]
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
  (format "~a : {'mode' : Modes.~a, 'exp' : ~a }"
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


(define (haxe-parser grammar)
  (format "~a~a\n~a~aclass ~a extends org.waxeye.parser.Parser\n{\n~a}\n"
          (haxe-header-comment)
          (gen-haxe-package)
          (gen-haxe-imports)
          (haxe-doc "A parser generated by the Waxeye Parser Generator." "" "@author Waxeye Parser Generator")
          *haxe-parser-name*
          (indent (string-append (gen-constructor) "\n" (gen-make-def  grammar)))))


(define (gen-haxe-package)
  (if *module-name*
      (format "package ~a;\n" *module-name*)
      "package;"))    


(define (gen-haxe-imports)
"
import org.waxeye.parser.*;
import org.waxeye.parser.Exp.ExpType;
import org.waxeye.parser.Modes;
//import haxe.ds.StringMap;
")


(define (gen-constructor)
  (format "~a~apublic function new()\n~a{\n~a~a}\n"
          (haxe-doc (format "Creates a new ~a." *haxe-parser-name*))
          (ind)
          (ind)
          (indent
           (format "~asuper(makeDefinition(), '~a');\n"
                   (ind)
                   *start-name*))
          (ind)))

          
          
(define (gen-make-def grammar)
  (format "~a~aprivate function makeDefinition():Dynamic\n~a{\n~a~a}\n~a\n"
      (haxe-doc "Builds the grammar definitions for the parser." "" "@return The definitions grammar for the parser.")
        (ind)
        (ind)
        (indent
          (string-append
          (format "~avar def:Dynamic = ~a\n" (ind) (gen-defs grammar))
          "\n"
          (string-append (ind) "return def;\n")))
          (ind)
        (ind)
))
)






