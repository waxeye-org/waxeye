#lang racket/base
(require (only-in racket/list add-between)
         waxeye/ast
         waxeye/fa
         "code.rkt" "dfa.rkt" "gen.rkt")
(provide gen-php)

(define (gen-php grammar path)
  (let ((parser-file (string-append path "GenParser.php")))
    (dump-string (php-parser grammar) parser-file)
    (list parser-file)))

(define (php-parser grammar)
  (format "<?php\n\n~a\n\nclass GenParser extends Parser\n{\n~a\n{\n~a\n~a\n~a\n}\n}\n"
          (gen-php-imports)
          (indent (gen-constructor))
          (indent (gen-init))
          (gen-defs grammar)
          (indent (gen-parent-call))))

(define (gen-defs ast)
  (gen-map gen-def (ast-c ast)))

(define (gen-def a)
 (let ([mode-name (case (ast-t (list-ref (ast-c a) 1))
                         ((voidArrow) "VOIDING")
                         ((pruneArrow) "PRUNING")
                         ((leftArrow) "NORMAL"))]
        [nt-name (list->string (ast-c (list-ref (ast-c a) 0)))]
        [exp (gen-exp (list-ref (ast-c a) 2))])
      (format "$automata[\"~a\"] = new Automaton(\"~a\", NonTerminalMode::~a, ~a);"
        nt-name nt-name mode-name exp)))

(define (gen-exp a)
  (case (ast-t a)
    [(identifier)
     (format "Expression::NonTerminalExpression(\"~a\")" (list->string (ast-c a)))]
    [(literal)
     (if (<= (length (ast-c a)) 1)
         (format "Expression::CharExpression(~a)" (gen-char (car (ast-c a))))
         (format "Expression::SeqExpression(~a)" (gen-array gen-exp (map (lambda (b) (ast 'literal (cons b '()) '())) (ast-c a)))))]
    [(charClass)
     (format "Expression::CharClassExpression(~a)" (gen-char-class (ast-c a)))]
    [(void)
     (format "Expression::VoidExpression(~a)" (gen-exp (car (ast-c a))))]
    [(and)
     (format "Expression::AndExpression(~a)" (gen-exp (car (ast-c a))))]
    [(not)
     (format "Expression::NotExpression(~a)" (gen-exp (car (ast-c a))))]
    [(optional)
     (format "Expression::OptExpression(~a)" (gen-exp (car (ast-c a))))]
    [(closure)
     (format "Expression::StarExpression(~a)" (gen-exp (car (ast-c a))))]
    [(plus)
     (format "Expression::PlusExpression(~a)" (gen-exp (car (ast-c a))))]
    [(alternation)
     (format "Expression::AltExpression(~a)" (gen-array gen-exp (ast-c a)))]
    [(sequence)
     (format "Expression::SeqExpression(~a)" (gen-array gen-exp (ast-c a)))]
    [(wildCard)
     (format "Expression::AnyCharExpression()")]
    [else (format "unknown:~a" (ast-t a))]))

(define (gen-char-class char-class)
  (let* ((single (filter char? char-class))
         (ranges (filter pair? char-class))
         (min (map car ranges))
         (max (map cdr ranges)))
    (format "~a, ~a, ~a" (gen-char-list single) (gen-char-list min) (gen-char-list max))))

(define (gen-char-list l)
  (format "array(~a)"
          (if (null? l)
              ""
              (string-append
               (gen-char (car l))
               (apply string-append (map (lambda (a)
                                     (string-append ", " (gen-char a)))
                                   (cdr l)))))))

(define (gen-map fn data)
   (format "~a"
            (indent (if (null? data)
                        ""
                        (string-append (fn (car data))
                                       (apply string-append (map (lambda (a)
                                                             (string-append "\n" (ind) (fn a)))
                                                           (cdr data))))))))

(define (gen-php-imports)
  "use parser\\config\\Automata;
use parser\\config\\Automaton;
use parser\\config\\ParserConfig;
use parser\\expression\\Expression;
use parser\\expression\\Expressions;
use parser\\NonTerminalMode;
use parser\\Parser;")

(define (gen-parent-call)
  (format "$config = new ParserConfig($automata, \"~a\");
parent::__construct($config);" *start-name*))

(define (gen-constructor)
  (format "~apublic function __construct()" (ind)))

(define (gen-init)
  (format "~a~a$automata = new Automata();\n" (ind) (ind)))

(define (gen-char t)
  (format "~a" (char->integer t)))
  ;(let* ((hex-val (gen-hex t)))
;    (format "0x~a" hex-val)))
            ;(if (equal? (modulo (string-length hex-val) 2) 0)
             ;   hex-val
              ;  (string-append "0" hex-val)))))

(define (gen-hex chr)
  (format "~x" (char->integer chr)))

(define (gen-array fn data)
    (format "~a"
            (indent (if (null? data)
                        ""
                        ; Simulate string-join with string-append . add-between,
                        ; because racketscript cannot handle racket/string.
                        (apply string-append (add-between (map fn data) ", "))))))
