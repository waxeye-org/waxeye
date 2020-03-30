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
    [(wildCard) (values "ANY_CHAR" "")]
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


(define (gen-automaton automaton)
  (gen-states (fa-states automaton))
  (format "$automata[\"~a\"] = new Automaton(\"~a\", ~a, EXPRESSIONS);"
          (fa-type automaton)          
          (fa-type automaton)
          (case (fa-mode automaton)
            ((voidArrow) "NonTerminalMode::VOID")
            ((pruneArrow) "NonTerminalMode::PRUNING")
            ((leftArrow) "NonTerminalMode::NORMAL"))))

(define (gen-states state)
  (gen-array gen-state state))

(define (gen-state state)
  (format "$edges = new Edges();\n~a$states->append(new State($edges, ~a));"
          (gen-edges (state-edges state))
          (bool->s (state-match state))))

(define (gen-edges edges)
  (gen-array gen-edge (list->vector edges)))

(define (gen-edge edge)
  (format "$edges->append(new Edge(~a, ~a, ~a));\n"
          (gen-transition (edge-t edge))
          (edge-s edge)
          (bool->s (edge-v edge))))

(define (gen-transition transition)
  (cond
    ((equal? transition 'wild) (gen-wildcard-transition))
    ((integer? transition) (gen-automaton-transition transition))
    ((char? transition) (gen-char-transition transition))
    ((pair? transition) (gen-char-class-transition transition))))

(define (gen-wildcard-transition transition)
  (format "new WildcardTransition()"))

(define (gen-automaton-transition transition)
  (format "new AutomatonTransition(~a)" transition))

(define (gen-char-transition transition)
  (format "new CharTransition(new CharArray(~a), new CharArray(), new CharArray())" (gen-char transition)))

(define (gen-char-class-transition transition)
  (let* ((single (filter char? transition))
         (ranges (filter pair? transition))
         (min (map car ranges))
         (max (map cdr ranges)))
    (format "new CharTransition(new CharArray(~a), new CharArray(~a), new CharArray(~a))"
          (gen-char-list single)
          (gen-char-list min)
          (gen-char-list max))))
  




(define (gen-char t)
  (format "'~a~a'"
          (if (escape-for-java-char? t) "\\" "")
          (cond
           ((equal? t #\linefeed) "\\n")
           ((equal? t #\tab) "\\t")
           ((equal? t #\return) "\\r")
           (else t))))

(define (gen-array fn data)
    (format "~a"
            (indent (if (null? data)
                        ""
                        ; Simulate string-join with string-append . add-between,
                        ; because racketscript cannot handle racket/string.
                        (apply string-append (add-between (map fn data) ", "))))))
