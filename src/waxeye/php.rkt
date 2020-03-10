#lang racket/base
(require waxeye/ast
         waxeye/fa
         "code.rkt" "dfa.rkt" "gen.rkt")
(provide gen-php)

(define (gen-php grammar path)
  (let ((parser-file (string-append path "GenParser.php")))
    (dump-string (php-parser grammar) parser-file)
    (list parser-file)))

(define (php-parser grammar)
  (printf "generating php parser with grammar ~a\n" grammar)
  (display-ast grammar)
  (format "<?php\n\n~a\n\nclass GenParser extends Parser\n{\n~a\n{\n~a\n~a\n~a\n}\n}\n"
          (gen-php-imports)
          (indent (gen-constructor))
          (indent (gen-init))
          (gen-array gen-automaton (make-automata grammar))
          (indent (gen-parent-call))))

(define (gen-php-imports)
  "use parser\\AutomatonTransition;
use parser\\CharTransition;
use parser\\Edge;
use parser\\Edges;
use parser\\FA;
use parser\\FAs;
use parser\\Parser;
use parser\\State;
use parser\\States;
use parser\\WildcardTransition;
use util\\CharArray;")

(define (gen-parent-call)
  (format "printf('%s', $fas);
parent::__construct($fas, \"~a\");" *start-name*))

(define (gen-constructor)
  (format "~apublic function __construct()" (ind)))

(define (gen-init)
  (format "~a~a$fas = new FAs();\n" (ind) (ind)))


(define (gen-automaton automaton)
  (printf "generating automaton (~a,~a,~a)\n" (fa-type automaton) (fa-states automaton) (fa-mode automaton))
  (format "~a\n$states = new States();\n~a\n$fas[] = new FA(\"~a\", $states, ~a);\n~a\n"
          (gen-automaton-begin-comment automaton)
          (gen-states (fa-states automaton))
          (fa-type automaton)
          (case (fa-mode automaton)
            ((voidArrow) "FA::VOID")
            ((pruneArrow) "FA::PRUNE")
            ((leftArrow) "FA::LEFT"))
          (gen-automaton-end-comment automaton)))

(define (gen-automaton-begin-comment automaton)
  (format "//BEGIN: AUTOMATON ~a" (fa-type automaton)))

(define (gen-automaton-end-comment automaton)
  (format "//END: AUTOMATON ~a" (fa-type automaton)))

(define (gen-states state)
  (gen-array gen-state state))

(define (gen-state state)
  (printf "\tgenerating state (~a, ~a)\n" (state-edges state) (state-match state))
  (format "$edges = new Edges();\n~a$states->append(new State($edges, ~a));"
          (gen-edges (state-edges state))
          (bool->s (state-match state))))

(define (gen-edges edges)
  (gen-array gen-edge (list->vector edges)))

(define (gen-edge edge)
  (printf "\t\tgenerating edge (~a, ~a, ~a)\n" (edge-t edge) (edge-s edge) (edge-v edge))
  (format "$edges->append(new Edge(~a, ~a, ~a));\n"
          (gen-transition (edge-t edge))
          (edge-s edge)
          (bool->s (edge-v edge))))

(define (gen-transition transition)
  (printf "\t\t\tgenerating trans ~a\n" transition)
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
  

(define (gen-char-list l)
  (format "~a"
          (if (null? l)
              ""
              (string-append
               (gen-char (car l))
               (apply string-append (map (lambda (a)
                                     (string-append ", " (gen-char a)))
                                   (cdr l)))))))


(define (gen-char t)
  (format "'~a~a'"
          (if (escape-for-java-char? t) "\\" "")
          (cond
           ((equal? t #\linefeed) "\\n")
           ((equal? t #\tab) "\\t")
           ((equal? t #\return) "\\r")
           (else t))))

(define (gen-array fn data)
  (let ((ss (vector->list data)))
    (format "~a"
            (indent (if (null? ss)
                        ""
                        (string-append (fn (car ss)) (apply string-append (map (lambda (a) (string-append "\n" (ind) (fn a))) (cdr ss)))))))))