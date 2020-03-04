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
  (displayln "generating php parser")
  (format "<?php\n\n~a\n\nclass GenParser extends Parser\n{\n~a\n~a{\n~a\n~a}\n}\n" (gen-php-imports) (indent (gen-constructor)) (indent (ind)) (indent (gen-automata (make-automata grammar))) (indent (ind))))

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

(define (gen-constructor)
  (format "~apublic function __construct()" (ind)))

(define (gen-automata automata)
  (let ((fas (vector->list automata)))
    (format "~a~a$fas = new FAs();\n\n~a" (ind)(ind) (map (lambda (automaton) (gen-states automaton)) fas))))
;;    (for ([fa fas])
;;     (gen-states fa)))))

(define (gen-states automaton)
  (printf "generating states from automaton (~a,~a,~a)\n" (fa-type automaton) (fa-states automaton) (fa-mode automaton))
  (format "~a~a$states = new States();\n" (ind)(ind)))