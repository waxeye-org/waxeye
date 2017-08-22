;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
interp
mzscheme

(require (lib "ast.rkt" "waxeye")
         (lib "parser.rkt" "waxeye")
         "dfa.rkt"
         "gen.rkt"
         "scheme.rkt"
         "util.rkt")

(provide dynamic-parser interpreter)


(define (dynamic-parser grammar)
  (make-parser *start-index* *eof-check* (make-automata grammar)))


(define (interpreter grammar input)
  (let ((input-ast ((dynamic-parser grammar) input)))
    (if (parse-error? input-ast)
        (display-parse-error input-ast)
        (display-ast input-ast))))

)
