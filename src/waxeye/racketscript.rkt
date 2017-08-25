;; Compiles an entry point for using Waxeye compiled to JavaScript via RacketScript.
;; Defines a window.waxeyeCompiler object.

#lang racketscript/base

(require racketscript/interop
         racketscript/browser
         waxeye/ast
         (only-in "gen.rkt" start-nt!)
         (only-in "transform.rkt" transform-grammar)
         (only-in "javascript.rkt" gen-javascript-parser)
         (only-in "grammar-parser.rkt" grammar-parser))

(define (syntax-error-message error)
  (string-append "syntax error in grammar: " (parse-error->string error)))

(define (transform-and-set-start grammar-or-error [start-name ""])
  (if (ast? grammar-or-error)
      (let ([grammar grammar-or-error])
           (transform-grammar grammar)
           (start-nt! start-name grammar)
           grammar)
      grammar-or-error))

(define (js-result grammar-or-error fn)
  (if (ast? grammar-or-error)
      ($/array (fn grammar-or-error) $/null)
      ($/array $/null (syntax-error-message grammar-or-error))))

(define (grammar-to-ast-string grammar-src)
  (js-result (grammar-parser grammar-src) ast->string))

(define (grammar-to-transformed-ast-string grammar-src)
  (js-result (transform-and-set-start (grammar-parser grammar-src)) ast->string))

(define (generate-parser grammar-src)
  (js-result (transform-and-set-start (grammar-parser grammar-src)) gen-javascript-parser))

($/:= #js*.window.waxeyeCompiler
  ($/obj
    [grammarToAstString grammar-to-ast-string]
    [grammarToTransformedAstString grammar-to-transformed-ast-string]
    [generateParser generate-parser]))

