;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
example
mzscheme

(require "parser.scm")

(define (run)
  ;; Parse our input
  (let ((ast (parser "42")))
    ;; Print the ast
    (display-ast ast)))

(run)
)
