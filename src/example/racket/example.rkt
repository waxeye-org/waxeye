;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang racket/base
(require "parser.rkt")

;; Parse our input
(let ((ast (parser "42")))
  ;; Print our AST
  (display-ast ast))
