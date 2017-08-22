;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
debug
mzscheme

(provide debug *debug* debug!)

(define *debug* #f)

(define (debug! v)
  (set! *debug* v))

(define-syntax debug
  (syntax-rules ()
    ((_ a ...)
     (when *debug*
           a ...))))

)
