;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang racket/base


(define (display-dot name state)
  (let ((visited (make-hasheq)))
    (hash-set! visited state "match")
    (displayln (string-append "digraph " name " {"))
    (display-state visited state)
    (displayln "\"match\" [ label = \"match\" ];")
    (displayln "}")))


(define (display-state visited state)
  (define (get-state-name table state)
    (let ((val (hash-ref table state #f)))
      (if val
          val
          (begin
            (let ((v2 (gensym)))
              (hash-set! table state v2)
              v2)))))
  (unless (state-match state)
          (displayln (string-append
                        "\"" (get-state-name visited state) "\""
                        "[ label = \"\" ];"))
          (for-each (lambda (a)
                      (displayln (string-append
                                    "\"" (get-state-name visited state) "\""
                                    "->"
                                    "\"" (get-state-name visited (cdr a)) "\""
                                    "[ label = \"" (car a) "\" ];"))
                      (display-state visited (cdr a)))
                    (state-edges state))))
