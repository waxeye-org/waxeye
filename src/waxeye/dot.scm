;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
dot
mzscheme

(require "util.scm")


(define (display-dot name state)
  (let ((visited (make-hash-table)))
    (hash-table-put! visited state "match")
    (display-ln "digraph " name " {")
    (display-state visited state)
    (display-ln "\"match\" [ label = \"match\" ];")
    (display-ln "}")))


(define (display-state visited state)
  (define (get-state-name table state)
    (let ((val (hash-table-get table state #f)))
      (if val
          val
          (begin
            (let ((v2 (gensym)))
              (hash-table-put! table state v2)
              v2)))))
  (unless (state-match state)
          (display-ln "\"" (get-state-name visited state) "\""
                      "[ label = \"\" ];")
          (for-each (lambda (a)
                      (display-ln "\"" (get-state-name visited state) "\""
                                  "->"
                                  "\"" (get-state-name visited (cdr a)) "\""
                                  "[ label = \"" (car a) "\" ];")
                      (display-state visited (cdr a)))
                    (state-edges state))))


)
