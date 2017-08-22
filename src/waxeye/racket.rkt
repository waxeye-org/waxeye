;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang racket/base
(require         waxeye/ast
         waxeye/fa
         "code.rkt" "dfa.rkt" "gen.rkt")
(provide gen-racket gen-racket-parser)


(define (gen-racket grammar path)
  (indent-unit! 1)
  (let ((file-path (string-append path (if *name-prefix*
                                           (string-append *name-prefix* "-parser.rkt")
                                           "parser.rkt"))))
    (dump-string (gen-racket-parser grammar) file-path)
    (list file-path)))


(define (racket-comment lines)
  (comment-base ";;" lines))


(define (gen-racket-trans a)
  (define (gen-list-item a)
    (if (char? a)
        (format "~s" a)
        (format "(cons ~s ~s)" (car a) (cdr a))))
  (cond
   ((symbol? a) (format "'~s" a))
   ((list? a)
    (format "(list ~a~a)"
            (gen-list-item (car a))
            (apply string-append (map (lambda (b) (format " ~a" (gen-list-item b)))
                                      (cdr a)))))
   (else (format "~s" a))))


(define (gen-racket-edge a)
  (format "\n~a(edge ~a ~a ~a)"
          (ind)
          (gen-racket-trans (edge-t a))
          (edge-s a)
          (edge-v a)))


(define (gen-racket-edges edges)
  (indent (format "(list~a)" (apply string-append (map gen-racket-edge edges)))))


(define (gen-racket-state a)
  (format "\n~a(state ~a ~a)"
          (ind)
          (gen-racket-edges (state-edges a))
          (state-match a)))


(define (gen-racket-states states)
  (indent (format "(vector~a)" (apply string-append (map gen-racket-state (vector->list states))))))


(define (gen-racket-fa a)
  (format "\n~a(fa '~a ~a '~a)"
          (ind)
          (camel-case-lower (symbol->string (fa-type a)))
          (gen-racket-states (fa-states a))
          (fa-mode a)))


(define (gen-racket-parser grammar)
  (let ((parser-name (if *name-prefix*
                         (string-append *name-prefix* "-parser")
                         "parser")))
    (format
#<<EOF
~a
#lang racket/base
(require waxeye/ast waxeye/fa waxeye/parser)
(provide ~a (all-from-out waxeye/ast))

~a

EOF

(if *file-header*
    (racket-comment *file-header*)
    (racket-comment *default-header*))

parser-name

(indentn 2 (format

#<<EOF
(define automata
~a~a)

(define ~a (make-parser ~a ~a automata))
EOF

(ind)
(indent (format "(vector~a)" (apply string-append (map gen-racket-fa (vector->list (make-automata grammar))))))
parser-name
*start-index*
*eof-check*
))

)))
