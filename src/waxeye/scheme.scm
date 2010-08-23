;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
scheme
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
(provide gen-scheme gen-scheme-parser)


(define (gen-scheme grammar path)
  (indent-unit! 1)
  (let ((file-path (string-append path (if *name-prefix*
                                           (string-append *name-prefix* "-parser.scm")
                                           "parser.scm"))))
    (dump-string (gen-scheme-parser grammar) file-path)
    (list file-path)))


(define (scheme-comment lines)
  (comment-base ";;" lines))


(define (gen-scheme-trans a)
  (define (gen-list-item a)
    (if (char? a)
        (format "~s" a)
        (format "(cons ~s ~s)" (car a) (cdr a))))
  (cond
   ((symbol? a) (format "'~s" a))
   ((list? a)
    (format "(list ~a~a)"
            (gen-list-item (car a))
            (string-concat (map (lambda (b)
                                  (format " ~a" (gen-list-item b)))
                                (cdr a)))))
   (else (format "~s" a))))


(define (gen-scheme-edge a)
  (format "\n~a(make-edge ~a ~a ~a)"
          (ind)
          (gen-scheme-trans (edge-t a))
          (edge-s a)
          (edge-v a)))


(define (gen-scheme-edges edges)
  (indent (format "(list~a)" (string-concat (map gen-scheme-edge edges)))))


(define (gen-scheme-state a)
  (format "\n~a(make-state ~a ~a)"
          (ind)
          (gen-scheme-edges (state-edges a))
          (state-match a)))


(define (gen-scheme-states states)
  (indent (format "(vector~a)" (string-concat (map gen-scheme-state (vector->list states))))))


(define (gen-scheme-fa a)
  (format "\n~a(make-fa '~a ~a '~a)"
          (ind)
          (camel-case-lower (symbol->string (fa-type a)))
          (gen-scheme-states (fa-states a))
          (fa-mode a)))


(define (gen-scheme-parser grammar)
  (let ((parser-name (if *name-prefix*
                         (string-append *name-prefix* "-parser")
                         "parser")))
    (format
#<<EOF
~a
(module
~a
mzscheme

(require (lib "ast.ss" "waxeye") (lib "fa.ss" "waxeye") (lib "parser.ss" "waxeye"))
(provide ~a (all-from (lib "ast.ss" "waxeye")))

~a
)

EOF

(if *file-header*
    (scheme-comment *file-header*)
    (scheme-comment *default-header*))

parser-name

parser-name

(indentn 2 (format

#<<EOF
(define automata
~a~a)

(define ~a (make-parser ~a ~a automata))
EOF

(ind)
(indent (format "(vector~a)" (string-concat (map gen-scheme-fa (vector->list (make-automata grammar))))))
parser-name
*start-index*
*eof-check*
))

)))

)
