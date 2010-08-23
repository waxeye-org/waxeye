;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
ast
mzscheme

(require (only (lib "9.ss" "srfi") define-record-type))
(provide (all-defined))


;; ast
;;
;; t = The type of the ast as a symbol
;; c = The list of the ast's children as nested asts or characters
;; p = The position of the ast in the original string as a pair of start and end indexes
(define-record-type :ast
  (make-ast t c p)
  ast?
  (t ast-t ast-t!)
  (c ast-c ast-c!)
  (p ast-p ast-p!))


(define-record-type :parse-error
  (make-parse-error pos line col nt)
  parse-error?
  (pos parse-error-pos parse-error-pos!)
  (line parse-error-line parse-error-line!)
  (col parse-error-col parse-error-col!)
  (nt parse-error-nt parse-error-nt!))


(define (ast->string ast)
  (let ((indent-level 0) (o (open-output-string)))
    (define (display-a c)
      (when (> indent-level 0)
            (display "->  " o))
      (display (ast-t c) o)
      (set! indent-level (+ indent-level 1))
      (for-each (lambda (a)
                  (newline o)
                  (display-iter a))
                (ast-c c))
      (set! indent-level (- indent-level 1)))
    (define (display-c c)
      (when (> indent-level 0)
            (display "|   " o))
      (display c o))
    (define (display-iter ast)
      (when (or (char? ast) (ast? ast))
            (let loop ((i 1))
              (when (< i indent-level)
                    (display "    " o)
                    (loop (+ i 1))))
            (if (char? ast)
                (display-c ast)
                (display-a ast))))
    (display-iter ast)
    (get-output-string o)))


(define (display-ast ast)
  (display
   (cond
    ((ast? ast) (ast->string ast))
    ((parse-error? ast) (parse-error->string ast))
    (else ast)))
  (newline))


(define (ast->string-sexpr ast)
  (let ((o (open-output-string)))
    (define (display-iter ast)
      (display "(" o)
      (display (ast-t ast) o)
      (for-each (lambda (a)
                  (display " " o)
                  (if (ast? a)
                      (display-iter a)
                      (display a o)))
                (ast-c ast))
      (display ")" o))
    (display-iter ast)
    (get-output-string o)))


(define (parse-error->string error)
  (let ((pos (parse-error-pos error))
        (line (parse-error-line error))
        (col (parse-error-col error))
        (nt (parse-error-nt error)))
    (string-append
     "parse error: failed to match '"
     (symbol->string nt)
     "' at line="
     (number->string line)
     ", col="
     (number->string col)
     ", pos="
     (number->string pos))))


(define (display-parse-error error)
  (display (parse-error->string error))
  (newline))

)
