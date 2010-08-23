;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
calculator
mzscheme

(require "parser.scm")

;; A commandline arithmetic calculator.

(define (calc input)
  (let ((ast (parser input)))
    (if (ast? ast)
        (begin (display (sum (car (ast-c ast))))
               (newline))
        (display-parse-error ast))))


(define (bin-op ast fn ch op1 op2)
  (let* ((chil (list->vector (ast-c ast)))
         (val (fn (vector-ref chil 0))))
    (let loop ((i 1))
      (unless (= i (vector-length chil))
              ;; Increment val by the operator applied to val and the operand
              (set! val ((if (equal? (vector-ref chil i) ch) op1 op2)
                         val (fn (vector-ref chil (+ i 1)))))
              (loop (+ i 2))))
    val))


(define (sum ast)
  (bin-op ast prod #\+ + -))


(define (prod ast)
  (bin-op ast unary #\* * /))


(define (unary ast)
  (case (ast-t ast)
    ((unary) (- (unary (cadr (ast-c ast)))))
    ((sum) (sum ast))
    (else (num ast))))


(define (num ast)
  (string->number (list->string (ast-c ast))))


(define (rl)
  (display "calc> ")
  (read-line (current-input-port)))


(let loop ((input (rl)))
  (if (eof-object? input)
      (newline)
      (begin (calc input)
             (loop (rl)))))

)
