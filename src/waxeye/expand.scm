;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
expand
mzscheme

(require (lib "ast.ss" "waxeye")
         (only (lib "list.ss" "mzlib") sort memf)
         "gen.scm")
(provide (all-defined))

(define (expand-grammar grammar)
  (define (lift-only-sub-exp visitor exp)
    (let ((chil (ast-c exp)))
      (for-each visitor chil)
      (when (= (length chil) 1) ; When we only have the one exp
            (let ((only (car chil))); Lift that to become our new expression
              (ast-t! exp (ast-t only))
              (ast-c! exp (ast-c only))
              (ast-p! exp (ast-p only))))))

  (define (visit-alternation exp)
    (lift-only-sub-exp visit-sequence exp))

  (define (visit-sequence exp)
    (ast-c! exp (map expand-unit (ast-c exp)))
    (lift-only-sub-exp visit-exp exp))

  (define (visit-only-child exp)
    (visit-exp (car (ast-c exp))))

  (define (visit-exp exp)
    (let ((type (ast-t exp)))
      (case type
       ((action) (void))
       ((alternation) (visit-alternation exp))
       ((and) (visit-only-child exp))
       ((caseLiteral) (visit-case-literal exp))
       ((charClass) (visit-char-class exp))
       ((closure) (visit-only-child exp))
       ((identifier) (void))
       ((label) (void))
       ((literal) (visit-literal exp))
       ((not) (visit-only-child exp))
       ((optional) (visit-only-child exp))
       ((plus) (visit-only-child exp))
       ((sequence) (visit-sequence exp))
       ((void) (visit-only-child exp))
       ((wildCard) (void))
       (else (error 'expand-grammar "unknown expression type: ~s" type)))))

  (define (expand-def def)
    (visit-alternation (caddr (ast-c def))))
  (for-each expand-def (get-defs grammar)))


(define (expand-unit exp)
  (define (make-prefix v e)
    (let ((r (car (ast-c v))))
      (make-ast
       (cond
        ((equal? r #\*) 'closure)
        ((equal? r #\+) 'plus)
        ((equal? r #\?) 'optional)
        ((equal? r #\:) 'void)
        ((equal? r #\&) 'and)
        ((equal? r #\!) 'not)
        (else (error 'make-prefix "unknown expression type: ~s" r)))
       (list e)
       (cons 0 0))))

  (define (make-label v e)
    (let ((r (car (ast-c v))))
      (make-ast 'label (list e) (cons 0 0))))

  (define (expand-unit-iter el)
    (let ((rest (cdr el)))
      (if (null? rest)
          (car el)
          (let ((type (ast-t (car el))))
            ((case type
              ((prefix) make-prefix)
              ((label) make-label)
              (else (error 'expand-unit-iter "unknown expression type: ~s" type)))
             (car el)
             (expand-unit-iter rest))))))
  (expand-unit-iter (ast-c exp)))


(define (visit-case-literal exp)
  (define (cc-chil c)
    (if (char-alphabetic? c)
        (list (char-upcase c) (char-downcase c))
        (list c)))
  (convert-chars! exp)
  (let ((letters (ast-c exp)))
    (if (memf char-alphabetic? letters)
        (if (null? (cdr letters))
            (let ((c (car letters)))
              (ast-t! exp 'charClass)
              (ast-c! exp (cc-chil c)))
            (begin
              (ast-t! exp 'sequence)
              (ast-c! exp (map (lambda (a)
                                        (make-ast 'charClass (cc-chil a) (cons 0 0)))
                                      letters))))
        (ast-t! exp 'literal))))


(define (convert-char c)
  (define (cc-char c)
    (let ((chil (ast-c c)))
      (if (= (length chil) 1)
          (car chil)
          (let ((s (cadr chil)))
            (cond
             ((equal? s #\n) #\linefeed)
             ((equal? s #\t) #\tab)
             ((equal? s #\r) #\return)
             (else s))))))
  (define (cc-hex c)
    (integer->char (string->number (list->string (ast-c c)) 16)))
  (if (equal? (ast-t c) 'hex)
      (cc-hex c)
      (cc-char c)))


(define (convert-chars! exp)
  (ast-c! exp (map convert-char (ast-c exp))))


(define (visit-literal exp)
  (convert-chars! exp))


(define (visit-char-class exp)
  (define (cc-part part)
    (let ((range (ast-c part)))
      (if (= (length range) 1)
          (convert-char (car range))
          (let ((r1 (convert-char (car range))) (r2 (convert-char (cadr range))))
            (cond
             ((char=? r1 r2) r1)
             ((char<? r1 r2) (cons r1 r2))
             (else
              (cons r2 r1)))))))

  ;; The order of ranges with the same start doesn't matter as they get
  ;; merged no matter what their ends are.
  (define (cc-less-than? a b)
    (char<? (if (char? a)
                a
                (car a))
            (if (char? b)
                b
                (car b))))

  (define (minimise cc)
    (define (next-to? a b)
      (= (- (char->integer b) (char->integer a)) 1))
    (if (null? cc)
        '()
        (let ((a (car cc)) (rest (cdr cc)))
          (if (null? rest)
              cc
              (let ((b (car rest)))
                (if (char? a)
                    (if (char? b)
                        (if (char=? a b) ; Is duplicate char?
                            (minimise (cons a (cdr rest)))
                            (if (next-to? a b) ; Is a next to b?
                                (minimise (cons (cons a b) (cdr rest)))
                                (cons a (minimise rest))))
                        (if (next-to? a (car b)) ; Is a next to range b?
                            (minimise (cons (cons a (cdr b)) (cdr rest)))
                            (cons a (minimise rest))))
                    (if (char? b)
                        (if (or (char=? b (car a)) ; Is b within range a?
                                (char<=? b (cdr a)))
                            (minimise (cons a (cdr rest)))
                            (if (next-to? (cdr a) b) ; Is b next to range a?
                                (minimise (cons (cons (car a) b) (cdr rest)))
                                (cons a (minimise rest))))
                        (if (or (char<=? (car b) (cdr a)) ; Can we merge the ranges?
                                (next-to? (cdr a) (car b)))
                            (minimise (cons
                                       (cons (integer->char (min (char->integer (car a)) (char->integer (car b))))
                                             (integer->char (max (char->integer (cdr a)) (char->integer (cdr b)))))
                                       (cdr rest)))
                            (cons a (minimise rest))))))))))

  (ast-c! exp (minimise (sort (map cc-part (ast-c exp)) cc-less-than?))))

)
