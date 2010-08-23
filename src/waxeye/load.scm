;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
load
mzscheme

(require (lib "ast.ss" "waxeye")
         (only (lib "list.ss" "mzlib") filter)
         "file.scm"
         "gen.scm"
         "grammar-parser.scm"
         "interp.scm"
         "util.scm")
(provide load-grammar modular-grammar!)


(define *modular-grammar* #f)
(define (modular-grammar! val)
  (set! *modular-grammar* val))


(define *load-cache* (make-hash-table 'equal))


(define (load-grammar path)
  (if *modular-grammar*
      (load-modular-grammar path)
      (load-waxeye-grammar path)))


(define (load-waxeye-grammar path)
  (let ((v (hash-table-get *load-cache* path #f)))
    (if v
        v
        (let ((grammar-tree (grammar-parser (file-as-string path))))
          (if (ast? grammar-tree)
              (begin
                (hash-table-put! *load-cache* path grammar-tree)
                grammar-tree)
              (error 'waxeye (string-append "syntax error in grammar " path "\n" (parse-error->string grammar-tree))))))))


(define (load-modular-grammar path)
  ;; Returns the list of modular grammar expressions
  (define (read-modular i)
    (let ((m (read i)))
      (if (eof-object? m)
          '()
          (cons m (read-modular i)))))
  (let ((base-path (call-with-values (lambda () (split-path path)) (lambda (a b c) a))))
    (make-ast 'grammar (list-concat (map (lambda (a)
                                           (resolve-modular a base-path))
                                         (call-with-input-file path read-modular))) (cons 0 0))))


;; Resolve the modular expression
(define (resolve-modular m base-path)
  (cond
   ((string? m)
    (ast-c (load-waxeye-grammar (if (or (absolute-path? m) (equal? base-path 'relative) (not base-path))
                                    m
                                    (build-path base-path m)))))
   ((list? m)
    (apply (case (car m)
             ((rename) resolve-rename)
             ((only) resolve-only)
             ((all-except) resolve-all-except)
             ((prefix) resolve-prefix)
             ((prefix-only) resolve-prefix-only)
             ((prefix-all-except) resolve-prefix-all-except)
             ((join) resolve-join)
             (else (error 'load-modular-grammar "Bad modular grammar expression type: ~s" (car m))))
           (cons base-path (cdr m))))
   (else (error 'load-modular-grammar "Bad modular grammar expression: ~s" m))))


(define (rename-list nts names)
  (let ((t (make-hash-table 'equal)))
    (define (visit-alternation exp)
      (visit-multi-child visit-sequence exp))

    (define (visit-sequence exp)
      (visit-multi-child visit-exp exp))

    (define (visit-multi-child visitor exp)
      (make-ast (ast-t exp) (map visitor (ast-c exp)) (ast-p exp)))

    (define (visit-unit exp)
      (define (visit-unit-children cs)
        (let ((c (car cs)) (rest (cdr cs)))
          (if (null? rest)
              (list (visit-exp c))
              (cons c (visit-unit-children rest)))))
      (make-ast (ast-t exp)
                (visit-unit-children (ast-c exp))
                (ast-p exp)))

    (define (visit-ident exp)
      (let* ((name (string->symbol (list->string (ast-c exp))))
             (new-name (hash-table-get t name #f)))
        (if new-name
            (make-ast (ast-t exp) (string->list (symbol->string new-name)) (ast-p exp))
            exp)))

    (define (visit-exp exp)
      (let ((type (ast-t exp)))
        (case type
          ((action) exp)
          ((alternation) (visit-alternation exp))
          ((caseLiteral) exp)
          ((charClass) exp)
          ((identifier) (visit-ident exp))
          ((label) exp)
          ((literal) exp)
          ((sequence) (visit-sequence exp))
          ((unit) (visit-unit exp))
          ((wildCard) exp)
          (else (error 'expand-grammar "unknown expression type: ~s" type)))))

    (define (rename nt)
      (let* ((name (string->symbol (get-non-term nt)))
             (new-name (hash-table-get t name #f)))
        (make-ast
         (ast-t nt)
         `(,(make-ast 'identifier
                      (string->list (symbol->string (if new-name
                                                        new-name
                                                        name)))
                      (cons 0 0))
           ,(cadr (ast-c nt))
           ,(visit-alternation (caddr (ast-c nt))))
         (ast-p nt))))
    (for-each (lambda (a)
                (hash-table-put! t (car a) (cdr a)))
              names)
    (map rename nts)))


(define (resolve-only base-path exp . non-terms)
  (filter (lambda (a)
            (member (string->symbol (get-non-term a)) non-terms))
          (resolve-modular exp base-path)))


(define (resolve-all-except base-path exp . non-terms)
  (filter (lambda (a)
            (not (member (string->symbol (get-non-term a)) non-terms)))
          (resolve-modular exp base-path)))


(define (resolve-rename base-path exp . names)
  (rename-list (resolve-modular exp base-path) names))


(define (resolve-prefix base-path prefix exp)
  (let ((nts (resolve-modular exp base-path)) (p (symbol->string prefix)))
    (rename-list nts (map (lambda (a)
                            (let ((n (get-non-term a)))
                              (cons (string->symbol n) (string->symbol (string-append p n)))))
                          nts))))


(define (resolve-prefix-only base-path prefix exp . non-terms)
  (let ((nts (resolve-modular exp base-path)) (p (symbol->string prefix)))
    (rename-list nts (map (lambda (a)
                            (let ((n (get-non-term a)))
                              (cons (string->symbol n) (string->symbol (string-append p n)))))
                          (filter (lambda (a)
                                    (member (string->symbol (get-non-term a)) non-terms))
                                  nts)))))


(define (resolve-prefix-all-except base-path prefix exp . non-terms)
  (let ((nts (resolve-modular exp base-path)) (p (symbol->string prefix)))
    (rename-list nts (map (lambda (a)
                            (let ((n (get-non-term a)))
                              (cons (string->symbol n) (string->symbol (string-append p n)))))
                          (filter (lambda (a)
                                    (not (member (string->symbol (get-non-term a)) non-terms)))
                                  nts)))))


(define (resolve-join base-path . exps)
  (list-concat (map (lambda (a)
                      (resolve-modular a base-path))
                    exps)))

)
