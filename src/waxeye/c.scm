;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
c
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         (only (lib "list.ss" "mzlib") filter)
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
(provide gen-c)


(define *c-prefix* "")
(define *c-parser-name* "")
(define *c-type-name* "")
(define *c-type-prefix* "")
(define *c-header-name* "")
(define *c-source-name* "")


;; have we generated a character class
;; used to avoid declaring variables we don't need
(define *done-cc* #f)


(define (gen-c-names)
  (set! *c-prefix* (if *name-prefix*
                       (string-append (camel-case-lower *name-prefix*) "_")
                       ""))
  (set! *c-parser-name* (string-append *c-prefix* "parser"))
  (set! *c-type-name* (string-append *c-prefix* "type"))
  (set! *c-type-prefix* (string-append (string->upper *c-prefix*) "TYPE_"))
  (set! *c-header-name* (string-append *c-parser-name* ".h"))
  (set! *c-source-name* (string-append *c-parser-name* ".c")))


(define (gen-c grammar path)
  (indent-unit! 4)
  (gen-c-names)
  (let ((header-path (string-append path *c-header-name*))
        (source-path (string-append path *c-source-name*)))
    (dump-string (gen-header grammar) header-path)
    (dump-string (gen-parser grammar) source-path)
    (list header-path source-path)))


(define (c-comment lines)
  (comment-bookend "/*" " *" " */" lines))


(define (c-header-comment)
  (if *file-header*
      (c-comment *file-header*)
      (c-comment *default-header*)))


(define (gen-header grammar)
  (let ((non-terms (get-non-terms grammar))
        (parser-name (if *name-prefix*
                         (string-append (camel-case-upper *name-prefix*) "parser")
                         "parser")))
    (format "~a
#ifndef ~a_H_
#define ~a_H_

#ifdef __cplusplus
extern \"C\" {
#endif

#include <waxeye.h>

enum ~a {
~a
};

#ifndef ~a_C_

extern const char *~a_strings[];
extern struct parser_t* ~a_new();

#endif /* ~a_C_ */

#ifdef __cplusplus
}
#endif

#endif /* ~a_H_ */
"
            (c-header-comment)
            (string->upper *c-parser-name*)
            (string->upper *c-parser-name*)
            *c-type-name*
            (indent
             (string-append
              (ind)
              *c-type-prefix*
              (string->upper (car non-terms))
              (string-concat
               (map (lambda (a)
                      (string-append ",\n" (ind) *c-type-prefix* (string->upper a)))
                    (cdr non-terms)))))
            (string->upper *c-parser-name*)
            *c-type-name*
            *c-parser-name*
            (string->upper *c-parser-name*)
            (string->upper *c-parser-name*))))


(define (gen-parser grammar)
  (let ((automata (make-automata grammar))
        (non-terms (get-non-terms grammar)))
    (format "~a
#define ~a_C_
#include \"~a\"
#include <assert.h>

const char *~a_strings[] = {
~a
};

struct parser_t* ~a_new() {
~a
}
"
            (c-header-comment)
            (string->upper *c-parser-name*)
            *c-header-name*
            *c-type-name*
            (indent
             (string-append
              (ind)
              "\"" (car non-terms) "\""
              (string-concat
               (map (lambda (a)
                      (string-append ",\n" (ind) "\"" a "\""))
                    (cdr non-terms)))))
            *c-parser-name*
            (indent
             ;; force the automaton to be generated first so we know
             ;; if the declarations for character classes are needed
             (let ((fas (mapi->s gen-fa (vector->list automata)))
                   (cc-decl (if (not *done-cc*)
                                ""
                                (format "~achar *single;
~achar *min;
~achar *max;
~asize_t num_single;
~asize_t num_range;\n"
                                        (ind) (ind) (ind) (ind) (ind)))))
               (format "~aconst size_t start = ~a;
~aconst bool eof_check = ~a;
~a~asize_t num_edges;
~asize_t num_states;
~aconst size_t num_automata = ~a;
~astruct trans_t trans;
~aunion trans_data trans_d;
~astruct edge_t *edges;
~astruct state_t *states;
~astruct fa_t *automata = calloc(num_automata, sizeof(struct fa_t));
~aassert(automata != NULL);

~a~areturn wparser_new(start, automata, num_automata, eof_check);"
                       (ind)
                       (number->string *start-index*)
                       (ind)
                       (bool->s *eof-check*)
                       cc-decl
                       (ind) (ind) (ind)
                       (number->string (vector-length automata))
                       (ind) (ind) (ind) (ind) (ind) (ind)
                       fas
                       (ind)))))))


(define (mapi fn l)
  (let ((i -1))
    (map (lambda (a)
           (set! i (+ i 1))
           (fn i a))
         l)))


(define (mapi->s fn l)
  (string-concat (mapi fn l)))


(define (gen-mode a)
  (let ((type (fa-type a)))
    (cond
     ((equal? type '&) "POS")
     ((equal? type '!) "NEG")
     (else
      (case (fa-mode a)
        ((voidArrow) "VOID")
        ((pruneArrow) "PRUNE")
        ((leftArrow) "LEFT"))))))


(define (gen-fa i a)
  (format "~anum_states = ~a;
~astates = calloc(num_states, sizeof(struct state_t));
~aassert(states != NULL);
~a~afa_init(&automata[~a], MODE_~a, ~a, states, num_states);\n\n"
          (ind)
          (vector-length (fa-states a))
          (ind) (ind)
          (mapi->s gen-state (vector->list (fa-states a)))
          (ind)
          i
          (gen-mode a)
          (let ((type (fa-type a)))
            (if (or (equal? type '&) (equal? type '!))
                0
                (string-append *c-type-prefix* (string->upper (symbol->string type)))))))


(define (gen-state i s)
  (format "~anum_edges = ~a;
~aedges = calloc(num_edges, sizeof(struct edge_t));
~aassert(edges != NULL);
~a~astate_init(&states[~a], edges, num_edges, ~a);\n"
          (ind)
          (length (state-edges s))
          (ind) (ind)
          (mapi->s gen-edge (state-edges s))
          (ind)
          i
          (bool->s (state-match s))))


(define (gen-edge i e)
  (format "~a~aedge_init(&edges[~a], trans, ~a, ~a);\n"
          (gen-trans (edge-t e))
          (ind)
          i
          (edge-s e)
          (bool->s (edge-v e))))


(define (gen-trans t)
  (cond
   ((equal? t 'wild) (gen-wild-card-trans))
   ((integer? t) (gen-automaton-trans t))
   ((char? t) (gen-char-trans t))
   ((pair? t) (gen-char-class-trans t))))


(define (gen-automaton-trans t)
  (format "~atrans_d.fa = ~a;
~atrans_init(&trans, TRANS_FA, trans_d);\n"
          (ind) t (ind)))


(define (gen-char-trans t)
  (format "~atrans_d.c = ~a;
~atrans_init(&trans, TRANS_CHAR, trans_d);\n"
          (ind) (gen-char t) (ind)))


(define (gen-char-class-trans t)
  (let* ((single (filter char? t))
         (ranges (filter pair? t))
         (min (map car ranges))
         (max (map cdr ranges)))
    (set! *done-cc* #t)
    (format "~anum_single = ~a;
~anum_range = ~a;
~a
~a
~a
~atrans_d.set = set_new(single, num_single, min, max, num_range);
~atrans_init(&trans, TRANS_SET, trans_d);\n"
            (ind) (length single)
            (ind) (length ranges)
            (gen-char-list "single" "single" single)
            (gen-char-list "min" "range" min)
            (gen-char-list "max" "range" max)
            (ind) (ind))))


(define (gen-char-list name size l)
  (define (ass-char i c)
    (format "\n~a~a[~a] = ~a;" (ind) name i (gen-char c)))
  (format "~a~a = calloc(num_~a, sizeof(char));
~aassert(~a != NULL);~a"
          (ind) name size
          (ind) name
          (if (null? l)
              ""
              (mapi->s ass-char l))))


(define (gen-char t)
  (format "'~a~a'"
          (if (escape-for-java-char? t) "\\" "")
          (cond
           ((equal? t #\linefeed) "\\n")
           ((equal? t #\tab) "\\t")
           ((equal? t #\return) "\\r")
           (else t))))


(define (gen-wild-card-trans)
  (format "~atrans_d.c = '\\0';
~atrans_init(&trans, TRANS_WILD, trans_d);\n"
          (ind) (ind)))


)
