;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
parser
mzscheme

(require (lib "ast.ss" "waxeye") (lib "fa.ss" "waxeye") (lib "set.ss" "waxeye"))
(provide make-parser)


(define-struct cache-item (val pos line col cr))


(define (make-parser start eof-check automata)
  (lambda (input)
    (let* ((input-len (string-length input))
           (input-pos 0)
           (line 1)
           (column 0)
           (last-cr #f)
           (error-pos 0)
           (error-line 1)
           (error-col 0)
           (error-nt '())
           (fa-stack '())
           (cache (make-hash-table 'equal)))
      (define (match-automaton index)
        (let* ((automaton (vector-ref automata index))
               (type (fa-type automaton))
               (states (fa-states automaton))
               (automaton-mode (fa-mode automaton)))

          ;; If the transition was made
          (define (match-edge edge)

            (define (mv)
              (let ((ch (string-ref input input-pos)))
                (set! input-pos (+ input-pos 1))
                (if (char=? ch #\return)
                    (begin
                      (set! line (+ line 1))
                      (set! column 0)
                      (set! last-cr #t))
                    (begin
                      (if (char=? ch #\linefeed)
                          (unless last-cr
                                  (set! line (+ line 1))
                                  (set! column 0))
                          (set! column (+ column 1)))
                      (set! last-cr #f)))
                ch))

            (let* ((start-pos input-pos)
                   (start-line line)
                   (start-col column)
                   (start-cr last-cr)
                   (t (edge-t edge))
                   (res (cond
                         ;; If we have a wild card expression
                         ((equal? 'wild t) (if (< input-pos input-len)
                                               (mv)
                                               (record-error)))
                         ;; If we have a character match
                         ((char? t) (if (and (< input-pos input-len) (equal? (string-ref input input-pos) t))
                                        (mv)
                                        (record-error)))
                         ;; If we have a character class
                         ((pair? t) (if (and (< input-pos input-len) (within-set? t (string-ref input input-pos)))
                                        (mv)
                                        (record-error)))
                         ;; If we have a reference to another automata
                         ((integer? t) (match-automaton t))
                         (else #f))))
              ;; If we are able to transition to the next state
              (if res
                  ;; Move to next state
                  (let ((tran-res (match-state (vector-ref states (edge-s edge)))))
                    (if tran-res
                        (if (or (edge-v edge) (equal? res #t))
                            tran-res
                            (cons res tran-res))
                        (begin
                          (restore-pos start-pos start-line start-col start-cr)
                          #f)))
                  #f)))

          (define (match-edges edges)
            (if (null? edges)
                #f
                (let ((res (match-edge (car edges))))
                  (if res
                      res
                      (match-edges (cdr edges))))))

          (define (match-state state)
            (let ((res (match-edges (state-edges state))))
              (if res
                  res
                  (and (state-match state) '()))))

          (let* ((key (cons index input-pos)) (value (hash-table-get cache key #f)))
            (if value
                (begin
                  (restore-pos (cache-item-pos value) (cache-item-line value) (cache-item-col value) (cache-item-cr value))
                  (cache-item-val value))
                (begin
                  ;; Push to the fa-stack
                  (set! fa-stack (cons (cons automaton #f) fa-stack))
                  (let ((v (let ((start-pos input-pos)
                                 (start-line line)
                                 (start-col column)
                                 (start-cr last-cr)
                                 (res (match-state (vector-ref states 0))))
                             (cond
                              ((equal? type '&) (restore-pos start-pos start-line start-col start-cr) (not (not res)))
                              ((equal? type '!)
                               (restore-pos start-pos start-line start-col start-cr)
                               (if res
                                   (update-error)
                                   #t))
                              (else
                               (if res
                                   (case automaton-mode
                                     ((voidArrow)
                                      #t)
                                     ((pruneArrow)
                                      (cond
                                       ((null? res)
                                        #t)
                                       ((null? (cdr res))
                                        (car res))
                                       (else
                                        (make-ast type res (cons start-pos input-pos)))))
                                     ((leftArrow)
                                      (make-ast type res (cons start-pos input-pos)))
                                     (else (error 'waxeye "Unknown automaton mode")))
                                   ;; Don't need to restore here since we already did
                                   (update-error)))))))
                    ;; Pop from the fa-stack
                    (set! fa-stack (cdr fa-stack))
                    (hash-table-put! cache key (make-cache-item v input-pos line column last-cr))
                    v))))))

      (define (restore-pos p l c cr)
        (set! input-pos p)
        (set! line l)
        (set! column c)
        (set! last-cr cr))

      (define (record-error)
        ;; did we find a deeper error
        (when (< error-pos input-pos)
              (set! error-pos input-pos)
              (set! error-line line)
              (set! error-col column)
              (set! error-nt '()))
        ;; record the name of the non-terminal for errors of same or greater depth
        (when (<= error-pos input-pos)
              (set! fa-stack (cons (cons (caar fa-stack) #t) (cdr fa-stack))))
        #f)

      (define (update-error)
        (when (cdar fa-stack) ;; when there was a reported error
              (set! error-nt (cons (fa-type (caar fa-stack)) error-nt)))
        #f)

      (define (do-eof-check res)
        (if res
            (if (and eof-check (< input-pos input-len))
                ;; Create a parse error - Not all input consumed
                (make-parse-error error-pos error-line error-col error-nt (received))
                res)
            ;; Create a parse error
            (make-parse-error error-pos error-line error-col error-nt (received))))

      (define (received)
        (if (= error-pos input-len)
            "<end of input>"
            (substring input error-pos (+ error-pos 1))))

      (do-eof-check (match-automaton start)))))

)
