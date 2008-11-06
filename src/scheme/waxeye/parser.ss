;;; Waxeye Parser Generator
;;; www.waxeye.org
;;; Copyright (C) 2008 Orlando D. A. R. Hill
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is furnished to do
;;; so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(module
parser
mzscheme

(require (lib "ast.ss" "waxeye") (lib "fa.ss" "waxeye") (lib "set.ss" "waxeye"))
(provide make-parser)


(define-struct cache-item (val pos line col cr))


(define (make-parser start eof-check line-counting tab-width automata)
  (lambda (input)
    (let* ((input-len (string-length input))
           (input-pos 0)
           (line 1)
           (column 0)
           (last-cr #f)
           (error-pos 0)
           (error-line 1)
           (error-col 0)
           (error-nt (fa-type (vector-ref automata start)))
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
                                               (update-error)))
                         ;; If we have a character match
                         ((char? t) (if (and (< input-pos input-len) (equal? (string-ref input input-pos) t))
                                        (mv)
                                        (update-error)))
                         ;; If we have a character class
                         ((pair? t) (if (and (< input-pos input-len) (within-set? t (string-ref input input-pos)))
                                        (mv)
                                        (update-error)))
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
                  (set! fa-stack (cons automaton fa-stack))
                  (let ((v (let ((start-pos input-pos)
                                 (start-line line)
                                 (start-col column)
                                 (start-cr last-cr)
                                 (res (match-state (vector-ref states 0))))
                             ;; Pop from the fa-stack
                             (set! fa-stack (cdr fa-stack))
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
                    (hash-table-put! cache key (make-cache-item v input-pos line column last-cr))
                    v))))))

      (define (restore-pos p l c cr)
        (set! input-pos p)
        (set! line l)
        (set! column c)
        (set! last-cr cr))

      (define (update-error)
        (when (< error-pos input-pos)
              (set! error-pos input-pos)
              (set! error-line line)
              (set! error-col column)
              (set! error-nt (fa-type (car fa-stack))))
        #f)

      (define (do-eof-check res)
        (if res
            (if (and eof-check (< input-pos input-len))
                ;; Create a parse error - Not all input consumed
                (make-parse-error error-pos error-line error-col error-nt)
                res)
            ;; Create a parse error
            (make-parse-error error-pos error-line error-col error-nt)))

      (do-eof-check (match-automaton start)))))

)
