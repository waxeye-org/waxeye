;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.
;;
;; ------------------------------------------------------------------------------
;;
;; This is the parser for Waxeye grammar files. It was generated from the grammar
;; 'grammars/waxeye.waxeye'.

#lang racket/base
(require waxeye/ast waxeye/fa waxeye/parser)
(provide grammar-parser (all-from-out waxeye/ast))

(define automata
  (vector
   (fa 'grammar (vector
    (state (list
     (edge 27 1 #f)) #f)
    (state (list
     (edge 1 1 #f)) #t)) 'leftArrow)
   (fa 'definition (vector
    (state (list
     (edge 8 1 #f)) #f)
    (state (list
     (edge 17 2 #f)
     (edge 18 2 #f)
     (edge 19 2 #f)) #f)
    (state (list
     (edge 2 3 #f)) #f)
    (state (list
     (edge 27 4 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'alternation (vector
    (state (list
     (edge 3 1 #f)) #f)
    (state (list
     (edge 20 2 #f)) #t)
    (state (list
     (edge 3 1 #f)) #f)) 'leftArrow)
   (fa 'sequence (vector
    (state (list
     (edge 4 1 #f)) #f)
    (state (list
     (edge 4 1 #f)) #t)) 'leftArrow)
   (fa 'unit (vector
    (state (list
     (edge 5 0 #f)
     (edge 6 0 #f)
     (edge 8 1 #f)
     (edge 21 3 #f)
     (edge 7 2 #f)
     (edge 9 2 #f)
     (edge 10 2 #f)
     (edge 12 2 #f)
     (edge 16 2 #f)) #f)
    (state (list
     (edge 28 2 #f)) #f)
    (state (list) #t)
    (state (list
     (edge 2 4 #f)) #f)
    (state (list
     (edge 22 2 #f)) #f)) 'leftArrow)
   (fa 'prefix (vector
    (state (list
     (edge (list #\! #\& (cons #\* #\+) #\: #\?) 1 #f)) #f)
    (state (list
     (edge 27 2 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'label (vector
    (state (list
     (edge 8 1 #f)) #f)
    (state (list
     (edge 27 2 #f)) #f)
    (state (list
     (edge #\= 3 #t)) #f)
    (state (list
     (edge 27 4 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'action (vector
    (state (list
     (edge #\@ 1 #t)) #f)
    (state (list
     (edge 8 2 #f)) #f)
    (state (list
     (edge #\< 3 #t)
     (edge 27 8 #f)) #f)
    (state (list
     (edge 27 4 #f)) #f)
    (state (list
     (edge 8 5 #f)) #f)
    (state (list
     (edge 23 6 #f)
     (edge #\> 7 #t)) #f)
    (state (list
     (edge 8 5 #f)) #f)
    (state (list
     (edge 27 8 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'identifier (vector
    (state (list
     (edge (list (cons #\A #\Z) #\_ (cons #\a #\z)) 1 #f)) #f)
    (state (list
     (edge (list #\- (cons #\0 #\9) (cons #\A #\Z) #\_ (cons #\a #\z)) 1 #f)
     (edge 27 2 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'literal (vector
    (state (list
     (edge (list #\') 1 #t)) #f)
    (state (list
     (edge 30 2 #f)) #f)
    (state (list
     (edge 11 3 #f)
     (edge 15 3 #f)) #f)
    (state (list
     (edge 29 4 #f)
     (edge (list #\') 5 #t)) #f)
    (state (list
     (edge 11 3 #f)
     (edge 15 3 #f)) #f)
    (state (list
     (edge 27 6 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'caseLiteral (vector
    (state (list
     (edge (list #\") 1 #t)) #f)
    (state (list
     (edge 32 2 #f)) #f)
    (state (list
     (edge 11 3 #f)
     (edge 15 3 #f)) #f)
    (state (list
     (edge 31 4 #f)
     (edge (list #\") 5 #t)) #f)
    (state (list
     (edge 11 3 #f)
     (edge 15 3 #f)) #f)
    (state (list
     (edge 27 6 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'lChar (vector
    (state (list
     (edge #\\ 1 #f)
     (edge 34 3 #f)) #f)
    (state (list
     (edge (list #\" #\' #\\ #\n #\r #\t) 2 #f)) #f)
    (state (list) #t)
    (state (list
     (edge 33 4 #f)) #f)
    (state (list
     (edge 'wild 2 #f)) #f)) 'leftArrow)
   (fa 'charClass (vector
    (state (list
     (edge #\[ 1 #t)) #f)
    (state (list
     (edge 35 2 #f)
     (edge #\] 3 #t)) #f)
    (state (list
     (edge 13 1 #f)) #f)
    (state (list
     (edge 27 4 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'range (vector
    (state (list
     (edge 14 1 #f)
     (edge 15 1 #f)) #f)
    (state (list
     (edge #\- 2 #t)) #t)
    (state (list
     (edge 14 3 #f)
     (edge 15 3 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'char (vector
    (state (list
     (edge #\\ 1 #f)
     (edge 38 3 #f)) #f)
    (state (list
     (edge (list #\- (cons #\\ #\]) #\n #\r #\t) 2 #f)) #f)
    (state (list) #t)
    (state (list
     (edge 37 4 #f)) #f)
    (state (list
     (edge 36 5 #f)) #f)
    (state (list
     (edge 'wild 2 #f)) #f)) 'leftArrow)
   (fa 'hex (vector
    (state (list
     (edge #\\ 1 #t)) #f)
    (state (list
     (edge #\u 2 #t)) #f)
    (state (list
     (edge #\{ 3 #t)) #f)
    (state (list
     (edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 4 #f)) #f)
    (state (list
     (edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 5 #f)
     (edge #\} 10 #t)) #f)
    (state (list
     (edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 6 #f)
     (edge #\} 10 #t)) #f)
    (state (list
     (edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 7 #f)
     (edge #\} 10 #t)) #f)
    (state (list
     (edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 8 #f)
     (edge #\} 10 #t)) #f)
    (state (list
     (edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 9 #f)
     (edge #\} 10 #t)) #f)
    (state (list
     (edge #\} 10 #t)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'wildCard (vector
    (state (list
     (edge #\. 1 #t)) #f)
    (state (list
     (edge 27 2 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'leftArrow (vector
    (state (list
     (edge #\< 1 #t)) #f)
    (state (list
     (edge #\- 2 #t)) #f)
    (state (list
     (edge 27 3 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'pruneArrow (vector
    (state (list
     (edge #\< 1 #t)) #f)
    (state (list
     (edge #\= 2 #t)) #f)
    (state (list
     (edge 27 3 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'voidArrow (vector
    (state (list
     (edge #\< 1 #t)) #f)
    (state (list
     (edge #\: 2 #t)) #f)
    (state (list
     (edge 27 3 #f)) #f)
    (state (list) #t)) 'leftArrow)
   (fa 'alt (vector
    (state (list
     (edge #\| 1 #f)) #f)
    (state (list
     (edge 27 2 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa 'open (vector
    (state (list
     (edge #\( 1 #f)) #f)
    (state (list
     (edge 27 2 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa 'close (vector
    (state (list
     (edge #\) 1 #f)) #f)
    (state (list
     (edge 27 2 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa 'comma (vector
    (state (list
     (edge #\, 1 #f)) #f)
    (state (list
     (edge 27 2 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa 'sComment (vector
    (state (list
     (edge #\# 1 #f)) #f)
    (state (list
     (edge 40 2 #f)
     (edge 26 3 #f)
     (edge 39 3 #f)) #f)
    (state (list
     (edge 'wild 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa 'mComment (vector
    (state (list
     (edge #\/ 1 #f)) #f)
    (state (list
     (edge #\* 2 #f)) #f)
    (state (list
     (edge 25 2 #f)
     (edge 41 3 #f)
     (edge #\* 4 #f)) #f)
    (state (list
     (edge 'wild 2 #f)) #f)
    (state (list
     (edge #\/ 5 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa 'endOfLine (vector
    (state (list
     (edge #\return 1 #f)
     (edge #\newline 2 #f)
     (edge #\return 2 #f)) #f)
    (state (list
     (edge #\newline 2 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa 'ws (vector
    (state (list
     (edge (list #\tab #\space) 0 #f)
     (edge 26 0 #f)
     (edge 24 0 #f)
     (edge 25 0 #f)) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge 17 1 #f)
     (edge 18 1 #f)
     (edge 19 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge (list #\') 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge (list #\') 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge (list #\") 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge (list #\") 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge 26 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge #\\ 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge #\] 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge 26 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge #\] 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge #\\ 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge 'wild 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge 26 1 #f)) #f)
    (state (list) #t)) 'voidArrow)
   (fa '! (vector
    (state (list
     (edge #\* 1 #f)) #f)
    (state (list
     (edge #\/ 2 #f)) #f)
    (state (list) #t)) 'voidArrow)))

(define grammar-parser (make-parser 0 #t automata))
