;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.
;;
;; ------------------------------------------------------------------------------
;;
;; This is the parser for Waxeye grammar files. It was generated from the grammar
;; 'grammars/waxeye.waxeye'.

(module
grammar-parser
mzscheme

(require (lib "ast.ss" "waxeye") (lib "fa.ss" "waxeye") (lib "parser.ss" "waxeye"))
(provide grammar-parser (all-from (lib "ast.ss" "waxeye")))

(define automata
  (vector
   (make-fa 'grammar (vector
    (make-state (list
     (make-edge 27 1 #f)) #f)
    (make-state (list
     (make-edge 1 1 #f)) #t)) 'leftArrow)
   (make-fa 'definition (vector
    (make-state (list
     (make-edge 8 1 #f)) #f)
    (make-state (list
     (make-edge 17 2 #f)
     (make-edge 18 2 #f)
     (make-edge 19 2 #f)) #f)
    (make-state (list
     (make-edge 2 3 #f)) #f)
    (make-state (list
     (make-edge 27 4 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'alternation (vector
    (make-state (list
     (make-edge 3 1 #f)) #f)
    (make-state (list
     (make-edge 20 2 #f)) #t)
    (make-state (list
     (make-edge 3 1 #f)) #f)) 'leftArrow)
   (make-fa 'sequence (vector
    (make-state (list
     (make-edge 4 1 #f)) #f)
    (make-state (list
     (make-edge 4 1 #f)) #t)) 'leftArrow)
   (make-fa 'unit (vector
    (make-state (list
     (make-edge 5 0 #f)
     (make-edge 6 0 #f)
     (make-edge 8 1 #f)
     (make-edge 21 3 #f)
     (make-edge 7 2 #f)
     (make-edge 9 2 #f)
     (make-edge 10 2 #f)
     (make-edge 12 2 #f)
     (make-edge 16 2 #f)) #f)
    (make-state (list
     (make-edge 28 2 #f)) #f)
    (make-state (list) #t)
    (make-state (list
     (make-edge 2 4 #f)) #f)
    (make-state (list
     (make-edge 22 2 #f)) #f)) 'leftArrow)
   (make-fa 'prefix (vector
    (make-state (list
     (make-edge (list #\! #\& (cons #\* #\+) #\: #\?) 1 #f)) #f)
    (make-state (list
     (make-edge 27 2 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'label (vector
    (make-state (list
     (make-edge 8 1 #f)) #f)
    (make-state (list
     (make-edge 27 2 #f)) #f)
    (make-state (list
     (make-edge #\= 3 #t)) #f)
    (make-state (list
     (make-edge 27 4 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'action (vector
    (make-state (list
     (make-edge #\@ 1 #t)) #f)
    (make-state (list
     (make-edge 8 2 #f)) #f)
    (make-state (list
     (make-edge #\< 3 #t)
     (make-edge 27 8 #f)) #f)
    (make-state (list
     (make-edge 27 4 #f)) #f)
    (make-state (list
     (make-edge 8 5 #f)) #f)
    (make-state (list
     (make-edge 23 6 #f)
     (make-edge #\> 7 #t)) #f)
    (make-state (list
     (make-edge 8 5 #f)) #f)
    (make-state (list
     (make-edge 27 8 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'identifier (vector
    (make-state (list
     (make-edge (list (cons #\A #\Z) (cons #\a #\z)) 1 #f)) #f)
    (make-state (list
     (make-edge (list #\- (cons #\0 #\9) (cons #\A #\Z) #\_ (cons #\a #\z)) 1 #f)
     (make-edge 27 2 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'literal (vector
    (make-state (list
     (make-edge (list #\') 1 #t)) #f)
    (make-state (list
     (make-edge 30 2 #f)) #f)
    (make-state (list
     (make-edge 11 3 #f)
     (make-edge 15 3 #f)) #f)
    (make-state (list
     (make-edge 29 4 #f)
     (make-edge (list #\') 5 #t)) #f)
    (make-state (list
     (make-edge 11 3 #f)
     (make-edge 15 3 #f)) #f)
    (make-state (list
     (make-edge 27 6 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'caseLiteral (vector
    (make-state (list
     (make-edge (list #\") 1 #t)) #f)
    (make-state (list
     (make-edge 32 2 #f)) #f)
    (make-state (list
     (make-edge 11 3 #f)
     (make-edge 15 3 #f)) #f)
    (make-state (list
     (make-edge 31 4 #f)
     (make-edge (list #\") 5 #t)) #f)
    (make-state (list
     (make-edge 11 3 #f)
     (make-edge 15 3 #f)) #f)
    (make-state (list
     (make-edge 27 6 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'lChar (vector
    (make-state (list
     (make-edge #\\ 1 #f)
     (make-edge 34 3 #f)) #f)
    (make-state (list
     (make-edge (list #\" #\' #\\ #\n #\r #\t) 2 #f)) #f)
    (make-state (list) #t)
    (make-state (list
     (make-edge 33 4 #f)) #f)
    (make-state (list
     (make-edge 'wild 2 #f)) #f)) 'leftArrow)
   (make-fa 'charClass (vector
    (make-state (list
     (make-edge #\[ 1 #t)) #f)
    (make-state (list
     (make-edge 35 2 #f)
     (make-edge #\] 3 #t)) #f)
    (make-state (list
     (make-edge 13 1 #f)) #f)
    (make-state (list
     (make-edge 27 4 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'range (vector
    (make-state (list
     (make-edge 14 1 #f)
     (make-edge 15 1 #f)) #f)
    (make-state (list
     (make-edge #\- 2 #t)) #t)
    (make-state (list
     (make-edge 14 3 #f)
     (make-edge 15 3 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'char (vector
    (make-state (list
     (make-edge #\\ 1 #f)
     (make-edge 38 3 #f)) #f)
    (make-state (list
     (make-edge (list #\- (cons #\\ #\]) #\n #\r #\t) 2 #f)) #f)
    (make-state (list) #t)
    (make-state (list
     (make-edge 37 4 #f)) #f)
    (make-state (list
     (make-edge 36 5 #f)) #f)
    (make-state (list
     (make-edge 'wild 2 #f)) #f)) 'leftArrow)
   (make-fa 'hex (vector
    (make-state (list
     (make-edge #\\ 1 #t)) #f)
    (make-state (list
     (make-edge #\< 2 #t)) #f)
    (make-state (list
     (make-edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 3 #f)) #f)
    (make-state (list
     (make-edge (list (cons #\0 #\9) (cons #\A #\F) (cons #\a #\f)) 4 #f)) #f)
    (make-state (list
     (make-edge #\> 5 #t)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'wildCard (vector
    (make-state (list
     (make-edge #\. 1 #t)) #f)
    (make-state (list
     (make-edge 27 2 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'leftArrow (vector
    (make-state (list
     (make-edge #\< 1 #t)) #f)
    (make-state (list
     (make-edge #\- 2 #t)) #f)
    (make-state (list
     (make-edge 27 3 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'pruneArrow (vector
    (make-state (list
     (make-edge #\< 1 #t)) #f)
    (make-state (list
     (make-edge #\= 2 #t)) #f)
    (make-state (list
     (make-edge 27 3 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'voidArrow (vector
    (make-state (list
     (make-edge #\< 1 #t)) #f)
    (make-state (list
     (make-edge #\: 2 #t)) #f)
    (make-state (list
     (make-edge 27 3 #f)) #f)
    (make-state (list) #t)) 'leftArrow)
   (make-fa 'alt (vector
    (make-state (list
     (make-edge #\| 1 #f)) #f)
    (make-state (list
     (make-edge 27 2 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa 'open (vector
    (make-state (list
     (make-edge #\( 1 #f)) #f)
    (make-state (list
     (make-edge 27 2 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa 'close (vector
    (make-state (list
     (make-edge #\) 1 #f)) #f)
    (make-state (list
     (make-edge 27 2 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa 'comma (vector
    (make-state (list
     (make-edge #\, 1 #f)) #f)
    (make-state (list
     (make-edge 27 2 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa 'sComment (vector
    (make-state (list
     (make-edge #\# 1 #f)) #f)
    (make-state (list
     (make-edge 40 2 #f)
     (make-edge 26 3 #f)
     (make-edge 39 3 #f)) #f)
    (make-state (list
     (make-edge 'wild 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa 'mComment (vector
    (make-state (list
     (make-edge #\/ 1 #f)) #f)
    (make-state (list
     (make-edge #\* 2 #f)) #f)
    (make-state (list
     (make-edge 25 2 #f)
     (make-edge 41 3 #f)
     (make-edge #\* 4 #f)) #f)
    (make-state (list
     (make-edge 'wild 2 #f)) #f)
    (make-state (list
     (make-edge #\/ 5 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa 'endOfLine (vector
    (make-state (list
     (make-edge #\return 1 #f)
     (make-edge #\newline 2 #f)
     (make-edge #\return 2 #f)) #f)
    (make-state (list
     (make-edge #\newline 2 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa 'ws (vector
    (make-state (list
     (make-edge (list #\tab #\space) 0 #f)
     (make-edge 26 0 #f)
     (make-edge 24 0 #f)
     (make-edge 25 0 #f)) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge 17 1 #f)
     (make-edge 18 1 #f)
     (make-edge 19 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge (list #\') 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge (list #\') 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge (list #\") 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge (list #\") 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge 26 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge #\\ 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge #\] 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge 26 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge #\] 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge #\\ 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge 'wild 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge 26 1 #f)) #f)
    (make-state (list) #t)) 'voidArrow)
   (make-fa '! (vector
    (make-state (list
     (make-edge #\* 1 #f)) #f)
    (make-state (list
     (make-edge #\/ 2 #f)) #f)
    (make-state (list) #t)) 'voidArrow)))

(define grammar-parser (make-parser 0 #t automata))
)
