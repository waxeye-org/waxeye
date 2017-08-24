;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.


;; These are tests for the 'Grammar' non-terminal
(Grammar ; <- This is the non-terminal's name

 ;; Following the name are pairs of input string and expected output. The
 ;; output is either the keyword 'pass', the keyword 'fail' or an AST. The AST
 ;; specifies the structure of the expected tree, the names of the nodes and
 ;; the individual characters. If you don't want to specify the whole tree,
 ;; just use the wild-card symbol '*' for the portion of the tree you want to
 ;; skip.

 "" ; <- This is the input
 (Grammar) ; <- This is the expected output

 "A <- 'a'"
 pass ; <- The keyword 'pass'

 "A"
 fail ; <- The keyword 'fail'

 "A <- 'a' B <- 'b'"
 (Grammar (Definition (Identifier #\A) *)  ; <- Here we skip some of
          (Definition (Identifier #\B) *)) ;    Definition's children

 "A <- 'a'"
 (Grammar (*)) ; <- Here we specify a child tree of any type

 "A <- [a-z] *[a-z0-9]"
 (Grammar (Definition (Identifier #\A) (LeftArrow) (Alternation *)))

 "A <- 'a'"
 (Grammar (Definition (Identifier #\A)
            (LeftArrow) (Alternation (Sequence (Unit (Literal (LChar #\a)))))))
 )


(Literal
 "'in'"
 (Literal (LChar #\i) (LChar #\n))

 "''"
 fail
 )


(Range
 ""
 fail

 "-"
 (Range (Char #\-))

 "a"
 (Range (Char #\a))

 "a-z"
 (Range (Char #\a) (Char #\z))

 "\\u{0C}"
 (Range (Hex #\0 #\C))

 "\\u{30}-\\u{39}"
 (Range (Hex #\3 #\0) (Hex #\3 #\9))

 "0-\\u{39}"
 (Range (Char #\0) (Hex #\3 #\9))

 "\\u{30}-9"
 (Range (Hex #\3 #\0) (Char #\9))

 "\\u{1F381}-\\u{1F382}"
 (Range (Hex #\1 #\F #\3 #\8 #\1) (Hex #\1 #\F #\3 #\8 #\2))
 )


(Alt
 "|"
 pass

 "| "
 pass

 " | "
 fail
 )
