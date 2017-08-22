;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
fa
mzscheme

(provide (all-defined))

;; t - The transition cost
;; s - The state to transition to
;; v - If the result of the cost should be included in the tree
(define-struct edge (t s v))

(define-struct state (edges match))

;; type - string if Non-Terminal
;; states - a vector of states
;; mode - the automaton mode
(define-struct fa (type states mode))

)
