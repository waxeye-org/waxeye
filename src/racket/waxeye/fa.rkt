;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang racket/base
(provide (all-defined-out))

;; t - The transition cost
;; s - The state to transition to
;; v - If the result of the cost should be included in the tree
(struct edge (t s v) #:mutable)

(struct state (edges match) #:mutable)

;; type - string if Non-Terminal
;; states - a vector of states
;; mode - the automaton mode
(struct fa (type states mode))
