;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

#lang typed/racket/base
(provide (all-defined-out))

; Union types for `edge.s` and `state` are currently necessary because the same types are used
; in NFA, DFA, and the  intermediate states while converting NFA->DFA.

(define-type NonTerminalName String)
(define-type AutomatonIndex Integer)
(define-type Wildcard 'wild)
(define-type CharRange (Pairof Char Char))
(define-type CharClass (Listof (U Char CharRange)))
(define-type OptionalOrClosure 'e)
(define-type Transition (U NonTerminalName AutomatonIndex Wildcard Char CharClass OptionalOrClosure))
(define-type State (U AutomatonIndex state))
(define-type AutomatonMode (U 'voidArrow 'pruneArrow 'leftArrow '& '!))

;; t - Allowed transition
;; s - The state to transition to
;; v - If the result of the cost should be included in the tree
(struct edge
  ([t : Transition]
   ; State in NFA, (Listof State) in DFA
   [s : (U State (Listof State))]
   [v : Boolean])
  #:mutable)

(struct state
  ([edges : (Listof edge)]
   [match : Boolean])
   #:mutable)

;; type - string if Non-Terminal
;; states - a vector of states
;; mode - the automaton mode
(struct fa
  ([type : Symbol]
   [states : (Vectorof state)]
   [mode : AutomatonMode]))
