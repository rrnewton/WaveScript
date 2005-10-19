
(define-syntax foo
  (syntax-rules ()
    [(_) bar]))

(module simulator_alpha
	(run-simulator-alpha
	 compile-simulate-alpha csa ; shorthand
	 test-this these-tests
	 testalpha testsalpha

	 posdist
	 )

(import simulator_alpha_datatypes)
(import alpha_lib_scheduler_simple)

;; We are loaded from the root directory, not the chez subdirectory.
;(include "generic/simulator_nought.examples.ss")
(include "generic/simulator_alpha.ss")

)

#!eof

;; obj-hash maps node-ids onto simobjects

;; [2005.03.13]  Adding this to represent events-to-happen in the simulator.
(define-structure )

;; [2005.05.06]
;; A first class representation of tokens:
(define-structure )
;; TODO: Change the system to use these ^^


;; This structure contains everything an executing token handler needs
;; to know about the local node.  "this" is a simobject.  tokstore is
;; a struct containing all the stored values.

;  [2005.03.05] Putting everything in simobject, "this" provides everything.
;(define-structure (localinfo this I-am-SOC tokstore))

;; Positions are just 2-element lists.
(define-structure )

;; This structure represents a simulated node:
;; Incoming is a list of token messages.
;; Redraw is a boolean indicating whether the object needs be redrawn.
;; [2004.06.11] Added homepage just for my internal hackery.
;; [2004.06.13] Be careful to change "cleanse-world" if you change
;;   this, we don't want multiple simulation to be thrashing eachother.
;; [2004.07.08] I don't know why I didn't do this, but I'm storing the
;;   token-cache in the structure too
(define-structure )
;; The token store is a hash table mapping simtok objects to token objects.
;; The token objects themselves are just vectors of stored variables.
;; By convention, the first slot of the token object is a counter for how many times the 
;; handler has been invoked.


;; This structure represents a message transmitted across a channel.
;; None of these should be mutated:
(define-structure )
