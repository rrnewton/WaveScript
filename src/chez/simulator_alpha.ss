
(define-syntax foo
  (syntax-rules ()
    [(_) bar]))

(module simulator_alpha
	(run-simulator-alpha
	 compile-simulate-alpha csa ; shorthand
	 test-this these-tests
	 testalpha testsalpha

	 ;; Extras
	 make-simtok bare-msg-object make-simevt logger
	 evntlessthan simalpha-total-messages
	 soc-return-buffer
	 
;; HACK: FIXME : FIND A BETTER SYSTEM

simworld?
simworld-graph 
simworld-object-graph 
simworld-all-objs 
simworld-obj-hash 
simworld-scheduler-queue
set-simworld-graph!
set-simworld-object-graph!
set-simworld-all-objs!
set-simworld-obj-hash!
set-simworld-scheduler-queue!

simevt?
simevt-vtime 
simevt-msgobj
set-simevt-vtime!
set-simevt-msgobj!

simtok?
simtok-name 
simtok-subid
set-simtok-name!
set-simtok-subid!

node?
node-id
node-pos
set-node-id!
set-node-pos!

simobject?
simobject-node
simobject-I-am-SOC
simobject-token-store
simobject-incoming-msg-buf 
simobject-local-msg-buf
simobject-outgoing-msg-buf
simobject-timed-token-buf 
simobject-local-sent-messages
simobject-local-recv-messages
simobject-redraw 
simobject-gobj 
simobject-homepage 
simobject-scheduler 
simobject-meta-handler	       	     
simobject-worldptr 
set-simobject-node!
set-simobject-I-am-SOC!
set-simobject-token-store!
set-simobject-incoming-msg-buf!
set-simobject-local-msg-buf!
set-simobject-outgoing-msg-buf!
set-simobject-timed-token-buf!
set-simobject-local-sent-messages!
set-simobject-local-recv-messages!
set-simobject-redraw!
set-simobject-gobj!
set-simobject-homepage!
set-simobject-scheduler!
set-simobject-meta-handler!
set-simobject-worldptr!

msg-object?
msg-object-token
msg-object-sent-time
msg-object-parent
msg-object-to
msg-object-args
set-msg-object-token!
set-msg-object-sent-time!
set-msg-object-parent!
set-msg-object-to!
set-msg-object-args!

)
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
