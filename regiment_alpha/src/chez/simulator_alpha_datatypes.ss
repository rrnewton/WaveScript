
(module simulator_alpha_datatypes
	(	 
	 simalpha-total-messages
	 simalpha-total-tokens
	 logger
	 evntlessthan 
	 soc-return-buffer
	 escape-alpha-sim

	 ;; Global parameter: temporary, for debugging:
	 global-graph	 

	 token->key key->token

	 ;; HACK: FIXME : FIND A BETTER SYSTEM
	 make-simworld
	 simworld?
	 simworld-graph 
	 simworld-object-graph 
	 simworld-all-objs 
	 simworld-obj-hash 
	 simworld-scheduler-queue
	 simworld-vtime
	 set-simworld-graph!
	 set-simworld-object-graph!
	 set-simworld-all-objs!
	 set-simworld-obj-hash!
	 set-simworld-scheduler-queue!
	 set-simworld-vtime!

	 make-simevt
	 simevt?
	 simevt-vtime 
	 simevt-msgobj
	 set-simevt-vtime!
	 set-simevt-msgobj!

	 make-simtok
	 simtok?
	 simtok-name 
	 simtok-subid
	 set-simtok-name!
	 set-simtok-subid!
	 
	 simtok-equal?

	 make-node
	 node?
	 node-id
	 node-pos
	 set-node-id!
	 set-node-pos!

	 make-simobject
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

	 make-msg-object
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

	 ;; Safer constructor:
	 bare-msg-object 
	 
	 )

(import scheme)

(include "generic/simulator_alpha_datatypes.ss")
)

