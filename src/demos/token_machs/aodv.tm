
;; Simulation configuration:
(parameters 
    [sim-num-nodes 30]
    [sim-timeout 10000]
    [simalpha-channel-model 'lossless]
    [simalpha-failure-model 'none])

;; This token machine program 

(token send-foo ()
  
  (void))


;; A Route Request packet
(token (RREQ . subid) (destid newest-dest-seq)
  (leds on blue)
  (printf "rreQ: ~a ~a ~a\n" (my-id) destid newest-dest-seq)
      
  ; If we are the destination, or if we have a newer route to the
  ; destination, then we send a RREP back to the source.
  (if (or (= destid (my-id))
	  (and (token-present? (tok RREP destid))
	       (> (gversion (tok RREP destid)) newest-dest-seq)))
      (gemit (tok RREP destid) (gparent (tok RREQ subid)))
      ; Otherwise, we continue forwarding the request:
      (grelay (tok RREQ subid) destid newest-dest-seq))
  )


;; A Route Reply packet:
(token (RREP . subid) (target)
  ;; Don't have unicast built into the TM model currently:
  (if (= target (my-id))      
      (begin (printf "rreP: ~a ~a\n" (my-id) target)
	     (leds on red)
	     ;; Are we back to the source?
	     (if (= (gorigin RREQ) (my-id))
		 (printf "\nBang!\n")
		 (grelay RREP (gparent RREQ))))
      ))

(token send-data (destid dat)
  (if (token-present? (tok RREP destid))
      (greturn dat
	       (to receive-data)
	       (via (tok RREP destid)))))
      
(token establish-route (destid)
  (printf "\nEstablish from ~a to ~a\n" (my-id) destid)
  (leds on green)
  (call establish-loop destid))

(token establish-loop (destid)
    (gemit RREQ destid)
  ;(timed-call 1000 send-loop destid)
  )


; ======================================================================  
; Below this is the "client" that uses the above "library".

;; One node randomly picks another from which to request a route:
(token SOC-start () (printf "!"))  
(token node-start ()
  (printf ".")
  (if (or (= 10 (my-id)))
      (let ((d (random 30)))
	(call establish-route d)
	(timed-call 2000 try-send d)))
  )

(token try-send (id)
  (call send-data id (random 1000)))

(token receive-data (dat)
  (printf "Received packet: ~a\n" dat))
