(parameters 

  [dummy-param ;; Just to eval this initialization:
   ;; This turns on the fire-simulation globally.  Can't be undone.
   (begin (install-firelightning))]

  ;[simalpha-placement-type 'connected]

;  [sim-num-nodes 100]  
;  [simalpha-inner-radius 600]
;  [simalpha-outer-radius 1000]
  
  [simalpha-realtime-mode #f]
  [simalpha-dbg-on #f]

  [default-slow-pulse (* 5 60 1000)] ;; 5 min
  [default-fast-pulse (*    3 1000)] ;; 3 sec

;  [desugar-gradients-mode 'inlined]
;  [simalpha-channel-model 'lossless]

  [simalpha-dbg-on #t]

  ;[sim-num-nodes 250]
  ;[sim-num-nodes 30]
;  [sim-timeout 500000]
  [sim-timeout 3600000] ;; An hour.

  )


(token read ()
  (vector (my-id)
	  (my-clock) ;(sync-sense '"time")
	  (sync-sense '"temp")))

`(token abovethresh (vec)
  (if (> (vector-ref vec 2) ,(varied-param))
      (begin (leds on red) #t)
      (begin (leds off red) #f)))

;; Read our data, gossip it.
(token exec ()
  (stored [count 0])
  ;(printf "Executing: count ~s\n" count)
  (let ([vec (subcall read)])
    (if (subcall abovethresh vec)
	;; Then we gossip our entry:
	(bcast gossip vec count)
	)
    (set! count (+ 1 count))
    (timed-call 3000 exec)))

;; Listen to gossip and maintain a table.
(token gossip (tup seqnum)
  (stored [table '()])
  (let ([id (vector-ref tup 0)]
	[time (vector-ref tup 1)]
	[temp (vector-ref tup 2)])
    (set! table (alist_update table id (vector seqnum time temp)))
    (subcall consider table)
    ))

;; Is this table worth reporting?
(token consider (table)
  (stored [seqnum 0])
  (printf "~s: Considering: ~a\n" (my-id) table)
  (let ([thisgen (filter (lambda (v) (= seqnum (vector-ref v 0)))
		   table)]
	[total (listLength table)])
    ;; TODO: Finish:
    (if (> (listLength thisgen) 2)
	(begin 
	  (printf "Returning...\n")
	  (call return-home thisgen))
    )))

(token node-start () (call exec))

(token SOC-start ()
  (dbg "Launching global tree!")
  (call root))
(token root ()
  (gemit globaltree)
  (timed-call 300000 root))
;; Rapidly spread throughout the whole network.
(token globaltree () (grelay))
(token return-home (x)
  (greturn x
	   (to SOC-return-handler)
	   (via globaltree)))

#;
(deglobalize-lang
  '(program
     (bindings)
     (socpgm (bindings) (call spread-global))
     (nodepgm
       (tokens (m_token_resultoftoplevel_6 () (void))
        (m_token_tables_12
          #0=(v t)
          (call f_token_tmprfilter_19 . #1=(v t)))
        (f_token_tmprfilter_19
          #2=(v t)
          (if (subcall tablefilt_19 . #3=(v))
              (call m_token_tmprfilter_19 . #4=(v t))))
        (tmpnonprim_20
          (pattmp_32)
          (let* ([tbl_33 (vector-ref pattmp_32 '1)]) tbl_33))
        (f_token_tmpworld_15
          ()
          (call
            m_token_tmpworld_15
            #5=(cons 'THISNODE (cons (my-id) '()))
            'WORLD-TREE))
        (tmpnonprim_14
	 (this_15 pattmp_14 table_13)
          (let* ([resultoftables_3 (vector
                                     (vector (my-id) table_13)
                                     (alist_update
                                       table_13
                                       (vector-ref pattmp_14 '0)
                                       (vector
                                         (vector-ref pattmp_14 '1)
                                         (vector-ref pattmp_14 '2))))])
            resultoftables_3))
        (m_token_tmprfilter_19
          (v t)
          (call f_token_resultoftoplevel_6 v t))
        (f_token_resultoftoplevel_6
          (v t)
          (call
            m_token_resultoftoplevel_6
            (subcall tmpnonprim_20 v)
            t))
        (m_token_resultoftoplevel_6 (v t) (call reg-return v))



        (m_token_tmpworld_15 (v t) (activate f_token_temps_9 v t))
        (f_token_temps_9
          (v t)
          "rmap with heartbeat"
          (call m_token_temps_9 (subcall read_2 #5#) t)
          (timed-call 5000 f_token_temps_9 v t))
        (m_token_temps_9 #0# (call f_token_heatevents_10 . #1#))
        (f_token_heatevents_10
          #2#
          (if (subcall abovethresh_4 . #3#)
              (call m_token_heatevents_10 . #4#)))
        (m_token_heatevents_10 (v t) (call f_token_strms_11 v t))
        (f_token_strms_11 (v t) (bcast m_token_strms_11 v t))
        (m_token_strms_11 (v t) (call f_token_tables_12 v t))
        (f_token_tables_12
          (v t)
          (stored (state '()))
          (let ([vec (subcall tmpnonprim_14 '#() v state)])
            (printf
              "INTEGRATE: old:~s  new:~s  emitted:~s\n"
              state
              (vector-ref vec 1)
              (vector-ref vec 0))
            (set! state (vector-ref vec 1))
            (call m_token_tables_12 (vector-ref vec 0) t)))
        (tablefilt_19
          (pattmp_20)
          (let* ([resultoftablefilt_5 (> (fold
                                           tmpnonprim_8
                                           '0
                                           (map tmpnonprim_7
                                                (vector-ref pattmp_20 '1)))
                                         '60)])
            resultoftablefilt_5))
        (tmpnonprim_8
          (a_31 b_30)
          (let* ([resultofanonlambda_4 (+ a_31 b_30)])
            resultofanonlambda_4))
        (tmpnonprim_7
          (pattmp_24)
          (let* ([z_26 (vector-ref (vector-ref pattmp_24 '1) '1)])
            z_26))
        (leaf-pulsar_tmpworld_15
          ()
          (call f_token_tmpworld_15)
          (timed-call 60000 leaf-pulsar_tmpworld_15))
        (spread-global
          ()
          (gemit global-tree)
          (timed-call 60000 spread-global))
        (global-tree () (grelay))
        (reg-return
          (v)
          (if (= (my-id) 0)
              (soc-return v)
              (greturn v (to SOC-return-handler) (via global-tree)))))
       (startup leaf-pulsar_tmpworld_15))))
