(deglobalize-lang
  '(program
    (bindings )
    (socpgm (bindings) (call source))
    (nodepgm
     (tokens
      [source () 
	      (emit tok1)
	      ;(timed-call 1500 source)
	      ]
      [tok1 () (relay)
	    (dbg "Woo: ~s ~s~n" (my-id) (dist))
	    ])
     (startup ) ;; seed tokens
     )))
