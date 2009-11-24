(deglobalize-lang
  '(program
    (bindings )
    (socpgm (bindings ) 
;	    (tokens [tok1_return (v)
;		       (disp "Got return!" v)]
	    (disp "SOC Running" (node-id (simobject-node this)))
	    (emit tok1))
    (nodepgm
     (tokens
      [tok1_return (v)
		   ;(disp "Got return!" v)
		   (printf "~s." v)
		   (soc-return v)]
      [tok1 ()
	    (display #\-)
;	    (disp "tok1" (node-id (simobject-node this)))
	    (call tok2)
	    (relay)
	    (return (dist)
		    (to tok1_return)
		    (via tok1)
		    )]
      [tok2 () 
	    (display #\_)
;	    (disp "tok2" (node-id (simobject-node this)))
	    (light-node 0 255 0)])
     (startup ) ;; seed tokens
     ))))
