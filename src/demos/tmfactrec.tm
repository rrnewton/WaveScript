(deglobalize-lang
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm 
     (tokens
      [tok1 () (dbg "TM: FACT Recursive returned: %d\\n" (call fact 6))]
      [fact (n) (if (= 0 n) 
		    1
		    (* n (call fact (- n 1))))]
      )
     (startup))))
