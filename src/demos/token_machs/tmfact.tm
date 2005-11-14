(deglobalize-lang
  '(program
    (bindings)
    (socpgm  (bindings) (emit tok1))
    (nodepgm 
     (tokens
      [tok1 () (timed-call 300 fact 6)]
      [fact (n) (call loop n 1)]
      [loop (n acc) (if (= 0 n)
			(dbg "TM: FACT DONE: %d\\n" acc)
			(call loop (- n 1) (* n acc)))]
      )
     (startup))))
