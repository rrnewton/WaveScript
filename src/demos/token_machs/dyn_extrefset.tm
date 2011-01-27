
;; gemit is transformed into a call 


(token SOC-start ()
  (printf "(Starting on soc)\n")
  (call x 3)
  (call f (tok x 0))
  )



(token f (t)
  (printf "(Static_ext-ref: %d)\n" (ext-ref x acc))
  (printf "(Static_ext-set: %d)\n" (ext-set! x acc 4))
  (printf "(Static_ext-ref: %d)\n" (ext-ref x acc))

  (printf "(Dynamic Token: %d)\n" t)
  (printf "(Dynamic ext-ref: %d)\n" (ext-ref t 0))
  
  )

(token x (v)
  (stored [acc v])
  (void))


; ======================================================================
; These are some commands for invoking this file from the interactive REPL:
; (mvlet (((prog _) (read-regiment-source-file "demos/token_machs/dyn_extrefset.tm"))) (set! theprog (at prog 'verbose 'barely-tokens)))
; (load-regiment "demos/regiment/nested_regions.rs")
