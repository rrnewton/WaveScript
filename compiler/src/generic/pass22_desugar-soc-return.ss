
;; [2005.04.20]
;; Soc-return's are a strange beast.

;; [2005.10.02] I was just doing this in cleanup-tokmac, but I'm going to move it here.




	     ;; For now this is just syntactic sugar for routing on the global tree:   
	     ;; return-retry indi
	     [(soc-return ,x)
;	      (loop `(return-retry ,x (to (tok SOC-return-handler 0)) (via (tok global-tree 0))))]
	      (let ([socretval (unique-name 'socretval)])
		(loop `(let ([,socretval ,x])
			 (if (= (my-id) ',BASE_ID)
			     (begin 
			       ,@(DEBUGMODE `(dbg "Soc return on basenode, returning directly: %d" ,socretval))
			       (call (tok SOC-return-handler 0) ,socretval))
			     (greturn ,socretval (to (tok SOC-return-handler 0)) (via (tok global-tree 0)))))))]
	     ;; Sending to subtok 1 indicates that we're finished.
;	     [(soc-return-finished ,x)
;	      (loop `(return ,x (to (tok SOC-return-handler 1)) (via (tok global-tree 0))))]
	     
