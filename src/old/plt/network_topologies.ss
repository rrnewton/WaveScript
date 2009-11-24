
(module network_toplogy  mzscheme
  (require "simulator_nought.ss")
  
  (include (build-path "generic" "network_topologies.ss"))
  
  ;; Insure provision of verify-regiment:
  (provide ;graph object-graph all-objs 
	   line-graph 
	   )
  )
