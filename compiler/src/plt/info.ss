
(module info (lib "infotab.ss" "setup")
  (define name "PLT Regiment")
;  (define mred-launcher-libraries (list))
;  (define mred-launcher-names (list))
  (define compile-omit-files 
    (list "pass_check_simple.ss"
	  "demo_display.ss"
	  "network_topologies.ss"
	  "lang00.ss"
	  "lang10.ss"
	  "language-mechanism.ss"
	  "define-structure.ss"
	  "temp.ss"
	  "tests.ss"
	  "test.ss"
	  "driver.ss"
	  "engines2.ss"

	  "aggregation_experiments.ss"

	  "rrn-current-test.ss"
	  "_SIM_nodeprog.ss"
	  "_SIM_socprog.ss"

	  ;; TEMPORARILY:
	  "simulator_nought.ss"
	  "simulator_nought_graphics.ss"

	  "alpha_lib_scheduler_simple.ss"

	  "pass15_add-routing.ss"	  
	  "passXX_separate-graph.ss"))
  (define compile-zo-omit-files (list))
  (define compile-extension-omit-files (list))
  (define compile-subcollections (list))
  )


;   (lambda (request failure-thunk)
;     (case request
;       [(name) "Plt Regiment"]
;       [(compile-prefix) '()]
;       [(compile-omit-files) (list "pass_check_simple.ss")] ;(list "sig.ss" "manuals.ss")]
;       [(compile-elaboration-zos) '()] ;(list "sig.ss")]
;       [(mred-launcher-libraries) '()] ;(list "help.ss")]
;       [(mred-launcher-names) '()] ;(list "Help Desk")]
;       [else (failure-thunk)]))

