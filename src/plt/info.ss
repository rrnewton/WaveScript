
(module info (lib "infotab.ss" "setup")
  (define name "PLT Regiment")
;  (define mred-launcher-libraries (list))
;  (define mred-launcher-names (list))

  (define omits     (list "pass_check_simple.ss"
	  "demo_display.ss"
	  "define-structure.ss"
	  "copy-struct.ss" ;; [2005.11.04] Don't need this any more.
	  "network_topologies.ss"
	  "lang00.ss"
	  "lang10.ss"
	  "language-mechanism.ss"
	  "reg:define-struct.ss"
	  "temp.ss"
	  "tests.ss"
	  "test.ss"
	  "testfoo.ss"
	  "driver.ss"
	  "engines2.ss"

;	  "rn-match.ss"

	  "aggregation_experiments.ss"

	  "rrn-current-test.ss"
	  "_SIM_nodeprog.ss"
	  "_SIM_socprog.ss"
	  "_genned_node_code.ss"
	  "._genned_node_code.ss"

	  ;; TEMPORARILY:
	  "simulator_nought.ss"
	  "simulator_nought_graphics.ss"


	  ;; Used only via INCLUDES, don't contain modules:
	  "prim_defs.ss"
	  "prim_defs_OLD.ss"
	  "pass23_desugar-gradients_verbose.ss"
	  "pass23_desugar-gradients_simple.ss"
	  "pass23_desugar-gradients_ETX.ss"

;	  "alpha_lib_scheduler_simple.ss"

	  "pass15_add-routing.ss"	  
	  "passXX_separate-graph.ss"))

  ;; These are all the files that SHOULD NOT be compiled as part of the package.
  (define compile-omit-files  omits)
  (define compile-zo-omit-files (list))
  (define compile-extension-omit-files 
    ;; Giving some problem at the moment:
    (list "regiment_parser.ss"
	  "constants.ss"))
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

