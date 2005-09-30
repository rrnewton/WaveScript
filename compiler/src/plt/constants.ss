
#cs ;; Case Sensitivity
(module constants mzscheme
	(require (lib "include.ss"))

	(provide 
         ;; Syntax:
         DEBUGMODE DEBUGPRINT DEBUGPRINT2 DEBUGASSERT
         
	 define-regiment-parameter regiment-parameters
	 regiment-verbose simulation-logger simulation-logger-count
	 regiment-consec-ids simulation-logger-level
	 
         slow-pulse fast-pulse
         
         unknown-place noplace
         
	 DEFAULT_SUBTOK DEFAULT_SUBTOK_VAR
         MAX_SUBTOK
         
         RADIO_DELAY ;PROCESSING_TIME ;; Not used yet

         world-xbound world-ybound radius numsimnodes SPECIAL_RETURN_TOKEN 
         BASE_ID NULL_ID	 
       	 return-window-size
         
         simalpha-num-nodes
         simalpha-output-port
         
         simalpha-dbg-on
         SCHEDULE_DELAY
         
         default-unit-tester-retries
                  
         )
	
	(include (build-path "generic" "constants.ss"))
  )
