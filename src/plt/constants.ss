

(module constants mzscheme
	(require (lib "include.ss"))
  ;(require "helpers.ss")

	(provide 
         ;; Syntax:
         DEBUGMODE DEBUGPRINT DEBUGPRINT2 DEBUGASSERT
         
	 define-regiment-parameter regiment-parameters
	 regiment-verbose simulation-logger simulation-logger-count
	 regiment-consec-ids simulation-logger-level
	 
         slow-pulse fast-pulse
         
         unknown-place noplace
         
	 world-xbound world-ybound radius numsimnodes SPECIAL_RETURN_TOKEN BASE_ID
	 
	 return-window-size

	 DEFAULT_SUBTOK DEFAULT_SUBTOK_VAR
         MAX_SUBTOK
         
         RADIO_DELAY PROCESSING_TIME
   
         
	 )
	
	(include (build-path "generic" "constants.ss"))
	)
