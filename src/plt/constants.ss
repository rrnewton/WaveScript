

(module constants mzscheme
	(require (lib "include.ss"))
	(require "helpers.ss")

	(provide 

	 define-regiment-parameter regiment-parameters
	 regiment-verbose simulation-logger simulation-logger-count
	 regiment-consec-ids simulation-logger-level
	 
         slow-pulse fast-pulse
         
         unknown-place noplace
         
	 world-xbound world-ybound radius numsimnodes SPECIAL_RETURN_TOKEN BASE_ID
	 
	 return-window-size

         RADIO_DELAY PROCESSING_TIME
         
	 )
	
	(include "../generic/constants.ss"))
	
