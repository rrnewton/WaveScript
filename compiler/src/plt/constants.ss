

(module constants mzscheme
	(require (lib "include.ss"))
	(require "helpers.ss")

	(provide 

	 define-regiment-parameter regiment-parameters
	 regiment-verbose simulation-logger regiment-consec-ids
	 
         slow-pulse fast-pulse
         
         unknown-place noplace
         
	 world-xbound world-ybound radius numsimnodes SPECIAL_RETURN_TOKEN BASE_ID
	 
	 return-window-size

	 )
	
	(include "../generic/constants.ss"))
	
