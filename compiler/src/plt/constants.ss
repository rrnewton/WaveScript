

(module constants mzscheme
	(require (lib "include.ss"))
	(require "helpers.ss")

	(provide 
	 
         slow-pulse fast-pulse
         
         unknown-place noplace
         
	 world-xbound world-ybound radius numprocs SPECIAL_RETURN_TOKEN BASE_ID
	 
	 return-window-size

	 define-regiment-parameter regiment-parameters
	 regiment-verbose 
	 )
	
	(include "../generic/constants.ss"))
	
