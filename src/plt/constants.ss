

(module constants mzscheme
	(require (lib "include.ss"))
	(require "helpers.ss")

	(provide 

	 define-regiment-parameter regiment-parameters
	 regiment-verbose 
	 
         slow-pulse fast-pulse
         
         unknown-place noplace
         
	 world-xbound world-ybound radius numprocs SPECIAL_RETURN_TOKEN BASE_ID
	 
	 return-window-size

	 )
	
	(include "../generic/constants.ss"))
	
