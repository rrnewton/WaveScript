

(module constants mzscheme
	(require (lib "include.ss"))

	(provide 
	 
         slow-pulse fast-pulse
         
	 world-xbound world-ybound radius numprocs SPECIAL_RETURN_TOKEN
	 
	 return-window-size
	 
	 )
	
	(include "../generic/constants.ss"))
	
