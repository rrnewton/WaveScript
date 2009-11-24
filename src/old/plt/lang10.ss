
(module lang10 mzscheme

	(require (lib "include")
		 "simulator_nought.ss")

	(provide deglobalize-lang)
	
	(include (build-path "generic" "lang10.ss"))
)
