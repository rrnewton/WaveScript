#cs ;; Case Sensitivity
(module source_loader mzscheme 
  (require "iu-match.ss"
           (lib "include.ss")
           ;(all-except (lib "list.ss") filter)
           "constants.ss"
           ;"hashtab.ss"
           (all-except "helpers.ss" test-this these-tests)
           (all-except "regiment_helpers.ss" test-this these-tests)
           (all-except "simulator_alpha.ss" test-this these-tests id)
           )

 (provide     	
  read-regiment-source-file
  load-regiment reg:load
  test_sourceloader
  ) ;; End provide

  ; =======================================================================  
  
  (include (build-path "generic" "source_loader.ss"))
  
  ; =======================================================================

)
