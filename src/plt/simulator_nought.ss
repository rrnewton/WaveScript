
(module simulator_nought mzscheme
  (require (lib "iu-match.ss")
           (lib "include.ss")
           (lib "load.ss" "slibinit")
           (lib "compat.ss")
           (all-except "helpers.ss" id)
;           (lib "9.ss" "srfi")
 ;          "engine.ss"
           )
  
  (include "../generic/simulator_nought.ss")
  
  
  )