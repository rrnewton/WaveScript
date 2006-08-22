
;;;; .title WaveScript EmitC
;;;; .author Ryan Newton

;;;; This uses the generic C-generation libary (c_generator.ss) and
;;;; provides procedures for producing a C-file following WaveScript
;;;; conventions, that is, a ".wsc" file.


(module wavescript_emit-c mzscheme 
  (require "c_generator.ss")
  (provide ;WSBox wscode->text
	   wsquery->text)	   
  (chezprovide )  
  (chezimports )


  ;; This is the only entry point to the file.  A complete query can
  ;; be transformed into a complete query file.
  


)