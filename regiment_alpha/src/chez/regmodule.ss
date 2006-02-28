
;;;; Regiment module syntax.
;;;; .author Ryan Newton
;;;;
;;;; [2006.02.26] <br>
;;;;    Having gotten sick of having separate module declarations in
;;;; the chez/ and plt/ directories (which then include code from the
;;;; generic/ directory), I've decided that I'm going to establish a
;;;; common denominator module syntax.  <br> <br>
;;;; 
;;;; In some ways this will be a least-common-denominator between the
;;;; two module systems.  Basically the bottom line is that I want to
;;;; share the list of _export symbols_.  I'm sick of having to
;;;; maintain the same list in two places within the Chez and PLT
;;;; code. <br><br>
;;;;
;;;; The import list can be implicit in Chez, or it can take the form of
;;;; "(import m)" statements, and in PLT is represented by the "require" form.
;;;; These are different enough that I'm not going to try to share
;;;; them.  The Chez version will ignore the (require ...) section,
;;;; and the PLT version will ignore the (chezimports ...) section.
;;;; Still this is much less duplicated effort than the long lists of
;;;; exported symbols.

(module reg:module (chez:module module)
  
  ;; Bind chez:module to be the *real* thing.
  (module (chez:module)
    (import scheme)
    (alias chez:module module))

  ;; This is our compatability syntax for PLT/Chez portability.
  (define-syntax module
    (let ()
      ;(import chez_module)
      (syntax-rules (require provide chezimports)
	[(_ name parent (require _ ...) (provide exports ...) (chezimports imp ...) exp ...)
	 
	 (chez:module name (exports ...)
		      (import imp) ...
		      exp ...)
	 ])))
  )


#;
(let ()
  (import regmodule)
  (module foo mzscheme (provide x) (require EUFOUTNH) (chezimports scheme)
	  (define x 9999999999999999999))
  (import foo)
  x)


