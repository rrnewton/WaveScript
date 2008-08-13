;; These are the modules that we aggregate together to form the main module:

;; Just use the common convenience package rather than the above:

 (ws common)

(ws util scheme_fft) ;; FFT from the chez users guide
(ws util slib_fft)
(ws util fft)
(ws util tsort)
(ws util hash)
(ws util slib_hashtab)
(ws util bos_oop) 
(prefix (ws util imperative_streams) imp:)

(ws compiler_components annotations)
(ws compiler_components c_generator)
(ws compiler_components source_loader)

;; These are miscellaneous small passes used by wavescript:
(ws passes small-ws-passes)
(ws passes partition-graph)
(ws passes graphviz)

(depends matpak)

;"generic/langs/language-mechanism.ss"
(ws langs lang_wavescript)
(ws testing lang_wavescript_tests)
(prefix (ws sim wavescript_sim_library_push) wssim:)

;; Need complex numbers!!
;(ws testing lang_wavescript_tests)

;   (include "generic/langs/lang00.ss")
;   (include "generic/langs/lang06_uncover-free.ss")
;   (include "generic/langs/lang07_lift-letrec.ss")

(ws passes normalize_source verify-regiment)
(ws passes normalize_source typecheck)
(ws passes normalize_source desugar-pattern-matching)
(ws passes normalize_source resolve-varrefs)
(ws passes normalize_source ws-label-mutable)

(ws passes normalize_source rename-vars)
(ws passes normalize_source eta-primitives)
(ws passes normalize_source desugar-misc)


(ws passes normalize_source remove-unquoted-constant)

(ws passes static_elaborate static-elaborate)

;(ws passes normalize_query remove-complex-opera)
(ws passes normalize_query reduce-primitives)

(ws passes normalize_query ws-remove-letrec)
(ws passes normalize_query ws-remove-complex-opera)
(ws passes normalize_query ws-lift-let)
(ws passes normalize_query remove-complex-constant)
(ws passes normalize_query uncover-free)
(ws passes normalize_query lift-letrec)
(ws passes normalize_query lift-letrec-body)
(ws passes normalize_query remove-lazy-letrec)
(ws passes normalize_query verify-core)

(ws passes normalize_query ws-normalize-context)


(ws passes static_elaborate interpret-meta)
(ws passes static_elaborate degeneralize-arithmetic)
(ws passes static_elaborate verify-elaborated)
(ws passes static_elaborate split-union-types)

(ws passes optimizations smoosh-together)
(ws passes optimizations rewrite_opts)
(ws passes optimizations merge-iterates)
(ws passes optimizations merge-iterates2)
(ws passes optimizations simple-merge-iterates)

(ws passes wavescope_bkend nominalize-types)
(ws passes wavescope_bkend convert-sums-to-tuples)
(ws passes wavescope_bkend reify-certain-types)
(ws passes wavescope_bkend type-annotate-misc)
(ws passes wavescope_bkend flatten-iterate-spine)
(ws passes wavescope_bkend anihilate-higher-order)
(ws passes wavescope_bkend explicit-stream-wiring)
(ws passes wavescope_bkend emit-c)
(ws passes wavescope_bkend insert-refcounts)
(ws passes wavescope_bkend emit-c2)
(ws passes wavescope_bkend emit-tinyos)
(ws passes wavescope_bkend emit-java)

(ws passes ocaml_bkend shared-emit-ml)
(ws passes ocaml_bkend emit-caml)
(ws passes mlton_bkend emit-mlton)

(ws passes analyze_data_rates annotate-with-data-rates)

