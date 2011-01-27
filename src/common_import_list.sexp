;#!/bin/bash -c ls
;./generate_main_r6rs.ss common_import_list.sexp '(ws common)'

;; This script generates a module that aggregates a set of very common
;; imports.  A convenience package.  It gets placed in (ws common).

(ws compat compat)

(ws globals)

(ws util hashtab)
(ws util iu-match)
(ws util reg_macros)
(ws util helpers)
(ws util streams)

(ws compiler_components prim_defs)
(ws compiler_components regiment_helpers)
(ws compiler_components type_environments)
(ws compiler_components reg_core_generic_traverse)
(ws compiler_components hm_type_inference)

(ws grammars grammar_checker)
(ws passes pass-mechanism_basic)
(ws passes pass-mechanism)
