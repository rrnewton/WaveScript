#! /bin/sh
#|
exec mred -qr "$0" ${1+"$@"}
|#

;(module simulate_tm mzscheme

(load/use-compiled "compiler_plt.ss")

'(begin (init-graphics)
       (close-graphics)
       (cleanse-world) 
       (graphical-repl)
       (exit))

(printf "Args: ~s~n" (vector->list (current-command-line-arguments)))

