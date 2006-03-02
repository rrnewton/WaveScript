#! /bin/sh
#| 
exec regiment i --script "$0" ${1+"$@"};
|#

;; Usage: ./Script <outfile>

;; First run the simulation for a tiny chunk of time just to get the parameters loaded.

(let ([devnull (open-output-file "/dev/null" 'append)])
  (parameterize ([current-output-port devnull])
    (load-regiment (++ (REGIMENTD) "/demos/firelightning/deadsimple_alarm.rs")
		   '[simalpha-dbg-on #f]	       
		   '[sim-timeout 2])))
(printf "Done running\n")

;; Next, freeze the world resulting from that run.

(define iced (freeze-world (simalpha-current-simworld)))

;(inspect (command-line-arguments))
(write iced  (open-output-file (car (command-line-arguments)) 'replace))
