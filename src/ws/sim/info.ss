(module info (lib "infotab.ss" "setup")
  (define name "Simulator(s) for token machines")

  (define compile-omit-files '(
			       "alpha_lib_scheduler.ss"
			       "simalpha_rollworld.ss"
			       "simalpha_ui.ss"
			       "simulator_nought.examples.ss"
			       "simulator_nought.ss"
			       "simulator_nought_graphics.ss"

			       ;; This experiment is done with:
			       "firelightning_sim.ss"
			       ))
  )
