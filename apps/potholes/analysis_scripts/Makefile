


all:
	ocamlopt.opt -c graphics.cmxa bigarray.cmxa utils.ml
	ocamlopt.opt unix.cmxa bigarray.cmxa graphics.cmxa utils.cmx tally_mileage_ALLCARS.ml -o tally_mileage_ALLCARS.exe
	ocamlopt.opt unix.cmxa bigarray.cmxa graphics.cmxa utils.cmx display.ml -o display.exe
#	ocamlopt.opt unix.cmxa bigarray.cmxa graphics.cmxa  tally_mileage.ml -o tally_mileage.exe


dbg:
#	ocamlc unix.cma bigarray.cma graphics.cma -g  tally_mileage.ml
	ocamlc unix.cma bigarray.cma graphics.cma  tally_mileage_ALLCARS.ml -g -o tally_mileage_ALLCARS

