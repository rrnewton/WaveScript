default:
	(cd src/; $(MAKE) )

wsparse:
	(cd src/; $(MAKE) wsparse)

chez:
	(cd src/; $(MAKE) chez)

ik:
	(cd src/; $(MAKE) ik)

pltbc:
	(cd src/; $(MAKE) pltbc)


clean:
	(cd src/; $(MAKE) clean)



# Benchmarking stuff
####################

.phony: bench
bench: bench_deps
	./run_benchmarks.exe

bench_deps: run_benchmarks.exe default

run_benchmarks.exe: run_benchmarks.cabal run_benchmarks.hs
	cabal sandbox init
	cabal install ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion -j
	cabal install --bindir=. --program-suffix=.exe ./

clean_bench:
	rm run_benchmarks.exe
	rm cabal.sandbox.config
	rm -rf .cabal-sandbox
