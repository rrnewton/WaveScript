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

HSBENCHD=$(REGIMENTD)/HSBencher

ifeq ($(CABAL),)
  CABAL=cabal
endif
ifeq ($(MACHINECLASS),)
  MACHINECLASS=$(shell hostname -s)
endif

RUNID=$(shell hostname -s)_$(shell date "+%s")

ifeq ($(JENKINS_GHC),)
  JENKINS_GHC=7.8.3
endif

PKGS= ./ ./HSBencher/hsbencher ./HSBencher/hgdata ./HSBencher/hsbencher-tool #./HSBencher/hsbencher-fusion ./HSBencher/hsbencher-codespeed
CBLARGS= --disable-documentation  --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls

bench_deps: run_benchmarks.cabal run_benchmarks.hs
	which -a $(CABAL)
	$(CABAL) sandbox init
	#$(CABAL) install $(CBLARGS) --only-dep $(PKGS)
	$(CABAL) install $(CBLARGS) $(PKGS)

.phony: clean_bench
clean_bench:
	$(CABAL) clean

.phony: deepclean_bench
deepclean_bench: clean_bench
	cd $(HSBENCHD)/hsbencher
	$(CABAL) clean
	cd $(HSBENCHD)/hgdata
	$(CABAL) clean
	cd $(HSBENCHD)/hsbencher-tool
	$(CABAL) clean
	cd $(HSBENCHD)/hsbencher-fusion
	$(CABAL) clean
	cd $(HSBENCHD)/hsbencher-codespeed
	$(CABAL) clean
	cd $(REGIMENTD)

purge_bench: deepclean_bench
	rm -rf .cabal-sandbox/
	rm cabal.sandbox.config
	rm run_benchmarks.exe
