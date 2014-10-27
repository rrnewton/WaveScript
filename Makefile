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

PKGS= ./ ./HSBencher/hgdata ./HSBencher/hsbencher ./HSBencher/hsbencher-tool #./HSBencher/hsbencher-fusion ./HSBencher/hsbencher-codespeed
CBLARGS= --disable-documentation  --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls

bench_deps: run_benchmarks.cabal run_benchmarks.hs
	which -a $(CABAL)
	$(CABAL) sandbox init
	$(CABAL) install $(CBLARGS) --bindir=. $(PKGS)

clean_bench:
	rm run_benchmarks.exe
	rm cabal.sandbox.config
	rm -rf .cabal-sandbox
