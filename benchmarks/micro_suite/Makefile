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

PKGS= ./ ./HSBencher/hsbencher ./HSBencher/hgdata ./HSBencher/hsbencher-fusion #./HSBencher/hsbencher-tool ./HSBencher/hsbencher-codespeed
CBLARGS= -j --disable-documentation --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls

default: run_benchmarks.exe

run_benchmarks.exe: run_benchmarks.cabal run_benchmarks.hs
	which -a $(CABAL)
	$(CABAL) sandbox init
	$(CABAL) install $(CBLARGS) $(PKGS)
	ln -s .cabal-sandbox/bin/run_benchmarks.exe run_benchmarks.exe

.phony: clean
clean:
	$(CABAL) clean
	rm -f run_benchmarks.exe
	rm -f micro_suite/*/query.*

.phony: deepclean
deepclean: clean
	cd $(REGIMENTD) && git submodule foreach --recursive 'git clean -fd'

purge: deepclean
	rm -rf .cabal-sandbox/
	rm -f cabal.sandbox.config
