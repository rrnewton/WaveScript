

#TOPDF=epstopdf
TOPDF=ps2pdf


all: 
	@echo "  Running all benchmarks to generate report, takes approximately ?? minutes."
	$(MAKE) engine
	$(MAKE) topbefore
	$(MAKE) alldeps
	$(MAKE) topafter
	$(MAKE) perfreport.pdf
## Really, the correct thing to do would be to monitor CPU utilization
## during the execution of the benchmarks to make sure nothing else
## was stealing CPU from us.

topbefore:
	echo '\begin{verbatim}' > top_before.tex
	top -b | head -n 20    >> top_before.tex
	echo '\end{verbatim}'  >> top_before.tex

topafter:
	echo '\begin{verbatim}' > top_after.tex
	top -b | head -n 20    >> top_after.tex
	echo '\end{verbatim}'  >> top_after.tex

#  enginesvn.tex 
alldeps: machineinfo.tex wssvn.tex perfreport.tex microbench/microbench1.pdf language_shootout/shootout.pdf microbench/RESULTS.tex language_shootout/RESULTS.tex appbench/MARMOT.pdf
# datareps/alloc_arrarr.pdf ## DISABLING

perfreport.pdf:
	pdflatex -halt-on-error perfreport.tex
#	pdflatex perfreport.tex


machineinfo.tex:
	echo '\begin{verbatim}' > machineinfo.tex
	uname -a >> machineinfo.tex
	echo "LD_PRELOAD: $(LD_PRELOAD)  CC: $(CC)" >> machineinfo.tex
	echo '\end{verbatim}' >> machineinfo.tex

wssvn.tex:
	echo '\begin{verbatim}' > wssvn.tex
	svn info | grep Revision >> wssvn.tex
	echo '\end{verbatim}' >> wssvn.tex

enginesvn.tex:
	echo '\begin{verbatim}' > enginesvn.tex
	(cd $(WAVESCOPED); svn info | grep Revision) >> enginesvn.tex
	echo '\end{verbatim}' >> enginesvn.tex

appbench/MARMOT.pdf:
	(cd appbench; $(MAKE))
#	(cd appbench; $(MAKE) marmot.pdf)

# HACK: We assume if this one's there then the other ones are too:
datareps/alloc_arrarr.pdf:
	(cd datareps; $(MAKE))

microbench/microbench1.pdf: microbench/microbench1.eps
	(cd microbench; $(TOPDF) microbench1.eps)
#                            microbench/RESULTS.txt
# This does the real work to run the microbenches:
microbench/microbench1.eps: 
	(cd microbench; $(MAKE))

#microbench/RESULTS.txt:
#	(cd microbench; $(MAKE))

language_shootout/shootout.pdf: language_shootout/shootout.eps
	(cd language_shootout; $(TOPDF) shootout.eps)
language_shootout/shootout.eps:
	(cd language_shootout; $(MAKE))

#microbench/RESULTS.txt
microbench/RESULTS.tex: microbench/microbench1.pdf 
	echo '\begin{verbatim}'            > microbench/RESULTS.tex
	cat microbench/RESULTS_misc.txt   >> microbench/RESULTS.tex
	echo '\end{verbatim}'             >> microbench/RESULTS.tex
language_shootout/RESULTS.tex: language_shootout/RESULTS.txt
	echo '\begin{verbatim}'            > language_shootout/RESULTS.tex
	cat language_shootout/RESULTS.txt >> language_shootout/RESULTS.tex
	echo '\end{verbatim}'             >> language_shootout/RESULTS.tex

# appbench/RESULTS.tex: appbench/RESULTS.txt
# 	echo '\begin{verbatim}'            > appbench/RESULTS.tex
# 	cat appbench/RESULTS.txt          >> appbench/RESULTS.tex
# 	echo '\end{verbatim}'             >> appbench/RESULTS.tex


clean:
	rm -f machineinfo.tex perfreport.pdf perfreport.log perfreport.aux top_*.txt wssvn.tex enginesvn.tex
	rm -f top_after.tex top_before.tex

distclean: clean
	rm -rf ./engine
	rm -rf ./libws-*.a include*.tgz
	(cd language_shootout && $(MAKE) clean)
	(cd microbench && $(MAKE) clean)
	(cd appbench   && $(MAKE) clean)
	(cd datareps   && $(MAKE) clean)



# Microsuite benchmarks
#######################

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

PKGS= ./ ./HSBencher/hsbencher ./HSBencher/hgdata #./HSBencher/hsbencher-tool ./HSBencher/hsbencher-fusion ./HSBencher/hsbencher-codespeed
CBLARGS= --disable-documentation  --with-ghc=ghc-$(JENKINS_GHC) --force-reinstalls

microsuite: run_benchmarks.exe
	
run_benchmarks.exe: run_benchmarks.cabal run_benchmarks.hs
	which -a $(CABAL)
	$(CABAL) sandbox init
	#$(CABAL) install $(CBLARGS) --only-dep $(PKGS)
	$(CABAL) install $(CBLARGS) $(PKGS)
	ln -s .cabal-sandbox/bin/run_benchmarks.exe run_benchmarks.exe

.phony: clean_microsuite
clean_microsuite:
	$(CABAL) clean
	rm -f run_benchmarks.exe
	rm -f microbench/*/query.*

.phony: deepclean_microsuite
deepclean_microsuite: clean_microsuite
	git submodule foreach --recursive 'git clean -fd'

purge_microsuite: deepclean_microsuite
	rm -rf .cabal-sandbox/
	rm cabal.sandbox.config
