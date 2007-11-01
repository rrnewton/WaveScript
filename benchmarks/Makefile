

#TOPDF=epstopdf
TOPDF=ps2pdf


all: 
	@echo "  Running all benchmarks to generate report, takes approximately ?? minutes."
	$(MAKE) engine
	echo '\begin{verbatim}' > top_before.tex
	top -b | head -n 20    >> top_before.tex
	echo '\end{verbatim}'  >> top_before.tex
	$(MAKE) perfreport.pdf
	echo '\begin{verbatim}' > top_after.tex
	top -b | head -n 20    >> top_after.tex
	echo '\end{verbatim}'  >> top_after.tex
## Really, the correct thing to do would be to modify CPU utilization
## during the execution of the benchmarks to make sure nothing else
## was stealing CPU from us.

engine: 
	./setup_engines.sh

perfreport.pdf: machineinfo.tex wssvn.tex enginesvn.tex perfreport.tex microbench/microbench.pdf language_shootout/shootout.pdf microbench/RESULTS.tex language_shootout/RESULTS.tex appbench/marmot.pdf
	pdflatex -halt-on-error perfreport.tex

machineinfo.tex:
	echo '\begin{verbatim}' > machineinfo.tex
	uname -a >> machineinfo.tex
	echo '\end{verbatim}' >> machineinfo.tex

wssvn.tex:
	echo '\begin{verbatim}' > wssvn.tex
	svn info | grep Revision >> wssvn.tex
	echo '\end{verbatim}' >> wssvn.tex

enginesvn.tex:
	echo '\begin{verbatim}' > enginesvn.tex
	(cd $(WAVESCOPED); svn info | grep Revision) >> enginesvn.tex
	echo '\end{verbatim}' >> enginesvn.tex

appbench/marmot.pdf:
	(cd appbench; $(MAKE))
#	(cd appbench; $(MAKE) marmot.pdf)

microbench/microbench.pdf: microbench/microbench.eps
	(cd microbench; $(TOPDF) microbench.eps)
microbench/microbench.eps:
	(cd microbench; $(MAKE))

language_shootout/shootout.pdf: language_shootout/shootout.eps
	(cd language_shootout; $(TOPDF) shootout.eps)
language_shootout/shootout.eps:
	(cd language_shootout; $(MAKE))


microbench/RESULTS.tex: microbench/RESULTS.txt
	echo '\begin{verbatim}'            > microbench/RESULTS.tex
	cat microbench/RESULTS.txt        >> microbench/RESULTS.tex
	echo '\end{verbatim}'             >> microbench/RESULTS.tex
language_shootout/RESULTS.tex: language_shootout/RESULTS.txt
	echo '\begin{verbatim}'            > language_shootout/RESULTS.tex
	cat language_shootout/RESULTS.txt >> language_shootout/RESULTS.tex
	echo '\end{verbatim}'             >> language_shootout/RESULTS.tex

clean:
	rm -f machineinfo.tex perfreport.pdf perfreport.log perfreport.aux top_*.txt wssvn.tex enginesvn.tex

distclean: clean
	rm -rf ./engine
	rm -rf ./libws-*.a include*.tgz
	(cd language_shootout && $(MAKE) clean)
	(cd microbench && $(MAKE) clean)
	(cd appbench   && $(MAKE) clean)
	(cd datareps   && $(MAKE) clean)
