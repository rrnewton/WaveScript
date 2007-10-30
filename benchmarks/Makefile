

#TOPDF=epstopdf
TOPDF=ps2pdf

all: 
	@echo "  Running all benchmarks to generate report, takes approximately ?? minutes."
	echo '\begin{verbatim}' > top_before.txt
	top -b | head -n 20    >> top_before.txt
	echo '\end{verbatim}'  >> top_before.txt
	$(MAKE) perfreport.pdf
	echo '\begin{verbatim}' > top_after.txt
	top -b | head -n 20    >> top_after.txt
	echo '\end{verbatim}'  >> top_after.txt
## Really, the correct thing to do would be to modify CPU utilization
## during the execution of the benchmarks to make sure nothing else
## was stealing CPU from us.

perfreport.pdf: machineinfo.tex wssvn.tex enginesvn.tex perfreport.tex microbench/microbench.pdf language_shootout/shootout.pdf 
	pdflatex perfreport.tex

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



microbench/microbench.pdf: microbench/microbench.eps
	(cd microbench; $(TOPDF) microbench.eps)
microbench/microbench.eps:
	(cd microbench; $(MAKE))

language_shootout/shootout.pdf: language_shootout/shootout.eps
	(cd language_shootout; $(TOPDF) shootout.eps)
language_shootout/shootout.eps:
	(cd language_shootout; $(MAKE))

clean:
	rm -f machineinfo.tex perfreport.pdf perfreport.log perfreport.aux top_*.txt wssvn.tex enginesvn.tex

distclean: clean
	(cd language_shootout; $(MAKE) clean)
	(cd microbench; $(MAKE) clean)
