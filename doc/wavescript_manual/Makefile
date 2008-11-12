

WWWDIR=/var/www/chastity/

all: svnrev pdf html

svnrev:
	svn info | grep Revision | sed 's/Revision://' > svnrev.txt
#	svn info | grep Revision | awk '{ print $2 }' > svnrev.txt

install:
	cp -a manual/* $(WWWDIR)/wsmanual/
	cp -a manual.pdf $(WWWDIR)
	(cd $(WWWDIR); ./setperms)

primtable.tex:
	echo '\begin{verbatim}' > primtable.tex
	echo '(with-output-to-file "primtable.tex" (lambda () (print-ws-prim-table)) (quote append))' | regiment i
	echo '\end{verbatim}' >> primtable.tex

pdf: primtable.tex
	pdflatex manual

html: primtable.tex
	latex2html -split 1 manual.tex

clean:
	rm -rf *.aux *.toc *.log *.pdf manual
