

all: pdf html

install:
	cp -a manual/* /var/www/wsmanual/
	cp -a manual.pdf /var/www/
	(cd /var/www; ./setperms)

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
