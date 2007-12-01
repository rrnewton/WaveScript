

all: pdf html

install:
	cp -a manual/* /var/www/wsmanual/
	cp -a manual.pdf /var/www/
	(cd /var/www; ./setperms)

primtable.txt:
	echo '\begin{verbatim}' > primtable.txt
	echo '(with-output-to-file "primtable.txt" (lambda () (print-ws-prim-table)) (quote append))' | regiment i
	echo '\end{verbatim}' >> primtable.txt

pdf: primtable.txt
	pdflatex manual

html: primtable.txt
	latex2html -split 1 manual.tex

clean:
	rm -rf *.aux *.toc *.log *.pdf manual
