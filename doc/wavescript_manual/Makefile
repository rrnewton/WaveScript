

all: pdf html
	cp -a manual/* /var/www/wsmanual/
	(cd /var/www; ./setperms)

pdf:
	pdflatex manual

html: 
	latex2html -split 1 manual.tex

clean:
	rm -rf *.aux *.toc *.log *.pdf manual
