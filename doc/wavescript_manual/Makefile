

all: pdf html


pdf:
	pdflatex manual

html: 
	latex2html -split 1 manual.tex

clean:
	rm -rf *.aux *.toc *.log *.pdf manual
