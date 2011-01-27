all: bos.ps bos.info

bos.ps: bos.dvi
	dvips -o bos.ps bos.dvi

bos.dvi: bos.texi
	-rm -f bos.ps
	tex bos.texi
	texindex bos.??
	tex bos.texi

bos.info: bos.texi
	makeinfo bos.texi

tar: bos.tar.gz

bos.tar.gz: README Makefile bos.scm macros.scm utilities.scm \
				examples.scm bos.texi
	cd .. && tar cf - bos/README bos/*.scm bos/bos.texi \
		bos/Makefile | gzip -9 > bos/bos.tar.gz

clean:; -rm -f *~ *.aux *.cp *.dvi *.fn *.ilg *.ind *.info *.ky *.log \
	*.pg *.toc *.tp *.vr *.cps *.fns *.kys *.pgs *.tps *.vrs *.ps \
	*.tar.gz
