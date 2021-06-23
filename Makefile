DOCDIR	= doc/
BIBDIR  = bib/
IMGDIR  = img/
TEX    != find ${DOCDIR} -name '*.tex'
PDF	= ${TEX:S/.tex/.pdf/g}

.PHONY: all depend papers clean
.SUFFIXES:
.SUFFIXES: .tex .pdf .dot .svg

all: doc/consideration-dist-obj.pdf #${PDFOUT}

.dot.svg:
	dot -Tsvg $< >$@

.tex.pdf:
	latex $<
	bibtex ${<F}
	latex $<
	latex $<

papers:
	./bin/dl-papers

depend:
	./bin/mktexdep ${TEX}

.depend: depend

clean:
	rm *.log

fullclean: clean
	rm ${DOCDIR}*.pdf
	rm ${IMGDIR}*.svg
