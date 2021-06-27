DOCDIR	= doc
BIBDIR  = bib
IMGDIR  = img
SRCDIR  = src
BINDIR  = bin
TEX    != find ${DOCDIR} -name '*.tex'
PDF	= ${TEX:S/.tex/.pdf/g}
BINPROGS= mktexdep
PROGS	= mktexdep
VPATH	= ${SRCDIR}

.PHONY: all depend papers clean
.SUFFIXES: .tex .pdf .dot .svg

all: src #doc/consideration-dist-obj.pdf #${PDFOUT}

src: ${BINDIR}/${PROGS}

.for PROG in ${PROGS}
PROGSRC	!= find ${SRCDIR} -name "${PROG}.*"
${BINDIR}/${PROG}: ${PROGSRC}
	${MAKE} ${PROG} && mv ${PROG} ${BINDIR}
.endfor

.dot.svg:
	dot -Tsvg ${.IMPSRC} >${.TARGET}

.tex.pdf:
	latex ${.IMPSRC}
	bibtex ${.PREFIX}
	latex ${.IMPSRC}
	latex ${.IMPSRC}

fetch-papers:
	./bin/dl-papers

depend:
	./bin/mktexdep ${TEX}

.depend: depend

clean:
	rm *.log

fullclean: clean
	rm ${DOCDIR}/*.pdf
	rm ${IMGDIR}/*.svg
