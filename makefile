DOCDIR	= doc
BIBDIR  = bib
IMGDIR  = img
SRCDIR  = src
BINDIR  = bin
REPDIR	= out
VPATH	= ${SRCDIR} ${DOCDIR}

DOCS   != find ${DOCDIR} -name '*.tex' -exec basename {} \;
REPORTS	= ${DOCS:S/.tex/.pdf/g}
PROGS	= mktexdep

.PHONY: all depend papers clean
.SUFFIXES: .dot .svg

all: ${REPDIR}/test.pdf #programs reports

programs: ${BINDIR}/${PROGS}

reports: ${REPDIR}/${REPORTS}

.for REPORT in ${REPORTS}
REPSRC	= ${REPORT:S/.pdf/.tex/g}
${REPDIR}/${REPORT}: ${DOCDIR}/${REPSRC}
	cd ${DOCDIR} && ${MAKE} ${REPORT} && mv ${REPORT} ../${REPDIR}
.endfor

.for PROG in ${PROGS}
PROGSRC	!= find ${SRCDIR} -name "${PROG}.*"
SRCS	+= PROGSRC
${BINDIR}/${PROG}: ${PROGSRC}
	cd ${SRCDIR} && ${MAKE} ${PROG} && mv ${PROG} ../${BINDIR}
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
	./bin/mktexdep ${DOCS}
	mkdep -ap ${SRCS}

.depend: depend

clean:
	rm *.log

fullclean: clean
	rm ${DOCDIR}/*.pdf
	rm ${IMGDIR}/*.svg
