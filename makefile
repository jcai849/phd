DOCDIR	= doc
BIBDIR  = bib
IMGDIR  = img
SRCDIR  = src
BINDIR  = bin
REPDIR	= out
.PATH	= ${SRCDIR} #VPATH (try .PATH.suffix)

DOCS   != find ${DOCDIR} -maxdepth 1 -name '*.tex' -exec basename {} \;
REPORTS	= ${DOCS:S/.tex/.pdf/g}
PROGS	= mktexdep

.PHONY: all depend papers clean
.SUFFIXES: .dot .pdf

all: ${REPDIR}/test.pdf #programs reports
${REPDIR}/test.pdf: ${IMGDIR}/test.pdf

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

.dot.pdf:
	dot -Tpdf ${.IMPSRC} >${.TARGET}

.tex.pdf:
	latex ${.IMPSRC}
	bibtex ${.PREFIX}
	latex ${.IMPSRC}
	latex ${.IMPSRC}

depend:
	./bin/mktexdep ${DOCS}
	mkdep -ap ${SRCS}

.depend: depend

clean:
	rm ${DOCDIR}/*.log
	rm ${DOCDIR}/*.aux

fullclean: clean
	rm ${DOCDIR}/*.pdf
	rm ${IMGDIR}/*.svg
