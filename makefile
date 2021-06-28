DOCDIR	= doc
BIBDIR  = bib
IMGDIR  = img
SRCDIR  = src
BINDIR  = bin
REPDIR	= out
RM	= rm -f
.PATH	= ${SRCDIR} #VPATH (try .PATH.suffix)
DOCS   != find ${DOCDIR} -maxdepth 1 -name '*.tex' -exec basename {} \;
REPORTS	= ${DOCS:S/.tex/.pdf/g}
PROGS	= mktexdep

.PHONY: all depend clean
.SUFFIXES: .dot .pdf

all: programs ${REPDIR}/test.pdf #programs reports
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

depend:
	./bin/mktexdep ${DOCS:S/^/doc\//g} >.depend

.depend: depend

clean:
	${RM} ${DOCDIR}/*.aux
	${RM} ${DOCDIR}/*.bbl
	${RM} ${DOCDIR}/*.blg
	${RM} ${DOCDIR}/*.log
	${RM} ${DOCDIR}/*.out

fullclean: clean
	${RM} ${DOCDIR}/*.pdf
	${RM} ${IMGDIR}/*.svg
	${RM} ${PROGS:S/^/bin\//g}
