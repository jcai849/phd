DOCDIR	= doc
BIBDIR  = bib
IMGDIR  = img
SRCDIR  = src
BINDIR  = bin
REPDIR	= out
RM	= rm -f
RSYNC	= rsync -a --delete
REMOTE	= japeca:/var/www/htdocs/japeca.com
.PATH	= ${SRCDIR} #VPATH (try .PATH.suffix)
DOCS   != find ${DOCDIR} -maxdepth 1 -name '*.tex' -exec basename {} \;
REPORTS	= ${DOCS:S/.tex/.pdf/g}
PROGS	= mktexdep

.PHONY: all depend clean
.SUFFIXES: .gv .pdf

all: programs depend ${REPDIR}/test.pdf ${REPDIR}/dreduce.pdf #programs reports
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

.gv.pdf:
	dot -Tpdf ${.IMPSRC} >${.TARGET}

depend: bin/mktexdep
	@./bin/mktexdep ${DOCS:S/^/doc\//g} >.depend

.depend: depend 

push: all
	@./bin/mklinks ${REPORTS} >${REPDIR}/index.html
	${RSYNC} ${REPDIR}/ ${REMOTE}

clean:
	${RM} ${DOCDIR}/*.aux
	${RM} ${DOCDIR}/*.bbl
	${RM} ${DOCDIR}/*.blg
	${RM} ${DOCDIR}/*.log
	${RM} ${DOCDIR}/*.out

fullclean: clean
	${RM} ${IMGDIR}/*.svg
	${RM} ${PROGS:S/^/bin\//g}
	${RM} ${REPDIR}/*.html
	${RM} ${REPDIR}/*.pdf
