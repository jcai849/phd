DOCDIR	= doc
BIBDIR  = bib
IMGDIR  = img
SRCDIR  = src
BINDIR  = bin
REPDIR	= out
RM	= rm -f
RSYNC	= rsync -a --delete
REMOTE	= japeca:/var/www/htdocs/japeca.com
.PATH	= ${SRCDIR}
DOCS   != find ${DOCDIR} -maxdepth 1 -name '*.tex' -exec basename {} \;
REPORTS	= ${DOCS:S/.tex/.pdf/g}
PROGS	= mktexdep
PLANTUML= java -jar ~/plantuml/plantuml.jar -pipe

.PHONY: all depend clean
.SUFFIXES: .gv .puml .pdf

all: depend programs reports

programs: ${PROGS:S/^/bin\//g}

reports: ${REPORTS:S/^/out\//g}

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
	dot -Tpdf -Gsize=4,6\! -Gdpi=100 ${.IMPSRC} >${.TARGET}

.puml.pdf:
	cat ${.IMPSRC} | ${PLANTUML} -tpdf >${.TARGET}

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
	${RM} ${IMGDIR}/*.pdf
	${RM} ${PROGS:S/^/bin\//g}
	${RM} ${REPDIR}/*.html
	${RM} ${REPDIR}/*.pdf
