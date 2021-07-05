LATEX	= pdflatex
LFLAGS	= -interaction=batchmode

.SUFFIXES:
.SUFFIXES: .tex .pdf

.tex.pdf:
	${LATEX} ${LFLAGS} ${.IMPSRC}
	bibtex ${.PREFIX}
	${LATEX} ${LFLAGS} ${.IMPSRC}
	${LATEX} ${LFLAGS} ${.IMPSRC}
