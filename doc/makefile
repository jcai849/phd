LATEX	= pdflatex

.SUFFIXES:
.SUFFIXES: .tex .pdf

.tex.pdf:
	${LATEX} ${.IMPSRC}
	bibtex ${.PREFIX}
	${LATEX} ${.IMPSRC}
	${LATEX} ${.IMPSRC}
