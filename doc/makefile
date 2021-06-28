.SUFFIXES:
.SUFFIXES: .tex .pdf
.tex.pdf:
	latex ${.IMPSRC}
	bibtex ${.PREFIX}
	latex ${.IMPSRC}
	latex ${.IMPSRC}
