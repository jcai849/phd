TEX := pdflatex
BIB := biber
OBJECTS := $(wildcard *.tex)

.PHONY: all clean

all: $(OBJECTS:.tex=.pdf)

%.pdf: %.tex
	pdflatex $(<:.tex=)
	biber $(<:.tex=)
	pdflatex $(<:.tex=)

# does *.{aux,bbl} globbing not work in make?? seems not to..

clean:
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg
	rm -f *.bcf
	rm -f *.log
	rm -f *.out
	rm -f *.xml
