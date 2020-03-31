DOCDIR := doc
OUTDIR := out
DOCS := $(wildcard $(DOCDIR)/*.tex)



.PHONY: all clean

all: $(addprefix $(OUTDIR)/, $(notdir $(DOCS:.tex=.pdf)))

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
	pdflatex -output-directory $(OUTDIR) $(basename $<)
	biber --input-directory $(OUTDIR) $(notdir $(basename $<))
	pdflatex -output-directory $(OUTDIR) $(basename $<)

# does *.{aux,bbl} globbing not work in make?? seems not to..

clean:
	rm -f out/*.aux
	rm -f out/*.bbl
	rm -f out/*.blg
	rm -f out/*.bcf
	rm -f out/*.log
	rm -f out/*.out
	rm -f out/*.xml
	rm -f out/*.rip
	rm -f out/*.toc
	rm -f *.bbl
	rm -f *.blg
	rm -f *.log
