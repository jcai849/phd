DOCDIR	:= doc
OUTDIR	:= out
BIBDIR  := bib
DOCS	:= $(wildcard $(DOCDIR)/*.tex)

.PHONY: all clean format-bib format-tex

all: $(addprefix $(OUTDIR)/, $(notdir $(DOCS:.tex=.pdf)))

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
	mkdir -p $(OUTDIR)
	pdflatex -output-directory $(OUTDIR) $<
	biber --input-directory $(OUTDIR) $(OUTDIR)/$(notdir $(basename $<))
	pdflatex -output-directory $(OUTDIR) $<

print-%  : ; @echo $* = $($*)

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
	rm -f out/*.bak*

