DOCDIR	:= doc
OUTDIR	:= out
BIBDIR  := bib
TEX	:= $(wildcard $(DOCDIR)/*.tex)
TEXOUT  := $(addprefix $(OUTDIR)/, $(notdir $(TEX:.tex=.pdf)))

.PHONY: all clean

all: $(TEXOUT)

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
	mkdir -p $(OUTDIR)
	pdflatex -output-directory $(OUTDIR) $<
	makeglossaries -d $(OUTDIR) $(notdir $(basename $<))
	biber --input-directory $(OUTDIR) $(OUTDIR)/$(notdir $(basename $<))
	pdflatex -output-directory $(OUTDIR) $<

print-%  : ; @echo $* = $($*)

clean:
	rm -f $(OUTDIR)/*.aux
	rm -f $(OUTDIR)/*.svg
	rm -f $(OUTDIR)/*.bbl
	rm -f $(OUTDIR)/*.blg
	rm -f $(OUTDIR)/*.bcf
	rm -f $(OUTDIR)/*.log
	rm -f $(OUTDIR)/*.out
	rm -f $(OUTDIR)/*.xml
	rm -f $(OUTDIR)/*.rip 
	rm -f $(OUTDIR)/*.toc
	rm -f $(OUTDIR)/*.bak*
	rm -f $(OUTDIR)/*.glg
	rm -f $(OUTDIR)/*.glo
	rm -f $(OUTDIR)/*.gls
	rm -f $(OUTDIR)/*.ist
