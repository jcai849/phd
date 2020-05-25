DOCDIR	:= doc
OUTDIR	:= out
BIBDIR  := bib
TEX	:= $(wildcard $(DOCDIR)/*.tex)
TEXOUT  := $(addprefix $(OUTDIR)/, $(notdir $(TEX:.tex=.pdf)))
ADOC    := $(wildcard $(DOCDIR)/*.txt)
ADOCOUT := $(addprefix $(OUTDIR)/, $(notdir $(ADOC:.txt=.pdf)))

.PHONY: all clean

all: $(TEXOUT) $(ADOCOUT)

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
	mkdir -p $(OUTDIR)
	pdflatex -output-directory $(OUTDIR) $<
	biber --input-directory $(OUTDIR) $(OUTDIR)/$(notdir $(basename $<))
	pdflatex -output-directory $(OUTDIR) $<

$(OUTDIR)/%.pdf: $(DOCDIR)/%.txt
	a2x -f pdf -D $(OUTDIR) $<

print-%  : ; @echo $* = $($*)

clean:
	rm -f out/*.aux
	rm -f out/*.svg
	rm -f out/*.bbl
	rm -f out/*.blg
	rm -f out/*.bcf
	rm -f out/*.log
	rm -f out/*.out
	rm -f out/*.xml
	rm -f out/*.rip 
	rm -f out/*.toc
	rm -f out/*.bak*
