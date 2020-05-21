DOCDIR	:= doc
OUTDIR	:= out
BIBDIR  := bib
TEX	:= $(wildcard $(DOCDIR)/*.tex)
TEXOUT  := $(addprefix $(OUTDIR)/, $(notdir $(TEX:.tex=.pdf)))
ADOC    := $(wildcard $(DOCDIR)/*.adoc)
ADOCOUT := $(addprefix $(OUTDIR)/, $(notdir $(ADOC:.adoc=.html)))

.PHONY: all clean

all: $(TEXOUT) $(ADOCOUT)

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
	mkdir -p $(OUTDIR)
	pdflatex -output-directory $(OUTDIR) $<
	biber --input-directory $(OUTDIR) $(OUTDIR)/$(notdir $(basename $<))
	pdflatex -output-directory $(OUTDIR) $<

$(OUTDIR)/%.pdf: $(DOCDIR)/%.adoc
	asciidoctor-pdf -r asciidoctor-bibtex -b pdf -o $@ $<
	# TODO: make use of mathoid to pre-render any stem blocks

$(OUTDIR)/%.html: $(DOCDIR)/%.adoc
	asciidoctor -r asciidoctor-bibtex -o $@ $<

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
