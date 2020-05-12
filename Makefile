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
	rm -f */*.aux
	rm -f */*.bbl
	rm -f */*.blg
	rm -f */*.bcf
	rm -f */*.log
	rm -f */*.out
	rm -f */*.xml
	rm -f */*.rip
	rm -f */*.toc
	rm -f */*.bak*

