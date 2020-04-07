DOCDIR	:= doc
OUTDIR	:= out
BIBDIR  := bib
DOCS	:= $(wildcard $(DOCDIR)/*.tex)

.PHONY: all clean format-bib format-tex

all: $(addprefix $(OUTDIR)/, $(notdir $(DOCS:.tex=.pdf)))

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
	mkdir -p $(OUTDIR)
	cd $(DOCDIR); pdflatex -output-directory ../$(OUTDIR) $(notdir $(basename $<))
	cd $(DOCDIR); biber --input-directory ../$(OUTDIR) $(notdir $(basename $<))
	cd $(DOCDIR); pdflatex -output-directory ../$(OUTDIR) $(notdir $(basename $<))

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

format-bib:
	biber --tool --output-encoding=ascii --output-align --output-file=$(BIBDIR)/bibliography.bib $(BIBDIR)/bibliography.bib

format-tex:
	ls $(DOCDIR)/*.tex | xargs -I {} -n 1 latexindent -w -s -m {}
