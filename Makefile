DOCDIR	:= doc
OUTDIR	:= out
DOCS	:= $(wildcard $(DOCDIR)/*.tex)



.PHONY: all clean

all: $(addprefix $(OUTDIR)/, $(notdir $(DOCS:.tex=.pdf)))

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
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

