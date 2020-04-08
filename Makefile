DOCDIR	:= doc
OUTDIR	:= out
BIBDIR  := bib
LOGDIR  := log
DOCS	:= $(wildcard $(DOCDIR)/*.tex)
LOGS	:= $(wildcard $(LOGDIR)/*.adoc)

.PHONY: all clean format-bib format-tex logs

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

format-bib:
	biber --tool --output-encoding=ascii --output-align --output-file=$(BIBDIR)/bibliography.bib $(BIBDIR)/bibliography.bib

format-tex: $(DOCS)
	ls $(DOCDIR)/*.tex | xargs -I {} -n 1 latexindent -w -s -m {}

logs: $(LOGS:.adoc=.html)

$(LOGDIR)/%.html: $(LOGDIR)/%.adoc
	asciidoc -o $@ $<
