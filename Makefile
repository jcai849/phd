DOCDIR	:= doc
OUTDIR	:= out
BIBDIR  := bib
TEX	:= $(wildcard $(DOCDIR)/*.tex)
TEXOUT  := $(addprefix $(OUTDIR)/, $(notdir $(TEX:.tex=.pdf)))
MD	:= $(wildcard $(DOCDIR)/*.md)
MDOUT	:= $(addprefix $(OUTDIR)/, $(notdir $(MD:.md=.html)))
DOT	:= $(wildcard $(DOCDIR)/*.dot)
DOTOUT  := $(addprefix $(DOCDIR)/, $(notdir $(DOT:.dot=.svg)))

.PHONY: all clean

all: $(TEXOUT) $(MDOUT) $(DOTOUT)

$(OUTDIR)/%.html: $(DOCDIR)/%.md
	pandoc -V theme=white --self-contained -t revealjs $< -o $@

$(DOCDIR)/%.svg: $(DOCDIR)/%.dot
	dot -Tsvg $< >$@

$(OUTDIR)/%.pdf: $(DOCDIR)/%.tex
	mkdir -p $(OUTDIR)
	lualatex -shell-escape -output-directory $(OUTDIR) $<
	makeglossaries -d $(OUTDIR) $(notdir $(basename $<))
	biber --input-directory $(OUTDIR) $(OUTDIR)/$(notdir $(basename $<))
	lualatex -shell-escape -output-directory $(OUTDIR) $<

print-%  : ; @echo $* = $($*)

clean:
	rm -f $(OUTDIR)/*.aux
	rm -rf $(OUTDIR)/_minted*
	rm -f $(OUTDIR)/*.eps
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
	rm -f $(OUTDIR)/*.nav
	rm -f $(OUTDIR)/*.snm
	rm -f $(OUTDIR)/*.lof
	rm -f $(OUTDIR)/*.lol
	rm -f $(OUTDIR)/*.lot
