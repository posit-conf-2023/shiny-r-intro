SLIDE_QMD_FILES := $(wildcard *.qmd)
SLIDE_HTML_FILES  := $(subst qmd,html,$(SLIDE_QMD_FILES))

all: $(SLIDE_HTML_FILES) $(SLIDE_PDF_FILES)

%.html: %.qmd
	quarto render $<
