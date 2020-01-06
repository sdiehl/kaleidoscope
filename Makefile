PANDOC = pandoc
IFORMAT = markdown

HFLAGS = --standalone --toc --highlight-style pygments
LFLAGS = --top-level-division=chapter -V documentclass=book

# Preprocessor
# ------------

PDF = tutorial.pdf

CC = gcc
GHC = stack ghc
OPTS =  --
PRE = cabal exec preprocessor

# Text
# ----

all: $(CHAPTERS) $(HTML)
html: $(HTML)
pdf: $(PDF)
examples: $(CHAPTERS)

HTML = tutorial.html
HTML_TEMPLATE = html/page.tmpl
HTML_STYLE = css/style.css
CHAPTERS = chapter2 chapter3 chapter4 chapter5 chapter6 chapter7

# Chapters
# --------

chapter1:

chapter2:
	$(GHC) $(OPTS) --make src/chapter2/*.hs -o chapter2

chapter3:
	$(GHC) $(OPTS) --make src/chapter3/*.hs -o chapter3

chapter4:
	$(CC) -fPIC -shared src/chapter4/cbits.c -o src/chapter4/cbits.so
	$(GHC) $(OPTS) src/chapter4/cbits.so --make src/chapter4/*.hs -o chapter4

chapter5:
	$(CC) -fPIC -shared src/chapter5/cbits.c -o src/chapter5/cbits.so
	$(GHC) $(OPTS) src/chapter5/cbits.so --make src/chapter5/*.hs -o chapter5

chapter6:
	$(CC) -fPIC -shared src/chapter6/cbits.c -o src/chapter6/cbits.so
	$(GHC) $(OPTS) src/chapter6/cbits.so --make src/chapter6/*.hs -o chapter6

chapter7:
	$(CC) -fPIC -shared src/chapter7/cbits.c -o src/chapter7/cbits.so
	$(GHC) $(OPTS) src/chapter7/cbits.so --make src/chapter7/*.hs -o chapter7

# Tutorial
# --------

preprocessor:
	cabal new-build preprocessor

%.html: %.md preprocessor
	$(PRE) -- < $< | $(PANDOC) -c $(HTML_STYLE) --template $(HTML_TEMPLATE) -s -f $(IFORMAT) -t html $(HFLAGS) -o $@

%.pdf: %.md preprocessor
	$(PRE) -- < $< | $(PANDOC) -f $(IFORMAT) --toc --pdf-engine=xelatex $(LFLAGS) -o $@

clean:
	-rm $(CHAPTERS) $(HTML)
