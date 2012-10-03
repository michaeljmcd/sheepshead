literate-files := $(wildcard *.sw)

src: sheepshead.sw
	spiralweb -t sheepshead.sw

doc: docs/[0-9]*.md

docs/[0-9]*.md: $(literate-files)
	spiralweb -w $(literate-files)

html: doc
	pandoc -i docs/[0-9]*.md -o docs/sheepshead.html --smart --standalone

pdf: doc
	pandoc -i docs/[0-9]*.md -o docs/sheepshead.pdf --smart --standalone
