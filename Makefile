literate-files := $(wildcard *.sw)

src: sheepshead.sw
	spiralweb tangle sheepshead.sw

doc: docs/[0-9]*.md

docs/[0-9]*.md: $(literate-files)
	spiralweb weave $(literate-files)

html: doc
	pandoc -i docs/[0-9]*.md -o docs/sheepshead.html --smart --standalone --toc

pdf: doc
	pandoc docs/[0-9]*.md -o docs/sheepshead.pdf --smart --standalone
