src: core.lisp sheepshead.asd

core.lisp: sheepshead.nw
	notangle -Rcore.lisp sheepshead.nw | cpif core.lisp

sheepshead.asd: sheepshead.nw
	notangle -Rsheepshead.asd sheepshead.nw | cpif sheepshead.asd

sheepshead.txt: sheepshead.nw
	noweave -asciidoc -index sheepshead.nw | cpif sheepshead.txt

html: sheepshead.txt
	asciidoc -a toc -d book -b xhtml11 sheepshead.txt
