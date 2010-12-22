src: core.lisp sheepshead.asd sheepshead-consoleui.asd consoleui.lisp

core.lisp: sheepshead.nw
	notangle -Rcore.lisp sheepshead.nw | cpif core.lisp

consoleui.lisp: sheepshead.nw
	notangle -Rconsoleui.lisp sheepshead.nw | cpif consoleui.lisp

sheepshead.asd: sheepshead.nw
	notangle -Rsheepshead.asd sheepshead.nw | cpif sheepshead.asd

sheepshead-consoleui.asd: sheepshead.nw
	notangle -Rsheepshead-consoleui.asd sheepshead.nw | cpif sheepshead-consoleui.asd

sheepshead.txt: sheepshead.nw
	noweave -asciidoc -index sheepshead.nw | cpif sheepshead.txt

html: sheepshead.txt
	asciidoc -a toc -d book -b xhtml11 sheepshead.txt

pdf: sheepshead.txt
	a2x -a toc -d book -f pdf sheepshead.txt
