src: core.lisp sheepshead.asd

core.lisp: sheepshead.nw
	notangle -Rcore.lisp sheepshead.nw | cpif core.lisp

sheepshead.asd: sheepshead.nw
	notangle -Rsheepshead.asd sheepshead.nw | cpif sheepshead.asd
