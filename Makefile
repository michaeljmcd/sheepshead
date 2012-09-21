src: sheepshead.sw
	spiralweb -t sheepshead.sw

docs: doc/sheepshead.md

doc/sheepshead.md: sheepshead.sw
	spiralweb -w sheepshead.sw

html: doc/sheepshead.md
	pandoc -i docs/sheepshead.md -o docs/sheepshead.html --smart --standalone
