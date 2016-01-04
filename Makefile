VERSION = 0.1.0

html: doc/html/specification-cli-client.html doc/html/specification-server.html

doc/html/specification-cli-client.html: doc/specification-cli-client.md
	cat doc/docs.m4 doc/specification-cli-client.md | m4 | pandoc -o doc/html/specification-cli-client.html --smart --standalone

doc/html/specification-server.html: doc/specification-server.md
	cat doc/docs.m4 doc/specification-server.md | m4 | pandoc -o doc/html/specification-server.html --smart --standalone --toc
