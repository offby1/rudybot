.PHONY: tags
tags:
	etags --language=scheme *.rkt

TAGS: tags

.PHONY: check test
check test:
	raco test -x .
