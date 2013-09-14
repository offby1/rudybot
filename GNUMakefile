.PHONY: tags
tags:
	etags --language=scheme *.rkt

TAGS: tags

.PHONY: check
check:
	raco test -x .
