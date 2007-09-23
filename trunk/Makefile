# BUGBUG -- we run svnversion as soon as "make" reads this makefile,
# but it'd be better if we could hold off until after we run 'svn up',
# since the resulting version string would then be cleaner (e.g.,
# "4839" versus "4837:4839")

svnversion:= $(shell svnversion -n .)
basename  := $(shell echo rudybot-$(svnversion) | sed s,:,-,)
plt-file  := $(basename).plt
tarball   := $(basename).tar.gz
distdir   := $(HOME)/public_html/bot

all: plt tar
plt: $(plt-file)
tar: $(tarball)

define make-export-dir
tempdir=`mktemp -d botXXXXXX` ; \
expdir=$${tempdir}/$(basename) ; \
svn export -rbase . $${expdir}
endef

define cleanup
rm -rf $${tempdir}
endef

FORCE:

sources=$(filter-out version.ss,$(wildcard *.ss))

bot: $(sources) version.ss
	mzc --exe $@ run-bot.ss

version.ss: FORCE
	echo -n '(define *svnversion-string* "' > $@
	echo -n $(svnversion) >> $@
	echo    '")' >> $@

$(plt-file): version.ss
	$(make-export-dir) && \
	cp $^ $${expdir} && \
	planet --create-archive $${expdir} && \
	$(cleanup)

$(tarball): version.ss
	$(make-export-dir) && \
	cp $^ $${expdir} && \
	tar -C $${tempdir} --create --gzip --file=$@ $(basename) && \
	$(cleanup)

svnup: 
	svn up

dist: svnup $(tarball)
	@mv -v $(tarball) $(distdir)
	@ln -svf $(distdir)/$(tarball) $(distdir)/rudybot.tar.gz

# To be blunt, run-all-tests.ss doesn't test as much as it should;
# dry-test hits some spots that run-all-tests misses.  So we run
# dry-tests too.  Alas, it doesn't report failures; you have to
# carefully examine the output for runtime errors.
check: version.ss
	./run-all-tests.ss
	@echo Feel free to kill the following if it runs for too long.
	./dry-test.sh

clean:
	rm -rf $(plt-file) $(tarball)

tags: TAGS

TAGS: $(sources)
	PATH=/usr/local/src/emacs-cvs/lib-src:$(dir $(EMACS))../lib-src:$$PATH etags --regex='/[ \t]*(def\w+[^ \t]+[ \t]+(?\([^ \t]*?\)\W+/\1/' *.ss

.PHONY: clean all dist plt tar check svnup
