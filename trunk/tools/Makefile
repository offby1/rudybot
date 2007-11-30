servlet: seen.ss

# TODO -- rather than hard-coding this directory, ask mzscheme where it is.
servlet_dir=/usr/local/stow/plt.371/lib/plt/collects/web-server/default-web-root/servlets/

install: seen.ss
	cp $< $(servlet_dir)

seen.ss:
	code=$$(pwd)/web-server-idea.ss; echo "(module seen mzscheme (require (file \"$$code\")) (provide (all-from  (file \"$$code\"))))" > $@

../quotes.cookies: ../quotes
	mzscheme -qr make-cookie-file.ss

.PHONY: install servlet
