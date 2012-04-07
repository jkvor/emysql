LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION=0.2
PKGNAME=emysql
APP_NAME=emysql
CRYPTO_PATH=/opt/local/var/macports/software/erlang/R14A_0/opt/local/lib/erlang/lib/crypto-2.0/ebin/

MODULES=$(shell ls -1 src/*.erl | awk -F[/.] '{ print $$2 }' | sed '$$q;s/$$/,/g')
MAKETIME=$(shell date)

all: app
	(cd src;$(MAKE))

app: ebin/$(PKGNAME).app

ebin/$(PKGNAME).app: src/$(PKGNAME).app.src
	mkdir -p ebin
	sed -e 's/modules, \[\]/{modules, [$(MODULES)]}/;s/%MAKETIME%/$(MAKETIME)/' < $< > $@

# Create doc HTML from source comments
docs:
	sed -E -f doc/markedoc.sed README.md > doc/readme.edoc
	sed -E -f doc/markedoc.sed CHANGES.md > doc/changes.edoc
	erl -noshell -run edoc_run application "'emysql'" '"."' '[{def,{vsn,""}},{stylesheet, "emysql-style.css"}]'
	sed -E -i "" -e "s/<table width=\"100%\" border=\"1\"/<table width=\"100%\" class=index border=\"0\"/" doc/*.html

# Pushes created docs into dir ../Emysql-github-pages to push to github pages.
# Make sure to do 'make docs' first.
# will fail if you haven't checked out github pages into ../Emysql-github-pages
pages:
	(cd ../Emysql-github-pages; git pull origin gh-pages)
	cp -r doc/* ../Emysql-github-pages
	(cd ../Emysql-github-pages; git add .; git commit -m 'make pages'; git push origin gh-pages)

# Create HTML from Markdown to test README.md appearance
markdown:
	lua etc/markdown.lua README.md

hello:
	erlc hello.erl
	erl -pa ./ebin -s hello run -s init stop -noshell

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)
	rm -rf ebin/*.app cover erl_crash.dump
	rm -f ebin/erl_crash.dump
	rm -f src/erl_crash.dump
	rm -f erl_crash.dump
	rm -f doc/*.html
	rm -rf test/ct_run*
	rm -f test/variables-ct*
	rm -f test/*.beam
	rm -f test/*.html
	rm -rf ct_run*
	rm -f variables-ct*
	rm -f *.beam
	rm -f *.html

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Makefile README src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install:
	@for i in ebin/*.beam ebin/*.app include/*.hrl src/*.erl; do install -m 644 -D $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done

all-test: test encoding-test test20

encoding-test: all
	(cd test; ct_run -suite latin_SUITE utf8_SUITE utf8_to_latindb_SUITE latin_to_utf8db_SUITE -pa ../ebin $(CRYPTO_PATH))

test: all
	(cd test; ct_run -suite environment_SUITE basics_SUITE conn_mgr_SUITE -pa ../ebin $(CRYPTO_PATH))

test20: all
	(cd test; ct_run -suite pool_SUITE -pa ../ebin $(CRYPTO_PATH))

test9: all
	(cd test; ct_run -suite con_mgr_SUITE -pa ../ebin $(CRYPTO_PATH))

prove: all
	(cd t;$(MAKE))
	prove t/*.t
