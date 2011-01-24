LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION=0.1.0
PKGNAME=emysql
APP_NAME=emysql

MODULES=$(shell ls -1 src/*.erl | awk -F[/.] '{ print $$2 }' | sed '$$q;s/$$/,/g')

all: app
	(cd src;$(MAKE))

app: ebin/$(PKGNAME).app

ebin/$(PKGNAME).app: src/$(PKGNAME).app.src
	mkdir -p ebin
	@sed -e 's/{modules, \[\]}/{modules, [$(MODULES)]}/' < $< > $@

docs:
	echo "@doc " > doc/readme.edoc; sed -E -f doc/overview.sed README.md >> doc/readme.edoc
	echo "@doc " > doc/changes.edoc; sed -E -f doc/overview.sed CHANGES.md >> doc/changes.edoc
	erl -noshell -run edoc_run application "'emysql'" '"."' '[{def,{vsn,""}},{stylesheet, "emysql-style.css"}]'
	sed -E -i "" -e "s/<table width=\"100%\" border=\"1\"/<table width=\"100%\" class=index border=\"0\"/" doc/*.html

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)
	rm -rf ebin/*.app cover erl_crash.dump
	rm doc/*.html

package: clean
	@mkdir emysql-$(VERSION)/ && cp -rf ebin include Makefile README src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install:
	@for i in ebin/*.beam ebin/*.app include/*.hrl src/*.erl; do install -m 644 -D $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done

test: all
	(cd t;$(MAKE))
	prove t/*.t
