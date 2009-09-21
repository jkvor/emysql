LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION=0.1.0
PKGNAME=emysql

all: app
	mkdir -p ebin
	(cd src;$(MAKE))

app:
	sh ebin/$(PKGNAME).app.in $(VERSION)

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)
	rm -rf ebin/*.app cover erl_crash.dump

package: clean
	@mkdir emysql-$(VERSION)/ && cp -rf ebin include Makefile README src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/
		
install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done

test: all
	(cd t;$(MAKE))
	prove t/*.t