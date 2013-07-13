OLDVER  = `head -n1 VERSION`
NEWVER  = $(OLDVER)
GRIZZL  = ../grizzl
PKGNAME = fiplr
FILES   = fiplr.el fiplr-pkg.el README.md

all: units package

# FIXME: Resolve these file names automatically
units:
	emacs -batch -l $(GRIZZL)/grizzl-core.el -l $(GRIZZL)/grizzl-read.el -l $(GRIZZL)/grizzl.el -l ert -l fiplr.el -l test/fiplr-test.el -f ert-run-tests-batch-and-exit

reversion:
	perl -pi -e "s/$(OLDVER)/$(NEWVER)/g" *.el
	echo $(NEWVER) > VERSION

package:
	mkdir -p $(PKGNAME)-$(NEWVER)
	cp $(FILES) $(PKGNAME)-$(NEWVER)/
	tar cvf $(PKGNAME)-$(NEWVER).tar $(PKGNAME)-$(NEWVER)
	rm -r $(PKGNAME)-$(NEWVER)
