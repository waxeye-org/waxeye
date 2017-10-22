# A GNU Make Makefile: https://www.gnu.org/software/make/manual/
SHELL=bash

# Waxeye version
VERSION=0.8.1

# Default task: Show help
.PHONY: help
help:
	@echo -ne "\
	Important targets:\\n\
	\\n\
	compiler   Build the Waxeye compiler binary\\n\
	runtimes   Build all the Waxeye runtimes\\n\
	book-html  Build the HTML version of the Manual book\\n\
	book-pdf   Build the PDF version of the Manual book\\n\
	site       Build the Waxeye homepage\\n\
	dist       Build packages for distribution\\n\
	test-all   Run all tests\\n\
	"

# Build the Waxeye compiler binary
.PHONY: compiler clean-compiler
compiler: bin/waxeye
bin/waxeye: src/waxeye/** src/racket/**
	# The PLTCOLLECTS environment variable is documented at:
	# https://docs.racket-lang.org/reference/collects.html#(part._collects-api)
	PLTCOLLECTS=":$(shell pwd)/src/racket/" \
	  raco exe -o waxeye src/waxeye/waxeye.rkt
	raco distribute . waxeye && rm waxeye
	@du -sh bin/waxeye
clean-compiler:
	rm -f bin/waxeye

# Build runtimes
.PHONY: runtimes clean-runtimes
runtimes: runtime-javascript runtime-rubygem runtime-java
clean-runtimes: clean-runtime-rubygem clean-runtime-java

# Runtimes
.PHONY: runtime-javascript
runtime-javascript: src/javascript/dist/waxeye.js src/waxeye/javascript.rkt
src/javascript/dist/waxeye.js: src/javascript/*.ts | src/javascript/node_modules
	cd src/javascript/ && npm run --silent compile
	cd src/javascript/ && npm run --silent format -- *.ts
	cd src/javascript/ && npm run --silent lint -- .
src/javascript/node_modules: \
  src/javascript/package.json src/javascript/package-lock.json
	cd src/javascript/ && npm install && touch node_modules

.PHONY: runtime-rubygem clean-runtime-rubygem
runtime-rubygem: lib/waxeye-$(VERSION).gem
lib/waxeye-$(VERSION).gem: src/ruby/**/*
	@mkdir -p lib
	cd src/ruby && gem build waxeye.gemspec && \
	  mv waxeye-$(VERSION).gem ../../lib/
	@du -sh lib/waxeye-$(VERSION).gem
clean-runtime-rubygem:
	rm -f lib/waxeye-*.gem

.PHONY: runtime-java clean-runtime-java
runtime-java: lib/waxeye.jar
ANT=$(shell which ant || echo Please install ant && exit 1)
lib/waxeye.jar: build/ant.xml src/java/**
	"$(ANT)" -quiet -f build/ant.xml jar
	@du -sh lib/waxeye.jar
clean-runtime-java:
	rm -f lib/waxeye.jar

# Waxeye homepage
.PHONY: site clean-site
site: book-html site-demo
clean-site: clean-book-html clean-site-demo

# RacketScript build of waxeye, for the web demo
.PHONY: site-demo clean-site-demo
site-demo: \
  docs/site/genfiles/racketscript-waxeye-compiler.js \
  docs/site/genfiles/traceur-runtime.js \
  docs/site/genfiles/waxeye.js
RACKS_BIN=$(shell racket -e \
  "(require setup/dirs) (display (find-user-console-bin-dir))")/racks
docs/site/genfiles/racketscript-waxeye-compiler.js: src/waxeye/** src/racket/**
	[ -e "$(RACKS_BIN)" ] || raco pkg install racketscript
	PLTCOLLECTS=":$(shell pwd)/src/racket/" $(RACKS_BIN) \
		--target traceur-browser -d tmp/racks src/waxeye/racketscript.rkt
	mkdir -p docs/site/genfiles/
	cp --preserve tmp/racks/dist/compiled.js \
	  docs/site/genfiles/racketscript-waxeye-compiler.js
docs/site/genfiles/traceur-runtime.js: \
  docs/site/genfiles/racketscript-waxeye-compiler.js
	cp --preserve tmp/racks/node_modules/traceur/bin/traceur-runtime.js \
	  docs/site/genfiles/
docs/site/genfiles/waxeye.js: src/javascript/dist/waxeye.js
	cp --preserve src/javascript/dist/waxeye.js \
	  src/javascript/dist/waxeye.js.map \
	  docs/site/genfiles/
clean-site-demo:
	rm -rf tmp/racks \
	  docs/site/genfiles/racketscript-waxeye-compiler.js \
	  docs/site/genfiles/traceur-runtime.js  \
	  docs/site/genfiles/waxeye.js

# Manual book in the HTML format
#
# To build on Ubuntu, install the following packages:
# sudo apt-get install asciidoc source-highlight
.PHONY: book-html clean-book-html
book-html: docs/site/manual.html
ASCIIDOC=asciidoc
docs/site/manual.html: docs/book/*
	# cd to the directory where source-highlight can find the .lang files
	cd docs/book &&	$(ASCIIDOC) -a toc -n -o ../site/manual.html book
clean-book-html:
	rm -f docs/site/manual.html

# Manual book in the PDF format
#
# To build on Ubuntu, install the following packages:
# sudo apt-get install asciidoc source-highlight xsltproc docbook-xsl fop
.PHONY: book-pdf clean-book-pdf
book-pdf: docs/manual.pdf
XSLTPROC=xsltproc
FOP=fop # Apache FOP
DOCBOOK_FO_XSL=/usr/share/xml/docbook/stylesheet/docbook-xsl/fo/docbook.xsl
docs/manual.pdf: docs/book/*
	mkdir -p tmp/book
	cd docs/book &&	$(ASCIIDOC) -a toc -b docbook --doctype=book \
	  -o ../../tmp/book/book.xml book
	$(XSLTPROC) -o tmp/book/book.fo $(DOCBOOK_FO_XSL) tmp/book/book.xml
	$(FOP) tmp/book/book.fo docs/manual.pdf
clean-book-pdf:
	rm -rf tmp/book docs/manual.pdf

# Packages for distribution
.PHONY: dist clean-dist
dist: site runtimes dist-compiler dist-src
clean-dist: clean-dist-compiler clean-dist-src

# Source code distribution
.PHONY: dist-src clean-dist-src
dist-src: \
  dist/waxeye-$(VERSION)-src.zip \
  dist/waxeye-$(VERSION)-src.tar.bz2
DIST_SRC_FILES=build grammars src test docs LICENSE README.md \
  lib/waxeye-$(VERSION).gem lib/waxeye.jar docs/site/manual.html
dist/waxeye-$(VERSION): $(DIST_SRC_FILES) | site runtimes
	rsync --recursive --archive --exclude=".*" --exclude="node_modules/" \
	  $(DIST_SRC_FILES) \
	  dist/waxeye-$(VERSION)
dist/waxeye-$(VERSION)-src.zip: | dist/waxeye-$(VERSION)
	cd dist && zip --recurse-paths --filesync --latest-time \
	  waxeye-$(VERSION)-src.zip waxeye-$(VERSION)
dist/waxeye-$(VERSION)-src.tar.bz2: | dist/waxeye-$(VERSION)
	tar --create --bzip2 \
	  -f dist/waxeye-$(VERSION)-src.tar.bz2 dist/waxeye-$(VERSION)
clean-dist-src:
	rm -f dist/waxeye-*-src.zip dist/waxeye-*-tar.bz2
	rm -rf dist/waxeye-*/

# Compiler binary distribution
.PHONY: dist-compiler clean-dist-compiler
dist-compiler: \
  dist/waxeye-compiler-$(VERSION)-bin-unix.tar.gz \
  dist/waxeye-compiler-$(VERSION)-bin-unix.tar.bz2
dist/waxeye-compiler-$(VERSION)-bin-unix.tar.gz: bin/waxeye
	tar --create --gzip -f dist/waxeye-compiler-$(VERSION)-bin-unix.tar.gz \
	  bin/waxeye
dist/waxeye-compiler-$(VERSION)-bin-unix.tar.bz2: bin/waxeye
	tar --create --bzip2 -f dist/waxeye-compiler-$(VERSION)-bin-unix.tar.bz2 \
	  bin/waxeye
clean-dist-compiler:
	rm -f dist/waxeye-compiler-*-bin-unix.tar.gz \
	  dist/waxeye-compiler-*-bin-unix.tar.bz2

# Testing
.PHONY: test-all test-c test-generate-parsers test-grammars test-java \
  test-javascript
test-all: test-c test-generate-parsers test-grammars test-java test-javascript
test-c:
	test/bin/test-c
test-generate-parsers:
	test/bin/test-generate-parsers
test-grammars:
	test/bin/test-grammars
test-java:
	test/bin/test-java
test-javascript: runtime-javascript
	cd src/javascript && npm run --silent format -- \
		../../test/javascript/{,**/}*.ts
	cd src/javascript && npm run --silent lint -- ../../test/javascript/
	test/bin/test-javascript

.PHONY: clean
clean: clean-dist clean-site clean-book-html clean-book-pdf \
  clean-compiler clean-runtimes
