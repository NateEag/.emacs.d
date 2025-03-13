EMACS ?= emacs

elpa: math2svg.js

math2svg/node_modules:
	cd math2svg && npm ci
	git apply math2svg/node_modules.patch

math2svg.js: math2svg/node_modules math2svg/*.js
	cd math2svg && npm run build

clean:
	rm -rf math2svg.js* *.elc math2svg/node_modules

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

.PHONY: deps clean elpa
