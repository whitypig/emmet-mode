DST=emmet-vars.el

all:	emmet-mode.elc emmet-vars.elc

emmet-mode.elc: emmet-mode.el emmet-vars.elc
	/usr/bin/env emacs --batch --eval '(progn (load-file "./emmet-vars.elc") (byte-compile-file "$<"))'

emmet-vars.elc: emmet-vars.el
	/usr/bin/env emacs --batch --eval '(byte-compile-file "emmet-vars.el")'

emmet-vars.el: conf/snippets.el conf/preferences.el
	rm -f $(DST)
	touch $(DST)
	echo "(eval-when-compile" >> $(DST)
	echo "  (defmacro emmet-defparameter (symbol &optional initvalue docstring)" >> $(DST)
	echo "    \`(progn" >> $(DST)
	echo "       (defvar ,symbol nil ,docstring)" >> $(DST)
	echo "       (setq   ,symbol ,initvalue))))" >> $(DST)
	cat conf/snippets.el >> $(DST)
	cat conf/preferences.el >> $(DST)
	echo "" >> $(DST)
	echo "(provide 'emmet-vars)" >> $(DST)
	echo ";;; emmet-vars.el ends here" >> $(DST)

conf/snippets.el: conf/snippets.json
	tools/json2hash conf/snippets.json -o conf/snippets.el --defvar 'emmet-snippets'

conf/preferences.el: conf/preferences.json
	tools/json2hash conf/preferences.json -o conf/preferences.el --defvar 'emmet-preferences'

clean:
	rm -f emmet-mode.elc emmet-vars.elc emmet-vars.el conf/snippets.el conf/preferences.el

test:
	/usr/bin/env emacs --quick --script test/test.el

docs:
	echo docs

.PHONY: all test docs clean
