all:
	@# doom started to annoy me with "do you want to see the diffs"?  Nope.
	@# it also asks me something like "do you want to continue"? This sucks.
	cd ~/.emacs.d; git pull --rebase
	cd ~/.emacs.d; git log --reverse --no-merges -p ORIG_HEAD..HEAD >NEWS
	cd ~/.emacs.d; bin/doom clean
	cd ~/.emacs.d; bin/doom sync -u
	@# recompile pdf-tools
	$(MAKE) -C ~/.emacs.d/.local/straight/build/pdf-tools/build/server
