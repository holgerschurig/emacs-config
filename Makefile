help::


install:
	$(MAKE) -C emacs.git install prefix=/usr/local/stow/emacs
	cd /usr/local/stow; stow emacs
help::
	@echo "sudo make install         install compiled emacs"

comp emacs.git/src/emacs: emacs.git/Makefile
	$(MAKE) -C emacs.git
help::
	@echo "make [-j] comp            compile emacs from source"

conf emacs.git/Makefile: emacs.git/configure
	cd emacs.git; ./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3
help::
	@echo "make conf                 configure emacs"

emacs.git/configure: emacs.git/.git/HEAD
	cd emacs.git; ./autogen.sh

pullemacs emacs.git/.git/HEAD:
ifeq ("$(wildcard emacs)","")
	git clone git://git.savannah.gnu.org/emacs.git emacs.git
else
	cd emacs.git; git pull
endif
help::
	@echo "make pullemacs            pull new emacs changes from git"

clean:
	cd emacs.git; git clean -fdx

uninstall:
	cd /usr/local/stow; stow -D emacs

help::
	@echo




pulldoom:
	@# doom started to annoy me with "do you want to see the diffs"?  Nope.
	@# it also asks me something like "do you want to continue"? This sucks.
	cd ~/.emacs.d; git pull --rebase
	cd ~/.emacs.d; git log --reverse --no-merges -p ORIG_HEAD..HEAD >NEWS
	cd ~/.emacs.d; bin/doom clean
	cd ~/.emacs.d; bin/doom sync -u
	@# recompile pdf-tools
	$(MAKE) -C ~/.emacs.d/.local/straight/build/pdf-tools/build/server
help::
	@echo "make pulldoom             pull new doom changes from git"

sync:
	cd ~/.emacs.d; bin/doom sync
help::
	@echo "make sync                 sync doom repos with packages.el"

build:
	cd ~/.emacs.d; bin/doom build
help::
	@echo "make build                rebuild binary doom modules"
