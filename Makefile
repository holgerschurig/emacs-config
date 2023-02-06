help::

UID := $(shell id -u)




#############################################################################
#
#  Emacs
#
#############################################################################


# Get / Pull emacs

ifeq ("$(wildcard emacs.git)","")
getemacs emacs.git/.git/HEAD:
	git clone git://git.savannah.gnu.org/emacs.git emacs.git
	git checkout -b emacs-29 origin/emacs-29
help::
	@echo "make getemacs             get current emacs-29 git"
else
pullemacs emacs.git/.git/HEAD:
	cd emacs.git; git pull
help::
	@echo "make pullemacs            pull new emacs changes from git"
endif


# Configure Emacs

ifeq ($(WAYLAND_DISPLAY),)
EMACS_EXTRA_CONF=--with-toolkit-scroll-bars --with-xwidgets
else
EMACS_EXTRA_CONF=--with-pgtk
endif

emacs.git/configure: emacs.git/.git/HEAD
	cd emacs.git; ./autogen.sh

confemacs emacs.git/Makefile: emacs.git/configure
	cd emacs.git; \
	./configure \
		$(EMACS_EXTRA_CONF) \
		-C \
		--with-cairo \
		--with-dbus \
		--with-file-notification=inotify \
		--with-gnutls \
		--with-gpm=no \
		--with-harfbuzz \
		--with-imagemagick \
		--with-json \
		--with-modules \
		--with-native-compilation=aot \
		--with-rsvg \
		--with-small-ja-dic \
		--with-sound=alsa \
		--with-threads \
		--with-tree-sitter \
		--with-x-toolkit=gtk3 \
		--with-xml2 \
		--without-gconf \
		--without-gpm \
		--without-gsettings \
		--without-hesiod \
		--without-kerberos \
		--without-kerberos5 \
		--without-ns \
		--without-pop \
		--without-selinux \
		--without-wide-int \
		--without-xft \
		--without-xim \
		CFLAGS='-g -O2 -march=native -ffile-prefix-map=/home/holger=. -fstack-protector-strong -Wformat -Werror=format-security -Wall -fno-omit-frame-pointer'
		CPPFLAGS='-Wdate-time -D_FORTIFY_SOURCE=2' \
		LDFLAGS='-Wl,-z,relro'

help::
	@echo "make confemacs            configure emacs"


# Compile

compemacs emacs.git/src/emacs: emacs.git/Makefile
	$(MAKE) --no-print-directory -C emacs.git
cleanemacs:
	cd emacs.git; git clean -fdx
help::
	@echo "make [-j] compemacs       compile emacs from source"
	@echo "make cleanemacs           clean Emacs' source tree"


# Install / Uninstall

instemacs: emacs.git/src/emacs
ifeq ($(UID),0)
	cd /usr/local/stow; stow --delete emacs || true
	rm -rf /usr/local/stow/emacs
	$(MAKE) --no-print-directory -C emacs.git install prefix=/usr/local/stow/emacs
	cd /usr/local/stow; stow emacs
else
	sudo $(MAKE) --no-print-directory instemacs
endif
uninstemacs:
	cd /usr/local/stow; stow --delete emacs
help::
	@echo "make instemacs            install compiled emacs"
	@echo "make uninstemacs          uninstall emacs"



help::
	@echo




#############################################################################
#
#  Doom
#
#############################################################################

pulldoom:
	@# doom started to annoy me with "do you want to see the diffs"?  Nope.
	@# it also asks me something like "do you want to continue"? This sucks.
	cd ~/.emacs.d; git pull --rebase
	cd ~/.emacs.d; bin/doom clean
	cd ~/.emacs.d; bin/doom sync -u
	cd ~/.emacs.d; bin/doom build -r
	cd ~/.emacs.d; bin/doom purge -g
help::
	@echo "make pulldoom             pull new doom changes from git"


syncdoom:
	cd ~/.emacs.d; bin/doom sync
help::
	@echo "make syncdoom             sync doom repos with packages.el"


builddoom:
	cd ~/.emacs.d; bin/doom build -r
	cd ~/.emacs.d; bin/doom purge -g
help::
	@echo "make builddoom            rebuild binary doom modules"




#############################################################################
#
#  Experimantal stuff
#
#############################################################################


PDF_BUILD=~/.emacs.d/.local/straight/build-$(shell emacs -Q --batch --eval '(princ emacs-version)')/pdf-tools
showbuild:
	@echo $(PDF_BUILD)
comppdf $(PDF_BUILD)/build/server/epdfinfo:
	cd $(PDF_BUILD)/build/server; ./autobuild -i $(PDF_BUILD)
help::
	@echo "make comppdf              compile pdf-utils"
