help::


instemacs:
ifeq ($(UID),0)
	cd /usr/local/stow; stow --delete emacs
	rm -rf /usr/local/stow/emacs
	$(MAKE) -C emacs.git install prefix=/usr/local/stow/emacs
	cd /usr/local/stow; stow emacs
else
	sudo $(MAKE) instemacs
endif
help::
	@echo "make instemacs            install compiled emacs"

compemacs emacs.git/src/emacs: emacs.git/Makefile
	$(MAKE) -C emacs.git
help::
	@echo "make [-j] compemacs       compile emacs from source"

ifeq ($(shell hostname),holger)
EMACS_EXTRA_CONF=--with-pgtk
else
EMACS_EXTRA_CONF=--with-toolkit-scroll-bars --with-xwidgets
endif

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

emacs.git/configure: emacs.git/.git/HEAD
	cd emacs.git; ./autogen.sh

getemacs emacs.git/.git/HEAD:
ifeq ("$(wildcard emacs.git)","")
	git clone git://git.savannah.gnu.org/emacs.git emacs.git
	git checkout -b emacs-29 origin/emacs-29
else
	cd emacs.git; git pull
endif
help::
	@echo "make getemacs             pull new emacs changes from git"

clean:
	cd emacs.git; git clean -fdx

uninstall:
	cd /usr/local/stow; stow -D emacs



# PDF_BUILD=~/.emacs.d/.local/straight/build-28.0.91/pdf-tools/build/server
# $(PDF_BUILD)/configure:
# 	cd $(PDF_BUILD); ./autogen.sh
# $(PDF_BUILD)/Makefile: $(PDF_BUILD)/configure
# 	cd $(PDF_BUILD); ./configure
# comppdf: $(PDF_BUILD)/Makefile
# 	$(MAKE) -C $(PDF_BUILD)
# help::
# 	@echo "make comppdf              compile pdf-utils"



pulldoom:
	@# doom started to annoy me with "do you want to see the diffs"?  Nope.
	@# it also asks me something like "do you want to continue"? This sucks.
	cd ~/.emacs.d; git pull --rebase
	cd ~/.emacs.d; bin/doom clean
	cd ~/.emacs.d; bin/doom sync -u
	cd ~/.emacs.d; bin/doom purge -g
	cd ~/.emacs.d; bin/doom build -r
help::
	@echo "make pulldoom             pull new doom changes from git"

sync:
	cd ~/.emacs.d; bin/doom sync
	cd ~/.emacs.d; bin/doom purge -g
help::
	@echo "make sync                 sync doom repos with packages.el"

build:
	cd ~/.emacs.d; bin/doom build
help::
	@echo "make build                rebuild binary doom modules"
