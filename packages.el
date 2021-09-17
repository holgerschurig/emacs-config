;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


(package! clean-aindent-mode)

(package! dts-mode)

(package! kurecolor)            ;; https://github.com/emacsfodder/kurecolor  -- edit colors inline

(package! meson-mode)           ;; https://github.com/wentasah/meson-mode

(package! modus-themes          ;; https://protesilaos.com/modus-themes/
 :recipe (:host github :repo "protesilaos/modus-themes"
          :files ("*.el")))

(package! mu4e                  ;; https://github.com/djcb/mu  -- eMail
  :recipe (:host github :repo "djcb/mu"
           :files ("mu4e/*.el")))

(package! nov)                  ;; https://depp.brause.cc/nov.el/  -- read ePUB

(package! nswbuff               ;; https://github.com/joostkremers/nswbuff  -- buffer switching
  :recipe (:host github :repo "joostkremers/nswbuff"))

(package! org-roam)             ;; https://www.orgroam.com/ and https://github.com/org-roam/org-roam/

(package! ox-hugo)              ;; https://github.com/kaushalmodi/ox-hugo

(package! rainbow-mode)         ;; https://github.com/emacs-straight/rainbow-mode  -- Colorize color names

(package! symbol-overlay)       ;; https://github.com/wolray/symbol-overlay  -- highlight & jump

(package! wgrep)                ;; https://github.com/mhayashi1120/Emacs-wgrep  -- writable grep


(disable-packages!
  better-jumper ;; looks like it only works in evil mode
  demangle-mode ;; objdump can do this quite well
  dhall-mode    ;; no need for this "programmable configuration language"
  dired-rsync   ;; would be pulled in by modules/data, but I don't need it
  disaster      ;; needs makefile support to build an object, useless with Ninja
  doom-themes   ;; modus-vivendi is better
  drag-stuff    ;; I'll never drag stuff around
  helm-bibtex   ;; I won't write bibliographic entries
  hl-line       ;; highlights the current cursor line
  irony         ;; I'll use lsp/eglot
  ivy-bibtex    ;; I won't write bibliographic entries
  jsonnet-mode  ;; would be pulled in by modules/data, but I don't need it
  org-yt        ;; youtube links in org-mode?  Who wants that?  :-)
  rtags         ;; I'll use lsp/eglot
  solaire-mode  ;; does interference with modus-vivendi-theme
  smartparens   ;; makes many M-<cursor keypad> keys behave weird
  ivy-rtags     ;; I'll use lsp/eglot. And no ivy
  irony-eldoc   ;; I'll use lsp/eglot
  company-dict  ;; we don't load complete from dictionaries
  ;; I use magit, not vc
  vc
  vc-git
  vc-hooks
  )
