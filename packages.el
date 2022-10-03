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

(package! cape)                 ;; https://github.com/minad/cape -- completion at point extensions

(package! corfu)                ;; https://github.com/minad/corfu -- completion overlay

(package! dts-mode)             ;; https://github.com/bgamari/dts-mode -- Device tree mode

(package! kurecolor)            ;; https://github.com/emacsfodder/kurecolor  -- edit colors inline

(package! meson-mode)           ;; https://github.com/wentasah/meson-mode

(package! modus-themes          ;; https://protesilaos.com/modus-themes/
 :recipe (:host github :repo "protesilaos/modus-themes"
          :files ("*.el")))

;;(package! nov)                ;; https://depp.brause.cc/nov.el/  -- read ePUB

(package! nswbuff               ;; https://github.com/joostkremers/nswbuff  -- buffer switching
  :recipe (:host github :repo "joostkremers/nswbuff"))

(unpin! org-contrib)            ;; to fix: Warning (straight): Could not reset to commit "e3183921779eb4f36a2170ebb58e43eb0e84a07e" in repository "org-contrib"

(package! org-reveal)           ;; https://github.com/yjwen/org-reveal/ -- slide shows
(package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin")))

;; (package! ox-hugo            ;; https://github.com/kaushalmodi/ox-hugo
;;   :recipe (:host github :repo "kaushalmodi/ox-hugo" :branch "main"))

(package! rainbow-mode)         ;; https://github.com/emacs-straight/rainbow-mode  -- Colorize color names

(package! symbol-overlay)       ;; https://github.com/wolray/symbol-overlay  -- highlight & jump

(package! tempel)               ;; https://github.com/minad/tempel -- simple templates

(package! wgrep)                ;; https://github.com/mhayashi1120/Emacs-wgrep  -- writable grep

(package! 0x0)                  ;; https://gitlab.com/willvaughn/emacs-0x0 -- pastebin like service

(unpin! embark embark-consult consult vertico marginalia orderless)

(unpin! magit notmuch)

(disable-packages!
  better-jumper ;; looks like it only works in evil mode
  company-dict  ;; we don't load complete from dictionaries
  code-review   ;; I like magit, but so far I don't think I need any in-emacs code-review
  demangle-mode ;; objdump can do this quite well
  dhall-mode    ;; no need for this "programmable configuration language"
  dired-rsync   ;; would be pulled in by modules/data, but I don't need it
  disaster      ;; needs makefile support to build an object, useless with Ninja
  diredfl       ;; no need of extra fontlocking in dired
  doom-themes   ;; modus-vivendi is better
  drag-stuff    ;; I'll never drag stuff around
  helm-bibtex   ;; I won't write bibliographic entries
  hl-line       ;; highlights the current cursor line
  irony         ;; I'll use lsp/eglot
  irony-eldoc   ;; I'll use lsp/eglot
  ivy-bibtex    ;; I won't write bibliographic entries
  ivy-rtags     ;; I'll use lsp/eglot. And no ivy
  jsonnet-mode  ;; would be pulled in by modules/data, but I don't need it
  org-yt        ;; youtube links in org-mode?  Who wants that?  :-)
  rtags         ;; I'll use lsp/eglot
  smartparens   ;; makes many M-<cursor keypad> keys behave weird
  solaire-mode  ;; does interfere with modus-vivendi-theme
  which-key     ;; Embark's C-h does it better
  ;; I don't want to pull in tree-sitter just for indentation of a language I don't even code in ...
  emacs-tree-sitter
  tree-sitter-indent
  tree-sitter-langs
  csharp-mode
  ;; I use magit, not vc
  vc
  vc-git
  vc-hooks
  )
