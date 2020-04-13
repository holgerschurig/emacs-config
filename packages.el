;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
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

(package! clean-aindent-mode)

(package! ivy-hydra)

(package! kurecolor)

(package! modus-vivendi-theme :recipe (:host gitlab :repo "protesilaos/modus-themes"))

(package! nswbuff :recipe (:host github :repo "joostkremers/nswbuff"))

(package! rainbow-mode)

(package! symbol-overlay)

(disable-packages!
  demangle-mode ;; objdump can do this quite well
  dhall-mode    ;; no need for this "programmable configuration language"
  dired-rsync   ;; would be pulled in by modules/data, but I don't need it
  disaster      ;; can show the dissembly of a C function
  doom-themes   ;; modus-vivendi is better
  drag-stuff    ;; I'll never drag stuff around
  forge         ;; would be pulled in by tools/magit, but I don't need it
  github-review ;; would be pulled in by tools/magit, but I don't need it
  graphql-mode  ;; would be pulled in by modules/data, but I don't need it
  helm-bibtex   ;; I won't write bibliographic entries
  hl-line       ;; highlights the current cursor line
  irony         ;; I'll use lsp eventually
  ivy-bibtex    ;; I won't write bibliographic entries
  jsonnet-mode  ;; would be pulled in by modules/data, but I don't need it
  magit-gitflow ;; would be pulled in by tools/magit, but I don't need it
  magit-todos   ;; would be pulled in by tools/magit, but I don't need it
  org-yt        ;; youtube links in org-mode?  Who wants that?  :-)
  rtags         ;; I'll use lsp eventually
  ;; smartparens   ;; makes many M-<cursor keypad> keys behave weird
  ivy-rtags     ;; I'll use lsp eventually
  irony-eldoc
  company-dict	;; we don't load complete from dictionaries
  )
