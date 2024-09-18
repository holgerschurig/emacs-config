;;; -*- lexical-binding: t; -*-

;; Based on: James Cherti, https://github.com/jamescherti/minimal-emacs.d
;; SPDX-License-Identifier: GPL-3.0-or-later


;; TODO: use pulsar (instead of nav-flash)


;;; Personal information

(setq user-full-name "Holger Schurig")
(setq user-mail-address "holgerschurig@gmail.com")
(defvar my-irc-password nil "Password for the IRC networks")
(require 'private (locate-user-emacs-file "private.el") 'noerror)


;;; functions

(defun 822date ()
  "Insert date at point format the RFC822 way."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y %H:%M:%S %z")))

(defun isodate ()
  "Insert date at point format a ISO-like way."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun dos2unix()
  "Convert MSDOS (^M) end of line to Unix end of line."
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; Taken from Doom:

(defmacro setq! (&rest settings)
  "A more sensible `setopt' for setting customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be used
instead of `setopt'. Unlike `setq', this triggers custom setters on variables.
Unlike `setopt', this won't needlessly pull in dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                              ',var ,val))))


;; TODO
;; (defun sudo-edit () ... )




;;; Package: elpaca

;; https://github.com/progfolio/elpaca
;; https://github.com/progfolio/elpaca/blob/master/doc/manual.md
;; https://www.youtube.com/watch?v=5Ud-TE3iIQY

;; Search for packages?   M-x elpaca-menu-item
;;                        M-x elpaca-manager

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (locate-user-emacs-file "elpaca/"))
(defvar elpaca-builds-directory (locate-user-emacs-file "elpaca/builds/"))
(defvar elpaca-repos-directory (locate-user-emacs-file "elpaca/repos/"))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)

;; Somehow a let-binding don't prevent the pointless elpaca warning
(setq emacs-version "29.3")
(elpaca `(,@elpaca-order))
(setq emacs-version "29.3.50")

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(bind-key "q" #'kill-buffer-and-window 'elpaca-log-mode-map)


;;; Package: pre/exec-path-from-shell

;; https://github.com/purcell/exec-path-from-shell

(use-package exec-path-from-shell
  :ensure t
  :defer nil

  :custom
  (exec-path-from-shell-arguments '("-l")) ;; no -i (interactive shell)
  (exec-path-from-shell-variables '("PATH"
                                    "SSH_AUTH_SOCK" "SSH_AGENT_PID"
                                    "GPG_AGENT_INFO"
                                    "LANG"
                                    "LC_CTYPE"
                                    ;; "NIX_SSL_CERT_FILE" "NIX_PATH"
                                    ))
  :init
  (exec-path-from-shell-initialize)
)



;;; Package: core/align

(defadvice align-regexp (around align-regexp-with-spaces activate)
  "Make `align-regexp` not use tabs."
  (let ((indent-tabs-mode nil))
    ad-do-it))



;;; Package: core/auth-source

(use-package auth-source
  :config
  (setq! auth-sources (list (locate-user-emacs-file "authinfo.gpg")
                             "~/.authinfo.gpg"
                             "~/.authinfo"))
)



;;; Package: core/bookmark

;; taken from https://protesilaos.com/codelog/2023-06-28-emacs-mark-register-basics/

(use-package bookmark
  :defer t

  :config
  (defun my-bookmark-save-no-prompt (&rest _)
    "Run `bookmark-save' without prompts.

The intent of this function is to be added as an :after advice to
`bookmark-set-internal'.  Concretely, this means that when
`bookmark-set-internal' is called, this function is called right
afterwards.  We set this up because there is no hook after
setting a bookmark and we want to automatically save bookmarks at
that point."
    (funcall 'bookmark-save))

  (advice-add 'bookmark-set-internal :after 'my-bookmark-save-no-prompt)
)



;;; Package: core/browse-url

;; see https://www.emacswiki.org/emacs/BrowseUrl#h5o-7
(use-package browse-url
  :defer t

  :custom
  (browse-url-new-window-flag  t)
  (browse-url-firefox-new-window-is-tab t)

  :config
  (if is-mac
      (setopt browse-url-browser-function 'browse-url-chromium)
    (setopt browse-url-browser-function 'browse-url-firefox))
)



;;; Package: core/buffers

(setq-default tab-width 4)

;; Indent and formatting
(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the middle of a
;; word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

(setq kill-buffer-query-functions '(my-kill-without-query))

;; Don't asks you if you want to kill a buffer with a live process
;; attached to it:
(remove-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function)

(defun my-kill-without-query ()
  "Mark a buffer not modified, to make it killable without a
 query. Use with kill-buffer-query-functions."
  (not-modified) t)


;; revert buffer with one keystroke
(defun revert-buffer-no-confirm ()
  "Revert buffer, no questions asked"
  (interactive)
  (revert-buffer nil t t))

(bind-key "S-<f2>" #'revert-buffer-no-confirm)  ;; F2 is save, S-F2 is the opposite ;-)


(defun my-zoom-next-buffer ()
  (let ((curbuf (current-buffer))
        (firstbuf nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        ;;(princ (format "name %s, fn %s\n" (buffer-name) buffer-file-name))
        (unless (or
                 ;; Don't mention internal buffers.
                 (string= (substring (buffer-name) 0 1) " ")
                 ;; No buffers without files.
                 (not buffer-file-name)
                 ;; Skip the current buffer
                 (eq buffer curbuf)
                 )
          ;;(princ (format " nme %s, fn %s\n" (buffer-name) buffer-file-name))
          (unless firstbuf
            (setq firstbuf buffer))
          ;;(print buffer)
          )))
    (when firstbuf
      ;;(princ (format "new buffer: %s.\n" firstbuf))
      (bury-buffer)
      (switch-to-buffer firstbuf))))


(defun my-explode-window ()
  "If there is only one window displayed, act like C-x2. If there
are two windows displayed, act like C-x1:"
  (interactive)
  (if (one-window-p t)
      (progn
        (split-window-vertically)
        (other-window 1)
        (my-zoom-next-buffer)
        (other-window -1))
    (delete-other-windows)))

(bind-key "<f5>" #'my-explode-window)


;; If there is only one window displayed, swap it with previous buffer.
;; If there are two windows displayed, act like "C-x o".
(defun my-switch-to-buffer ()
  "If there is only one window displayed, swap it with previous buffer.
If there are two windows displayed, act like \"C-x o\"."
  (interactive)
  (if (one-window-p t)
      (mode-line-other-buffer)
    (other-window -1)))

(bind-key "<f6>" #'my-switch-to-buffer)



;;; Package: core/bytecomp

(use-package bytecomp
  :defer t

  :custom
  ;; free-vars   references to variables not in the current lexical scope.
  ;; unresolved  calls to unknown functions.
  ;; callargs    function calls with args that don't match the definition.
  ;; redefine    function name redefined from a macro to ordinary function or vice
  ;;             versa, or redefined to take a different number of arguments.
  ;; obsolete    obsolete variables and functions.
  ;; noruntime   functions that may not be defined at runtime (typically
  ;;             defined only under eval-when-compile).
  ;; interactive-only
  ;;             commands that normally shouldn't be called from Lisp code.
  ;; lexical     global/dynamic variables lacking a prefix.
  ;; lexical-dynamic
  ;;             lexically bound variable declared dynamic elsewhere
  ;; make-local  calls to make-variable-buffer-local that may be incorrect.
  ;; mapcar      mapcar called for effect.
  ;; not-unused  warning about using variables with symbol names starting with _.
  ;; constants   let-binding of, or assignment to, constants/nonvariables.
  ;; docstrings  docstrings that are too wide (longer than
  ;;             byte-compile-docstring-max-column or
  ;;             fill-column characters, whichever is bigger) or
  ;;             have other stylistic issues.
  ;; docstrings-non-ascii-quotes docstrings that have non-ASCII quotes.
  ;;                             This depends on the docstrings warning type.
  ;; suspicious  constructs that usually don't do what the coder wanted.
  (byte-compile-warnings '(
                           free-vars
                           unresolved
                           callargs
                           redefine
                           obsolete
                           noruntime
                           interactive-only
                           lexical
                           lexical-dynamic
                           make-local
                           mapcar
                           not-unused
                           constants
                           ;;docstrings
                           docstrings-non-ascii-quotes
                           suspicious
                           ))
)


;;; Package: core/calc

(use-package calc
  :defer t

  :custom
  (calc-angle-mode 'rad)  ; radians are radians, 0..2*pi
  (calc-symbolic-mode t)
)

;;; Package: core/calender

(use-package calendar
  :defer t

  :custom
  (setq! calendar-date-style 'iso)
  (setq! calendar-mark-holidays-flag t)
  (setq! calendar-week-start-day 1)
)



;;; Package: core/cus-edit

(use-package cus-edit
  :defer t

  :custom
  ;; keep lisp names in the custom buffers, don't capitalize.
  (custom-unlispify-tag-names nil)
  ;; kill old buffers.
  (custom-buffer-done-kill t)
)



;;; Package: core/dabbrev

(use-package dabbrev
  :defer t

  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
)



;;; Package: core/dictionary

(use-package dictionary
  :defer t

  :custom
  (dictionary-server "dict.org")
  (dictionary-use-single-buffer t)

  :config
  (advice-add
   'dictionary-lookup-definition
   :after
   (lambda (&rest r)
     (delete-other-windows)))

  :bind
  ("C-c d" . dictionary-lookup-definition)
)



;;; Package: core/dispnew

;; No beeping or blinking
(setq! visible-bell nil)



;;; Package: core/dnd

(use-package dnd
  :defer t

  :custom
  (dnd-indicate-insertion-point t)
  (dnd-scroll-margin 2)
)



;;; Package: core/eshell

(use-package eshell
  :commands eshell

  :custom
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-prefer-lisp-functions t)
  (eshell-directory-name (locate-user-emacs-file "var/eshell/"))

  :config
  (eshell/addpath "~/.local/bin")
  (eshell/addpath "~/bin")
)

(use-package em-alias
  :defer t
  :after eshell

  :config
  (defun my-eshell-setup ()
    (eshell-read-aliases-list)
    (setq-local show-trailing-whitespace nil)
    (eshell/alias "v" "view-file $1")
    (eshell/alias "x" "embark-open-externally $1")
    (eshell/alias "ll" "ls -l $*")
    )
  (add-hook 'eshell-mode-hook #'my-eshell-setup)

  (defun eshell/pro (project)
    (interactive)
    (let* ((home  (getenv "HOME"))
           (prodir (concat home "/d/" project))
           (srcdir (concat "/usr/src/" project)))
      (if (f-directory-p prodir)
          (eshell/cd prodir)
        (if (f-directory-p srcdir)
            (eshell/cd srcdir)
          (error "project %s not found" project)))))

  (defun my-eshell-clear ()
    (interactive)
    (eshell/clear-scrollback)
    (eshell-send-input))

  :bind (:map eshell-mode-map
         ("C-l" . my-eshell-clear))

  ;; (add-to-list 'eshell-modules-list 'eshell-smart)
)

(use-package em-banner
  :defer t
  :after eshell

  :custom
  (eshell-banner-message "")
)

(use-package em-hist
  :defer t
  :after eshell

  :config
  (setq eshell-hist-ignoredups t)
)

;; (use-package em-smart
;;   :after eshell
;;   :config
;;   ;; Put cursor at end of last command, so that I can easily change command line options
;;   (setq eshell-where-to-jump 'end)
;;   ;; don't keep cursor at end of commands without output
;;   (setq eshell-review-quick-commands t)
;; )

(use-package em-term
  :after eshell

  :config
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
)



;;; Package: core/executable

(use-package executable
  :defer t
  :hook
  ;; Make files with shebang ("#!" at start) executable when we save them
  (after-save . executable-make-buffer-file-executable-if-script-p)
)



;;; Package: core/eval

(setq! debugger-stack-frame-as-list t)



;;; Package: core/files

(use-package files
  :defer t

  :custom
  (confirm-kill-emacs nil)
  (confirm-kill-processes nil)

  ;; We don't need no stinkin' auto-savin'
  (auto-save-default nil)

  ;; Look for sibling files (this is a list of (MATCH EXPANSION) entries)
  (find-sibling-rules '(("\\([^/]+\\)\\.c\\'" "\\1.h")
                        ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
                        ("\\([^/]+\\)\\.h\\'" "\\1.c")
                        ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp")))

  ;; Preserve hard links to the file you're editing (this is
  ;; especially important if you edit system files)
  (backup-by-copying-when-linked t)

  ;; Just never create backup files at all
  (make-backup-files nil)

  ;; Alternatively put backup files into their own directory
  ;; (backup-directory-alist (list (cons "." (locate-user-emacs-file "tmp/bak/"))))

  ;; According to the POSIX, a line is defined as "a sequence of zero or more
  ;; non-newline characters followed by a terminating newline".
  (require-final-newline t)

  ;; No backup files please
  (make-backup-files nil)
  (create-lockfiles nil)

  ;; Disable the warning "X and Y are the same file". Ignoring this warning is
  ;; acceptable since it will redirect you to the existing buffer regardless.
  (find-file-suppress-same-file-warnings t)

  ;; Skip confirmation prompts when creating a new file or buffer
  (confirm-nonexistent-file-or-buffer nil)

  ;; Resolve symlinks when opening files
  (find-file-visit-truename t)

  :config
  ;; prevent "Modification-flag cleared" message spam
  (defun not-modified (&optional arg)
    "Mark current buffer as unmodified, not needing to be saved.
With prefix ARG, mark buffer as modified, so \\[save-buffer] will save.

It is not a good idea to use this function in Lisp programs, because it
prints a message in the minibuffer.  Instead, use `set-buffer-modified-p'."
  (declare (interactive-only set-buffer-modified-p))
  (interactive "P")
  (set-buffer-modified-p arg))

  :bind
  ("<f2>" . save-buffer)
  ("<f3>" . find-sibling-file)  ;; toggle between .c and .h
)





;;; Package: core/font-core

;; I got the idea from here:
;; http://amitp.blogspot.de/2013/05/emacs-highlight-active-buffer.html

;; With Wayland I use the window manager Hyprland, which can dim
;; windows without focus by itself, so no need to do that in Emacs via
;; font-lock games.

(unless is-way
  (defun highlight-focus:app-focus-in ()
    (global-font-lock-mode 1))

  (defun highlight-focus:app-focus-out ()
    (global-font-lock-mode -1))

  (add-hook 'focus-in-hook  #'highlight-focus:app-focus-in)
  (add-hook 'focus-out-hook #'highlight-focus:app-focus-out))



;;; Package: core/font-lock

(use-package font-lock
  :custom
  (font-lock-maximum-decoration 2)  ;; was t
)



;;; Package: core/frame

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq! frame-resize-pixelwise t)

;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
;; Additionally, it can cause freezing, especially on macOS, for users with
;; customized and colored cursors.
;; TODO revisit
;; (blink-cursor-mode -1)


;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider` does not.
(use-package frame
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)

  :config
  (add-hook 'after-init-hook #'window-divider-mode)
)



;;; Package: core/hippie-exp (disabled)

;; https://www.masteringemacs.org/article/text-expansion-hippie-expand

(use-package hippie-exp
  :disabled t

  :config

  ;; Use C-h a ^try to see which try-* functions exist:
  ;;
  ;; try-complete-file-name                 Try to complete text as a file name.
  ;; try-complete-file-name-partially       Try to complete text as a file name, as many characters a…
  ;; try-complete-lisp-symbol               Try to complete word as an Emacs Lisp symbol.
  ;; try-complete-lisp-symbol-partially     Try to complete as an Emacs Lisp symbol, as many characte…
  ;; try-expand-all-abbrevs                 Try to expand word before point according to all abbrev t…
  ;; try-expand-dabbrev                     Try to expand word "dynamically", searching the current b…
  ;; try-expand-dabbrev-all-buffers         Try to expand word "dynamically", searching all other buf…
  ;; try-expand-dabbrev-from-kill           Try to expand word "dynamically", searching the kill ring.
  ;; try-expand-dabbrev-visible             Try to expand word "dynamically", searching visible windo…
  ;; try-expand-line                        Try to complete the current line to an entire line in the…
  ;; try-expand-line-all-buffers            Try to complete the current line, searching all other buf…
  ;; try-expand-list                        Try to complete the current beginning of a list.
  ;; try-expand-list-all-buffers            Try to complete the current list, searching all other buf…
  ;; try-expand-whole-kill                  Try to complete text with something from the kill ring.
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
  :bind
  ("M-/" . hippie-expand)
)




;;; Package: core/indent

;; TAB first tries to indent the current line, and if the line was already
;; indented, then try to complete the thing at point.
;;
;; See also c-tab-always-indent
(setq! tab-always-indent 'complete)

;; Governs the behavior of TAB completion on the first press of the key.
;;
;; When nil, complete.  When eol, only complete if point is at the
;; end of a line.  When word, complete unless the next character
;; has word syntax (according to syntax-after).  When
;; word-or-paren, complete unless the next character is part of a
;; word or a parenthesis.  When word-or-paren-or-punct, complete
;; unless the next character is part of a word, parenthesis, or
;; punctuation.  Typing TAB a second time always results in
;; completion.
(setq! tab-first-completion 'eol)



;;; Package: core/minibuf

(setq! history-length 1000)
(setq! history-delete-duplicates t)
(setq! resize-mini-windows t)
;;(setq! completion-cycle-threshold 3)
(setq! enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)



;;; Package: core/misc

(use-package misc
  :defer t

  :bind
  ("C-z" . zap-up-to-char)   ;; was: syspend-frame
)



;;; Package: core/mouse


(use-package mouse
  :custom
  (mouse-drag-mode-line-buffer t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-drag-and-drop-region-scroll-margin t)
  (mouse-drag-copy-region 'non-empty)

  (mouse-yank-at-point t)
)



;;; Package: core/mule-util

(use-package mule-util
  :defer t

  :custom
  (truncate-string-ellipsis "…")
)



;;; Package: core/mwheel

(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 2)
)



;;; Package: core/ns & core/mac

(when is-mac
  ;; https://emacsformacosx.com/ uses ns- prefixes
  ;; https://goykhman.ca/gene/blog/2024-08-17-upgrading-to-emacs-294-on-apple-silicon-macs.html which uses Mituhari's work uses mac- prefixes

  (when (boundp 'ns-alternate-modifier)
    ;; Behavior of left command key
    (setopt ns-command-modifier 'meta)
    ;; Behavior of left option key
    (setopt ns-alternate-modifier 'meta))

  (when (boundp 'mac-command-modifier)
    ;; Ctrl key, has 'control as default so no need to change
    ;;(setopt mac-control-modifier 'control)
    ;; Windows key, now the Mac command key
    (setopt mac-command-modifier 'super)
    (bind-key "s-c" #'kill-ring-save)    ;; emulate Command-C
    (bind-key "s-x" #'kill-region)       ;; emulate Command-X
    (bind-key "s-v" #'yank)              ;; emulate Command-V
    (bind-key "s-a" #'mark-hwole-buffer) ;; emulate Command-A
    (bind-key "s-z" #'undo)              ;; emulate Command-Z
    (bind-key "s-a" #'mark-whole-buffer) ;; emulate Command-A
    (bind-key "s-z" #'undo)              ;; emulate Command-Z
    ;; Alt key
    (setopt mac-option-modifier 'meta)
    ;; ALTGR key, now we can type ALTGR-Q to get a @
    (setopt mac-right-option-modifier nil)))



;;; Package: core/nsm

(use-package nsm
  :defer t

  :custom
  (nsm-settings-file (locate-user-emacs-file "var/network-security.data"))
)


;;; package: core/package

(use-package package
  :defer t

  :custom
  (package-user-dir (locate-user-emacs-file "var/elpa"))
)



;;; package: core/paragraphs

;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
(setq! sentence-end-double-space nil)



;;; Package: core/paren

;; If the other side of the parenthesis is offscreen, then an overlay is shown
;; showing where the other side belongs to.

(use-package paren
  :defer t

  :custom
  (show-paren-context-when-offscreen 'overlay)
)



;;; Package: core/pixel-scroll

;; This is helpful if there are large images in the buffer, which most likely
;; will be in org-mode.

(use-package pixel-scroll
  :defer t

  :functions (pixel-scroll-precision-mode)
)



;;; Package: core/process


(setopt read-process-output-max (* 1024 1024))



;;; Package: core/recentf

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup 300)  ;; cleanup when Emacs was 300 seconds idle
  (recentf-max-menu-items 20)
  (recentf-save-file (locate-user-emacs-file "var/recentf"))

  :config
  (dolist (i `(".*-autoloads\\.el\\'"
               ".*CMakeFiles.*"
               ".pdf$"
               "COMMIT_EDITMSG"
               "COMMIT_EDITMSG"
               "TAG_EDITMSG"
               "\\.html$"
               "\\.org_archive$"
               "\\.png$"
               "^/tmp/"
               "svn-commit\\.tmp$"))
    (add-to-list 'recentf-exclude i))
)



;;; Package: core/rect

;; Turn the rectangle-mark-mode on with C-x <SPACE>

(use-package rect
  :defer t

  :bind (
  :map rectangle-mark-mode-map
  ("t" . string-rectangle)                   ;; replace rectange with string
  ("o" . open-rectangle)                     ;; blank rectange, shifting text to right
  ("c" . clear-rectangle)                    ;; replace with blanks
  ("k" . kill-rectangle)                     ;; delete rectangle and save to kill-ring
  ("d" . delete-rectangle)                   ;; delete rectangle, don't save
  ("y" . yank-rectangle)                     ;; yank last killed rectange to upper left
  ("w" . copy-rectangle-as-kill)             ;; save rectange to kill-ring
  ("n" . rectangle-number-lines)
  ("x" . rectangle-exchange-point-and-mark)
  ("s" . string-rectangle)                   ;; replace rectange with string
  )
  ;; already defined:
  ;; "n"       rectangle-number-lines
  ;; C-x C-x   cycle between the four corners
)



;;; Package: core/replace

;; https://www.reddit.com/r/emacs/comments/rk5t3b/do_you_use_interactive_regexp_replace_with_emacs/

;; If you don't use pcre or some other package then you may find emacs' own
;; regexps are clumsy when doing interactive regexp replace, because you have to
;; escape some of the most frequent constructs like \(...\) and \|.
;;
;; This snippet reverses the semantics for these when using M-x
;; query-replace-regexp, so that you can simply type ( ) for capture and | for
;; alternation and use \( etc. for matching an actual paren which is needed less
;; often:

(defun my-perform-replace (origfun &rest args)
  (apply
   origfun
   (if (eq this-command 'query-replace-regexp) ;; do conversion only for regexp replace
       (cons (let ((s (car args))
                   (placeholder (format "@placeholder%s@"
                                        (int-to-string
                                         (buffer-modified-tick)))))
               (dolist (char '("|" "(" ")"))
                 (setq s (replace-regexp-in-string
                          placeholder char
                          (replace-regexp-in-string
                           char (concat "\\\\" char)
                           (replace-regexp-in-string
                            (concat "\\\\" char) placeholder
                            s)))))
               s)

             (cdr args))

     args)))
(advice-add 'perform-replace :around 'my-perform-replace)



;;; Package: core/register

(use-package register
  :defer t
  :custom
  (setq! register-preview-delay 0)

  :config
  (setq register-preview-function #'consult-register-format)
)



;;; Package: core/server

(use-package server
  :if (display-graphic-p)
  :defer 1

  :config
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  (unless (server-running-p)
    (server-start)))



;;; Package: core/simple

;; React faster to keystrokes
(setq! idle-update-delay 0.35)

;; Be silent when killing text from read only buffer:
(setq! kill-read-only-ok t)

;; Read quoted chars with radix 16 --- octal is sooooo 1960
(setq! read-quoted-char-radix 16)

;; Deleting past a tab normally changes tab into spaces. Don't do
;; that, kill the tab instead.
(setq! backward-delete-char-untabify-method nil)

(setq! kill-ring-max 500)

;; Don't type C-u C-SPC C-u C-SPC to pop 2 marks, now you can do C-u C-SPC C-SPC
(setq! set-mark-command-repeat-pop t)

;; Remove duplicates from the kill ring to reduce clutter
(setq! kill-do-not-save-duplicates t)

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil)

;; Don't blink the paren matching the one at point, it's too distracting.
;; TODO revisit
;;(setq! blink-matching-paren nil)

;; If we copy an unmarked region, don't move the cursor
;; (setq copy-region-blink-predicate #'region-indistinguishable-p)


;; The following may be of interest to people who (a) are happy with
;; "C-w" and friends for killing and yanking, (b) use
;; "transient-mark-mode", (c) also like the traditional Unix tty
;; behavior that "C-w" deletes a word backwards. It tweaks "C-w" so
;; that, if the mark is inactive, it deletes a word backwards instead
;; of killing the region. Without that tweak, the C-w would create an
;; error text without an active region.
;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking#toc2

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
  backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))


(defun my-next-error (&optional arg)
  (interactive)
  (let ((p))
    (when (and (boundp 'flycheck-mode) flycheck-mode)
      (ignore-errors
        (save-excursion (call-interactively #'flycheck-next-error arg)
                        (cl-pushnew (point) p))))
    (when (and (boundp 'flymake-mode) flymake-mode)
      (ignore-errors
        (save-excursion (call-interactively #'flymake-goto-next-error arg)
                        (cl-pushnew (point) p))))
    (when (and (boundp 'spell-fu-mode) spell-fu-mode)
      (ignore-errors
        (save-excursion (call-interactively #'spell-fu-goto-next-error)
                        (cl-pushnew (point) p))))
    (when (and (boundp 'jinx-mode) jinx-mode)
      (ignore-errors
        (save-excursion (call-interactively #'jinx-next)
                        (cl-pushnew (point) p))))
    (ignore-errors
      (save-excursion (call-interactively #'next-error arg)
                      (cl-pushnew (point) p)))
    (unless (null p)
      (goto-char (apply 'min p)))))

(defun my-previous-error (&optional arg)
  (interactive)
  (let ((p))
    (when (and (boundp 'flycheck-mode) flycheck-mode)
      (ignore-errors
        (save-excursion (call-interactively #'flycheck-prev-error arg)
                        (cl-pushnew (point) p))))
    (when (and (boundp 'flymake-mode) flymake-mode)
      (ignore-errors
        (save-excursion (call-interactively #'flymake-goto-prev-error arg)
                        (cl-pushnew (point) p))))
    (when (and (boundp 'spell-fu-mode) spell-fu-mode)
      (ignore-errors
        (save-excursion (call-interactively #'spell-fu-goto-previous-error)
                        (cl-pushnew (point) p))))
    (when (and (boundp 'jinx-mode) jinx-mode)
      (ignore-errors
        (save-excursion (call-interactively #'jinx-previous)
                        (cl-pushnew (point) p))))
    (ignore-errors
      (save-excursion (call-interactively #'previous-error arg)
                      (cl-pushnew (point) p)))
    (unless (null p)
      (goto-char (apply 'min p)))))

(bind-key "<f8>"    #'my-next-error)
(bind-key "S-<f8>"  #'my-previous-error)


(bind-key "C-x I" #'insert-buffer)
(bind-key "M-SPC" #'cycle-spacing)     ;; just-one-space
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)
(bind-key "M-g s" #'scratch-buffer)
(bind-key "M-o" #'delete-blank-lines)  ;; opposite of C-o)

;; On MacOS these are normally doing begin-of-buffer / end-of-buffer
(bind-key "<home>" #'move-beginning-of-line)
(bind-key "<end>" #'move-end-of-line)


;; From https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode and
;; https://www.youtube.com/watch?v=ppbcLsc-F20
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Make overly long lines at least visible in all their glory
(global-visual-line-mode)


;;; Package: core/savehist

(use-package savehist
  ;; persist variables across sessions
  :defer 1

  :init
  (savehist-mode)

  :custom
  (savehist-file (locate-user-emacs-file "var/savehist"))

  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
)



;; Package: saveplace

;; `save-place-mode` enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.

(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "var/saveplace"))
  (save-place-limit 600)

  :init
  (save-place-mode)
)



;;; Package: core/startup

(setq! auto-save-list-file-prefix nil)



;;; Package: core/subr

;; Enable/Disable Commands

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)



;;; Package: core/tabify

(use-package tabify
  :defer t

  :config
  ;; only tabify initial whitespace
  (setq tabify-regexp "^\t* [ \t]+")
)



;;; Package: core/terminal

(setq! ring-bell-function #'ignore)



;;; Package: core/tramp

;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd

;; TODO: Not sure if this is actually needed, since tramp-methods these days
;; contain podman.
;;
(use-package tramp
  :defer t

  :custom
  (remote-file-name-inhibit-cache 60)
  (tramp-completion-reread-directory-timeout 60)
  (tramp-verbose 1)
)



;;; Package: core/uniqify

(use-package uniquify
  :defer t

  :custom
  (uniquify-buffer-name-style 'forward)
)



;;; Package: core/uniqify

(use-package url
  :defer t

  :custom
  (url-configuration-directory (locate-user-emacs-file "var/url/"))
)



;;; Package: core/man

;; Well, woman can't render the page, but at least it doesn't appear to hang ...

(when is-mac
  (defalias 'man #'woman
    "On mac, M-x woman is fast, but M-x man is dead-slow."))



;;; Package: core/window

(use-package window
  :custom
  (fast-but-imprecise-scrolling nil)
  (scroll-preserve-screen-position t)
  ;; Do not resize windows pixelwise, as this can cause crashes in some
  ;; cases when resizing too many windows at once or rapidly. Yaih, GTK.
  (window-resize-pixelwise nil)

  ;; TODO Prefer vertical splits over horizontal ones
  (split-width-threshold 170)
  (split-height-threshold nil)

  :config
  ;; Minimize cursor lag slightly by preventing automatic adjustment of
  ;; `window-vscroll' for tall lines.
  (setq auto-window-vscroll nil)

  ;; This makes "kill-buffer-and-window" not emit annoying
  ;; "Attempt to delete minibuffer or sole ordinary window"
  ;;
  ;; I just added an (ignore-errors ...) around (delete-windows)
  (defun kill-buffer-and-window ()
    "Kill the current buffer and delete the selected window."
    (interactive)
    (let ((window-to-delete (selected-window))
          (buffer-to-kill (current-buffer))
          (delete-window-hook (lambda () (ignore-errors (delete-window)))))
      (unwind-protect
          (progn
            (add-hook 'kill-buffer-hook delete-window-hook t t)
            (if (kill-buffer (current-buffer))
                ;; If `delete-window' failed before, we rerun it to regenerate
                ;; the error so it can be seen in the echo area.
                (when (eq (selected-window) window-to-delete)
                  (ignore-errors (delete-window)))))
        ;; If the buffer is not dead for some reason (probably because
        ;; of a `quit' signal), remove the hook again.
        (ignore-errors
          (with-current-buffer buffer-to-kill
            (remove-hook 'kill-buffer-hook delete-window-hook t))))))

  ;; Allow scrolling up and down
  (defun my-scroll-up ()
    "Scrolls the whole buffer contents up, while keeping cursor where it is."
    (interactive)
    (scroll-up-command 1))

  (defun my-scroll-down ()
    "Scrolls the whole buffer contents down, while keeping cursor where it is."
    (interactive)
    (scroll-down-command 1))

  :bind
  ;; This doesn't do the annoying prompt that kill-buffer normally does but
  ;; instead kills the buffer immediately
  ("C-x k" . kill-buffer-and-window)

  ("S-<f5>"     . previous-buffer)
  ("S-<f6>"     . next-buffer)
  ("C-S-<up>"   . my-scroll-down)
  ("C-S-<down>" . my-scroll-up)
)



;;; Package: core/xdisp

;; (setq! hscroll-step 1)
(setq! hscroll-margin 0)
(setq! scroll-step 1)
;; (setq! scroll-margin 0)
(setq! scroll-conservatively 101)
(setq! scroll-up-aggressively 0.01)
(setq! scroll-down-aggressively 0.01)
(setq! scroll-preserve-screen-position 'always)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
;; TODO: revisit
;; (setq! truncate-partial-width-windows nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq! x-stretch-cursor nil)

(setq! resize-mini-windows 'grow-only)

;; Enables faster scrolling through unfontified regions. This may result in
;; brief periods of inaccurate syntax highlighting immediately after scrolling,
;; which should quickly self-correct.
(setq! fast-but-imprecise-scrolling t)

(setq! hscroll-margin 2) ;; TODO maybe 0?
(setq! hscroll-step 1)

;; Emacs spends excessive time recentering the screen when the cursor
;; moves more than N lines past the window edges (where N is the value of
;; `scroll-conservatively`). This can be particularly slow in larger files
;; during extensive scrolling. If `scroll-conservatively` is set above
;; 100, the window is never automatically recentered. The default value of
;; 0 triggers recentering too aggressively. Setting it to 10 reduces
;; excessive recentering and only recenters the window when scrolling
;; significantly off-screen.
(setq! scroll-conservatively 10)


;;; Package: core/vc

(use-package vc-hooks
  :defer t

  :custom
  ;; Remove most back-ends from vc-mode
  (vc-handled-backends '(Git))
  ;; Disable version control when using tramp
  (vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules"))

  ; Do not backup version controlled files
  (vc-make-backup-files nil)

  ;; Resolve symlinks when opening files
  (vc-follow-symlinks t)
)



;;; Package: core/winner

;; C-c left:  winner-undo
;; C-c right: winner-redo

(use-package winner
  ;; If you don't like C-c left / C-c right:
  ;; :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :init
  (winner-mode)

  :custom
  (winner-boring-buffers '(
                           "*Apropos*"
                           "*Buffer List*"
                           "*Compile-Log*"
                           "*Completion*"
                           "*Fuzzy Completions*"
                           "*Help*"
                           "*Ibuffer*"
                           "*cvs*"
                           "*esh command on file*"
                           "*inferior-lisp*"
                           ))

)



;;; Package: core/xref

(use-package xref
  :ensure t  ;; we need a newer version
  :defer t

  :custom
  (xref-file-name-display 'project-relative)                   ;; was abs
  (xref-search-program 'ripgrep)                               ;; was grep
  (xref-show-xrefs-function #'consult-xref)                    ;; was xref--show-xref-buffer
  (xref-show-definitions-function #'consult-xref)              ;; was xref-definition-xref-buffer

  :bind (
   ("M-s u" . xref-find-references)                             ;; like "search usage"

   ;; This basically swaps TAB and RET
   :map xref--xref-buffer-mode-map
   ("RET" . xref-quit-and-goto-xref) ;; was xref-goto-xref
   ("TAB" . xref-goto-xref)          ;; was xref-quit-and-goto-xref
  )
)



;;; Package: gui/delight

;; https://www.emacswiki.org/emacs/DelightedModes

(use-package delight
  :ensure t
)



;;; Package: gui/indent-bars-mode

;; Make indentation level visible with bars
;;
;; https://github.com/jdtsmith/indent-bars

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :defer t

  :custom
  (indent-bars-display-on-blank-lines nil)

  :hook
  (python-mode   . indent-bars-mode)
  (yaml-mode     . indent-bars-mode)
  (makefile-mode . indent-bars-mode)
)



;;; Package: gui/info

;; https://github.com/kickingvegas/casual-info

(use-package casual-info
  :ensure t
  :defer t
  :after info

  :bind (
    :map Info-mode-map
    ("C-o" . casual-info-tmenu)

    ;; Use web-browser history navigation bindings
    ("M-[" . Info-history-back)
    ("M-]" . Info-history-forward)
    ;; Bind p and n to paragraph navigation
    ("p" . casual-info-browse-backward-paragraph)
    ("n" . casual-info-browse-forward-paragraph)
    ;; Bind h and l to navigate to previous and next nodes
    ;; Bind j and k to navigate to next and previous references
    ("h" . Info-prev)
    ("j" . Info-next-reference)
    ("k" . Info-prev-reference)
    ("l" . Info-next)
    ;; Bind / to search
    ("/" . Info-search)
    ;; Set Bookmark
    ("B" . bookmark-set)
  )
)



;;; Package: gui/helpful

;; https://github.com/Wilfred/helpful

(use-package helpful
  :ensure t
  :defer t

  :config
  (add-hook 'helpful-mode-hook #'visual-line-mode)

  :bind (
   ;;("C-h f" . helpful-callable)
   ([remap describe-function] . helpful-callable)
   ("C-h v" . helpful-variable)
   ;;("C-h k" . helpful-key)
   ([remap describe-key] . helpful-key)
   ;;("C-h s" . helpful-symbol)
   ([remap describe-symbol] . helpful-symbol)
   ;;("C-h x" . helpful-command)
   ([remap describe-command] . helpful-command)
   ("C-h h" . helpful-at-point)   ;; was: display-hello-file
   ("C-h K" . describe-keymap)
   ("C-h F" . helpful-function)

   :map helpful-mode-map
   ([remap revert-buffer] . helpful-update)
   ("a" . describe-symbol)
   ("q" . kill-buffer-and-window)
  )
)



;;; Package: gui/hl-todo

(use-package hl-todo
  :ensure t
  :commands (hl-todo-mode)

  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold)))

  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'rust-mode-hook #'hl-todo-mode)
  (add-hook 'org-mode-hook #'hl-todo-mode)
  (add-hook 'yaml-mode-hook #'hl-todo-mode)
)



;;; Package: gui/hydra

;; TODO: remove once we converted to transient
(use-package hydra
  :ensure t
  :defer t
)



;;; Package: gui/kurecolor

;; https://github.com/emacsfodder/kurecolor

;; This package allows interactive modification of color values. I don't use a
;; keybinding, just M-x kurecolor if you need it.

(use-package kurecolor
  :ensure t
  :defer t

  :functions (casual-kurecolor-tmenu)

  :config
  (transient-define-prefix casual-kurecolor-tmenu ()
  "Transient menu for Kurecolor."
  [["Brightness"
    ("j"
     "Dec"
     kurecolor-decrease-brightness-by-step
     :transient t)
    ("J"
     "Inc"
     kurecolor-increase-brightness-by-step
     :transient t)
    ("sj"
     "Set"
     kurecolor-set-brightness)
    ("gj"
     "Gradient"
     kurecolor-hex-val-group)]
   ["Saturation"
    ("k"
     "Dec"
     kurecolor-decrease-saturation-by-step
     :transient t)
    ("K"
     "Inc"
     kurecolor-increase-saturation-by-step
     :transient t)
    ("sk"
     "Set"
     kurecolor-set-saturation)
    ("gk"
     "Gradient"
     kurecolor-hex-hue-group)]
  ["Hue"
   ("l"
    "Dec"
    kurecolor-decrease-hue-by-step
    :transient t)
   ("L"
    "Inc"
    kurecolor-increase-hue-by-step
    :transient t)
   ("sl"
    "Set"
    kurecolor-set-hue)
   ("gL"
    "Gradient"
    kurecolor-hex-hue-group)]]

  [["Convert"
    ("ch"
     "CSS-RGB → Hex"
     kurecolor-cssrgb-at-point-or-region-to-hex)
    ("cr"
     "Hex → CSS-RGB"
     kurecolor-cssrgb-at-point-or-region-to-hex)
    ("cR"
     "Hex → CSS-RGBA"
     kurecolor-hexcolor-at-point-or-region-to-css-rgba)]

   ["Misc"
    ("q" "Quit" transient-quit-one)]])

  :init
  (defun kurecolor ()
    "Turns on rainbow mode and lets you modify the current color code. The
cursor must be sitting over a CSS-like color string, e.g. \"#ff008c\"."
    (interactive)
    (rainbow-mode t)
    (require 'kurecolor)
    (casual-kurecolor-tmenu))
)



;;; Package: gui/minibuffer

(use-package minibuffer

  :custom
  ;; don't show "*Completions*" buffer
  ;; TODO not sure if needed when I use vertica+consult
  ;; (completion-auto-help nil)

  ;; Shorter default format formatting
  ;; (minibuffer-default-prompt-format " [%s]")

  ;; Don't insert current directory into minubuffer
  (insert-default-directory nil)

  :config
  (minibuffer-depth-indicate-mode 1)

  ;; TODO not sure if needed when I use vertica+consult
  ;; ;; Allow to type space chars in minibuffer input (for `timeclock-in',
  ;; ;; for example).
  ;; (define-key minibuffer-local-completion-map " " nil t)
  ;; (define-key minibuffer-local-must-match-map " " nil t))
)



;;; Package: gui/modus-vivendi theme

;; https://protesilaos.com/emacs/modus-themes
;; https://gitlab.com/protesilaos/modus-themes

;; Recipe to get modus from upstream, not the emacs version:
;; :recipe (:host github :repo "protesilaos/modus-themes"
;;          :files ("*.el"))


;; Built-in themes must use require-theme, not require
(require-theme 'modus-themes)

(setq! modus-themes-bold-constructs t)
(setq! modus-themes-italic-constructs nil)
(setq! modus-themes-paren-match '(intense))
(setq! modus-themes-mode-line '(borderless accented))
(setq! modus-themes-region '(bg-only))
(setq! modus-themes-syntax '(alt-syntax))
;;(setq! modus-themes-headings '((t . (rainbow))) )
(setq! modus-themes-common-palette-overrides
        '((fg-heading-0 blue-cooler)
          (fg-heading-1 magenta-cooler)
          (fg-heading-2 magenta-warmer)
          (fg-heading-3 blue)
          (fg-heading-4 cyan)
          (fg-heading-5 green-warmer)
          (fg-heading-6 yellow)
          (fg-heading-7 red)
          (fg-heading-8 magenta)

          (bg-tab-current "#ff4000")
          (bg-tab-other   "#777777")
          (bg-tab-bar     "#777777")
          ))
(load-theme 'modus-vivendi)



;; Package: gui/rainbow-mode

;; Colorize strings that look like colors

(use-package rainbow-mode
  :ensure (:host github :repo "emacsmirror/rainbow-mode" :branch "master")
  :defer t
)



;;; Package: gui/nerd-icons

;; https://github.com/emacsmirror/nerd-icons


(use-package nerd-icons
  :ensure t
  :defer t
  :commands (nerd-icons-insert)

  ;; :config
  ;; TODO Takes too long, and there is no simple method to check if
  ;; some font is actually downloaded or not
  ;;
  ;; (nerd-icons-install-fonts t)
)


;;; Package: gui/pulsar

;; https://protesilaos.com/emacs/pulsar

(use-package pulsar
  :ensure t
  :commands (pulsar-pulse-line)

  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.03)
  (pulsar-iterations 20) ;; 0.03*20 = 0.6 seconds
  (pulsar-face 'pulsar-magenta)

  :config
  (add-to-list 'pulsar-pulse-functions 'my-explode-window)
  (add-to-list 'pulsar-pulse-functions 'my-switch-to-buffer)
  (add-to-list 'pulsar-pulse-functions 'consult-imenu)

  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
  (add-hook 'after-focus-change-function  #'pulsar-pulse-line)

  ;; integration with the `consult' package:
  (with-eval-after-load 'consult
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))

  ;; integration with the built-in `imenu':
  (with-eval-after-load 'imenu
    (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))

  (pulsar-global-mode 1)
)



;;; Package: gui/shackle

;; https://depp.brause.cc/shackle/

(use-package shackle
  :ensure t

  :custom
  ;; :select                select the popped up window
  ;; :inhibit-window-quit   disable the q key
  ;; :custom                override with custom action
  ;; :ignore                ignore specified buffer
  ;; :other                 use or re-use other window
  ;; :same                  use this window
  ;; :popup                 pop up a new window
  ;; :align                 align 'above, 'below, 'left or 'right, t or (function)
  ;; :size                  fractonal=ratio, integer=lines
  ;; :frame                 pop up a frame instead of a new window
  (shackle-rules '(
           (compilation-mode       :select nil :align 'bottom :size 0.3)
		   (proced-mode            :select t   :same t)
		   (dired-mode             :select t   :same t)
		   (messages-buffer-mode   :select nil :other t :inhibit-window-quit t)
           (helpful-mode           :select t   :same t  :inhibit-window-quit t)
           (help-mode              :select t   :same t  :inhibit-window-quit t)
		   ("\\*magit"   :regexp t :select t   :same t)
		   ("\\*shell.*" :regexp t :select t   :same t)
		   ("\\*Cargo.*" :regexp t :select nil :other t)
		   ("\\*Pp Eval" :regexp t :select t   :other t)
                 ))

  (shackle-default-rule nil)

  :config
  (shackle-mode)
)



;;; Package: gui/transient

; We need to :ensure this so that elpaca loads it, the bundled transient is too old
(use-package transient
  :ensure t
  :defer t

  :custom
  (transient-history-file (locate-user-emacs-file "var/history.el") )

  :config
  (transient-bind-q-to-quit)
)



;;; Package: gui/which-key

;; Original: https://github.com/justbur/emacs-which-key

(use-package which-key
  :ensure t
  :defer nil

  :custom
  (which-key-compute-remaps t)
  (which-key-dont-use-unicode nil)
  ;; (which-key-preserve-window-configuration t)
  (which-key-show-remaining-keys t)
  ;; (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-lighter "")

  :config
  (which-key-mode)
  (which-key-setup-minibuffer)
)



;;; Package: edit/autorevert

(use-package autorevert
  :defer 1

  :custom
  ;; Revert other buffers (e.g, Dired)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 1)
  (auto-revert-verbose nil)

  ;; TODO needed? (revert-without-query '(".")  ; Do not prompt
  (auto-revert-stop-on-user-input nil)
  ;; TODO revisit (auto-revert-verbose t)

  :init
  (global-auto-revert-mode)
)



;;; Package: edit/avy

;; https://github.com/abo-abo/avy
;; https://karthinks.com/software/avy-can-do-anything/
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el
;; https://melpa.org/#/?q=avy

;; Jump to char(s): M-j <quickly enter char(s)> <wait 0.2s> <enter highligher>
;; Action:          M-j <quickly enter char(s)> <wait 0.2s> <enter action> <enter highligher>

(use-package avy
  :ensure t
  :defer t

  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)

  :custom
  (avy-timeout-seconds 0.20)

  ;; Use more letters for targets (i.E. not just the home row
  ;; HINT: Only use keyss here that aren't in avy-dispatch-alist
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?o ?v ?b ?n ?, ?, ?. ?-))

  ;; ORIGINAL:
  ;; HINT: Only use keys here that aren't in avy-keys
  ;; HINT: use ?\C-m or ?M-m for modifier keys
  (avy-dispatch-alist'((?c . avy-action-mark-to-char)
                       (?e . avy-action-embark)
                       ;;(?h . avy-action-helpful) ;; do this via embark
                       (?i . avy-action-ispell)    ;; TODO: do this with jinx-correct, without turning on jinx-mode?
                       (?k . avy-action-kill-stay)
                       (?t . avy-action-teleport)
                       (?m . avy-action-mark)
                       (?n . avy-action-copy)
                       (?x . avy-action-exchange)
                       (?y . avy-action-yank)
                       (?Y . avy-action-yank-line)
                       (?z . avy-action-zap-to-char)
                       ))

  :config

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  :bind (
    ("M-j" . avy-goto-char-timer)
    :map isearch-mode-map
    ("M-j" . avy-isearch)
  )
)


;;; Package: edit/embark

;; The following keymaps already exist, so you can simply add actions to them.
;; Position the cursor somewhere and press "C-,". To view which actions are
;; available, run "M-x describe-keybinding" or "C-h".

;; +vertico/embark-doom-package-map
;; embark-become-file+buffer-map
;; embark-become-help-map
;; embark-become-keymaps
;; embark-become-match-map
;; embark-become-shell-command-map
;; embark-bookmark-map
;; embark-buffer-map
;; embark-collect-direct-action-minor-mode-map
;; embark-collect-mode-map
;; embark-command-map
;; embark-consult-async-search-map
;; embark-consult-search-map
;; embark-consult-sync-search-map
;; embark-defun-map
;; embark-email-map
;; embark-encode-map
;; embark-expression-map
;; embark-face-map
;; embark-file-map
;; embark-function-map
;; embark-general-map
;; embark-heading-map
;; embark-identifier-map
;; embark-keymap-alist
;; embark-keymap-prompter-key
;; embark-kill-ring-map
;; embark-library-map
;; embark-meta-map
;; embark-package-map
;; embark-paragraph-map
;; embark-prose-map
;; embark-region-map
;; embark-sentence-map
;; embark-sort-map
;; embark-symbol-map
;; embark-tab-map
;; embark-unicode-name-map
;; embark-url-map
;; embark-variable-map
;; embark-vc-file-map

;; https://github.com/oantolin/embark
;; https://github.com/oantolin/embark/wiki/Default-Actions
;;
;; E.g. go to a lisp symbol and hit "C-; h" to get help on symbol
;;      go to an URL        and hit "C-; e" to open the URL in eww (or "C-, b" to browse it normally)
;; generally hit "C-, C-h" to get help on available actions, which sometimes
;; display more entries than which-keys
;; Switch scope with                "C-; C-;"

(use-package embark
  :ensure t
  :defer t

  :config
  (defun which-key--hide-popup-ignore-command ()
    "Empty dummy function.")

  ;; Always come up with the various options
  ;; use @ <key binding> or narrowing
  ;;(setq embark-prompter #'embark-completing-read-prompter)
  ;; or use the default, then use C-h
  (setq embark-prompter #'embark-keymap-prompter)

  ;; Now that we don't use which-keys anymore, remove this indicator
  (remove-hook 'embark-indicators #'+vertico-embark-which-key-indicator)
  (remove-hook 'embark-indicators #'embark-minimal-indicator)
  (remove-hook 'embark-indicators #'embark-highlight-indicator)
  (remove-hook 'embark-indicators #'embark-isearch-highlight-indicator)
  (add-hook 'embark-indicators #'embark-minimal-indicator 10)
  (add-hook 'embark-indicators #'embark-highlight-indicator 20)
  (add-hook 'embark-indicators #'embark-isearch-highlight-indicator 30)

  ;; Emphasize current line a bit better
  (add-hook 'embark-collect-mode #'hl-line-mode)

  ;; This allows you to use C-; after a prefix key, e.g. "C-x C-;" to get
  ;; an embark-narrowable list of items
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Make some actions work without a special query
  (assq-delete-all 'delete-file                   embark-pre-action-hooks)
  (assq-delete-all 'delete-directory              embark-pre-action-hooks)
  (assq-delete-all 'kill-buffer                   embark-pre-action-hooks)
  (assq-delete-all 'embark-kill-buffer-and-window embark-pre-action-hooks)

  ;; Keep Embark from trying to insert current selection into a y-or-n prompt
  (setq y-or-n-p-use-read-key t)

  ;; Always prompt via vertico?  Something like "C-a u" won't then work :-(
  ;; (setq embark-prompter 'embark-completing-read-prompter)

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'

   :map embark-general-map
   ("C-;" . embark-cycle)
   ;; L used to be embark-collect-life, which isn't that helpful if one already
   ;; uses something like vertico or selectrum
   ("L" . nil)

   :map embark-collect-mode-map
   ("M-t" . toggle-truncate-lines)

   :map embark-file-map
   ("U" . 0x0-upload-file)

   ;; doesn't seem to work
   :map embark-function-map
   ("l" . eldoc)
   :map embark-variable-map
   ("l" . eldoc)
   ;; Used to be customize-variable, better use "=" to set the variable
   ("u" . nil)
   :map embark-expression-map
   ("l" . eldoc)

   :map embark-region-map
   ("U" . 0x0-upload-text)
  )
)



;;; Package: edit/embark-consult

;; https://github.com/emacs-straight/embark-consult

(use-package embark-consult
  :ensure t
  :after (embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
)



;;; Package: edit/expand-region

(use-package expand-region
  :ensure t
  :defer t

  :custom
  (expand-region-reset-fast-key    "<ESC><ESC>")
  (expand-region-smart-cursor t)

  :bind
  ("C-+" . er/expand-region)
)



;;; Package: edit/isearch

;; https://github.com/kickingvegas/casual-isearch

(use-package isearch
  :defer t

  :custom
  ;; Scrolling (including C-s) while searching:
  (isearch-allow-scroll t)

  ;; Show number of matches
  (isearch-lazy-count t)

  ;; Do less flickering be removing highlighting immediately
  (lazy-highlight-initial-delay 0)

  :config
  ;; Let isearch wrap
  (setq isearch-wrap-function
        (lambda ()
          (if isearch-forward
              (goto-char (window-start))
            (goto-char (window-end))))
        isearch-wrap-pause 'no)
)


(use-package casual-isearch
  :ensure t
  :defer t
  :after isearch

  :bind (
    :map isearch-mode-map
    ("C-o" . casual-isearch-tmenu)
  )
)




;;; Package: edit/link-hint

;; use avy to open a visible link

(use-package link-hint
  :ensure t
  :defer t

  :bind
  ("M-g l" . link-hint-open-link)
)



;;; Package: edit/symbol-overlay (jump / manipulate symbols)

;; https://github.com/wolray/symbol-overlay

(use-package symbol-overlay
  :ensure t
  :defer t

  :bind
  ("M-<up>"   . symbol-overlay-jump-prev)
  ("M-<down>" . symbol-overlay-jump-next)
)



;;; Package: edit/undo-fu

;; https://github.com/emacsmirror/undo-fu

(use-package undo-fu
  :ensure t

  :init
  (define-key global-map [remap undo] #'undo-fu-only-undo)
  (define-key global-map [remap redo] #'undo-fu-only-redo)

  :custom
  ;; Increase undo history limits to reduce likelihood of data loss
  (undo-limit 400000)           ; 400kb (default is 160kb)
  (undo-strong-limit 3000000)   ; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)   ; 48mb  (default is 24mb)

  :bind
  ("C-_" . undo-fu-only-undo)
  ("M-_" . undo-fu-only-redo)
  ("C-M-_". undo-fu-only-redo-all)
)



;;; Package: edit/wgrep  (writable grep)

;; https://github.com/mhayashi1120/Emacs-wgrep

;; Use: C-c C-p

(use-package wgrep
  :ensure t
)

;;; Package: ide/clean-aindent-mode

;; Nice tip from tuhdo, see https://www.emacswiki.org/emacs/CleanAutoIndent
;; https://github.com/pmarinov/clean-aindent-mode

(use-package clean-aindent-mode
  :ensure t
  :defer t

  :hook
  (prog-mode-hook . clean-aindent-mode)
)



;;; Package: ide/comment-dwim-2

(use-package comment-dwim-2
  :disabled t
  :ensure t
  :defer t

  :bind (
   ("M-;" . comment-dwim-2)
   :map org-mode-map
   ("M-;" . org-comment-dwim-2))
)



;;; Package: ide/compile

(use-package compile
  :defer t

  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)

  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  (defun my-compilation-setup ()
    (setq-local truncate-lines nil))
  (add-hook 'compilation-mode-hook #'my-compilation-setup)

  (defun my-compile-autoclose (buffer string)
    "Auto close compile log if there are no errors"
    (when (string-match "finished" string)
      (delete-window (get-buffer-window buffer t))
      (bury-buffer-internal buffer)))
  (add-to-list 'compilation-finish-functions #'my-compile-autoclose)

  ;; the next-error function weirdly stops at "In file included from
  ;; config.cpp:14:0:". Stop that:
  ;; http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode
  ;; (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
)



;;; Package: ide/consult-flycheck

(use-package consult-flycheck
  :ensure t
  :defer t
  :after (consult flycheck)

  :bind
  ("M-g f" . consult-flycheck)
)



;;; Package: ide/consult-lsp

(use-package consult-lsp
  :ensure t
  :defer t
  :after (consult lsp)

  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)

  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols))
)



;;; Package: ide/direnv

;; https://github.com/wbolster/emacs-direnv
;; https://github.com/purcell/envrc

;; Both are IMHO too chatty, create *envrc* buffers here and there,
;; emmitting messages warnings and whatnot.



;;; Package: ide/dtrt-indent

(defvar-local inhibit-dtrt-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")

(defvar dtrt-excluded-modes
  '(pascal-mode
    so-long-mode
    c-mode  ;; uses it's own call to dtrt-indent-mode
    c++-mode
    sh-mode
    ;; Automatic indent detection in org files is meaningless. Not to mention, a
    ;; non-standard `tab-width' causes an error in org-mode.
    org-mode)
  "A list of major modes where indentation shouldn't be auto-detected.")

(use-package dtrt-indent
  :ensure t
  :unless noninteractive
  :commands (dtrt-indent-mode)

  :config
  (defun my-detect-indentation ()
    (unless (or (not after-init-time)
                inhibit-dtrt-detection
                (eq major-mode 'fundamental-mode)
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (apply #'derived-mode-p dtrt-excluded-modes)
                )
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1))))
  (add-hook 'prog-mode-hook #'my-detect-indentation)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  :custom
  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (dtrt-indent-max-lines 2000)
)



;;; Package: ide/editorconfig

;; https://github.com/editorconfig/editorconfig-emacs

(use-package editorconfig
  :ensure t
  :delight

  :config
  ;; Archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are
  ;; zipped XML formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'")
  (editorconfig-mode 1)
)



;;; Package: ide/eglot (disabled, trying lsp again)

;; If you don't have a compilation database (e.g. in projects not using
;; CMake or Meson), you can use https://github.com/rizsotto/Bear

;; TODO: convert hydra to transient

(use-package eglot
  :disabled t
  :defer t

  :custom

  (eglot-stay-out-of '(company))

  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-15" "-j=2" "--clang-tidy" "--compile-commands-dir=build")))
  (add-to-list 'eglot-server-programs '(c-mode  .  ("clangd-15" "-j=2" "--clang-tidy" "--compile-commands-dir=build")))

  ;; Eglot modifies the completion defaults, undo this. Remove-hook is used as
  ;; remove-from-list :-)
  (remove-hook 'completion-category-defaults '(eglot (styles flex basic)))

  ;; Default to have the hints mode off in managed buffers
  (defun my-eglot-managed-hook ()
    (eglot-inlay-hints-mode -1))
  (add-hook 'eglot-managed-mode-hook #'my-eglot-managed-hook)

  (defhydra eglot-help (:color blue :hint nil)
    "
^^Actions      ^^Find              ^^Modify       Toggles
^^-------------^^------------------^^-------------^^-------
_a_ctions      _r_eferences        _f_ormat       el_D_oc
_E_xtract      de_c_laration       _B_ormat buf   _h_ints
_I_nline       _d_efinition        _F_ix buf      fly_m_ake
_R_ename var   _i_mplementation    _I_mports
re_W_rite      _t_ype definition
^^             consult _s_ymbol
"
    ("a" eglot-code-actions)
    ("E" eglot-code-action-extract)
    ("I" eglot-code-action-inline)
    ("R" eglot-rename)
    ("W" eglot-code-action-rewrite)

    ("r" xref-find-references)
    ("c" eglot-find-declaration)
    ("d" xref-find-definitions)
    ("i" eglot-find-implementation)
    ("t" eglot-find-typeDefinition)
    ("s" consult-eglot-symbols)

    ("f" eglot-format)
    ("B" eglot-format-buffer)
    ("F" eglot-code-action-quickfix)
    ("I" eglot-code-action-organize-imports)

    ("D" eldoc-mode :exit nil)
    ("h" eglot-inlay-hints-mode :exit nil)
    ;; eglot is built in, and flymake is built in. So let's toggle flymake here
    ("m" flymake-mode :exit nil)
  )

  :bind
  (:map eglot-mode-map
  ("C-c a" . eglot-code-actions)                   ;; was embark-act, but that is also in C-;
  ("C-o"   . eglot-help/body)                      ;; like the casual menus
  ("C-c R" . eglot-rename)                         ;; unused in c-mode
  ;; (M-;   . xref-go-back)
  ;; (M-.   . xref-find-definitions)
  ;; (M-g e . consult-compile-error)               ;; for errors, maybe even warnings
  ;; (M-s u . xref-find-reference)                 ;; like "search usage"
  )
)



;;; Package: ide/flycheck

;; https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html

(use-package flycheck
  :ensure t
  :defer 1

  :init
  (global-flycheck-mode +1)

  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
)



;;; Package: ide/flymake (disabled)

;; we're using flycheck, since lsp kind-of depends on it (there are many lsp-flycheck-XXX functions

(use-package flymake
  :disabled t
  :defer t

  :custom
  (flymake-wrap-around nil)
)



;;; Package: ide/git-commit

;; This is a dependency of transient
(use-package git-commit
  :ensure t
  :defer t

  :custom
  ;; Anything longer will be highlighted
  (git-commit-summary-max-length 70)
)



;;; Package: ide/magit

(use-package magit
  :ensure t
  :defer t

  :custom
  ;; Open magit window full-screen
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; When calling magit-status, save all buffers without further ado
  (magit-save-repository-buffers 'dontask)
  ;; make [MASTER] appear at the end of the summary line
  (magit-log-show-refname-after-summary t)
  ;; I want my (yet) untracked files to be diplayed at all times
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (stashes . hide)))

  ;; Switch repositories with magit-list-repositories
  (magit-repository-directories
   '(
     ("~/d"      . 1)
     ("~/src"    . 1)
     ))

  :bind
  ("M-g m" . magit-status)
  ("M-g M" . magit-list-repositories)
)



;;; Package: ide/make-mode

(use-package make-mode
  :ensure nil
  :defer t

  :mode (("\\.mk\\'" . makefile-gmake-mode))
)



;;; Package: ide/my-compile

(use-package my-compile
  :load-path "lisp"
  :defer t

  :bind
  ("S-<f7>" . my-compile-select-command-and-run)
  ("<f7>"   . my-compile)
)



;;; Package: ide/lsp-mode

;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/  <-- shows what LSP displays


(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp-mode lsp-deferred)

  :custom
  ;; from: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;; 1. Symbol highlighting
  (lsp-enable-symbol-highlighting nil)
  ;; 2. lsp-io-doc is in the lsp-ui section
  ;; 3. Lenses
  (lsp-lens-enable nil)
  ;; 4. Headerline
  (lsp-headerline-breadcrumb-enable t)
  ;; 5. Sideline code actions is in the lsp-ui section
  (eldoc-documentation-functions nil)
  (lsp-signature-auto-activate nil)
  ;; 6. Symbols info in sideline is in the lsp-ui section
  ;; 7. Modeline code actions
  (lsp-modeline-code-acions-enable t)
  ;; 8. Flycheck/Flymake
  (lsp-diagnostics-provider :flycheck)
  ;; 9. Sideline diagnostics is in the lsp-ui section
  ;; 10. Eldoc
  (lsp-eldoc-render-all t)
  (lsp-eldoc-enable-hover t)
  ;; 11. Modeline diagnostics statistics
  (lsp-modeline-diagnostics-enable t)
  ;; 12. Signature help
  (lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
  ;; 13. Signature help documentation
  (lsp-signature-render-documentation t)
  ;; 14. Completion
  (lsp-completion-provider :none) ;; needs to be :none if we use corfu, because otherwise LSP wants to configure company-mode ...
  ;; 15. Completion item detail
  (lsp-completion-show-detail nil)
  ;; 16. Completion item kind
  (lsp-completion-show-kind nil)

  ;; Using the easier to type C-c
  (lsp-keymap-prefix "C-c") ;; was s-l

  ;; Don't litter my .emacs.d
  (lsp-session-file (locate-user-emacs-file "var/lsp-session-v1"))
  (lsp-server-install-dir (locate-user-emacs-file "var/lsp"))

  ;; I'm using tempel currently, this is only for yasnippet
  (lsp-enable-snippet nil)

  ;; The default error handler is a bit useless, emit even "contents modified"
  (lsp-default-create-error-handler-fn (lambda (&rest ignored) #'ignore))

  ;; if you want to debug the LSP daemon interaction
  ;; (lsp-log-io t)

  ;; (lsp-inlay-hint-enable t)

  ;; :config
  ;; (transient-define-prefix casual-lsp-tmenu ()
  ;;   ;; see also lsp-command-map for examples of what we could add, and
  ;;   ;; what predicates might be applicable
  ;;   "Transient menu for Mastodon."
  ;;   [["Actions"
  ;;     ("a" "Action"           lsp-execute-code-action)
  ;;     ("R" "rename Var"       lsp-rename)]
  ;;    ["Find"
  ;;     ("r" "References"       lsp-find-references)
  ;;     ("c" "deClaration"      lsp-find-declaration)
  ;;     ("d" "Definition"       lsp-find-definition)
  ;;     ("i" "Implementation"   lsp-find-implementation)
  ;;     ("t" "Type defintion"   lsp-find-type-definition)]
  ;;    ["Modify"
  ;;     ("f" "Format region"    lsp-format-region)
  ;;     ("b" "format Buffer"    lsp-format-buffer)
  ;;     ("i" "sort Imports"     lsp-organize-imports)]
  ;;    ["Toggles"
  ;;     ("D" "doc mode"         lsp-ui-doc-mode :transient t)
  ;;     ("s" "ui Sideline"      lsp-ui-sideline-mode :transient t)
  ;;     ("A" "modeline Actions" lsp-modeline-code-actions-mode :transient t)
  ;;     ("B" "Breadcrumps"      lsp-headerline-breadcrumb-mode :transient t)
  ;;     ("S" "Signature"        lsp-toggle-signature-auto-activate :transient t)
  ;;     ("h" "inlay Hints"      lsp-inlay-hints-mode :transient t)
  ;;     ("c" "flycheck"         flycheck-mode :transient t)]
  ;;   ["Misc"
  ;;     ("?" "help"             describe-mode)
  ;;     ("q" "quit"             transient-quit-one)
  ;;     ]])

  :bind (:map lsp-command-map
         ("C-o" . casual-lsp-tmenu)
         ;; :map lsp-mode-map
         ;; ("M-." . lsp-find-definition)
        )

  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ;;(lsp-mode . lsp-ui-mode)
  (lsp-mode . corfu-mode)
  (lsp-mode . corfu-popupinfo-mode)
)




;;; Package: ide/lsp-rust

;: https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/

(use-package lsp-rust
  :ensure nil ;; comes with LSP
  :defer t

  ;; :custom
  ;; Lenses are this "> Run > Debug" overlays in the source code
  ;; (lsp-rust-analyzer-lens-enable nil)
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy") ;; was "check"
  ;; (lsp-rust-analyzer-display-chaining-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-display-parameter-hints t)
)



;;; Package: ide/lsp-ui

;; https://emacs-lsp.github.io/lsp-ui/

(use-package lsp-ui
  :ensure t
  :defer t

  :commands (lsp-ui-mode)

  :custom
  ;; from: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;; 2. Docs on hovering
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  ;; 5. Sideline code actions
  (lsp-ui-sideline-enable t)  ;; this is the whole sideline
  (lsp-ui-sideline-show-code-actions t)
  ;; 6. Symbols info in sideline, see also M-x lsp-ui-sideline-toggle-symbols-info
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil)
  ;; 9. Sideline diagnostics is in the lsp-ui section
  (lsp-ui-sideline-show-diagnostics nil) ;; show only errors if nil
)



;;; Package: ide/project

(use-package project
  :ensure t
  :defer t
  :functions (project-root)
)



;;; Package: prog/bazel

;; https://github.com/bazelbuild/emacs-bazel-mode

;; TODO: investigate buildifier, see https://github.com/bazelbuild/buildtools/tree/main/buildifier

(use-package bazel
  :ensure t
  ;;:ensure (:host github :repo "bazelbuild/emacs-bazel-mode")
  :defer t

  :mode
  ;; This are the same additons to auto-mode-alist that bazel.el would
  ;; set, but here we set it before loading it (which is faster)
  ("/\\.bazeliskrc\\'" . bazeliskrc-mode)
  ("/\\.bazelignore\\'" . bazelignore-mode)
  ("/\\(?:\\(?:bazel\\)?\\.bazelrc\\)\\'" . bazelrc-mode)
  ("/.+\\.bzl\\'" . bazel-starlark-mode)
  ("/MODULE\\.bazel\\'" . bazel-module-mode)
  ("/\\(?:WORKSPACE\\(?:\\.b\\(?:azel\\|zlmod\\)\\)?\\)\\'" . bazel-workspace-mode)
  ("/\\(?:BUILD\\(?:\\.bazel\\)?\\)\\'" . bazel-build-mode)

  :config
  (transient-define-prefix casual-bazel-tmenu ()
    "Transient menu for Bazel."
    [:pad-keys t
     ["Bazel"
      ("b"   "Build"         bazel-build)
      ("C-c" "Compile this"  bazel-compile-current-file)
      ("t"   "test"          bazel-test)
      ("C-t" "test at point" bazel-test-at-point)
      ("c"   "Coverage"      bazel-coverage)
      ("r"   "Run"           bazel-run)
      ("f"   "buildiFier"    bazel-buildifier)]

     ["Visit"
      ("R" "consuming Rule" bazel-show-consuming-rule)
      ("B" "Build file"     bazel-find-build-file)
      ("W" "Workspace file" bazel-find-workspace-file)]

     ["Misc"
      ("d" "remove coverage Display" bazel-remove-coverage-display)
      ("q" transient-quit-one)]])

  :bind (:map bazel-mode-map
         ("C-c C-o" . casual-bazel-tmenu))
)


;;; Package: prog/bb-mode (Bitbake)

;; https://github.com/mferland/bb-mode

(use-package bb-mode
  :ensure (:host github :repo "mferland/bb-mode")
  :defer t

  :mode (("\\.bb$" . bb-mode)
         ("\\.bbappend$" . bb-mode)
         ("\\.bbclass$" . bb-mode)
         ("src/poky/.*\\.inc$" . bb-mode)  ;; probably wrong, but I don't want to bind all .inc files
         ("src/poky/.*\\.conf$" . bb-mode)
         )
)



;;; Package: prog/cc-mode

(use-package cc-mode
  :defer t

  :config
  (c-add-style "qt-gnu"
               '("gnu" (c-access-key .
                                     "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")))
  (setq-default c-electric-flag nil)

  (defun my-c-mode-setup ()
    ;; TODO check for compilation database and if found, start eglot
    ;;(eglot-ensure)

    ;; need to check the mode because I run this also at the revert hook!
    (modify-syntax-entry ?_ "w")
    (setq c-recognize-knr-p nil)

    ;; Default, might be overwrriten by dtrt-indent, but if dtrt-indent cannot guess, we want to use tabs
    (setq indent-tabs-mode t)

    ;; Guess tabs vs. spaces, 4 vs 8
    ;; (require 'dtrt-mode)
    (dtrt-indent-mode +1)

    ;; use "// " for commenting in both C and C++
    (setq comment-start "// "
          comment-end "")

    (if (and buffer-file-name (string-match "/linux" buffer-file-name))
       ;; only for Linux C files
       (progn (c-set-style "linux-tabs-only")
            (setq tab-width 8
                  c-basic-offset 8))
      ;; (message "qt-gnu tw 4")
      (c-set-style "qt-gnu")
      (setq tab-width 4
            c-basic-offset 4))
    )

  (add-hook 'c-mode-hook   #'my-c-mode-setup)
  (add-hook 'c++-mode-hook #'my-c-mode-setup)

  :bind (
   :map c-mode-base-map
   ("TAB" . indent-for-tab-command)  ;; was c-indent-line-or-region
  )

)


;;; Package: prog/dts-mode (Device tree)

;; https://github.com/bgamari/dts-mode

(use-package dts-mode
  :ensure t
  :defer t

  :mode (("\\.dts\\'"     . dts-mode)
         ("\\.overlay\\'" . dts-mode))
)




;;; Package: prog/js-mode

(use-package js-mode
  :defer t

  :mode ("\\.ns\\'")  ;; bitburner .ns files
)




;;; Package: prog/lisp-mode

(use-package lisp-mode
  :defer t

  :config
  (defun my-emacs-lisp-mode-setup ()
    "My emacs lisp mode setup function."
    (interactive)
    ;; "-" is almost always part of a function- or variable-name
    (modify-syntax-entry ?- "w")

    ;; make sure we cannot save with imbalanced parenthesisa
    (add-hook 'write-file-functions 'check-parens nil t)

    ;; Modify completions, elisp-completion-at-point wouldn't allow me to
    ;; complete elisp things in comments.
    ;; (defalias 'my-elisp-capf (cape-capf-super #'elisp-completion-at-point
    ;;                                           #'cape-dabbrev
    ;;                                           #'cape-file
    ;;                                           #'cape-dict
    ;;                                           #'cape-elisp-symbol))
    ;;   (setq-local completion-at-point-functions '(my-elisp-capf t)))
    (setq-local completion-at-point-functions '(tempel-expand elisp-completion-at-point t))
    (corfu-mode)
    (add-to-list 'imenu-generic-expression
                 '("Section" "^;;[;]\\{1,8\\} \\(.*$\\)" 1)))

  (add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-setup)
)



;; Flycheck used checkdoc. And checkdoc checks for various Emacs
;; specific things that absolutely makes no sense. I mean, who needs
;; 'foo end here' comments? Or who cannot differentiate between a
;; header and the code and need a special ';; Code:' entry? I don't,
;; and I dislike being told by (flycheck/flymake)+checkdoc how to
;; write comments.
(use-package checkdoc
  :defer t

  :config
  (defun checkdoc-file-comments-engine()
    "Dummy replacement function."
    nil)
)



;;; Package: prog/lua

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
)



;;; Package: prog/prog-mode

(defun show-trailing-whitespace ()
  "Show trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace t))

(defun hide-trailing-whitespace ()
  "Hide trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace nil))

(use-package prog-mode
  :defer t

  :config
  ;; Show trailing whitespace when programming

  (add-hook 'prog-mode-hook #'show-trailing-whitespace)
)



;;; Package: prog/meson

;; https://github.com/wentasah/meson-mode

(use-package meson-mode
  :ensure t
  :defer t

  :mode (("/meson.build\\'" . meson-mode))

  :config
  (setq! meson-indent-basic 4)
)




;;; Package: prog/nim

(use-package nim-mode
  :ensure t
  :defer t

  :mode (("\\.build\\'" . nim-mode))

  :config
  (setq nim-pretty-triple-double-quotes nil)

  (setq nim-font-lock-keywords-extra
        `(;; export properties
          (,(nim-rx
             line-start (1+ " ")
             (? "case" (+ " "))
             (group
              (or identifier quoted-chars) "*"
              (? (and "[" word "]"))
              (0+ (and "," (? (0+ " "))
                       (or identifier quoted-chars) "*")))
             (0+ " ") (or ":" "{." "=") (0+ nonl)
             line-end)
           (1 'nim-font-lock-export-face))
          ;; Number literal
          (,(nim-rx nim-numbers)
           (0 'nim-font-lock-number-face))
          ;; Highlight identifier enclosed by "`"
          (nim-backtick-matcher
           (10 font-lock-constant-face prepend))

          ;; Highlight word after ‘is’ and ‘distinct’
          (,(nim-rx " " (or "is" "distinct") (+ " ")
                    (group identifier))
           (1 font-lock-type-face))
          ;; pragma
          (nim-pragma-matcher . (0 'nim-font-lock-pragma-face))))

  (when (fboundp 'rainbow-delimiters-mode)
    (add-hook 'nim-mode-hook #'rainbow-delimiters-mode))
  (add-hook 'nim-mode-hook #'subword-mode)
  ;; TODO (add-hook 'nim-mode-hook #'nimsuggest-mode)
)



;;; Package: prog/plantuml

(use-package plantuml-mode
  :ensure t
  :defer t

  :commands plantuml-download-jar

  :init
  (setq plantuml-jar-path "~//.cache/plantuml.jar"
        org-plantuml-jar-path plantuml-jar-path)

  :config
  (setq plantuml-default-exec-mode
        (cond ((file-exists-p plantuml-jar-path) 'jar)
              ((executable-find "plantuml") 'executable)
              (plantuml-default-exec-mode)))
)



;;; Package: prog/python

;; Don't forget that we can indent/detent blocks with C-x TAB ...

(use-package python
  :defer t

  :config
  (defun my-python-setup ()
    (interactive)
    (setq indent-tabs-mode t
          python-indent-offset 4
          tab-width 4
          ;; this fixes the weird indentation when entering a colon
          ;; from http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem
          electric-indent-chars (delq ?: electric-indent-chars)))
  (add-hook 'python-mode-hook #'my-python-setup)
)



;;; Package: prog/rustic

;; https://github.com/emacs-rustic/rustic
;; https://robert.kra.hn/posts/rust-emacs-setup/
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/
;; https://rust-analyzer.github.io/manual.html#assists-code-actions

;; General: C-c C-c
;; Popup:   C-c C-p

(use-package rustic
  :ensure t
  :defer t

  :mode ("\\.rs\\'" . rustic-mode)

  :custom
  (rustic-lsp-setup-p nil) ;; don't ask if I want to install LSP
  (rustic-indent-method-chain t)
  (rustic-babel-format-src-block nil) ;; TODO -> rustic-babel
  ;; See also rust-mode's lsp-rust-XXX variables!

  :config
  ;; (add-hook 'rustic-mode-hook #'lsp-mode)

  ;; (add-to-list 'flycheck-checkers 'rustic-clippy))
  ;; (add-hook 'rustic-mode-local-vars-hook #'tree-sitter! 'append))

  ;; These bindings bind some keys that are already in flycheck or lsp
  ;; again, but then they are all nicely in C-c C-c which faster to
  ;; type than e.g. C-c = or C-c !
  :bind (:map rustic-mode-map
         ("C-c C-c Q" . lsp-workspace-shutdown)
         ("C-c C-c q" . lsp-workspace-restart)
         ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
         ("C-c C-c a" . lsp-execute-code-action)
         ("C-c C-c r" . lsp-rename) ;; was: rustic-cargo-rename

         ;; Handling crates in Cargo.toml
         ("C-c C-c A" . rustic-cargo-add)
         ("C-c C-c R" . rustic-cargo-rm)
         ("C-c C-c m" . rustic-cargo-add-missing-dependencies)
         ;; C-c C-c u is already bound to rustic-cargo-upgrade

         ("C-c C-c s" . lsp-rust-analyzer-status)
         ("C-c w a" . lsp-rust-analyzer-status))
)



;;; Package: prog/sh-script

(use-package sh-script
  :defer t

  :custom
  ;; How to indent after &&. Here: align with previous indentation
  (sh-indent-statement-after-and nil)
)



;;; Package: modes/diff-mode

(use-package diff-mode
  :defer t

  :init
  ;; The following let the commits from "git diff >foo.diff" stand out more:
  (defun my-diff-mode-setup ()
    (hi-lock-line-face-buffer "^commit"))

  :hook
  (diff-mode-hook . my-diff-mode-setup)
)




;;; Package: modes/pdf-tools

;; https://github.com/vedang/pdf-tools
;; https://pdftools.wiki/

(use-package pdf-tools
  :disabled t
  :ensure t

  :commands (pdf-view-mode pdf-tools-install)

  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)

  :magic ("%PDF" . pdf-view-mode)

  :custom
  (pdf-view-display-size 'fit-page)

  :config
  ;; The annotations currently throw an error with Emacs 29.0.50
  (remove-hook 'pdf-tools-enabled-modes 'pdf-annot-minor-mode)

  ;; Remove the bounding boxes
  (add-hook 'pdf-view-mode-hook #'pdf-view-auto-slice-minor-mode)

  (pdf-tools-install)

  ;; (add-to-list 'org-file-apps
  ;;              '("\\.pdf\\'" . (lambda (file link)
  ;;                                (org-pdfview-open link)))))
)



;;; Package: modes/ediff

(use-package ediff
  :defer t

  :config
  (setq ediff-split-window-function 'split-window-vertically)

  (add-hook 'ediff-after-quit-hook-internal #'winner-undo)
)




;;; Package: completion/consult

(use-package consult
  :ensure t
  :defer t

  :config
  ;; this forces recentf to load directly, not delayed. So a C-x C-b directly
  ;; after starting emacs will show previously used files
  (recentf-mode)

  ;; Optionally tweak the register preview window. This adds zebra stripes,
  ;; sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :custom
  ;; don't show line numbers for consult-goto-line
  (consult-goto-line-numbers nil)

  ;; no previews
  (consult-preview-key nil)

  :bind (
    ;; C-c bindings (mode-specific-map)
    ("C-c h"   . consult-history)
    ("C-c m"   . consult-mode-command)
    ("C-c k"   . consult-kmacro)
    ;; C-c r   -> vertico-repeat-last
    ;; C-c C-r -> vertico-repeat

    ;; C-x bindings (ctl-x-map)
    ("C-x M-:"  . consult-complex-command)     ;; was: repeat-complex-command)
    ("C-x C-b"  . consult-buffer)              ;; was: list-buffers)
    ("C-x 4 b"  . consult-buffer-other-window) ;; was: switch-to-buffer-other-window)
    ("C-x 5 b"  . consult-buffer-other-frame)  ;; was: switch-to-buffer-other-frame)
    ("C-x r b"  . consult-bookmark)
    ("C-x p b"  . consult-project-buffer)

    ;; Custom M-# bindings for fast register access
    ;; How would I type this on a german keyboard?
    ("M-#"      . consult-register-load)
    ("M-'"      . consult-register-store)
    ("C-M-#"    . consult-register)

    ;; M-g bindings (goto-map)
    ("M-g e"    . consult-compile-error)
    ("M-g g"    . consult-goto-line)           ;; was: goto-line
    ("M-g M-g"  . consult-goto-line)           ;; was: goto-line
    ("M-g o"    . consult-outline)
    ("M-g k"    . consult-mark)
    ("M-g K"    . consult-global-mark)
    ("M-g i"    . consult-imenu)               ;; maybe use M-g o (consult-outline) instead of this
    ("M-g I"    . consult-imenu-multi)

    ;; M-s bindings (search-map)
    ("M-s e"    . consult-isearch-history)
    ("M-s f"    . consult-find)
    ("M-s F"    . consult-locate)
    ;; Searching (mostly)
    ("M-s g"    . consult-git-grep)
    ("M-s i"    . consult-info)
    ("M-s r"    . consult-ripgrep)
    ("M-s l"    . consult-line)                ;; similar to swiper
    ("M-s L"    . consult-line-multi)
    ("M-s k"    . consult-keep-lines)
    ;; bound elsewhere:
    ;; - C-M .   consul-eglot-symbols
    ;; unbound:
    ;; - consult-grep
    ;; - consult-man
    ;; - consult-focus-lines
    ;; - consult-minor-mode-menu
    ;; - consult-notmuch
    ;; - consult-notmuch-tree
    ;; - consult-org-agenda
    ;; - consult-org-heading
    ;; - consult-recent-file
    ;; - consult-yank-from-kill-ring
    ;; - consult-yank-replace

    :map help-map
    ("a" . describe-symbol)

    :map isearch-mode-map
    ("M-e"   . consult-isearch-history)     ;; was: isearch-edit-string
    ("M-s e" . consult-isearch-history)     ;; was: isearch-edit-string
    ("M-s l" . consult-line)                ;; needed by consult-line to detect isearch
    ("M-s L" . consult-line-multi)          ;; needed by consult-line to detect isearch

    ;; access history of minibuffer completions, e.g. do "M-s l" (consult-line) "M-r" (consult-history)
    :map minibuffer-local-map
    ("M-s" . consult-history)               ;; was: next-matching-history-element
    ("M-r" . consult-history)               ;; was: previous-matching-history-element

    ;; :map compilation-mode-map
    ;; ("e" . consult-compile-error)
    ;; :map compilation-minor-mode-map
    ;; ("e" . consult-compile-error)

    ;; Unfortunately, the DEL key is hijacked by vertico, so I cannot unnarrow
    ;; But we don't really need the delete-forward-char function, so we can use
    ;; that key to unnarrow.
    :map consult-narrow-map
    ("<deletechar>" . consult--narrow-delete)     ;; was: delete-forward-char
  )
)



;;; Package: completion/corfu

;; https://github.com/minad/corfu

(use-package corfu
  :ensure t
  :defer
  :commands (corfu-mode)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.5) ;; 0 would interfere with tempel
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
              ;; these two would interfere with tempel, use cursor keys instead
              ;; ("TAB"        . corfu-next)
              ;; ([tab]        . corfu-next)
              ;; And then don't use them either, for symetry reasons
              ;; ("S-TAB"      . corfu-previous)
              ;; ([backtab]    . corfu-previous)

              ("RET"        . corfu-insert)
              ("S-<return>" . corfu-insert))
  :config
  (corfu-history-mode)
)



;; https://github.com/LuigiPiucco/nerd-icons-corfu

;;; Package: completion/orderless

;; https://github.com/oantolin/orderless
;; https://github.com/minad/consult/wiki#minads-orderless-configuration

(use-package orderless
  :ensure t

  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)

  ;; The available styles are listed in completion-styles-alist.
  ;; - +vertico-basic-remote: use basic completion on remote files only
  ;; - orderless:             completion of multiple components, in any order
  ;; - emacs21
  ;; - emacs22
  ;; - basic
  ;; - partial
  ;; - substring
  ;; - flex
  ;; - initials:              completion of acronyms and initialisms
  ;; - shorthand
  (completion-styles '(orderless basic)) ;; basic must be preset, can be at the end
  (completion-category-overrides '((file (styles partial-completion))))
)



;;; Package: completion/marginalia

(use-package marginalia
  :ensure t
  :defer 1

  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind
  (:map minibuffer-local-map
   ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode)
)



;;; Package: completion/tempel

;; https://github.com/minad/tempel#template-syntax

;; Basically, one types the template name, and it appears. And then one uses keymaps from
;; tempel-map:
;;
;; M-<        tempel-beginning
;; C-<down>   tempel-next
;; M-<down>   tempel-next
;; C-<up>     tempel-previous
;; M-<up>     tempel-previous
;; M->        tempel-end
;; M-ESC ESC  tempel-abort
;; M-k        tempel-kill
;; C-g        tempel-done

(use-package tempel
  :ensure t

  :custom
  (tempel-path (locate-user-emacs-file "templates.el"))

  :init
  (global-tempel-abbrev-mode)
  (add-to-list 'completion-at-point-functions #'tempel-expand)

  :bind (
    ("M-+" . tempel-complete)    ;;  completes a template name at point in the buffer and subsequently expands the template
    ("M-+" . tempel-complete)    ;;  completes a template name at point in the buffer and subsequently expands the template
    ("M-*" . tempel-insert)      ;;  selects a template by name and insert it into the current buffer
    :map tempel-map
    ;; This map is active while filling out a tempel template
    ("TAB"     . tempel-next)
    ([tab]     . tempel-next)
    ("S-TAB"   . tempel-previous)
    ([backtab] . tempel-previous)
    ("C-c C-c" . tempel-done)
    )
)



;;; Package: completion/vertico

;; https://github.com/minad/vertico

(use-package vertico
  :ensure t

  :init
  (vertico-mode)

  :config
  ;; Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq! read-extended-command-predicate #'command-completion-default-include-p)
  (setq! vertico-count (/ (* 6 (frame-height)) 10))

  :bind
  (("C-c r"   . vertico-repeat-last) ;; always the last, but can be enter while inside a vertico prompt to recall the last one
   ("C-c C-r" . vertico-repeat)      ;; the last one, can be called with C-u to get a list of previous vertico calls
   :map vertico-map
   ([next] . nil)     ;; was next-history-element
   ([prior] . nil)   ;; was previous-history-element
   ("M-<up>"   . next-history-element)
   ("M-<down>" . previous-history-element))
)



;;; Package: misc/async

;; https://github.com/jwiegley/emacs-async

(use-package async
  :ensure t
  :defer t

  :commands (dired-async-mode)

)



;;; Package: misc/calc

;; https://github.com/kickingvegas/casual-calc

(use-package casual-calc
  :ensure t
  :defer t
  :after calc

  :bind (
    :map calc-mode-map
    ("C-o" . casual-calc-tmenu)
    :map calc-alg-map
    ("C-o" . casual-calc-tmenu)
  )
)



;;; Package: misc/casual

(with-eval-after-load 'casual-lib

  ;; The same as the original ones, just with "q" to quit instead of C-g. The
  ;; reason is that almost any read-only buffer these days (magit, dired,
  ;; ibuffer, describe-buffers, ...) can be quit with "q"

  (transient-define-suffix casual-lib-quit-all ()
    "Casual suffix to call `transient-quit-all'."
    :transient nil
    :if-not #'casual-lib-quit-all-hide-navigation-p
    :key "q"
    :description "Dismiss"
    (interactive)
    (transient-quit-all))

  (transient-define-suffix casual-lib-quit-one ()
    "Casual suffix to call `transient-quit-one'."
    :transient nil
    :if-not #'casual-lib-hide-navigation-p
    :key "q"
    :description #'casual-lib--quit-one-suffix-label
    (interactive)
    (transient-quit-one))
)


;;; Package: misc/dired

(use-package dired
  :defer t

  :custom
  ;; adding -Gh1v --group-directories-first
  ;; -G  no group
  ;; -h  human readable sizes
  ;; -1  one column
  ;; -v  natural sort of versions numbers within text
  (dired-listing-switches "-laGhv --group-directories-first")
  ;; revert when revisiting
  (dired-auto-revert-buffer t)
  ;; work in a Norton Commander like mode if 2 dired panes are open
  (dired-dwim-target t)


  :config
  ;; less confirmations (some of them don't work in :custom)
  (setq! dired-no-confirm t)
  (setq! dired-deletion-confirmer '(lambda (x) t))
  (setq! dired-confirm-shell-command nil)
  (setq! dired-recursive-deletes 'always)

  ;; less details, use '(' inside dired to toggle them
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; from emacs-async
  (require 'async)
  (dired-async-mode 1)

  (when is-mac
    (setq! insert-directory-program "gls" dired-use-ls-dired t)
    (setq! dired-use-ls-dired nil))

  :bind (("C-x C-d" . dired)   ;; was: list-directory (an IMHO entirely useless command)
         :map dired-mode-map
         ("C-x C-p" . wdired-change-to-wdired-mode))
)


(use-package dired-aux
  :defer t
  :after dired

  :custom
  ;; If dwim, Isearch matches file names when initial point position
  ;; is on a file name. Otherwise, it searches the whole buffer
  ;; without restrictions.
  (dired-isearch-filenames 'dwim)
)


(use-package casual-dired
  :ensure t
  :defer t
  :after dired

  :bind (
    :map dired-mode-map
    ("C-o" . casual-dired-tmenu)
    ("s"   . casual-dired-sort-by-tmenu)
  )
)



;;; Package: misc/ibuffer

(use-package ibuffer
  :defer t

  :custom
  ;; see `ibuffer-filtering-alist` for what is possible beside "name" and "mode"
  (ibuffer-saved-filter-groups
   `(("default"
      ("Programming"  (or
                       (derived-mode . prog-mode)
                       (filename . ,(concat "^" (getenv "HOME") "/d/"))
                       ))
      ("Dired" (mode . dired-mode))
      ("Mail"  (or
                (mode . mu4e-view-mode)
                (mode . mu4e-compose-mode)
                (mode . mu4e-headers-mode)))
      ("IRC"   (or
                (mode . erc-mode)
                (mode . circe-server-mode)
                (mode . circe-query-mode)
                (mode . circe-channel-mode)
                ))
      ("Feeds" (or
                (mode . elfeed-show-mode)
                (mode . elfeed-search-mode)
                (name . "elfeed.org$")
                (name . "^\\*elfeed.log\\*$")
                ))
      ("Documentation" (or
                        (name . "^\\*info")
                        (name . "^\\*help")
                        ))
      ("Emacs" (or
                (name . "^\\*")
                ))
      )))

  ;; no empty sections
  (ibuffer-show-empty-filter-groups nil)

  ;; less annoying questions
  (ibuffer-expert t)

  :preface
  (defun my-ibuffer-setup ()
    (ibuffer-switch-to-saved-filter-groups "default")
    (ibuffer-auto-mode 1))

  (add-hook 'ibuffer-mode-hook #'my-ibuffer-setup)

  :bind
  ("C-x b" . ibuffer)                     ;; was: switch-to-buffer)
)



;; https://github.com/kickingvegas/casual-ibuffer

(use-package casual-ibuffer
  :ensure t
  :after (ibuffer)

  :bind (:map ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked) ; optional
         ("}" . ibuffer-forward-next-marked)   ; optional
         ("[" . ibuffer-backward-filter-group) ; optional
         ("]" . ibuffer-forward-filter-group)  ; optional
         ("$" . ibuffer-toggle-filter-group))  ; optional
)



;;; Package: misc/jinx (spellchecking)

;; https://github.com/minad/jinx
;;
;; use: left mouse button for jinx-correct
;;      M-x     jinx-languages
;;      C-M-$   also calls jinx-languages
;;      M-$     correct this word
;;      C-u M-$ correct whole buffer
;;      ~/.config/enchant/enchant.ordering to set back ends
;;              better have it contain this line: *:hunspell,nuspell,aspell
;;
;; For more info on Libenchant, consider:
;; https://abiword.github.io/enchant/ and
;; https://abiword.github.io/enchant/src/enchant.html for more info

(use-package jinx
  :ensure t
  :defer t
  ;; it actually doesn't need hunspell, but since we use hunspell
  ;; setup in libenchant's ordering, we might have installed anyway.
  :if (locate-file "hunspell" exec-path)

  ;; jinx-camel-modes: add maybe python-mode and nim-mode?

  :hook
  (text-mode-hook . jinx-mode)
  (conf-mode-hook . jinx-mode)
  ;; (prog-mode-hook . jinx-mode)

  :bind
  ;; since we don't ue ispell, we can re-assign it's keybinding
  ("M-$"   . jinx-correct)
  ("C-M-$" . jinx-languages)
)



;;; Package: misc/gptel

;; gptel:         https://github.com/karthink/gptel
;; ollama:        https://github.com/ollama/ollama
;; Model library: https://ollama.com/library

(use-package gptel
  :ensure t
  :defer t
  :if (locate-file "ollama" exec-path)

  :commands (gptel gptel-menu)

  :custom
  ;; (gptel-model "mistral:latest")

  (gptel-directives '(
     ;; Removed the "living in Emacs", as some models get back rather snarky with "I don't live in Emacs"
     (default     . "To assist: Be terse. Do not offer unprompted advice or clarifications. Speak in specific,
 topic relevant terminology. Do NOT hedge or qualify. Do not waffle. Speak
 directly and be willing to make creative guesses. Explain your reasoning. if you
 don’t know, say you don’t know.

 Remain neutral on all topics. Be willing to reference less reputable sources for
 ideas.

 Never apologize. Ask questions when unsure.
You are a helpful assistant. Respond concisely.")
     (programming . "You are a careful programmer. Provide code and only code as output without any additional text, prompt or note. Do NOT use markdown backticks (```) to format your response.")
     (cli         . "You are a command line helper. Generate command line commands that do what is requested, without any additional description or explanation. Generate ONLY the command, without any markdown code fences.")
     (emacser     . "You are an Emacs maven. Reply only with the most appropriate built-in Emacs command for the task I specify. Do NOT generate any additional description or explanation.")
     (chat        . "You are a conversation partner. Respond concisely.")
     (explain     . "Explain what this code does to a novice programmer.")
     (english     . "Translate the following to english: ")
     (deutsch     . "Translate the following to german: ")
     (typo        . "Fix typos, grammar and style of the following: ")))

  :config
  (setq! gptel-backend (gptel-make-ollama "Ollama"              ; Any name of your choosing
                         :host "localhost:11434"                ; Where it's running
                         :stream t                              ; Stream responses
                         :models '("llama3.1:latest"))          ; List of models
         gptel-model "llama3.1:latest")

  ;; Scroll automatically, move cursor to next pronpt automatically
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  ;; Weird, this didn't work in a :bind clause
  (bind-key "C-c C-c" #'gptel-send gptel-mode-map)
)



;;; Package: misc/nov (reading epubs)

;; https://depp.brause.cc/nov.el/

(use-package nov
  :ensure t
  :defer t

  :mode (("\\epub\\'" . nov-mode))
)



;;; Package: org/org

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-replace-disputed-keys t)

(use-package org
  :defer t

  :custom
  (org-directory "~/Sync/Data") ;; TODO
  (org-fontify-whole-heading-line t)
  ;;(org-hide-leading-stars t)
  (org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (org-todo-keywords (quote
                      ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       ;; "@" means to add a note (with time) WHEN ENTERING STATE
                       ;; "!" means to record only the time of the state change WHEN LEAVING STATE
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (org-return-follows-link t)

  :config
  (defun my-org-setup ()
    (setq indent-line-function #'indent-relative-first-indent-point))
  (add-hook 'org-mode-hook #'my-org-setup)

  ;; Enable PlantUML
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  (add-hook 'org-mode-hook #'pixel-scroll-precision-mode)

  :bind
  ("C-c l"   . org-store-link)
  ("C-c C-l" . org-insert-link)
)


;;; Package: org/org-agenda

(use-package org-agenda
  :defer t
  :after org

  :custom
  (org-agenda-skip-scheduled-if-done t)  ; Don't show scheduled items in agenda when they are done
  (org-agenda-skip-deadline-if-done t)   ; Don't show deadlines when the corresponding item is done
  (org-agenda-compact-blocks t)          ; Make the block agenda more compact
  (org-agenda-start-with-log-mode 'clockcheck)
  (org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

  :bind
  ("C-c a" . org-agenda)
)




;;; Package: org/org-capture

(use-package org-capture
  :defer t

  :custom
  (org-capture-templates
          '(("e" "Einkaufen" entry (file+headline "~/Sync/Data/TODO.org" "Einkaufen Neu")
             "* TODO %?\n")
            ("t" "TODO" entry (file+headline "~/Sync/Data/TODO.org" "Aufgaben")
             "* TODO %?\n")))

  :bind
  ("C-c c" . org-capture)
)



;;; Package: org/org-persist

(use-package org-persist
  :ensure nil  ;; comes with org-mode
  :defer t

  :custom
  (org-persist-directory (locate-user-emacs-file "var/org-persist/"))
)



;;; Package: org/org-tempo

;; https://orgmode.org/manual/Structure-Templates.html

;; a    #+BEGIN_EXPORT ascii  #+END_EXPORT
;; c    #+BEGIN_CENTER        #+END_CENTER
;; C    #+BEGIN_COMMENT       #+END_COMMENT
;; e    #+BEGIN_EXAMPLE       #+END_EXAMPLE
;; E    #+BEGIN_EXPORT        #+END_EXPORT
;; h    #+BEGIN_EXPORT html   #+END_EXPORT
;; l    #+BEGIN_EXPORT latex  #+END_EXPORT
;; q    #+BEGIN_QUOTE         #+END_QUOTE
;; s    #+BEGIN_SRC           #+END_SRC
;; v    #+BEGIN_VERSE         #+END_VERSE
;;
;; Type:
;;
;; <e TAB
;; <cc TAB

(use-package org-tempo
  :after org

  :config
  (add-to-list 'org-structure-template-alist '("cc" . "SRC C"))
  (add-to-list 'org-structure-template-alist '("cp" . "SRC cpp"))
  (add-to-list 'org-structure-template-alist '("el" . "SRC emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "SRC js"))
  (add-to-list 'org-structure-template-alist '("mk" . "SRC makefile"))
  (add-to-list 'org-structure-template-alist '("py" . "SRC python"))
  (add-to-list 'org-structure-template-alist '("sh" . "SRC bash"))
)



;;; Package: org/ox

(use-package ox
  :defer t

  :custom
  ;; The following make some +OPTIONS permanent:
  ;; #+OPTIONS ':t
  (org-export-with-smart-quotes t)
  ;; #+OPTIONS num:nil
  (org-export-with-section-numbers nil)
  ;; #+OPTIONS stat:t
  ;; (setq org-export-with-statistics-cookies nil)
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a headline
  (org-export-with-toc nil)
  ;; #+OPTIONS ^:{}
  (org-export-with-sub-superscripts nil)

  ;; This exports broken links as [BROKEN LINK %s], so we can actually
  ;; find them. The default value nil just aborts the export process
  ;; with an error message "Unable to resolve link: nil". This doesn't
  ;; give any hint on which line the broken link actually is :-(
  (org-export-with-broken-links 'mark)

  (org-export-time-stamp-file nil)
)


;;; Package: org/ox-html

(use-package ox-html
  :defer t

  :custom
  (org-html-postamble-format '(("en" "<p class=\"author\">Author: %a</p><p class=\"creator\">Created with %c</p>")))
  (org-html-validation-link nil)
  (org-html-postamble nil)
  (org-html-style-default "<style type=\"text/css\">\n <!--/*--><![CDATA[/*><!--*/\n  body { text-align: center; font-family: \"Aria\", sans-serif; }\n  #content { margin: 0 auto; width: 860px; text-align: left; }\n  #text-table-of-contents > ul > li { margin-top: 1em; }\n  .title  { text-align: center; }\n  .todo   { color: red; }\n  .done   { color: green; }\n  .WAIT, .DELE   { color: blue; }\n  .done   { color: green; }\n  .tag    { background-color: #eee; font-family: monospace;\n            padding: 2px; font-size: 80%; font-weight: normal; }\n  .timestamp { color: #bebebe; }\n  .timestamp-kwd { color: #5f9ea0; }\n  .right  { margin-left: auto; margin-right: 0px;  text-align: right; }\n  .left   { margin-left: 0px;  margin-right: auto; text-align: left; }\n  .center { margin-left: auto; margin-right: auto; text-align: center; }\n  .underline { text-decoration: underline; }\n  #postamble p, #preamble p { font-size: 90%; margin: .2em; }\n  p.verse { margin-left: 3%; }\n  pre {\n    border: 1px solid #ccc;\n    box-shadow: 3px 3px 3px #eee;\n    padding: 8pt;\n    font-family: monospace;\n    overflow: auto;\n    margin: 1em 0;\n  }\n  pre.src {\n    position: relative;\n    overflow: visible;\n    padding-top: 8pt;\n  }\n  pre.src:before {\n    display: none;\n    position: absolute;\n    background-color: white;\n    top: -10px;\n    right: 10px;\n    padding: 3px;\n    border: 1px solid black;\n  }\n  pre.src:hover:before { display: inline;}\n  pre.src-sh:before    { content: 'sh'; }\n  pre.src-bash:before  { content: 'sh'; }\n  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }\n  pre.src-R:before     { content: 'R'; }\n  pre.src-perl:before  { content: 'Perl'; }\n  pre.src-java:before  { content: 'Java'; }\n  pre.src-sql:before   { content: 'SQL'; }\n\n  table { border-collapse:collapse; }\n  caption.t-above { caption-side: top; }\n  caption.t-bottom { caption-side: bottom; }\n  td, th { vertical-align:top;  }\n  th.right  { text-align: center;  }\n  th.left   { text-align: center;   }\n  th.center { text-align: center; }\n  td.right  { text-align: right;  }\n  td.left   { text-align: left;   }\n  td.center { text-align: center; }\n  dt { font-weight: bold; }\n  .footpara:nth-child(2) { display: inline; }\n  .footpara { display: block; }\n  .footdef  { margin-bottom: 1em; }\n  .figure { padding: 1em; }\n  .figure p { text-align: center; }\n  .inlinetask {\n    padding: 10px;\n    border: 2px solid gray;\n    margin: 10px;\n    background: #ffffcc;\n  }\n  #org-div-home-and-up\n   { text-align: right; font-size: 70%; white-space: nowrap; }\n  textarea { overflow-x: auto; }\n  .linenr { font-size: smaller }\n  .code-highlighted { background-color: #ffff00; }\n  .org-info-js_info-navigation { border-style: none; }\n  #org-info-js_console-label\n    { font-size: 10px; font-weight: bold; white-space: nowrap; }\n  .org-info-js_search-highlight\n    { background-color: #ffff00; color: #000000; font-weight: bold; }\n  .ulClassNameOrID > li {}\n  /*]]>*/-->\n</style>")
  (org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6"))
  (org-html-postamble t)
)




;;; Package: org/ox-hugo

;; https://ox-hugo.scripter.co/

(use-package ox-hugo
  :ensure t
  :defer t
  :commands (org-hugo-export-wim-to-md org2hugo)

  :custom
  (org-hugo-base-dir "~/src/hpg/")
)

(with-eval-after-load 'org
  (defun org2hugo-ensure-properties ()
    (let ((mandatory `(("EXPORT_HUGO_SECTION" . "en")
                       ("EXPORT_FILE_NAME" . "filename")
                       ("EXPORT_DATE" . ,(format-time-string "%Y-%m-%d" (org-current-time)))))
          (optional '(("EXPORT_HUGO_TAGS" . "")
                      ("EXPORT_HUGO_CATEGORIES" . "")))
          (first))

      ;; loop through mandatory entries, enter them into property if not there, note first missing one
      (dolist (elem mandatory)
        (unless (org-entry-get nil (car elem) t)
          (org-entry-put nil (car elem) (cdr elem))
          (unless first
            (setq first (car elem)))))
      ;; loop through optional entries, enter them into property if not there
      (dolist (elem optional)
        (unless (org-entry-get nil (car elem) t)
          (org-entry-put nil (car elem) (cdr elem))))
      ;; move behind first mandatory entry
      (when first
        (goto-char (org-entry-beginning-position))
        ;; The following opens the drawer
        (forward-line 1)
        (beginning-of-line 1)
        (when (looking-at org-drawer-regexp)
          (org-flag-drawer nil))
        ;; And now move to the drawer property
        (search-forward (concat ":" first ":"))
        (end-of-line))
      ;; return first non-filled entry
      first))

  (defun org2hugo ()
    (interactive)
    (progn ;save-window-excursion
      (save-excursion
        (unless (org2hugo-ensure-properties)
          (let ((title (org-entry-get nil "TITLE" t))
                (file "/tmp/blog.md") ;; TODO
                (blog
                 ))

            ;; Create block
            (end-of-line)
            (search-backward ":EXPORT_HUGO_SECTION:")
            (org-hugo-export-wim-to-md)
            )))))

  (bind-key "C-c h" #'org2hugo org-mode-map)
)



;;; Packages: comm/0x0 (pastebin service)

;; https://gitlab.com/willvaughn/emacs-0x0

(use-package 0x0
  :ensure t
  :defer t

  :commands (0x0-upload-file 0x0-upload-text)
)



;;; Packages: comm/mastodon

;; https://codeberg.org/martianh/mastodon.el
;;
;; TODO: write transient of what used to be the old hydra

(use-package mastodon
  :ensure t
  :defer t
  :commands (mastodon)

  :custom
  (mastodon-active-user user-mail-address)
  (mastodon-instance-url "https://emacs.ch")

  :config
  (setq! mastodon-client--token-file (locate-user-emacs-file "var/mastodon.plstore"))
  (mastodon-discover)

  (defun my-mastoron-more ()
    "Can now load without the PgDn function"
    (interactive)
    (mastodon-tl--more))

  (transient-define-prefix casual-mastodon-tmenu ()
    "Transient menu for Mastodon."
    [["Timelines"
      ("H" "home"            mastodon-tl--get-home-timeline)
      ("L" "local"           mastodon-tl--get-local-timeline)
      ("F" "federated"       mastodon-tl--get-federated-timeline)
      ("K" "bookmarks"       mastodon-profile--view-bookmarks)
      ("V" "favorites"       mastodon-profile--view-favourites)
      ("'" "followed tags"   mastodon-tl--followed-tags-timeline)
      ("@" "mentions"        mastodon-notifications--get-mentions)
      ("N" "notifications"   mastodon-notifications-get)
      ("\\" "of remote host" mastodon-tl--get-remote-local-timeline)]

     ;; u                    mastodon-tl--update

     ["Search"
      ("s" "search"          mastodon-search--query)
      ("#" "tagged"          mastodon-tl--get-tag-timeline)
      ("\"" "followed tags"  mastodon-tl--list-followed-tags)
      ("I" "filter"          mastodon-views--view-filters)
      ("X" "lists"           mastodon-views--view-lists)]

     ["Toots"
      ("n" "next"            mastodon-tl--goto-next-item :transient t)
      ("p" "prev"            mastodon-tl--goto-prev-item :transient t)
      ("c" "spoiler"         mastodon-tl--toggle-spoiler-text-in-toot :transient t)
      ("T" "thread"          mastodon-tl--thread)
      ("b" "(un)boost"       mastodon-toot--toggle-boost :transient t)
      ("f" "(un)fav"         mastodon-toot--toggle-favourite :transient t)
      ("i" "(un)pin"         mastodon-toot--pin-toot-toggle :transient t)
      ("k" "(un)bookmark"    mastodon-toot--toggle-bookmark :transient t)
      ("v" "vote"            mastodon-tl--poll-vote)]

     ;; Z                    mastodon-tl--report-to-mods
     ;; o                    mastodon-toot--open-toot-url

     ["Own Toots"
      ("r" "replay"          mastodon-toot--reply)
      ("t" "write"           mastodon-toot)
      ("e" "edit"            mastodon-toot--edit-toot-at-point)
      ("d" "delete"          mastodon-toot--delete-toot)
      ("D" "del & redraft"   mastodon-toot--delete-and-redraft-toot)
      ("E" "show edits"      mastodon-toot--view-toot-edits)]

     ;; S                    mastodon-views--view-scheduled-toots

     ["Users"
      ("W" "follow"          mastodon-tl--follow-user)
      ("R" "follow req"      mastodon-views--view-follow-requests)
      ("G" "suggestions"     mastodon-views--view-follow-suggestions)
      ("M" "mute user"       mastodon-tl--mute-user)
      ("B" "block user"      mastodon-tl--block-user)
      ("m" "message user"    mastodon-tl--dm-user)
      ""
      ("," "favouriters"     mastodon-toot--list-toot-favouriters)
      ("." "boosters"        mastodon-toot--list-toot-boosters)]]

     ;; S-RET                mastodon-tl--unmute-user
     ;; C-S-b                mastodon-tl--unblock-user

    [["Profiles"
      ("A" "author"          mastodon-profile--get-toot-author)
      ("P" "any user"        mastodon-profile--show-user)
      ("O" "own"             mastodon-profile--my-profile)
      ("U" "update own"      mastodon-profile--update-user-profile-note)]

     ["Misc"
      ("C" "copy URL"        mastodon-toot--copy-toot-url)
      ("?" "help"            describe-mode)
      ("q" "quit"            transient-quit-one)
      ]])

     ;; These won't go into the transient ... except when I find out
     ;; if/how these functions are actually useful for me
     ;;
     ;; TAB             mastodon-tl--next-tab-item
     ;; SPC             mastodon-tl--more
     ;; !               mastodon-tl--fold-post-toggle
     ;; /               mastodon-switch-to-buffer
     ;; ;               mastodon-views--view-instance-description
     ;; <               beginning-of-buffer
     ;; >               end-of-buffer
     ;; Q               mastodon-kill-window
     ;; h               describe-mode
     ;; l               recenter-top-bottom
     ;; z               bury-buffer
     ;; DEL             scroll-down-command
     ;; S-TAB           mastodon-tl--previous-tab-item
     ;; C-S-w           mastodon-tl--unfollow-user
     ;; S-SPC           scroll-down-command
     ;; <backtab>       mastodon-tl--previous-tab-item
     ;; C-M-i           mastodon-tl--previous-tab-item
     ;; C-M-q           mastodon-kill-all-buffers
     ;; M-n             mastodon-tl--next-tab-item
     ;; M-p             mastodon-tl--previous-tab-item

  :bind (:map mastodon-mode-map
         ("C-o" . casual-mastodon-tmenu)
         ("SPC" . my-mastoron-more))
)


(use-package persist
  :ensure (:host github :repo "emacsmirror/persist" :branch "master")
  :defer t

  :config
  (setq! persist--directory-location (locate-user-emacs-file "var"))
)



;;; Misc

;; switch-to-buffer runs pop-to-buffer-same-window instead
(setq switch-to-buffer-obey-display-actions t)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(setq whitespace-line-column nil)  ; whitespace-mode

;; I reduced the default value of 9 to simplify the font-lock keyword,
;; aiming to improve performance. This package helps differentiate
;; nested delimiter pairs, particularly in languages with heavy use of
;; parentheses.
(setq rainbow-delimiters-max-face-count 5)

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)


;;(setq truncate-string-ellipsis "…")
