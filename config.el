;; $DOOMDIR/config.el -*- lexical-binding: t; -*-



;;; Error handling

;;(toggle-debug-on-error)
(setq debugger-stack-frame-as-list t)




;;; Personal information

(setq user-full-name "Holger Schurig")
(setq user-mail-address "holgerschurig@gmail.com")
(defvar my-freenode-password nil "Password for the IRC network freenode.net")
(require 'private (expand-file-name "private.el" doom-private-dir) 'noerror)




;;; Commands

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'update-region 'disabled nil)
(put 'customize-group 'disabled nil)




;;; Misc

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

(defalias 'sudo-edit 'doom/sudo-this-file
   "Edit currently visited file as root.")

(setenv "PATH" "/home/schurig/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")


;; https://blog.lambda.cx/posts/emacs-align-columns/
(defun my-align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))


;;; Misc keybindings
;; This is like the start of modules/config/default/+emacs-bindings.el:

;; Allow scrolling up and down
(global-set-key (kbd "C-S-<up>")   (kbd "C-u 1 M-v"))
(global-set-key (kbd "C-S-<down>") (kbd "C-u 1 C-v"))



;;; Package: core/align

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))


;;; Package: core/auth-sources

(after! auth-source
  (setq auth-sources (list (concat doom-etc-dir "authinfo.gpg")
                           "~/.authinfo.gpg"
                           "~/.authinfo")))



;;; Package: core/bookmark

;; taken from https://protesilaos.com/codelog/2023-06-28-emacs-mark-register-basics/

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




;;; Package: core/browse-url

;; see https://www.emacswiki.org/emacs/BrowseUrl#h5o-7
(after! browse-url
  (setq browse-url-browser-function 'browse-url-firefox
        browse-url-new-window-flag  t
        browse-url-firefox-new-window-is-tab t))




;;; Package: core/buffers

(setq-default tab-width 4)

;; Don't asks you if you want to kill a buffer with a live process
;; attached to it:
(remove-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function)

;; Make the messages be displayed full-screen
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Messages*" eos)
               (display-buffer-reuse-window display-buffer-same-window)
               (reusable-frames . visible))
             )

(defun my-kill-without-query ()
  "Mark a buffer not modified, to make it killable without a
 query. Use with kill-buffer-query-functions."
  (not-modified) t)

(setq kill-buffer-query-functions '(my-kill-without-query))



;; revert buffer with one keystroke
(defun revert-buffer-no-confirm ()
  "Revert buffer, no questions asked"
  (interactive)
  (revert-buffer nil t t))
(map! "<f3>" #'revert-buffer-no-confirm)


(defun my-zoom-next-buffer2 ()
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
        (my-zoom-next-buffer2)
        (other-window -1))
    (delete-other-windows)))

(map! "<f5>" #'my-explode-window)


;; If there is only one window displayed, swap it with previous buffer.
;; If there are two windows displayed, act like "C-x o".
(defun my-switch-to-buffer ()
  "If there is only one window displayed, swap it with previous buffer.
If there are two windows displayed, act like \"C-x o\"."
  (interactive)
  (if (one-window-p t)
      (mode-line-other-buffer)
    (other-window -1)))

(map! "<f6>" #'my-switch-to-buffer)




;;; Package: core/bytecomp

(setq byte-compile-warnings '(callargs
                              constants
                              ;;docstrings
                              docstrings-non-ascii-quotes
                              free-vars
                              interactive-only
                              lexical
                              lexical-dynamic
                              make-local
                              mapcar
                              noruntime
                              not-unused
                              obsolete
                              redefine
                              suspicious
                              unresolved))


;;; Package: core/calc

(after! calc
  (setq calc-angle-mode 'rad  ; radians are radians, 0..2*pi
        calc-symbolic-mode t))



;;; Package: core/calender

(setopt calendar-date-style 'iso
        calendar-mark-holidays-flag t
        calendar-week-start-day 1)



;;; Package: core/cus-edit

(after! cus-edit
  ;; keep lisp names in the custom buffers, don't capitalize.
  (setq custom-unlispify-tag-names nil)
  ;; kill old buffers.
  (setq custom-buffer-done-kill t))




;;; Package: core/dabbrev

(use-package! dabbrev
  :defer t

  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
)



;;; Package: core/dictionary

(use-package! dictionary
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

  :general
  ("C-c d" #'dictionary-lookup-definition)
)



;;; Package: core/dnd

(after! dnd
  (setq dnd-indicate-insertion-point t
        dnd-scroll-margin 2)
)



;;; Package: core/eshell

(use-package eshell
  :commands eshell

  :custom
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-prefer-lisp-functions t)

  :config
  (eshell/addpath "~/.local/bin")
)

(use-package em-alias
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

  (map! :map eshell-mode-map
        "C-l" #'my-eshell-clear)

  ;; (add-to-list 'eshell-modules-list 'eshell-smart)
)

(use-package em-banner
  :after eshell

  :custom
  (eshell-banner-message "")
)

(use-package em-hist
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

(use-package! executable
  :hook
  ;; Make files with shebang ("#!" at start) executable when we save them
  (after-save . executable-make-buffer-file-executable-if-script-p)
)



;;; Package: core/files

(use-package! files
  :custom
  (confirm-kill-emacs nil)
  (confirm-kill-processes nil)

  ;; Look for sibling files (this is a list of (MATCH EXPANSION) entries)
  (find-sibling-rules '(("\\([^/]+\\)\\.c\\'" "\\1.h")
                        ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
                        ("\\([^/]+\\)\\.h\\'" "\\1.c")
                        ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp")))

  ;; Preserve hard links to the file you´re editing (this is
  ;; especially important if you edit system files)
  (backup-by-copying-when-linked t)

  ;; Just never create backup files at all
  ;; (make-backup-files nil)

  ;; Alternatively put backup files into their own directory
  (backup-directory-alist (list (cons "." (locate-user-emacs-file "tmp/bak/"))))

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

  :general
  ("<f2>" #'save-buffer)
  ("M-<f6>" #'find-sibling-file)
)



;;; Package: core/help

;; makes things like  M-x describe bindings  pop up full screen
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Help*" eos)
               (display-buffer-reuse-window display-buffer-same-window)
               (reusable-frames . visible))
             )



;;; Package: core/hippie-exp (disabled)

;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(use-package! hippie-exp
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
  :general
  ("M-/" #'hippie-expand)
)



;;; Package: core/ibuffer

(after! ibuffer

  ;; see `ibuffer-filtering-alist` for what is possible beside "name" and "mode"
  (setq ibuffer-saved-filter-groups
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
  (setq ibuffer-show-empty-filter-groups nil)

  ;; less annoying questions
  (setq ibuffer-expert t)

  :preface
  (defun my-ibuffer-setup ()
    (ibuffer-switch-to-saved-filter-groups "default")
    (ibuffer-auto-mode 1))

  (add-hook 'ibuffer-mode-hook #'my-ibuffer-setup)

  (map! "C-x b" #'ibuffer)
)



;;; Package: core/isearch

(after! isearch
  ;; Scrolling (including C-s) while searching:
  (setq isearch-allow-scroll t)

  ;; Show number of matches
  (setq isearch-lazy-count t)

  ;; Do less flickering be removing highlighting immediately
  (setq lazy-highlight-initial-delay 0)

  ;; Let isearch wrap
  (setq isearch-wrap-function
        (lambda ()
          (if isearch-forward
              (goto-char (window-start))
            (goto-char (window-end))))
        isearch-wrap-pause 'no)
)






;;; Package: core/recentf

(after! recentf
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
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 300
        recentf-max-menu-items 20))



;;; Package: core/register

(after! register
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format))



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



;;; Package: core/message

(after! message
  (setq message-send-mail-function #'message-send-mail-with-sendmail
        message-citation-line-format "On %b, %Y-%m-%d %H:%S, %N wrote ..."
        message-citation-line-function 'message-insert-formatted-citation-line))



;;; Package: core/misc

(map! "C-z" #'zap-up-to-char)



;;; Package: core/minibuf.c

(setq history-length 1000)

(setq history-delete-duplicates t)

(setq resize-mini-windows t)



;;; Package: core/mouse

(setq mouse-drag-mode-line-buffer t
      mouse-drag-and-drop-region-cross-program t
      mouse-drag-and-drop-region-scroll-margin t
      mouse-drag-copy-region 'non-empty)



;;; Package: core/mule-util

(after! mule-util
  (setq truncate-string-ellipsis "…"))



;;; Package: core/paren

(setq show-paren-context-when-offscreen 'overlay)



;;; Package: core/pixel-scroll

(use-package! pixel-scroll
  :defer t

  :hook
  (org-mode-hook . pixel-scroll-precision-mode)
)



;;; Package: core/proced

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Proced*" eos)
               (display-buffer-reuse-window display-buffer-same-window)
               (reusable-frames . visible))
             )



;;; Package: core/project

;; Switch projects:  C-x p p
;; Remove project:   M-x project-forget-project
;; otherwise, see (describe-map project-prefix-map)



;;; Package: core/rectangle

;; Turn the rectangle-mark-mode on with C-x <SPACE>

(use-package! rect
  :general
  (rectangle-mark-mode-map "t" #'string-rectangle)                   ;; replace rectange with string
  (rectangle-mark-mode-map "o" #'open-rectangle)                     ;; blank rectange, shifting text to right
  (rectangle-mark-mode-map "c" #'clear-rectangle)                    ;; replace with blanks
  (rectangle-mark-mode-map "k" #'kill-rectangle)                     ;; delete rectangle and save to kill-ring
  (rectangle-mark-mode-map "d" #'delete-rectangle)                   ;; delete rectangle, don't save
  (rectangle-mark-mode-map "y" #'yank-rectangle)                     ;; yank last killed rectange to upper left
  (rectangle-mark-mode-map "w" #'copy-rectangle-as-kill)             ;; save rectange to kill-ring
  (rectangle-mark-mode-map "n" #'rectangle-number-lines)
  (rectangle-mark-mode-map "x" #'rectangle-exchange-point-and-mark)
  (rectangle-mark-mode-map "s" #'string-rectangle)                   ;; replace rectange with string
  ;; already defined:
  ;; "n"       rectangle-number-lines
  ;; C-x C-x   cycle between the four corners
)



;;; Package: core/savehist

;; The kill-ring can blow up the .local/cache/savehist to 8 megabytes or more

(remove-hook 'savehist-additional-variables 'kill-ring)



;;; Package: core/sendmail

(after! sendmail
  (setq sendmail-program "msmtp"
        send-mail-function #'smtpmail-send-it))



;;; Package: core/simple

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

;; React faster to keystrokes
(setq idle-update-delay 0.35)

;; Be silent when killing text from read only buffer:
(setq kill-read-only-ok t)

;; Read quoted chars with radix 16 --- octal is sooooo 1960
(setq read-quoted-char-radix 16)

;; Deleting past a tab normally changes tab into spaces. Don't do
;; that, kill the tab instead.
(setq backward-delete-char-untabify-method nil)

(setq kill-ring-max 500)

;; Don't type C-u C-SPC C-u C-SPC to pop 2 marks, now you can do C-u C-SPC C-SPC
(setq set-mark-command-repeat-pop t)

;; If we copy an unmarked region, don't move the cursor
;; (setq copy-region-blink-predicate #'region-indistinguishable-p)

(defun my-next-error (&optional arg)
  (interactive)
  (let ((p))
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


(map! "C-x I" #'insert-buffer

      "M-SPC" #'cycle-spacing   ;; TODO was: just-one-space

      "M-c" #'capitalize-dwim
      "M-l" #'downcase-dwim
      "M-u" #'upcase-dwim

      "M-g s" #'scratch-buffer

      "M-o" #'delete-blank-lines  ; opposite of C-o

      ;; Error navigation
      "<f8>"    #'my-next-error
      "S-<f8>"  #'my-previous-error)


;; From https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode and
;; https://www.youtube.com/watch?v=ppbcLsc-F20
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)



;;; Package: core/sh

(eval-after-load 'sh
  (setq sh-indent-statement-after-and nil)
)



;;; Package: core/shr

(after! shr
  (setq shr-color-visible-luminance-min 80
        shr-bullet "• "
        shr-folding-mode t))


;;; Package: core/timer-list

;; Enable this command
(put 'list-timers 'disabled nil)



;;; Package: core/tramp-docker

(after! tramp
  (setq tramp-docker-program "podman")
)



;;; Package: core/treesit (disabled)

;; https://archive.casouri.cc/note/2023/tree-sitter-starter-guide/index.html
;; https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html

(use-package treesit
  :disabled t
  :init
  (setq treesit-language-source-alist
        ;; The compiled grammars are in ~/.emacs.d/.local/cache/tree-sitter/
        '(;; Format:    (URL REVISION SOURCE-DIR CC C++))
          ;; see also: https://tree-sitter.github.io/tree-sitter/#parsers
          ;; (agda       . ("https://github.com/tree-sitter/tree-sitter-agda")) ;; version mismatch
          (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (c-sharp    . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (clojure    . ("https://github.com/sogaiu/tree-sitter-clojure"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (d          . ("https://github.com/CyberShadow/tree-sitter-d"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (elixier    . ("https://github.com/elixir-lang/tree-sitter-elixir"))
          (elm        . ("https://github.com/razzeee/tree-sitter-elm"))
          (erlang     . ("https://github.com/WhatsApp/tree-sitter-erlang"))
          (fennel     . ("https://github.com/TravonteD/tree-sitter-fennel"))
          ;; (fluent     . ("https://github.com/tree-sitter/tree-sitter-fluent"))  ;; version mismatch
          (glsl       . ("https://github.com/theHamsta/tree-sitter-glsl"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (haskell    . ("https://github.com/tree-sitter/tree-sitter-haskell"))
          (hcl        . ("https://github.com/MichaHoffmann/tree-sitter-hcl"))
          (heex       . ("https://github.com/phoenixframework/tree-sitter-heex"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (janet-simple . ("https://github.com/sogaiu/tree-sitter-janet-simple"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (jsdoc      . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (julia      . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown"))
          (nix        . ("https://github.com/cstrahan/tree-sitter-nix"))
          (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
          (org        . ("https://github.com/milisims/tree-sitter-org"))
          (perl       . ("https://github.com/ganezdragon/tree-sitter-perl"))
          (pgn        . ("https://github.com/rolandwalker/tree-sitter-pgn"))
          (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
          (prisma     . ("https://github.com/LumaKernel/tree-sitter-prisma"))
          (proto      . ("https://github.com/mitchellh/tree-sitter-proto"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (ql         . ("https://github.com/tree-sitter/tree-sitter-ql"))
          (r          . ("https://github.com/r-lib/tree-sitter-r"))
          (regex      . ("https://github.com/tree-sitter/tree-sitter-regex"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (scala      . ("https://github.com/tree-sitter/tree-sitter-scala"))
          (sqlite     . ("https://github.com/dhcmrlchtdj/tree-sitter-sqlite"))
          (swift      . ("https://github.com/tree-sitter/tree-sitter-swift"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (verilog    . ("https://github.com/tree-sitter/tree-sitter-verilog"))
          (vhdl       . ("https://github.com/alemuller/tree-sitter-vhdl"))
          (wgsl       . ("https://github.com/mehmetoguzderin/tree-sitter-wgsl"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))
          ))

  (setq major-mode-remap-alist
        '(;; Other modes in current Emacs GIT not yet utilized:
          ;; see: rg -l -- -ts-mode emacs.git/lisp/ | sort
          ;; (c++-mode . c++-ts-mode)   ;; c++ts-mode cannot handle Qt/C++
          (c-mode . c-or-c++-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode) ;; go-mod-ts-mode?
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (sh-mode . bash-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (yaml-mode . yaml-ts-mode)
          ))

  :config
  ;; https://www.nathanfurnal.xyz/posts/building-tree-sitter-langs-emacs/
  (defun my-treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75))))
)


;;; Package: core/vc

(after! vc-hooks
  ;; Remove most back-ends from vc-mode
  (setq vc-handled-backends '(Git))
  ;; Disable version control when using tramp
  (setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)))



;;; Package: core/url-vars

(after! url-vars
  (setq url-privacy-level '(email agent cookies lastloc)))



;;; Package: core/window

(setq ;;auto-window-vscroll nil
      fast-but-imprecise-scrolling nil)

(map! "C-x k" #'kill-buffer-and-window)

(map! "S-<f5>" #'previous-buffer
      "S-<f6>" #'next-buffer)



;;; Package: core/xdisp

(setq ;; hscroll-step 1
      hscroll-margin 0
      scroll-step 1
      ;; scroll-margin 0
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)



;;; Package: core/xref

(use-package! xref
  :custom
  (xref-file-name-display 'project-relative)                   ;; was abs
  (xref-search-program 'ripgrep)                               ;; was grep
  (xref-show-xrefs-function #'consult-xref)                    ;; was xref--show-xref-buffer
  (xref-show-definitions-function #'consult-xref)              ;; was xref-definition-xref-buffer

  :general
  ("M-s u" #'xref-find-references)                             ;; like "search usage"
  (xref--xref-buffer-mode-map "RET" #'xref-quit-and-goto-xref) ;; was xref-goto-xref
  (xref--xref-buffer-mode-map "TAB" #'xref-goto-xref)          ;; was xref-quit-and-goto-xref

  :config
  (advice-remove #'xref-push-marker-stack #'doom-set-jump-a)
)



;;; Package: gui/display-line-numbers

;; This removes the display of the line numbers

(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)




;;; Package: gui/minibuffer

(after! minibuffer
  ;; don't show "*Completions*" buffer
  (setq completion-auto-help nil)

  ;; Shorter default format formatting
  (setq minibuffer-default-prompt-format " [%s]")

  ;; Don't insert current directory into minubuffer
  (setq insert-default-directory nil)

  (minibuffer-depth-indicate-mode 1)

  ;; Allow to type space chars in minibuffer input (for `timeclock-in',
  ;; for example).
  (define-key minibuffer-local-completion-map " " nil t)
  (define-key minibuffer-local-must-match-map " " nil t))




;;; Package: gui/current-window-only

(after! current-window-only
  (current-window-only-mode)

  ;; The advice break delete-other-window and also my F5 `my-explode-window`, so
  ;; let's undo this:
  (advice-remove
   'delete-other-windows
   #'current-window-only--delete-other-windows)
)




;;; Package: theme/font-core

(setq doom-font (font-spec :family "JetBrainsMono" :size 13) ;; :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13)
      doom-symbol-font (font-spec :family "JuliaMono")
      doom-big-font (font-spec :family "JetBrainsMono" :size 24))

;; I got the idea from here:
;; http://amitp.blogspot.de/2013/05/emacs-highlight-active-buffer.html but I
;; disabled it for now since my Wayland compositor "hyprland" can dim inactive
;; windows.

;; (defun highlight-focus:app-focus-in ()
;;   (global-font-lock-mode 1))

;; (defun highlight-focus:app-focus-out ()
;;   (global-font-lock-mode -1))

;; (add-hook 'focus-in-hook  #'highlight-focus:app-focus-in)
;; (add-hook 'focus-out-hook #'highlight-focus:app-focus-out)




;;; Package: theme/font-lock

(after! font-lock
  (setq font-lock-maximum-decoration 2))  ;; was t



;;; Package: theme/modus-themes
;; https://protesilaos.com/emacs/modus-themes
;; https://gitlab.com/protesilaos/modus-themes

;; Note: don't set doom-theme to modus-vivendi. Because if you do this, then
;; somehow comments are rendered in italic, if you customize
;; modus-themes-italic-constructs or not. Unfortunately the backslash \ just
;; looks thrash then.

(use-package! modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs nil)
  (modus-themes-paren-match '(intense))
  (modus-themes-mode-line '(borderless accented))
  (modus-themes-region '(bg-only))
  (modus-themes-syntax '(alt-syntax))
  ;;(modus-themes-headings '((t . (rainbow))) )
  (modus-themes-common-palette-overrides '((fg-heading-0 blue-cooler)
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


  :config
  (load-theme 'modus-vivendi t)
)



;;; Package: edit/autorevert

(after! autorevert
  (setq global-auto-revert-non-file-buffers t
        auto-revert-interval 1
        auto-revert-verbose nil))



;;; Package: edit/avy

;; https://github.com/abo-abo/avy
;; https://karthinks.com/software/avy-can-do-anything/
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el
;; https://melpa.org/#/?q=avy

;; Jump to char(s): M-j <quickly enter char(s)> <wait 0.2s> <enter highligher>
;; Action:          M-j <quickly enter char(s)> <wait 0.2s> <enter action> <enter highligher>

(use-package! avy
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)

  :config
  (setq avy-timeout-seconds 0.20)

  ;; Use more letters for targets (i.E. not just the home row
  ;; HINT: Only use keyss here that aren't in avy-dispatch-alist
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?o
                   ?v ?b ?n ?, ?, ?. ?-))

  ;; ORIGINAL:
  ;; HINT: Only use keys here that aren't in avy-keys
  ;; HINT: use ?\C-m or ?M-m for modifier keys
  (setq avy-dispatch-alist'((?c . avy-action-mark-to-char)
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

  :general
  ("M-j"        '(avy-goto-char-timer       :wk "Avy goto char timer"))
  (isearch-mode-map "M-j" #'avy-isearch)
)


;;; Package: edit/better-jumper

;; I disabled it in my packages.el, but some other parts of Doom think its enabled.

(defun doom-set-jump-a (fn &rest args)
  "Dummy function."
  (apply fn args))



;;; Package: edit/clean-aindent-mode

;; Nice tip from tuhdo, see https://www.emacswiki.org/emacs/CleanAutoIndent
;; https://github.com/pmarinov/clean-aindent-mode

(add-hook 'prog-mode-hook #'clean-aindent-mode)



;;; Package: edit/expand-region

(after! expand-region
  (setq expand-region-reset-fast-key    "<ESC><ESC>")
  (setq expand-region-smart-cursor t))

(map! "C-+" #'er/expand-region)



;;; Package: edit/kurecolor
;; https://github.com/emacsfodder/kurecolor
;;
;; This package allows interactive modification of color values.

(defhydra hydra-kurecolor (:color pink :hint  nil)
  "
Dec/Inc      _j_/_J_ brightness      _k_/_K_ saturation      _l_/_L_ hue
Gradient     _gj_ ^^ brightness      _gk_ ^^ saturation      _gl_ ^^ hue
Set          _sj_ ^^ brightness      _sk_ ^^ saturation      _sl_ ^^ hue

Convert      _ch_ ^^ RGB → Hex       _cr_ ^^ Hex → RGB       _cR_ ^^ Hex → RGBA
"
  ("j"  kurecolor-decrease-brightness-by-step)
  ("J"  kurecolor-increase-brightness-by-step)
  ("k"  kurecolor-decrease-saturation-by-step)
  ("K"  kurecolor-increase-saturation-by-step)
  ("l"  kurecolor-decrease-hue-by-step)
  ("L"  kurecolor-increase-hue-by-step)
  ("sj" kurecolor-set-brightness :color blue)
  ("sk" kurecolor-set-saturation :color blue)
  ("sl" kurecolor-set-hue :color blue)
  ("gj" kurecolor-hex-val-group :color blue)
  ("gk" kurecolor-hex-sat-group :color blue)
  ("gl" kurecolor-hex-hue-group :color blue)
  ("ch" kurecolor-cssrgb-at-point-or-region-to-hex :color blue)
  ("cr" kurecolor-hexcolor-at-point-or-region-to-css-rgb :color blue)
  ("cR" kurecolor-hexcolor-at-point-or-region-to-css-rgba :color blue)
  ("q"  nil "cancel" :color blue))

(defun kurecolor ()
  "Turns on rainbow mode and lets you modify the current color code. The
cursor must be sitting over a CSS-like color string, e.g. \"#ff008c\"."
  (interactive)
  (rainbow-mode t)
  (hydra-kurecolor/body))




;;; Package: edit/link-hint

(after! link-hint
  (map! "M-g l"   #'link-hint-open-link)
)



;;; Package: edit/indent

;; This variable is used in indent-for-tab-command and calls and calls out to
;; completion-at-point
(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      tab-first-completion 'eol)




;;; Package: edit/smartparens

;; I hate this package, so I don't want it. But I must define some fake functions
;; in order to mitigate errors.
(cl-defun sp-local-pair (modes
                         open
                         close
                         &key
                         trigger
                         trigger-wrap
                         (actions '(:add))
                         (when '(:add))
                         (unless '(:add))
                         (pre-handlers '(:add))
                         (post-handlers '(:add))
                         wrap
                         bind
                         insert
                         prefix
                         suffix
                         skip-match)
  "Dummy")
(defun turn-off-smartparens-mode ()
  "Dummy")
(defun sp-point-in-comment (&optional pos)
  "Dummy"
  nil)
(defun sp-point-in-string (&optional pos)
  "Dummy"
  nil)




;;; Package: edit/symbol-overlay - jump / manipulate to symbols
;; https://github.com/wolray/symbol-overlay

(map! "M-<up>"   #'symbol-overlay-jump-prev
      "M-<down>" #'symbol-overlay-jump-next
      )





;;; Package: edit/tabify
(after! tabify
  ;; only tabify initial whitespace
  (setq tabify-regexp "^\t* [ \t]+"))




;;; Package: misc/gptel

(use-package! gptel
  :commands (gptel gptel-menu gptel-send gptel-set-topic)

  :config
  ;; (setq! gptel-api-key "your key")

  (setq-default gptel-model "mistral:latest")

  (setq-default gptel-backend (gptel-make-ollama
                               "Ollama"                               ;Any name of your choosing
                               :host "localhost:11434"
                                        ; Installed models:
                               :models '("mistral:latest" "stablelm2:latest")
                               :stream t))

  (add-to-list 'gptel-directives '(english . "Translate the following to english: "))
  (add-to-list 'gptel-directives '(deutsch . "Translate the following to german: "))
  (add-to-list 'gptel-directives '(typo    . "Fix typos, grammar and style of the following: "))
)

;;; Package: misc/embark


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

(after! embark

  (defun which-key--hide-popup-ignore-command ()
    "Empty dummy function.")

  (setq embark-collect-initial-view-alist '((buffer . list)                ;; was grid
                                            (consult-grep . list)
                                            (consult-location . list)
                                            (consult-register . zebra)
                                            (consult-yank . zebra)
                                            (file . list)                  ;; was grid
                                            (kill-ring . zebra)
                                            (line . list)
                                            (symbol . list)                ;; was grid
                                            (xref-location . list)
                                            (t . list)))

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

  ;; Always prompt via vertico?  Something like "C-a u" won't then work :-(
  ;; (setq embark-prompter 'embark-completing-read-prompter)

  (define-key!
    :keymaps '(embark-collect-mode-map)
    "M-t" #'toggle-truncate-lines)

  (define-key!
    :keymaps '(embark-file-map)
    "U" #'0x0-upload-file)

  (define-key!
    :keymaps '(embark-function-map embark-variable-map embark-expression-map)
    "l" #'eldoc)

  (define-key!
    :keymaps '(embark-general-map)
    ;; this is used to cycle where it works on (e.g. file, symbol, defun)
    "C-;" #'embark-cycle
    ;; L used to be embark-collect-life, which isn't that helpful if one already uses
    ;; something like vertico or selectrum
    "L" nil)

  (define-key!
    :keymaps '(embark-region-map)
    "U" #'0x0-upload-text)

  (define-key!
    :keymaps '(embark-variable-map)
    ;; Used to be customize-variable, but that is locked on Doom. Better use "=" to set
    ;; the variable
    "u" nil)

  (define-key!
    :keymaps '(+vertico/embark-doom-package-map)
    "P" #'straight-pull-package
    "c" #'straight-check-package
    "i" #'straight-use-package
    "r" #'straight-get-recipe
    ;; "u" #'straight-visit-package-website ;; was doom/help-package-homepage
    ;; "f" #'straight-fetch-package
    ;; "p" #'straight-push-package
    ;; "n" #'straight-normalize-package
    ;; "m" #'straight-merge-package
    )

  ;; Keep Embark from trying to insert current selection into a y-or-n prompt
  (setq y-or-n-p-use-read-key t)
)


;;; Package: modes/bb-mode

(use-package! bb-mode
  :mode (("\\.bb$" . bb-mode)
         ("src/poky/.*\\.inc$" . bb-mode)
         ("\\.bbappend$" . bb-mode)
         ("\\.bbclass$" . bb-mode)
         ("src/poky/.*\\.conf$" . bb-mode)
         ))



;;; Package: modes/dts-mode

(use-package! dts-mode
  :mode (("\\.dts\\'"     . dts-mode)
         ("\\.overlay\\'" . dts-mode))
)




;;; Package: modes/diff-mode

;; The following let the commits from "git diff >foo.diff" stand out more:
(after! diff-mode
  (defun my-diff-mode-setup ()
    (hi-lock-line-face-buffer "^commit"))
  (add-hook 'diff-mode-hook #'my-diff-mode-setup))




;;; Package: modes/dired

(after! dired
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  ;; revert when revisiting
  (setq dired-auto-revert-buffer t)
  ;; work in a Norton Commander like mode if 2 dired panes are open
  (setq dired-dwim-target t)
  ;; less confirmations
  (setq dired-confirm-shell-command nil
        dired-no-confirm t
        dired-deletion-confirmer '(lambda (x) t)
        dired-recursive-deletes 'always)
  ;; less details, use '(' inside dired to toggle them
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  ;; Copy things in the background, better for large files / subtrees
  (dired-async-mode 1)
  )

(after! dired-aux
  ;; If dwim, Isearch matches file names when initial point position
  ;; is on a file name. Otherwise, it searches the whole buffer
  ;; without restrictions.
  (setq dired-isearch-filenames 'dwim))

(map! :map dired-mode-map
      "q" #'dired-up-directory)





;;; Package: modes/ediff

(after! ediff
  :config
  (setq ediff-split-window-function 'split-window-vertically)

  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))




;;; Package: modes/helpful

(after! helpful
  ;; Make helpful frames displayed "full-screen"
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helpful" )
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . visible))
               )
  (add-hook 'helpful-mode-hook #'visual-line-mode)

  (map! :map helpful-mode-map
        "a" #'describe-symbol))

(map! "<f1> h" #'helpful-at-point
      :map helpful-mode-map
      "q" #'kill-buffer-and-window)




;;; Package: modes/jinx

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
  :defer t

  ;; jinx-camel-modes add maybe python-mode and nim-mode?

  :hook
  (text-mode-hook . jinx-mode)
  (conf-mode-hook . jinx-mode)
  ;; (prog-mode-hook . jinx-mode)

  :general
  ;; since we don't ue ispell, we can re-assign it's keybinding
  ("M-$"   #'jinx-correct)
  ("C-M-$" #'jinx-languages)
)



;;; Package: modes/js-mode

(use-package! js-mode
  :mode "\\.ns\\'"  ;; bitburner .ns files
)



;;; Package: modes/nov

;; https://depp.brause.cc/nov.el/
(use-package! nov
  :defer t
  :mode (("\\epub\\'" . nov-mode))
  :hook
  (nov-mode-hook . visual-line-mode)
)



;;; Package: modes/pdf-tools

;; https://github.com/vedang/pdf-tools
;; https://pdftools.wiki/

(use-package pdf-tools
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



;; Package: completion/consult

(use-package! consult
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

  :general
  ;; C-c bindings (mode-specific-map)
  ("C-c h"   #'consult-history)
  ("C-c m"   #'consult-mode-command)
  ("C-c k"   #'consult-kmacro)
  ;; C-c r   -> vertico-repeat-last
  ;; C-c C-r -> vertico-repeat

  ;; C-x bindings (ctl-x-map)
  ("C-x M-:"  #'consult-complex-command)     ;; was: repeat-complex-command)
  ("C-x b"    #'ibuffer)                     ;; was: switch-to-buffer)
  ("C-x C-b"  #'consult-buffer)              ;; was: list-buffers)
  ("C-x 4 b"  #'consult-buffer-other-window) ;; was: switch-to-buffer-other-window)
  ("C-x 5 b"  #'consult-buffer-other-frame)  ;; was: switch-to-buffer-other-frame)
  ("C-x r b"  #'consult-bookmark)
  ("C-x p b"  #'consult-project-buffer)

  ;; Custom M-# bindings for fast register access
  ;; How would I type this on a german keyboard?
  ("M-#"      #'consult-register-load)
  ("M-'"      #'consult-register-store)
  ("C-M-#"    #'consult-register)

  ;; M-g bindings (goto-map)
  ("M-g e"    #'consult-compile-error)
  ("M-g f"    #'consult-flymake)
  ("M-g g"    #'consult-goto-line)           ;; was: goto-line
  ("M-g M-g"  #'consult-goto-line)           ;; was: goto-line
  ("M-g o"    #'consult-outline)
  ("M-g k"    #'consult-mark)
  ("M-g K"    #'consult-global-mark)
  ("M-g i"    #'consult-imenu-multi)
  ("M-g I"    #'consult-imenu)               ;; prefer M-s o (consult-outline) instead of this

  ;; M-s bindings (search-map)
  ("M-s e"    #'consult-isearch-history)
  ("M-s f"    #'consult-find)
  ("M-s F"    #'consult-locate)
  ;; Searching (mostly)
  ("M-s g"    #'consult-git-grep)
  ("M-s i"    #'consult-info)
  ("M-s r"    #'consult-ripgrep)
  ("M-s l"    #'consult-line)                ;; similar to swiper
  ("M-s L"    #'consult-line-multi)
  ("M-s k"    #'consult-keep-lines)
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

  (help-map "a" #'describe-symbol)

  (isearch-mode-map "M-e"   #'consult-isearch-history)     ;; was: isearch-edit-string
  (isearch-mode-map "M-s e" #'consult-isearch-history)     ;; was: isearch-edit-string
  (isearch-mode-map "M-s l" #'consult-line)                ;; needed by consult-line to detect isearch
  (isearch-mode-map "M-s L" #'consult-line-multi)          ;; needed by consult-line to detect isearch

  ;; access history of minibuffer completions, e.g. do "M-s l" (consult-line) "M-r" (consult-history)
  (minibuffer-local-map "M-s" #'consult-history)           ;; was: next-matching-history-element
  (minibuffer-local-map "M-r" #'consult-history)           ;; was: previous-matching-history-element

  (compilation-mode-map       "e" #'consult-compile-error)
  (compilation-minor-mode-map "e" #'consult-compile-error)

  ;; Unfortunately, the DEL key is hijacked by vertico, so I cannot unnorrow
  ;; But we don't really need the delete-forward-char function, so we can use
  ;; that key to unnarrow.
  (consult-narrow-map "<deletechar>" #'consult--narrow-delete)     ;; was: delete-forward-char
)



;;; Package: completion/corfu

(after! corfu
  ;; keep if off
  (setopt corfu-auto nil)
  ;; but if we turn it on, use original times/limits
  (setopt corfu-auto-delay 0.2)
  (setopt corfu-auto-prefix 3)

  ;; Experiment with these:
  ;; (setopt corfu-quit-at-boundary t)
  ;; (setopt corfu-quit-no-match t)
)



;;; Package: completion/orderless

(after! orderless
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
  (setq completion-styles '(orderless flex))
  ;; make it possible to use "\ " (backslash space) to match for an actual space
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
)



;;; Package: completion/tempel

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
  :defer t

  :commands
  (tempel-expand tempel-complete)

  :custom
  (tempel-path (concat doom-private-dir "templates.el"))

  :general
  ("M-+" #'tempel-complete)    ;;  completes a template name at point in the buffer and subsequently expands the template
  ("M-*" #'tempel-insert)      ;;  selects a template by name and insert it into the current buffer

  (tempel-map "C-c C-c" #'tempel-done)

  :init
  (add-hook 'completion-at-point-functions #'tempel-expand)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (tempel-global-abbrev-mode)
)



;;; Package: completion/vertico

(after! vertico
  ;; use 60% of the screen estate for vertico
  (setq vertico-count (/ (* 6 (frame-height)) 10))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (map!
   ("C-c r"   #'vertico-repeat-last) ;; always the last, but can be enter while inside a vertico prompt to recall the last one
   ("C-c C-r" #'vertico-repeat)      ;; the last one, can be called with C-u to get a list of previous vertico calls
   :map vertico-map
   ([next] nil)     ;; was next-history-element
   ([prior] nil)   ;; was previous-history-element
   ("M-<up>"  #'next-history-element)
   ("M-<down>" #'previous-history-element))
)



;;; Package: lang/cc-mode

(after! c-ts-mode
  ;;(message "C-TS-MODE")

  (defun my-c-ts-base-mode-setup ()
    ;;(message "MY-C-TS-BASE-MODE-SETUP")

    (c-ts-mode-set-style 'gnu)

    (setq-local c-ts-mode-indent-offset 4)

    ;; Default, might be overwrriten by dtrt-indent
    (setq-local indent-tabs-mode t)

    ;; use "// " for commenting in both C and C++
    (setq comment-start "// "
          comment-end ""))
  (add-hook 'c-ts-base-mode-hook  #'my-c-ts-base-mode-setup))

(after! cc-mode
  ;;(message "CC-MODE")

  (map!
   :map c-mode-base-map
   ("TAB"  #'indent-for-tab-command))  ;; was c-indent-line-or-region

  (c-add-style "qt-gnu"
               '("gnu" (c-access-key .
                                     "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")))
  (setq-default c-electric-flag nil)

  (defun my-c-mode-setup ()
    ;;(eglot-ensure)

    ;; need to check the mode because I run this also at the revert hook!
    (modify-syntax-entry ?_ "w")
    (setq c-recognize-knr-p nil)

    ;; Default, might be overwrriten by dtrt-indent
    (setq indent-tabs-mode t)

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
    ;; (message "my-c-mode-setup done")
    )
  (add-hook 'c-mode-hook   #'my-c-mode-setup)
  (add-hook 'c++-mode-hook #'my-c-mode-setup)
  )




;;; Package: lang/compile

(after! compile
  (setq compilation-scroll-output t)

  (defun my-colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)

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




;;; Package: lang/elisp

(after! lisp-mode
  ;; You can replace evil-last-sexp with pp-eval-last-sexp
  ;; (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

  (defun my-emacs-lisp-mode-setup ()
    (interactive)
    "My emacs lisp mode setup function."
    ;; "-" is almost always part of a function- or variable-name
    (modify-syntax-entry ?- "w")

    ;; make sure we cannot save syntax errors
    (add-hook 'local-write-file-hooks 'check-parens)

    ;; Modify completions, elisp-completion-at-point wouldn't allow me to
    ;; complete elisp things in comments.
    (defalias 'my-elisp-capf (cape-capf-super #'elisp-completion-at-point
                                              #'cape-dabbrev
                                              #'cape-file
                                              #'cape-dict
                                              #'cape-elisp-symbol))
    (setq-local completion-at-point-functions '(my-elisp-capf t))

    ;; The following changes the imenu "M-g i" to care most about my ";;;" comments
    (setq lisp-imenu-generic-expression '())
    (setq imenu-generic-expression
          (list
           (list (purecopy "Type")
                 (purecopy (concat "^\\s-*("
                                   (eval-when-compile
                                     (regexp-opt
                                      '(;; Elisp
                                        "defgroup" "deftheme"
                                        "define-widget" "define-error"
                                        "defface" "cl-deftype" "cl-defstruct"
                                        ;; CL
                                        "deftype" "defstruct"
                                        "define-condition" "defpackage"
                                        ;; CLOS and EIEIO
                                        "defclass")
                                      t))
                                   "\\s-+'?\\(" lisp-mode-symbol-regexp "\\)"))
                 2)
           (list (purecopy "Variable")
                 (purecopy (concat "^\\s-*("
                                   (eval-when-compile
                                     (regexp-opt
                                      '(;; Elisp
                                        "defconst" "defcustom"
                                        ;; CL
                                        "defconstant"
                                        "defparameter" "define-symbol-macro")
                                      t))
                                   "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                 2)
           ;; For `defvar'/`defvar-local', we ignore (defvar FOO) constructs.
           (list (purecopy "Variable")
                 (purecopy (concat "^\\s-*(defvar\\(?:-local\\)?\\s-+\\("
                                   lisp-mode-symbol-regexp "\\)"
                                   "[[:space:]\n]+[^)]"))
                 1)
           (list "Function"
                 (purecopy (concat "^\\s-*("
                                   (eval-when-compile
                                     (regexp-opt
                                      '("defun" "defmacro"
                                        ;; Elisp.
                                        "defun*" "defsubst" "define-inline"
                                        "define-advice" "defadvice" "define-skeleton"
                                        "define-compilation-mode" "define-minor-mode"
                                        "define-global-minor-mode"
                                        "define-globalized-minor-mode"
                                        "define-derived-mode" "define-generic-mode"
                                        "ert-deftest"
                                        "cl-defun" "cl-defsubst" "cl-defmacro"
                                        "cl-define-compiler-macro" "cl-defgeneric"
                                        "cl-defmethod"
                                        ;; CL.
                                        "define-compiler-macro" "define-modify-macro"
                                        "defsetf" "define-setf-expander"
                                        "define-method-combination"
                                        ;; CLOS and EIEIO
                                        "defgeneric" "defmethod")
                                      t))
                                   "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                 2)
           (list "require"
                 (concat "^\\s-*(require\\s-+'\\(" lisp-mode-symbol-regexp "\\)")
                 1)
           (list "use-package"
                 (concat "^\\s-*(use-package\\s-+\\(" lisp-mode-symbol-regexp "\\)")
                 1)
           (list "Section"
                 "^;;[;]\\{1,8\\} \\(.*$\\)"
                 1))))
  (add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-setup))




;;; Package: lang/prog-mode

;; Show trailing whitespace when programming

(defun my-show-trailing-whitespace ()
  "Show trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace t))

(defun my-hide-trailing-whitespace ()
  "Hide trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace nil))

(after! prog-mode
  (add-hook 'prog-mode-hook #'my-show-trailing-whitespace)
  (add-hook 'prog-mode-hook #'goto-address-mode))




;;; Package: lang/eglot


;; Start eglot either with (eglot) or with Doom's (lsp!)
;;
;; If you don't have a compilation database (e.g. in projects not using
;; CMake or Meson), you can use https://github.com/rizsotto/Bear

(after! eglot

  (setq eglot-stay-out-of '(company))

  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-15" "-j=2" "--clang-tidy" "--compile-commands-dir=build")))
  (add-to-list 'eglot-server-programs '(c-mode  .  ("clangd-15" "-j=2" "--clang-tidy" "--compile-commands-dir=build")))

  ;; This disables the overrides of general-override-mode-map, which steals C-c a
  (general-override-mode 0)

  ;; Eglot modifies the completion defaults, undo this. Remove-hook is used as
  ;; remove-from-list :-)
  (remove-hook 'completion-category-defaults '(eglot (styles flex basic)))

  (defalias 'my-eglot-capf (cape-capf-super ;;#'tempel-expand
                                            #'eglot-completion-at-point
                                            #'cape-file
                                            #'cape-dabbrev
                                            #'cape-dict
                                            )
    "completion at point functions for eglot")

  (defun my-eglot-hook ()
    (add-to-list 'completion-at-point-functions #'my-eglot-capf)
    (add-to-list 'completion-at-point-functions #'tempel-expand)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    )
  (add-hook 'eglot-managed-mode-hook #'my-eglot-hook)

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
    ("m" flymake-mode :exit nil)
  )


  (map! :map eglot-mode-map
        "C-c a" #'eglot-code-actions                  ;; was embark-act, but that is also in C-;
        "C-c e" #'eglot-help/body
        "C-c R" #'eglot-rename                        ;; unused in c-mode
        ;; M-;    xref-go-back
        ;; M-.    xref-find-definitions
        ;; M-g e  consult-compile-error               ;; for errors, maybe even warnings
        ;; M-g f  consult-flymake                     ;; for errors, maybe even warnings
        ;; M-s u  xref-find-reference                 ;; like "search usage"
        )
)




;;; Package: lang/flymake

(use-package flymake
  :defer

  :custom
  (flymake-wrap-around nil)
)



;;; Package: lang/magit

(after! magit
  ;; Open magit window full-screen
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; When calling magit-status, save all buffers without further ado
  (setq magit-save-repository-buffers 'dontask)

  ;; make [MASTER] appear at the end of the summary line
  (setq magit-log-show-refname-after-summary t)

  ;; Switch repositories with magit-list-repositories
  (setq magit-repository-directories
        '(
          ("~/d"      . 1)
          ("~/src"    . 1)
          )))

(after! git-commit
  ;; Anything longer will be highlighted
  (setq git-commit-summary-max-length 70))

(add-to-list 'display-buffer-alist
             `(,(rx bos "magit-revision: ")
               (display-buffer-reuse-window display-buffer-same-window)
               (reusable-frames . visible))
             )

(map!
 "M-g m" #'magit-status
 "M-g M" #'magit-list-repositories)




;;; Package: lang/meson

(use-package! meson-mode
  :mode (("\\meson.build\\'" . meson-mode))
  :config
  (setq meson-indent-basic 4))




;;; Package: lang/my-compile

(use-package! my-compile
  :load-path doom-private-dir
  :defer t

  :general
  ("S-<f7>" #'my-compile-select-command-and-run)
  ("<f7>"   #'my-compile)
)



;;; Package: lang/nim

(after! nim-mode
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

  (add-hook 'nim-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'nim-mode-hook #'subword-mode)
  (add-hook 'nim-mode-hook #'nimsuggest-mode)
)

;;; Package: lang/plantuml

(after! plantuml-mode
  (setq plantuml-jar-path "/home/schurig/.cache/plantuml.jar"))




;;; Package: lang/python

;; Don't forget that we can indent/detent blocks with C-x TAB ...

(after! python
    (defun my-python-setup ()
      (interactive)
      (setq indent-tabs-mode t
            python-indent-offset 4
            tab-width 4
            ;; this fixes the weird indentation when entering a colon
            ;; from http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem
            electric-indent-chars (delq ?: electric-indent-chars)))
    (add-hook 'python-mode-hook #'my-python-setup))



;;; Package: lang/sh-script

(after! sh-script
  (defun my-sh-mode-setup ()
    (interactive)
    (setq-local indent-tabs-mode t)
    (setq tab-width 4)

    ;; Tab positions for M-i
    (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84))

    ;; (setq smie-config--buffer-local '((4 :after "{" 4)))

    ;; if you have the "shellcheck" debian package installed you can actually see your shell DONTs
    (flymake-mode 1)
    )

  (add-hook 'sh-mode-hook  #'my-sh-mode-setup))




;;; Package: lang/text-mode

(remove-hook 'text-mode-hook #'auto-fill-mode)




;;; Package: org/org

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-replace-disputed-keys t)

(after! org
  (setq org-directory "~/org/"
        org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil
        org-agenda-files '("~/.doom.d/todo.org"))
  (electric-indent-mode -1)

  (setopt org-todo-keywords
    (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            ;; "@" means to add a note (with time) WHEN ENTERING STATE
            ;; "!" means to record only the time of the state change WHEN LEAVING STATE
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  ;; PlantUML
  (after! org-plantuml
    (setq org-plantuml-jar-path "/usr/local/bin/plantuml.1.2020.16.jar"))
  (org-babel-do-load-languages 'org-babel-load-languages
                                 '(plantuml . t))
  (remove-hook 'org-mode-hook #'org-superstar-mode)
  (remove-hook 'org-open-at-point-functions #'doom-set-jump-h)

  (setopt org-return-follows-link t)

  (map! "C-c l"   #'org-store-link
        "C-c C-l" #'org-insert-link)
)


;;; Package: org/org-agenda

(use-package org-agenda
  :defer t

  :config
  (setopt org-capture-templates
          '(("e" "Einkaufen" entry (file+headline "~/Sync/Data/TODO.org" "Einkaufen Neu")
             "* TODO %?\n")
            ("t" "TODO" entry (file+headline "~/Sync/Data/TODO.org" "Aufgaben")
             "* TODO %?\n")))

  (setopt org-agenda-skip-scheduled-if-done t  ; Don't show scheduled items in agenda when they are done
          org-agenda-skip-deadline-if-done t   ; Don't show deadlines when the corresponding item is done
          org-agenda-compact-blocks t          ; Make the block agenda more compact
          org-agenda-start-with-log-mode 'clockcheck
          org-agenda-clockreport-parameter-plist
          (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

  :general
  ("C-c a" #'org-agenda)
)




;;; Package: org/org-capture

(use-package org-capture
  :general
  ("C-c c" #'org-capture))



;;; Package: org/org-roam

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Sync/Org")
  (org-roam-completion-everywhere t)
  :general
  ("C-c n l" #'org-roam-buffer-toggle)
  ("C-c n f" #'org-roam-node-find)
  ("C-c n i" #'org-roam-node-insert)
)


;;; Package: org/org-tempo

(use-package org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("cc" . "src C"))
  (add-to-list 'org-structure-template-alist '("cp" . "src cpp"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("mk" . "src makefile"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src bash"))
)



;;; Package: org/ox

(after! ox
  :defer t
  :config
  ;; The following make some +OPTIONS permanent:
  ;; #+OPTIONS ':t
  (setq org-export-with-smart-quotes t)
  ;; #+OPTIONS num:nil
  (setq org-export-with-section-numbers nil)
  ;; #+OPTIONS stat:t
  ;; (setq org-export-with-statistics-cookies nil)
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a headline
  (setq org-export-with-toc nil)
  ;; #+OPTIONS ^:{}
  (setq org-export-with-sub-superscripts nil)

  ;; This exports broken links as [BROKEN LINK %s], so we can actually
  ;; find them. The default value nil just aborts the export process
  ;; with an error message "Unable to resolve link: nil". This doesn't
  ;; give any hint on which line the broken link actually is :-(
  (setq org-export-with-broken-links 'mark)

  (setq org-export-time-stamp-file nil))



;;; Package: org/ox-html

(after! ox-html
  (setq org-html-postamble-format '(("en" "<p class=\"author\">Author: %a</p><p class=\"creator\">Created with %c</p>")))
  (setq org-html-validation-link nil)
  (setq org-html-postamble nil)
  (setq org-html-style-default "<style type=\"text/css\">\n <!--/*--><![CDATA[/*><!--*/\n  body { text-align: center; font-family: \"Aria\", sans-serif; }\n  #content { margin: 0 auto; width: 860px; text-align: left; }\n  #text-table-of-contents > ul > li { margin-top: 1em; }\n  .title  { text-align: center; }\n  .todo   { color: red; }\n  .done   { color: green; }\n  .WAIT, .DELE   { color: blue; }\n  .done   { color: green; }\n  .tag    { background-color: #eee; font-family: monospace;\n            padding: 2px; font-size: 80%; font-weight: normal; }\n  .timestamp { color: #bebebe; }\n  .timestamp-kwd { color: #5f9ea0; }\n  .right  { margin-left: auto; margin-right: 0px;  text-align: right; }\n  .left   { margin-left: 0px;  margin-right: auto; text-align: left; }\n  .center { margin-left: auto; margin-right: auto; text-align: center; }\n  .underline { text-decoration: underline; }\n  #postamble p, #preamble p { font-size: 90%; margin: .2em; }\n  p.verse { margin-left: 3%; }\n  pre {\n    border: 1px solid #ccc;\n    box-shadow: 3px 3px 3px #eee;\n    padding: 8pt;\n    font-family: monospace;\n    overflow: auto;\n    margin: 1em 0;\n  }\n  pre.src {\n    position: relative;\n    overflow: visible;\n    padding-top: 8pt;\n  }\n  pre.src:before {\n    display: none;\n    position: absolute;\n    background-color: white;\n    top: -10px;\n    right: 10px;\n    padding: 3px;\n    border: 1px solid black;\n  }\n  pre.src:hover:before { display: inline;}\n  pre.src-sh:before    { content: 'sh'; }\n  pre.src-bash:before  { content: 'sh'; }\n  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }\n  pre.src-R:before     { content: 'R'; }\n  pre.src-perl:before  { content: 'Perl'; }\n  pre.src-java:before  { content: 'Java'; }\n  pre.src-sql:before   { content: 'SQL'; }\n\n  table { border-collapse:collapse; }\n  caption.t-above { caption-side: top; }\n  caption.t-bottom { caption-side: bottom; }\n  td, th { vertical-align:top;  }\n  th.right  { text-align: center;  }\n  th.left   { text-align: center;   }\n  th.center { text-align: center; }\n  td.right  { text-align: right;  }\n  td.left   { text-align: left;   }\n  td.center { text-align: center; }\n  dt { font-weight: bold; }\n  .footpara:nth-child(2) { display: inline; }\n  .footpara { display: block; }\n  .footdef  { margin-bottom: 1em; }\n  .figure { padding: 1em; }\n  .figure p { text-align: center; }\n  .inlinetask {\n    padding: 10px;\n    border: 2px solid gray;\n    margin: 10px;\n    background: #ffffcc;\n  }\n  #org-div-home-and-up\n   { text-align: right; font-size: 70%; white-space: nowrap; }\n  textarea { overflow-x: auto; }\n  .linenr { font-size: smaller }\n  .code-highlighted { background-color: #ffff00; }\n  .org-info-js_info-navigation { border-style: none; }\n  #org-info-js_console-label\n    { font-size: 10px; font-weight: bold; white-space: nowrap; }\n  .org-info-js_search-highlight\n    { background-color: #ffff00; color: #000000; font-weight: bold; }\n  .ulClassNameOrID > li {}\n  /*]]>*/-->\n</style>")
  (setq org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6"))
  (setq org-html-postamble t))



;;; Package: org/ox-hugo

;; https://ox-hugo.scripter.co/

(use-package! ox-hugo
  :commands (org-hugo-export-wim-to-md)
  :after ox
  :custom
  (org-hugo-base-dir "~/src/hpg/")

  )
(after! org
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

  (map! :map org-mode-map
        "C-c h" #'org2hugo)
  )



;; Package: ox-reveal

;; https://github.com/yjwen/org-reveal
;; https://revealjs.com/
;; (customize-group "org-export-reveal")

(use-package! ox-reveal
  :commands (org-reveal-export-to-html)
  :config
  (setq org-reveal-root (concat "file://" doom-local-dir "straight/repos/reveal.js/"))
  ;; Options here are:
  ;; - const markdown  - render markdown
  ;; - const highlight - don't highlight via Emacs theme, but use highlight.js instead
  ;; - const zoom      - allow ctrl-click (Linux) or alt-clock (Windows) to zoom to some place
  ;; - const notes     - allow speaker-notes. They would open a popup window.
  ;; - const search    - search inside the slides
  ;; - const remotes
  ;; - const multiplex - provide slides also via a socket to clients
  (setq org-reveal-plugins '(search))
)



;;; Package: comm/circe


(defun my-auth-server-pass (server)
  (if-let ((secret (plist-get (car (auth-source-search :host server)) :secret)))
      (if (functionp secret)
          (funcall secret) secret)
    (error "Could not fetch password for host %s" server)))

(after! circe
  ;; Put something like this
  ;;    machine irc.libera.chat login yournick password "top-secret-string" port 6667
  ;; into ~/.authinfo. From https://github.com/emacs-circe/circe/wiki/Configuration#safer-password-management
  (defun my-fetch-password (&rest params)
    (require 'auth-source)
    (let ((match (car (apply 'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))
        (error "Password not found for %S" params))))

  (defun my-nickserv-password (server)
    (my-fetch-password :host server))

  (setq circe-network-options
      '(("Libera Chat"
         :logging t
         :nick "schurig"
         :channels (:after-auth "#emacs")
         :nickserv-password my-nickserv-password)))

  ;; (setq circe-format-self-say "{nick:+13s} ┃ {body}")
  (setq circe-active-users-timeout 300
        circe-channel-killed-confirmation nil
        circe-server-killed-confirmation 'kill-all
        circe-extra-nicks '("hschurig" "holgerschurig")
        circe-server-send-unknown-command-p t
        circe-chat-buffer-name "{target}_{network}")

  ;; who wants join and part messages?
  (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
  (circe-set-display-handler "PART" (lambda (&rest ignored) nil))
  (circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))
  (circe-set-display-handler "channel.quit" (lambda (&rest ignored) nil))


  (defun circe-command-RECONNECT (&optional ignored)
    (circe-reconnect))

  ;; (map! :map circe-channel-mode-map
  ;;       "C-c n" #'tracking-next-buffer
  ;;       "C-c p" #'tracking-previous-buffer
  ;;       )

  ;; (enable-lui-logging-globally)
  ;; (enable-lui-logging)
)

(after! lui-logging
  (setq lui-logging-directory (concat doom-local-dir "irc")
        ;;lui-logging-flush-delay 60
        )
)



;;; Package: comm/message

(after! message

  ;; When composing a mail, start the auto-fill-mode.
  (add-hook 'message-mode-hook 'turn-on-auto-fill)

  ;; Add mails to bbdb (in case I'll ever use that)
  ;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

  ;; Generate the mail headers before you edit your message.
  (setq message-generate-headers-first t)

  ;; When I reply, I don't want to have me in To or Cc
  (setq message-dont-reply-to-names (concat "\\("
     user-mail-address
     ;; Nor the Debian BTS
     ;; "\\|^submit@bugs.debian\\.org$"
     "\\)"))
)


;;; Package: comm/mm-decode

(after! mm-decode

  ;; Displaying zip/tar inline is a really, really stupid default!
  (setq mm-inlined-types
                (cl-remove-if (apply-partially #'string-match-p "\\(x-g?tar\\|zip\\)")
                                          mm-inlined-types))
)



;;; Package: comm/mastodon

;; https://codeberg.org/martianh/mastodon.el

(after! mastodon
  (setq mastodon-active-user "holgerschurig@gmail.com"
        mastodon-instance-url "https://emacs.ch")

  ;; Not in the following hydra, but mentioned in "M-x describe-mode". Also, the README.org
  ;; contains several functions that aren't in my hydra.
  ;;
  ;; TAB                     mastodon-tl--next-tab-item
  ;; D                       mastodon-toot--delete-and-redraft-toot
  ;; C-S-b                   mastodon-tl--unblock-user
  ;; S-TAB                   mastodon-tl--previous-tab-item
  ;; S-RET                   mastodon-tl--unmute-user
  ;; C-S-w                   mastodon-tl--unfollow-user
  ;; S-SPC                   scroll-down-command
  ;; <backtab>               mastodon-tl--previous-tab-item
  ;; C-M-i                   mastodon-tl--previous-tab-item
  ;; M-n                     mastodon-tl--next-tab-item
  ;; M-p                     mastodon-tl--previous-tab-item

  (defhydra mastodon-help (:color blue :hint nil)
    "
Timelines^^   Toots^^^^           Own Toots^^   Profiles^^      Users/Follows^^  Misc^^
^^-----------------^^^^--------------------^^----------^^-------------------^^------^^-----
_H_ome        _n_ext _p_rev       _r_eply       _A_uthors       follo_W_         _X_ lists
_L_ocal       _T_hread of toot^^  wri_t_e       user _P_rofile  _N_otifications  f_I_lter
_F_ederated   (un) _b_oost^^      _e_dit        ^^              _R_equests       _C_opy URL
fa_V_orites   (un) _f_avorite^^   _d_elete      _O_wn           su_G_estions     _S_earch
_#_ tagged    (un) p_i_n^^        ^^            _U_pdate own    _M_ute user      _h_elp
_@_ mentions  (un) boo_k_mark^^   show _E_dits  ^^              _B_lock user
boo_K_marks   _v_ote^^
trendin_g_
_u_pdate
"
    ("H" mastodon-tl--get-home-timeline)
    ("L" mastodon-tl--get-local-timeline)
    ("F" mastodon-tl--get-federated-timeline)
    ("V" mastodon-profile--view-favourites)
    ("#" mastodon-tl--get-tag-timeline)
    ("@" mastodon-notifications--get-mentions)
    ("K" mastodon-profile--view-bookmarks)
    ("g" mastodon-search--trending-tags)
    ("u" mastodon-tl--update :exit nil)

    ("n" mastodon-tl--goto-next-toot)
    ("p" mastodon-tl--goto-prev-toot)
    ("T" mastodon-tl--thread)
    ("b" mastodon-toot--toggle-boost :exit nil)
    ("f" mastodon-toot--toggle-favourite :exit nil)
    ("i" mastodon-toot--pin-toot-toggle :exit nil)
    ("k" mastodon-toot--bookmark-toot-toggle :exit nil)
    ("c" mastodon-tl--toggle-spoiler-text-in-toot)
    ("v" mastodon-tl--poll-vote)

    ("A" mastodon-profile--get-toot-author)
    ("P" mastodon-profile--show-user)
    ("O" mastodon-profile--my-profile)
    ("U" mastodon-profile--update-user-profile-note)

    ("W" mastodon-tl--follow-user)
    ("N" mastodon-notifications-get)
    ("R" mastodon-profile--view-follow-requests)
    ("G" mastodon-tl--get-follow-suggestions)
    ("M" mastodon-tl--mute-user)
    ("B" mastodon-tl--block-user)

    ("r" mastodon-toot--reply)
    ("t" mastodon-toot)
    ("e" mastodon-toot--edit-toot-at-point)
    ("d" mastodon-toot--delete-toot)
    ("E" mastodon-toot--view-toot-edits)

    ("I" mastodon-tl--view-filters)
    ("X" mastodon-tl--view-lists)
    ("C" mastodon-toot--copy-toot-url)
    ("S" mastodon-search--search-query)
    ("h" describe-mode)

    ("q" doom/escape)
  )
  (map! :map mastodon-mode-map
        "?" #'mastodon-help/body
        "SPC" #'mastodon-tl--more)

  ;; (mastodon-discover)
)



;;; Package: comm/notmuch

;; https://github.com/gauteh/lieer
;; - ~/mail/account.google/
;; - download client_secret.json from https://github.com/gauteh/lieer#using-your-own-api-key
;; - consider adding "spam" into the array of ignore_remote_labels of .gmailier.json
;; - cd .... && gmi init -c client_secret.json
;; - cd .... && gmi pull
;; - cd .... && gmi pull --resume
;; - cd .... && gmi sync

;; https://notmuchmail.org/
;; - ~/.notmuch-config
;;   [database]
;;   path=/home/holger/.mail
;;   [database]
;;   path=/home/holger/.mail
;;   [user]
;;   name=Holger Schurig
;;   primary_email=holgerschurig@gmail.com
;;   [new]
;;   tags=new
;;   ignore=.uidvalidity;.mbsyncstate;/.*[.](json|lock|bak)$/
;;   [search]
;;   exclude_tags=deleted;spam
;;   [maildir]
;;   synchronize_flags=true

;; https://afew.readthedocs.io/en/latest/
;; - ~/.config/afew/config
;;   [SpamFilter]
;;   [KillThreadsFilter]
;;   [ListMailsFilter]
;;   [ArchiveSentMailsFilter]
;;   [InboxFilter]

;; Polling mail
;; - ~/bin/pollmail
;;   #!/bin/sh
;;   cd ~/.mail
;;   gmi sync
;;   notmuch new
;;   afew --tag --new
;;   notmuch tag -inbox -- tag:lists

;; (use-package! notmuch
;;   :defer t
;;   :config
(after! notmuch

  ;; Change flagged lines to be dark blue in the background, not blue
  ;; in the foreground. This harmonizes much more with my dark theme.
  (add-to-list 'notmuch-search-line-faces '("flagged" :background "dark blue"))

  ;; Modify search results. First the columns:
  (setq notmuch-search-result-format
        '(("date" . "%10s ")     ;; YYYY-MM-DD
          ("count" . "%-6s ")
          ("authors" . "%-23s ")
          ("subject" . "%s ")
          ("tags" . "(%s)")
          ))

  ;; Redefine with these changes:
  ;; - never use the relative date, instead use YYYY-MM-DD
  ;;   (see also notmuch-show-relative-dates)
  ;; - the counts don't have the useless brackes: 2/2 instead of [2/2]
  (defun notmuch-search-insert-field (field format-string result)
    (cond
     ((string-equal field "date")
      (insert (propertize (format format-string (format-time-string "%Y-%m-%d" (plist-get result :timestamp)))
                          'face 'notmuch-search-date)))
     ((string-equal field "count")
      (insert (propertize (format format-string
                                  (format "%s/%s" (plist-get result :matched)
                                          (plist-get result :total)))
                          'face 'notmuch-search-count)))
     ((string-equal field "subject")
      (insert (propertize (format format-string
                                  (notmuch-sanitize (plist-get result :subject)))
                          'face 'notmuch-search-subject)))
     ((string-equal field "authors")
      (notmuch-search-insert-authors
       format-string (notmuch-sanitize (plist-get result :authors))))
     ((string-equal field "tags")
      (let ((tags (plist-get result :tags))
            (orig-tags (plist-get result :orig-tags)))
        (insert (format format-string (notmuch-tag-format-tags tags orig-tags)))))))

  ;; Easy deletion of messages with DEL
  (defun my-notmuch-search-delete (&rest beg end)
    (interactive)
    (notmuch-search-tag '("-unread" "-new" "-inbox" "-sent" "+trash") beg end)
    (notmuch-tree-next-message))
  (define-key! :keymaps 'notmuch-search-mode-map
    [delete] #'my-notmuch-search-delete)

  ;; Don't save message in some directory
  (setq notmuch-fcc-dirs nil)

  (setq-default notmuch-search-oldest-first nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-hello
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Display 3 columns
  (setq notmuch-column-control 0.33)

  ;; My own saved searches
  (setq notmuch-saved-searches
        ;; :name            Name of the search (required).
        ;; :query           Search to run (required).
        ;; :key             Optional shortcut key for ‘notmuch-jump-search’.
        ;; :count-query     Optional extra query to generate the count
        ;;                  shown. If not present then the :query property
        ;;                  is used.
        ;; :sort-order      Specify the sort order to be used for the search.
        ;;                  Possible values are ‘oldest-first’, ‘newest-first’
        ;;                  or nil. Nil means use the default sort order.
        ;; :search-type     Specify whether to run the search in search-mode,
        ;;                  tree mode or unthreaded mode. Set to ’tree to specify tree
        ;;                  mode, ’unthreaded to specify unthreaded mode, and set to nil
        ;;                  (or anything except tree and unthreaded) to specify search mode.
        '((:name "inbox"     :key "i" :query "NOT tag:trash AND tag:inbox")
          (:name "unread"    :key "u" :query "NOT tag:thash AND tag:unread")
          (:name "alt"       :key "a" :query "NOT tag:trash AND tag:Aufheben")
          (:name "linux-can" :key "c" :query "NOT tag:trash AND tag:lists/linux-can")
          (:name "etnaviv"   :key "e" :query "NOT tag:trash AND tag:lists/etnaviv")
          (:name "zephyr"    :key "z" :query "NOT tag:trash AND tag:lists/zephyr")

          (:name "Sent"      :key "S" :query "tag:sent")
          (:name "Trash"     :key "T" :query "tag:trash")
          (:name "All"       :key "A" :query "*")))

  ;; List of tags to be hidden in the "all tags"-section.
  (setq notmuch-hello-hide-tags '("important" "signed"))

  (setq notmuch-hello-sections (list
                                ;; Insert the default notmuch-hello header.
                                ;; #'notmuch-hello-insert-header

                                ;; Show an entry for each saved search and inboxed messages for each tag.
                                ;; But as I remove "inbox" tag from mailing list mails, this isn't really useful.
                                ;; Maybe some "unread" thing could be helpful?
                                ;; #'notmuch-hello-insert-inbox

                                ;; Insert the saved-searches section.
                                #'my-notmuch-hello-insert-saved-searches

                                ;; Insert a search widget
                                #'notmuch-hello-insert-search

                                ;; Insert up to notmuch-hello-recent-searches-max recent searches.
                                #'my-notmuch-hello-insert-recent-searches

                                ;; Insert a section displaying all tags and associated message counts.
                                #'notmuch-hello-insert-alltags

                                ;; Insert the notmuch-hello footer.
                                ;; #'notmuch-hello-insert-footer

                                #'my-notmuch-hello-insert-help
                                ))

  ;; The space as thousand separator looks odd, make it more european
  (setq notmuch-hello-thousands-separator ".")

  (defun my-notmuch-pollmail ()
    (interactive)
    (start-process "pollmail" ;; NAME
                   "*notmuch-pollmail*" ;; BUFFER
                   (expand-file-name "~/.local/bin/pollmail"))  ;; PROGRAM
    (message "pollmail running ..."))

  ;; Redesign of the hello screen
  (defun my-notmuch-hello-insert-recent-searches ()
    "Insert recent searches, but without the clear button."
    (when notmuch-search-history
      (widget-insert "Recent searches:\n\n")
      (let ((width (notmuch-search-item-field-width)))
        (dolist (search (seq-take notmuch-search-history
                                  notmuch-hello-recent-searches-max))
          (widget-create 'notmuch-search-item :value search :size width)))))

  (defun my-notmuch-hello-insert-saved-searches ()
  "Insert the saved-searches section, but without the edit button."
  (let ((searches (notmuch-hello-query-counts
                   (if notmuch-saved-search-sort-function
                       (funcall notmuch-saved-search-sort-function
                                notmuch-saved-searches)
                     notmuch-saved-searches)
                   :show-empty-searches notmuch-show-empty-saved-searches)))
    (when searches
      (widget-insert "Saved searches:\n\n")
      (let ((start (point)))
        (notmuch-hello-insert-buttons searches)
        (indent-rigidly start (point) notmuch-hello-indent)))))

  (defun my-notmuch-hello-insert-help ()
    (widget-insert "―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――\n")
    (widget-insert "c compose         s notmuch search  u search unthreaded\n")
    (widget-insert "j jump            t tag search      s search threaded\n")
    (widget-insert "G check new mail                    z search tree formatted\n"))

  (define-key! :keymaps 'notmuch-hello-mode-map
    "G" #'my-notmuch-pollmail
    "c" #'consult-notmuch-address) ;; used to be notmuch-mua-new-mail

  ;; Tell the rest of Emacs that we are now using notmuch
  (setq mail-user-agent 'notmuch-user-agent)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-mua
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; We don't need to cite everything
  (setq notmuch-mua-cite-function #'message-cite-original-without-signature)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-show
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The builtin imenu cannot display leading spaces, but consult's imenu
  ;; doesn't have a problem with that:
  (setq notmuch-show-imenu-indent t)

  ;; Probably not needed, as I am using iso date anyway
  (setq notmuch-show-relative-dates nil)

  ;; show the all-tags list
  (setq notmuch-show-all-tags-list t)

  (add-hook 'notmuch-show-mode-hook #'scroll-lock-mode)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-search
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun my-notmuch-show-mails (&optional elide-toggle)
    "Show one mail directly, or several mails in tree format"
    (interactive "P")
    (let* ((thread-id (notmuch-search-find-thread-id))
           (result (get-text-property (point) 'notmuch-search-result)))
      ;; one message?
      (if (= (plist-get result :total) 1)
          (notmuch-search-show-thread elide-toggle)
        (notmuch-tree thread-id notmuch-search-query-string))))

  (define-key! :keymaps 'notmuch-search-mode-map
    "RET" #'my-notmuch-show-mails)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-tag
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Programmatic tagging:
  ;;
  ;; db = notmuch.Database("/home/holger/.mail", mode=notmuch.Database.MODE.READ_WRITE)
  ;; query = ""
  ;; messages = notmuch.Query(db, query).search_messages()
  ;; for m in messages:
  ;;      t = list(m.get_tags())
  ;;      if len(t) != 0:
  ;;              continue
  ;;      print(m)

  ;; Hide some tags. Not optimal, as we will get some trailing spaces in the display.
  ;; and make the flagged tag be an star icong
  (setq notmuch-tag-formats
        '(
          ;; ("signed" (propertize tag 'invisible t))
          ;; ("lists" (propertize tag 'invisible t))
          ;; ("attachment" (propertize tag 'invisible t))
          ("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flagged" (propertize tag 'face 'notmuch-tag-flagged)
           (notmuch-tag-format-image-data tag (notmuch-tag-star-icon)))
          ))

  (add-hook 'notmuch-after-tag-hook #'notmuch-indicator-refresh)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; notmuch-tree
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; show mail completely, not in a split window mode
  (setq notmuch-tree-show-out t)

)

(defun my-notmuch-hello ()
  (interactive)
  (require 'notmuch)
  (notmuch-hello))

(define-key!
  "M-g n"   #'my-notmuch-hello ;; was next-error
  "M-g C-n" #'consult-notmuch-address
  "M-g p"   nil             ;; was previous-error
)




;;; Package: comm/notmuch-indicator

(use-package notmuch-indicator
  :config
  (setq notmuch-indicator-args '((:terms "tag:unread" :label "@")))
  (setq notmuch-indicator-hide-empty-counters t)

  (notmuch-indicator-mode +1)
)




;;; Package: comm/sendmail

(after! sendmail
  ;; use GMI to send mails
  (setq sendmail-program (expand-file-name "~/.local/bin/gmi")
        message-sendmail-extra-arguments '("send"
                                           "--quiet"
                                           "--read-recipients"
                                           "--path" "~/.mail"))

  ;; not sure if I need this ... but it doesn't hurt :-)
  (setq mail-specify-envelope-from t)
  )
