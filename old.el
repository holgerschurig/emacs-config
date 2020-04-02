;;; License

;; All code sections in this .org file are licensed under GPLv2 /
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html except
;; otherwise noted. For example, I derives some functions from
;; notmuch. And as notmuch uses GPLv3 or higher, this functions got
;; the same license.



;;; Error handling
;(toggle-debug-on-error)
(setq debugger-stack-frame-as-list t)


(defconst my-use-helm nil
  "Should use helm instead of ivy?")


;;; Package: straight

;; I am using a package manager called straight.el. This code, which
;; is taken from the README [1], bootstraps the system (because
;; obviously the package manager is unable to install and load itself,
;; if it is not already installed and loaded).
;;
;; See https://github.com/raxod502/straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (setq straight-check-for-modifications nil)
  (load bootstrap-file nil 'nomessage))



;;; Edit window: Truncation

;; Don't display continuation lines
(setq-default truncate-lines t)

;; Do `M-x toggle-truncate-lines` to toggle truncation mode.
;; `truncate-partial-width-windows' has to be nil for
;; `toggle-truncate-lines' to work in split windows
(setq truncate-partial-width-windows nil)


;;; GUI: Scratch message

;; Empty scratch message
(setq initial-scratch-message nil)

;;; GUI: Tool bar

(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))

;; GUI: Scroll bar

(unless (eq system-type 'windows-nt)
  (scroll-bar-mode -1))

;;; GUI: simpler yes or no prompt

;  Get rid of yes-or-no questions - y or n is enough
(fset 'yes-or-no-p 'y-or-n-p)



;;; Buffers: Buffers without toolbar, extra frame etc

(add-to-list 'special-display-buffer-names "*Backtrace*")
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))



;;; Window handling: Modify (delete-window)

;; If only one window in frame, `delete-frame'.
;;
;; From http://www.emacswiki.org/emacs/frame-cmds.el

(defadvice delete-window (around delete-window (&optional window) activate)
  (interactive)
  (save-current-buffer
	(setq window (or window (selected-window)))
	(select-window window)
	(if (one-window-p t)
	(delete-frame)
	  ad-do-it (selected-window))))



;;; Window handling: New (kill-buffer-and-window)

;; Replacement for interactive `kill-buffer'. We cannot redefine
;; `kill-buffer', because other elisp code relies on it's exact
;; behavior.

(defun my--kill-buffer-and-window (&optional buffer)
  "Kill buffer BUFFER-OR-NAME.
The argument may be a buffer or the name of an existing buffer.
Argument nil or omitted means kill the current buffer. Return t
if the buffer is actually killed, nil otherwise.

Unlike `kill-buffer', this also will delete the current window if
there are several windows open."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (unless (one-window-p)
	(delete-window))
  (kill-buffer buffer))

(bind-key "C-x k" 'my--kill-buffer-and-window)



;;; Window handling: Window zooming (F5)

;; If there is only one window displayed, act like "C-x 2". If there are
;; two windows displayed, act like "C-x 1".

(defun my-zoom-next-buffer2 ()
  (let ((curbuf (current-buffer))
	(firstbuf nil))
	(dolist (buffer (buffer-list))
	  (with-current-buffer buffer
	;(princ (format "name %s, fn %s\n" (buffer-name) buffer-file-name))
	(unless (or
		 ;; Don't mention internal buffers.
		 (string= (substring (buffer-name) 0 1) " ")
		 ;; No buffers without files.
		 (not buffer-file-name)
		 ;; Skip the current buffer
		 (eq buffer curbuf)
		 )
	  ;(princ (format " nme %s, fn %s\n" (buffer-name) buffer-file-name))
	  (unless firstbuf
		(setq firstbuf buffer))
		;;(print buffer)
	  )))
	(when firstbuf
	  ;(princ (format "new buffer: %s.\n" firstbuf))
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

(bind-key "<f5>" 'my-explode-window)



;;; Window handling: Windows toggle / Buffer switching (F6)

;; If there is only one window displayed, swap it with previous buffer.
;; If there are two windows displayed, act like "C-x o".

;; See also Shift-F6 !

(defun my-switch-to-buffer ()
  "If there is only one window displayed, swap it with previous buffer.
If there are two windows displayed, act like \"C-x o\"."
  (interactive)
  (if (one-window-p t)
	  (switch-to-buffer (other-buffer (current-buffer) 1))
	(other-window -1)))

(bind-key "<f6>" 'my-switch-to-buffer)



;;; Cursor movement

;; First we define code that allows us to bind multiple functions to
;; repeated commands. Taken from
;; http://www.emacswiki.org/cgi-bin/wiki/DoubleKeyBinding:

(defvar seq-times 0
  "Stores number of times command was executed.  It cotnains
random data before `seq-times' macro is called.")

(defmacro seq-times (&optional name max &rest body)
  "Returns number of times command NAME was executed and updates
`seq-times' variable accordingly.  If NAME is nil `this-command'
will be used.  If MAX is specified the counter will wrap around
at the value of MAX never reaching it.  If body is given it will
be evaluated if the command is run for the first time in a
sequence."
  (declare (indent 2))

  ;; Build incrementation part
  (setq max (cond ((null max) '(setq seq-times (1+ seq-times)))
		  ((atom max) (if (and (integerp max) (> max 0))
				  `(setq seq-times (% (1+ seq-times) ,max))
				'(setq seq-times (1+ seq-times))))
		  (t          `(let ((max ,max))
				 (if (and (integerp max) (> max 0))
					 (setq seq-times (% (1+ seq-times) max))
				   (setq seq-times (1+ seq-times)))))))

  ;; Make macro
  (if (eq name 'last-command)
	  max
	(cond ((null  name) (setq name 'this-command))
	  ((consp name) (setq name `(or ,name this-command))))
	`(if (eq last-command ,name)
	 ,max
	   ,@body
	   (setq seq-times 0))))

(defmacro seq-times-nth (name body &rest list)
  "Calls `seq-times' with arguments NAME, length and BODY
and (where length is the number of elements in LIST) then returns
`seq-times'th element of LIST."
  (declare (indent 2))
  `(nth (seq-times ,name ,(length list) ,body) ',list))

(defmacro seq-times-do (name body &rest commands)
  "Calls `seq-times' with arguments NAME, length and BODY (where
length is the number of COMMANDS) and then runs `seq-times'th
command from COMMANDS."
  (declare (indent 2))
  `(eval (nth (seq-times ,name ,(length commands) ,body) ',commands)))



;; Home / End

(defvar my--previous-position)

(defun my-home ()
  "Depending on how many times it was called moves the point to:

   - begin of indentation
   - beginning of line
   - begin of function
   - beginning of buffer
   - back to where it was"
  (interactive)
  (seq-times-do nil (setq my--previous-position (point))
	(back-to-indentation)
	(beginning-of-line)
	(beginning-of-defun)
	(goto-char (point-min))
	(goto-char my--previous-position)))

(bind-key "C-a" 'my-home)
(bind-key "<home>" 'my-home)

(defun my-end ()
  "Depending on how many times it was called moves the point to:

   - end of line
   - end of function
   - end of buffer
   - back to where it was"
  (interactive)
  (seq-times-do nil (setq my--previous-position (point))
	(end-of-line)
	(forward-paragraph)
	(end-of-defun)
	(goto-char (point-max))
	(goto-char my--previous-position)))

(bind-key "C-e" 'my-end)
(bind-key "<end>" 'my-end)



;; Recenter

(setq recenter-positions '(middle 4 -4))




;; TODO Smooth scrolling

;; Normally, at the top-of-screen or bottom-of-screen, emacs would
;; scroll quarter-page wise. This is a bit annoying, because I'll
;; always have to chase the cursor when this happens. But there's a
;; cure:

(setq scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)



;;; File opening/saving

;; Never show GTK file open dialog

(setq use-file-dialog nil)


;; DONE Don't create those pesky .#foo lockfiles:

(setq create-lockfiles nil)


;; DONE Kill means kill, not asking.

(setq kill-buffer-query-functions nil)


;; DONE Decompress compressed files

(auto-compression-mode t)




;; Automatically load .Xresources after changes

;; Sample ~/.Xresources:

;; Emacs.geometry: 120x55
;; Emacs.Font:	terminus 11

(defun merge-x-resources ()
  (let ((file (file-name-nondirectory (buffer-file-name))))
	(when (or (string= file ".Xdefaults")
		  (string= file ".Xresources"))
	  (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
	  (message (format "Merged %s into X resource database" file)))))
(add-hook 'after-save-hook 'merge-x-resources)


;;; Command: (calc-region)
;; From https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/:

;; Write some expression, e.g. "2+2*4" and then press "C-=".


(defun calc-region (arg)
  "Evaluate an expression in calc and communicate the result.

If the region is active evaluate that, otherwise search backwards
to the first whitespace character to find the beginning of the
expression. By default, replace the expression with its value. If
called with the universal prefix argument, keep the expression
and insert the result into the buffer after it. If called with a
negative prefix argument, just echo the result in the
minibuffer."
  (interactive "p")
  (let (start end)
	(if (use-region-p)
	(setq start (region-beginning) end (region-end))
	  (progn
	(setq end (point))
	(setq start (search-backward-regexp "\\s-\\|\n" 0 1))
	(setq start (1+ (if start start 0)))
	(goto-char end)))
	(let ((value (calc-eval (buffer-substring-no-properties start end))))
	  (pcase arg
	(1 (delete-region start end))
	(4 (insert " = ")))
	  (pcase arg
	((or 1 4) (insert value))
	(-1 (message value))))))
(bind-key "C-=" #'calc-region)




;;; Builtin package: abbrev

(use-package abbrev
  :if (not noninteractive)
  :defer t
  :config
  (setq abbrev-file-name (locate-user-emacs-file "tmp/abbrev_defs.el")))


;;; Builtin package: apropos

(use-package apropos
  :if (not noninteractive)
  :bind ("C-h a" . apropos)) ;; was: apropos-command


;;; Builtin package: bookmark - set bookmarks, maybe annotate them, jump to them later

(use-package bookmark
  :if (not noninteractive)
  :config
  (setq bookmark-default-file (locate-user-emacs-file "tmp/bookmarks.el")))


;;; Builtin package: bytecomp

(use-package bytecomp
  ;; :hook (after-save . my-byte-compile)

  :init
  (defun my-byte-compile ()
	"Byte-compile an .el file, if it's is in the `user-emacs-directory'."
	(interactive)
	(when (and (string= (file-name-directory (buffer-file-name)) (expand-file-name user-emacs-directory))
               (string= (file-name-extension (buffer-file-name)) "el"))
      (byte-compile-file (buffer-file-name) nil)))

  :config
  (setq byte-compile-verbose nil))


;;; Builtin package: cc-mode - mayor mode for C and C++

(use-package cc-mode
  ;; open *.h files normally in c++ mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
	 ("\\.inl\\'"                 . c++-mode))

  :config

  ;; This makes things like super_function_for_you a word
  (modify-syntax-entry ?_ "w")

  (setq fill-column 78)

  ;; Let RET break and continue a comment
  ;; C doesn't start functions with a ( in the first column
  (setq open-paren-in-column-0-is-defun-start nil)

  ;; Tab behavior
  (setq c-tab-always-indent nil  ;; insert real tab
	c-insert-tab-function 'indent-for-tab-command)

  ;; for C and C++ files
  (defun my-c-mode-common-setup ()
    (turn-off-auto-fill))
  (add-hook 'c-mode-common-hook 'my-c-mode-common-setup)

  (defun my-c-mode-setup ()
    ;; need to check the mode because I run this also at the revert hook!
    (when (derived-mode-p 'c++-mode 'c-mode)
	  (yas-minor-mode-on)
	  (set (make-local-variable 'company-backends)
           '((company-yasnippet
			  company-lsp
			  company-files
			  ;; company-dabbrev-code
			  )))
      (if (and buffer-file-name (string-match "/linux" buffer-file-name))
		  ;; only for Linux C files
		  (progn (c-set-style "linux-tabs-only")
				 (setq tab-width 8
					   c-basic-offset 8))
		(progn
		  (c-set-style "linux")
		  (setq tab-width 4
				c-basic-offset 4)))))
  (add-hook 'c-mode-hook 'my-c-mode-setup)
  (add-hook 'c++-mode-hook 'my-c-mode-setup)
  (add-hook 'after-revert-hook 'my-c-mode-setup))


;;; Builtin package: cc-vars

(use-package cc-vars
  :defer t
  :config
  (setq c-default-style '((java-mode . "java")
						  (awk-mode . "awk")
						  (other . "linux"))))


;;; Builtin package: cc-styles

(use-package cc-styles
  :commands (c-add-style)
  :config
  ;; Default style
  (c-add-style "linux-tabs-only"
	       '("linux" (c-offsets-alist (arglist-cont-nonempty
					   c-lineup-gcc-asm-reg
					   c-lineup-arglist-tabs-only)))))


;;; Builtin package: comint

(use-package comint
  :if (not noninteractive)
  :bind (:map comint-mode-map
			  ("<down>" . comint-next-input)
			  ("<up>"   . comint-previous-input)
			  ("C-n"    . comint-next-input)
			  ("C-p"    . comint-previous-input)
			  ("C-r"    . comint-history-isearch-backward))

  :config
  ;; Make the prompt readonly
  (setq comint-prompt-read-only t)
  ;; Activate isearch
  (setq comint-history-isearch t)
)


;;; Builtin package: eshell
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell

(use-package eshell
  :if (not noninteractive)
  :commands (eshell eshell/addpath eshell-read-aliases-list)
  :defines (eshell-visual-commands)
  :config
  (defun eshell/clear ()
	"Deletes the contents of eshell buffer, except the last prompt"
	(save-excursion
	  (goto-char eshell-last-output-end)
	  (let ((lines (count-lines 1 (point)))
		(inhibit-read-only t))
	(beginning-of-line)
	(let ((pos (point)))
	  (if (bobp)
		  (if (called-interactively-p 'interactive)
			  (error "Buffer too short to truncate"))
		(delete-region (point-min) (point)))))))

  ;; If I ever want my own eshell/foo commands overwrite real commands ...
  (setq eshell-prefer-lisp-functions t)

  ;; check if this is ok for my usage
  ;; eshell-visual-commands

  (defun my--eshell-hook ()
	(eshell-read-aliases-list)
	(setq global-hl-line-mode nil)
	(setq show-trailing-whitespace nil)
	(add-to-list 'eshell-visual-commands "ssh")
	(add-to-list 'eshell-visual-commands "htop")
	(add-to-list 'eshell-visual-commands "ncmpcpp")
	(add-to-list 'eshell-visual-commands "tail")
	(eshell/addpath "~/bin"))
  (add-hook 'eshell-mode-hook 'my--eshell-hook)

  ;; If I ever want my own eshell/foo commands overwrite real commands ...
  (setq eshell-prefer-lisp-functions t)

  (setq eshell-directory-name (locate-user-emacs-file "tmp")))

(use-package em-banner
  :if (not noninteractive)
  :defer t
  :defines (eshell-banner-message)
  :config
  ;; We don't need no banner
  (setq eshell-banner-message ""))


;;; Builtin package: flyspell - background spell checker

(use-package flyspell
  :disabled t
  :if (eq system-type 'gnu/linux)
  :diminish flyspell-mode
  :commands (flyspell-mode flyspell-prog-mode)

  :config
  (add-to-list 'flyspell-dictionaries-that-consider-dash-as-word-delimiter "german-new8")
  (setq flyspell-issue-welcome-flag nil)
  ;; M-Tab is owned by the window manager, correct with C-M-i
  (setq flyspell-use-meta-tab nil))


;; Flyspell is in elisp mode. And this in turn loads flyspell directly
;; after launching emacs, which is a bit unfortunate.

;; (defun my-flyspell-prog-mode ()
;;   (interactive)
;;   (unless (string= (buffer-name) "*scratch*")
;; 	(flyspell-prog-mode)))
;; (when (eq system-type 'gnu/linux)
;;   (add-hook 'prog-mode-hook  #'my-flyspell-prog-mode)
;;   (add-hook 'text-mode-hook  #'flyspell-mode)
;;   (add-hook 'org-mode-hook   #'flyspell-mode)
;;   (add-hook 'latex-mode-hook #'flyspell-mode)
;;   (add-hook 'LaTeX-mode-hook #'flyspell-mode))


;;; Builtin package: help-mode

(use-package help-mode
  ;; Make 'b' (back) go to the previous position in emacs help.
  :bind (:map help-mode-map
			  ("b" . help-go-back)))


;;; Builtin package: message

(use-package message
  :commands (message-mode message-cite-original-without-signature)
  :config

  ;; When composing a mail, start the auto-fill-mode.
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  ;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

  ;; Generate the mail headers before you edit your message.
  (setq message-generate-headers-first t)

  ;; The message buffer will be killed after sending a message.
  (setq message-kill-buffer-on-exit t)

  ;; When I reply, I don't want to have me in To or Cc
  (setq message-dont-reply-to-names (concat "\\("
											user-mail-address
											;; Nor the Debian BTS
											;; "\\|^submit@bugs.debian\\.org$"
											"\\)"))

  ;; based on http://mbork.pl/2016-02-06_An_attachment_reminder_in_mu4e
  (defun my-message-attachment-present-p ()
	"Return t if an attachment is found in the current message."
	(save-excursion
	  (save-restriction
		(widen)
		(goto-char (point-min))
		(when (search-forward "<#part" nil t) t))))

  (defvar my-message-attachment-intent-re
	(regexp-opt '("I attach"
				  "I have attached"
				  "I've attached"
				  "I have included"
				  "I've included"
				  "see the attached"
				  "see the attachment"
				  "attached file"))
	"A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning.")

  (defvar my-message-attachment-reminder
	"Are you sure you want to send this message without any attachment? "
	"The default question asked when trying to send a message
containing `my-message-attachment-intent-re' without an
actual attachment.")

  (defun my-message-warn-if-no-attachments ()
	"Ask the user if s?he wants to send the message even though
there are no attachments."
	(when (and (save-excursion
				 (save-restriction
				   (widen)
				   (goto-char (point-min))
				   (re-search-forward my-message-attachment-intent-re nil t)))
			   (not (my-message-attachment-present-p)))
	  (unless (y-or-n-p my-message-attachment-reminder)
		(keyboard-quit))))

  (add-hook 'message-send-hook #'my-message-warn-if-no-attachments))


;;; Builtin package: mm-decode

(use-package mm-decode
  :defer t
  :config
  ;; Displaying zip/tar inline is a really, really stupid default!
  (setq mm-inlined-types
		(cl-remove-if (apply-partially #'string-match-p "\\(x-g?tar\\|zip\\)")
					  mm-inlined-types)))


;;; Builtin package: mwheel - mouse scrolling

;; Make the mouse wheel scroll smoother.

(use-package mwheel
  :config
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil))
        mouse-wheel-progressive-speed nil))


;;; Builtin package: newcomment

(use-package newcomment
  :bind ("C-c c" . comment-dwim))


;;; Builtin package: nxml-mode: mayor mode for XML files

(use-package nxml-mode
  :mode ("\\.xml$" . nxml-mode)
  :commands (indent-xml-region)
  :defer t
  :config
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t)
  ;; (add-hook 'nxml-mode-hook (lambda () (emmet-mode t)))))

 (defun indent-xml-region (begin end)
   "Pretty format XML markup in region. The function inserts
 linebreaks to separate tags that have nothing but whitespace
 between them. It then indents the markup by using nxml's
 indentation rules."
   (interactive "r")
   (save-excursion
	 (nxml-mode)
	 (goto-char begin)
	 (while (search-forward-regexp "\>[ \\t]*\<" nil t)
	   (backward-char) (insert "\n"))
	 (indent-region begin end))))


;;; Builtin package: octave-mode: mayor-mode for GNU Octave

(use-package octave
  :mode (("\\.m\\'"       . octave-mode)))


;;; Builtin package: project

(use-package project
  :commands (project-current)
  :init
  (defun my-project-based-on-git (dir)
	"Find project root based on .git:"
	(message (concat "DIR " dir))
	(let ((root (locate-dominating-file dir ".git")))
	  (and root (cons 'transient root))))
  ;; test it: (project-current)

  :config
  (setq project-find-functions #'my-project-based-on-git))



;;; Builtin package: python - mayor mode for Python

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
  (add-hook 'python-mode-hook 'my-python-setup))


;;; Builtin package: paren - let parenthesis behave

(use-package paren
  ;; TODO :if (not noninteractive)
  ;; TODO :defer nil
  :commands (show-paren-mode)
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'mixed)
  (setq show-paren-delay 0))


;;; Builtin package: recentf - store all edited file names

(use-package recentf
  :if (not noninteractive)
  :defer nil
  :commands (recentf-mode)
  :config
  (setq recentf-save-file (locate-user-emacs-file "tmp/recentf.el")
		recentf-exclude '("^/tmp/"
						  "/\\.newsrc"
						  ".*CMakeFiles.*"
						  "bbdb$"
						  "svn-commit\\.tmp$"
						  ".*-autoloads\\.el\\'"
						  "\\.png$"
						  "COMMIT_EDITMSG" "COMMIT_EDITMSG" "TAG_EDITMSG")
		recentf-max-saved-items 1000
		recentf-auto-cleanup 300
		recentf-max-menu-items 20)
  (recentf-mode 1))


;;; Builtin package: sendmail
(use-package sendmail
  :defer t
  :commands (mail-mode mail-text)
  :defines (send-mail-function)
  :config

  (setq send-mail-function 'sendmail-send-it
		sendmail-program "/usr/bin/msmtp"
		mail-specify-envelope-from t))


;;; Builtin package: shr - Simple HTML Renderer, used by elfeed)

(use-package shr
  :defer t
  :config
  ;; don't use the (ugly)proportional font
  (setq shr-use-fonts nil))


;;; Builtin package: term - for ansi-term
;; This term understands ansi escape sequences.
;; On Debian, one should install the ansi-term debian package so that
;; the terminal "eterm-color" is available.

(use-package term
  :bind ("M-g s" . ansi-shell)
  :commands (ansi-term ansi-shell
					   term-in-line-mode
					   term-line-mode
					   term-char-mode)
  :defines (term-buffer-maximum-size
			show-dir-in-mode-line?)
  :init
  (defun ansi-shell ()
	"Start ansi-term with bash"
	(interactive)
	(ansi-term "/bin/bash"))

  :config
  ;; don't linger around when closing
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
	(if (memq (process-status proc) '(signal exit))
		(let ((buffer (process-buffer proc)))
		  ad-do-it
		  (kill-buffer buffer))
	  ad-do-it))
  (ad-activate 'term-sentinel)

  (defun term-toggle-mode ()
	(interactive)
	(if (term-in-line-mode)
		(term-char-mode)
	  (term-line-mode)))

  (defun my-term-hook ()
	(goto-address-mode)
	;; (bind-key "C-c C-j" #'term-toggle-mode term-mode-map)
	;; (bind-key "C-c C-k" #'term-toggle-mode term-mode-map)
	(setq global-hl-line-mode nil)
	(setq term-buffer-maximum-size 10000)
	(setq-local show-dir-in-mode-line? t) ;; also mode linec'
	(setq show-trailing-whitespace nil)
	;; disable company in favor of shell completion
	;;(company-mode -1)
	)
  (add-hook 'term-mode-hook 'my-term-hook))


;;; Builtin package: tex-mode

(use-package tex-mode
  :mode ("\\.tex\\'" . latex-mode))



;;; Builtin package: time

;; (display-time-world)
(use-package time
  :defer t
  :config
  (setq display-time-world-time-format "%d.%m %R %Z"
		display-time-world-list '(("Europe/Berlin" "Frankfurt")
								  ("US/Arizona"    "Tucson")
								  ("Asia/Taipei"   "Taiwan"))))


;;; Package: auto-compile - compile saving

(use-package auto-compile
  :disabled t
  :straight t
  :demand t
  :hook (auto-compile-inhibit-compile . auto-compile-inhibit-compile-detached-git-head)

  :init
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-update-autoloads t)

  :config
  ;; (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))


;;; Package: avy - jump to characters

(use-package avy
  :straight t
  :bind ("C-ö" . avy-goto-char-timer)

  :config
  (setq avy-keys (append (number-sequence ?a ?z)
						 (number-sequence ?0 ?9)))
  (setq avy-style 'at-full)
  (setq avy-all-windows nil)
  (setq avy-highlight-first t))


;;; Package: avy-zap - delete up to a character

;; This makes "M-z" ask via avy to which character text should be
;; deleted. The character itself will stay. If you use "M-Z", then this
;; character will be gone, too.

(use-package avy-zap
  :straight t
  :bind (("M-z" . avy-zap-up-to-char-dwim)
		 ("M-Z" . avy-zap-to-char-dwim)))


;;; Package: bind-key

;; One nice function here is (describe-personal-keybindings)

(use-package bind-key
  :straight t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))


;;; Package: browse-url

(use-package browse-url
  :commands (browse-url)

  :config
  (setq browse-url-browser-function 'browse-url-generic
		browse-url-generic-program "x-www-browser"))


;;; Package: column-marker

(use-package column-marker
  :straight t
  :commands (column-marker-1 column-marker-2)
  :init
  (defun my--column-marker-at-80 ()
	(interactive)
	(column-marker-2 80))
  (add-hook 'c-mode-hook 'my--column-marker-at-80))


;;; Package: company
;; This start company, but it isn't set to start itself after some timeout,
;; but instead is started by the TAB key
;;
;; See https://emacs.stackexchange.com/questions/12441/is-it-possible-to-start-company-completion-without-a-prefix

(use-package company
  :straight t
  :if (not noninteractive)
  :diminish company-mode
  :commands (company-mode-on company-complete-common-or-cycle)
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase company-backends)
  :functions (company-mode)

  :diminish t

  :bind (("C-ä" . company-complete)
		 :map company-active-map
		 ("<tab>"      . company-complete-common-or-cycle)
		 :map company-mode-map
		 ("C-M-i"      . company-complete-common-or-cycle)
		 :map company-active-map
		 ("ESC"        . company-abort)
		 ("C-g"        . company-abort)
		 ("<backtab>"  . company-complete-common-or-cycle-backward)
		 ("C-s"        . company-filter-candidates)
		 )

  :preface
  (defun my-company-complete ()
	(interactive)
  	(company-mode-on)
  	(company-complete-common-or-cycle))

  :init
  ;; This variable is used in indent-for-tab-command and calls and calls out to completion-at-point
  (setq tab-always-indent 'complete)

  ;; now we need to hook company into completion-at-point
  ;; (advice-add 'completion-at-point :override #'company-complete-common-or-cycle)
  (advice-add 'completion-at-point :override #'my-company-complete)

  :config
  ;; I don't want company to pop up automatically at all
  (setq company-idle-delay nil)

  ;; Number of suggestions
  (setq company-tooltip-limit 12)

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  (setq company-backends '(;; company-bbdb
						   company-nxml
						   company-css
						   ;; company-elisp       ; now done via company-capf
						   ;; company-eclim
						   ;; company-semantic    ; CEDET semantic completion
						   ;; company-clang
						   ;; company-xcode
						   ;; company-cmake
						   company-capf           ; hook into completion-at-point-functions
						   company-files          ; files and directories
						   (company-dabbrev-code  ; all symbols of current buffer that aren't strings/code
							;; company-gtags      ; tags from GNU global
							company-etags         ; tags from etags
							company-keywords      ; programming language keywords
							)
						   ;; company-oddmuse     ; seldom used wiki
						   company-dabbrev        ; all string of buffer
						   )))



;;; Package: company-c-headers

(use-package company-c-headers
  :straight t
  :defer t
  :if (not noninteractive)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/qt4/qt/")
  (add-to-list 'company-c-headers-path-system "/usr/include/qt4/QtCore/")
  (add-to-list 'company-c-headers-path-system "/usr/include/qt4/QtGui/"))


;;; Package: company-dabbrev

(use-package company-dabbrev
  :if (not noninteractive)
  :commands (company-dabbrev)
  :after company
  :config
  ;; Prevent Company completions from being lowercased in the
  ;; completion menu. This has only been observed to happen for
  ;; comments and strings in Clojure. (Although in general it will
  ;; happen wherever the Dabbrev backend is invoked.)
  (setq company-dabbrev-downcase nil)

  ;; search other buffers with the same major mode
  (setq company-dabbrev-other-buffers t)

  ;; this also allows to complete on elisp sexp and keys
  (setq company-dabbrev-char-regexp "[a-zA-Z0-9_-:]")

  ;; Make company-dabbrev case-sensitive. Case insensitivity seems
  ;; like a great idea, but it turns out to look really bad when you
  ;; have domain-specific words that have particular casing.
  (setq company-dabbrev-ignore-case nil))


;;; Package: company-lsp

(use-package company-lsp
  :straight t
  :after company)


;;; Package: cmake-font-lock

(use-package cmake-font-lock
  :straight t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))



;;; Package: cmake-mode

(use-package cmake-mode
  :straight t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
)


;;; Package: d-mode - mayor mode for D

(use-package d-mode
  :straight t
  :defer t
  :mode ("\\.d\\'" . d-mode))


;;; Package: deadgrep - use ripgrep interactively

(use-package deadgrep
  :straight t
  :bind ("M-g r" . deadgrep))


;;; Package: erc

(use-package my-erc
  :defer t
  :commands (freenode oftc)
  :bind ("M-g e" . freenode))


;;; Package: eros - show results of elisp evaluations

;; Eros-mode will show you the result of evaluating an elisp command
;; as an overlay in your elisp buffer. Try it out with "C-x C-" now!

;; - https://www.reddit.com/r/emacs/comments/5iw5ml/eros_evaluation_result_overlays_for_emacs_lisp/
;; - https://github.com/xiongtx/eros

(use-package eros
  :straight t
  :commands (eros-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))


;;; Package: flx - fuzzy matching with good sorting

;; This package is used for fuzzy search in ivy.

(use-package flx
  :disabled t ;; only-for-ivy
  :if (not noninteractive)
  :straight t
  :defer t)


;;; Package: flycheck - compile-checking on-the-fly

;; | Key     | Function        |
;; |---------+-----------------|
;; | C-c ! l | list all issues |
;; | C-c ! n | next issue      |
;; | C-c ! p | previous issue  |

(use-package flycheck
  :straight t
  :commands (flycheck-mode)
  :diminish flycheck-mode

  :config
  ;; Make the list of errors be displayed full-screen
  (add-to-list 'display-buffer-alist
  			   `(,(rx bos "*Flycheck errors*" eos)
  				 (display-buffer-reuse-window display-buffer-same-window)
			   (reusable-frames . visible))))


;;; Package: go-mode - mayor mode for Go

(use-package go-mode
  :straight t
  :defer t
)


;;; Package: git-timemachine

(use-package git-timemachine
  :straight t
  :commands (git-timemachine))


;;; Package: groovy-mode (for Jenkinsfile)

(use-package groovy-mode
  :straight t
  :defer t
  :config
  (groovy-mode)
  :mode ("Jenkinsfile"))


;;; CANC Package: helpful

;; https://github.com/Wilfred/helpful

;; TODO: marry with helm
(use-package helpful
  :disabled t
  :straight t
  :commands (helpful-callable helpful-variable)
  :bind (("<f1>"  . helpful-at-point)
		 ("C-h k" . helpful-key))

  :config
  (setq helpful-switch-buffer-function 'pop-to-buffer-same-window))


;;; Package: hydra - interactive stateful keybindings

(use-package hydra
  :straight t
  :if (not noninteractive)
  :commands (defhydra hydra-default-pre hydra-keyboard-quit
			 hydra-set-transient-map
			 hydra--call-interactively-remap-maybe
			 hydra-show-hint))


;;; Package: lv - semi-permanent messages

;; This package provides `lv-message' intended to be used in place of
;; `message' when semi-permanent hints are needed, in order to not
;; interfere with Echo Area.

(use-package lv
  :defer t
  :commands (lv-message))


;;; Package: ispell

;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

;; on Debian, install:
;; - aspell
;; - aspell-de
;; - aspell-en

(use-package ispell
  :defer t
  :commands ()
  :init
  (defun ispell-english ()
	"Switch to an english dictionary"
    (interactive)
    (ispell-change-dictionary "en_US")
	(flyspell-mode 1)
    (flyspell-buffer))

  (defun ispell-german ()
	"Switch to a german dictionary"
    (interactive)
    (ispell-change-dictionary "german-new8")
	(flyspell-mode 1)
    (flyspell-buffer))

  :config
  ; Standard location of personal dictionary
  (setq ispell-personal-dictionary "~/.flydict")
  ;; set the default to english, files can switch locally to german
  (setq ispell-dictionary "en_US")
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args
        '("--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
		  "--ignore=3"))    ;; ignore words less than 3 characters long

  ;; Ignore org-mode properties
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  ;; Ignore source code blocks and examples
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))


;; The following change allows me to have some files flychecked in
;; german-new8 and others in english (the default). I just need to put
;; this line into the file:

;; #+BEGIN_EXAMPLE
;; # -*- ispell-local-dictionary: "german-new8" -*-
;; #+END_EXAMPLE

;; into the file.

(add-to-list 'safe-local-variable-values '(ispell-dictionary . german-new8))


;;; Package: helm
(use-package helm
  :straight t
  :if (and my-use-helm (not noninteractive))
  :diminish helm-mode
  :commands (my-helm-mini)
  :bind (("C-h a"   . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         )
  :bind-keymap ("C-c h"   . helm-command-prefix)

  :config
  ;; (use-package helm-config
  ;; 	:defines (helm-command-map))

  (helm-mode t)

  ;; Open full helm frame
  ;; (setq helm-full-frame t)
  ;; Put helm window on somewhere else
  (setq helm-split-window-default-side 'below)

  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-move-to-line-cycle-in-source t)

  )

(use-package helm-bookmark
  :if (and my-use-helm (not noninteractive))
  :defer t
  :bind ("M-g b" . helm-bookmarks))


(use-package helm-buffers
  :if (and my-use-helm (not noninteractive))
  :bind ("C-x C-b" . my-helm-mini)

  :config
  (defun my-helm-mini()
    "Like `helm-mini`, but run the dialog in full-screen"
    (interactive)
    (let ((helm-full-frame t))
      (helm-mini)))

  (setq helm-buffer-max-length 28))


(use-package helm-files
  :if (and my-use-helm (not noninteractive))
  :defer t
  :config

  ;; don't show things like .o
  (setq helm-ff-skip-boring-files t)
  (add-to-list 'helm-boring-file-regexp-list "\\.#") ;; Emacs save file

  ;; search for library in `require' and `declare-function' sexp.
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-ff-newfile-prompt-p nil)

  ;; Make BACKSPACE/DEL delete one character or the last path (if before a '/')
  ;; from https://github.com/hatschipuh/better-helm
  ;; (defun my-dwim-helm-find-files-up-one-level-maybe ()
  ;;      (interactive)
  ;;      (if (looking-back "/" 1)
  ;;              (call-interactively 'helm-find-files-up-one-level)
  ;;        (delete-char -1)))
  ;; (bind-key "<backspace>" #'my-dwim-helm-find-files-up-one-level-maybe helm-read-file-map)
  ;; (bind-key "<backspace>" #'my-dwim-helm-find-files-up-one-level-maybe helm-find-files-map)
  ;; (bind-key "DEL" #'my-dwim-helm-find-files-up-one-level-maybe helm-read-file-map)
  ;; (bind-key "DEL" #'my-dwim-helm-find-files-up-one-level-maybe helm-find-files-map)

  ;; This stops helm-find-files from pre-setting the input to the file
  ;; under point and/or changing to another directory. Somehow setting
  ;; setq helm-ff-guess-ffap-filenames and helm-ff-guess-ffap-urls
  ;; didn't do the job.
  (defun helm-find-files-initial-input (&optional input)
    "Dummy function from my config.el to disable this helm feature"
       nil
    )

  ;; If on a directory, switch helm to this directory. Don't call it with dired.
  ;; from https://github.com/hatschipuh/better-helm
  ;; (defun my-dwim-helm-find-files-navigate-forward (orig-fun &rest args)
  ;; "Adjust how helm-execute-persistent actions behaves, depending on context"
  ;; (if (file-directory-p (helm-get-selection))
  ;;        (apply orig-fun args)
  ;;      (helm-maybe-exit-minibuffer)))
  ;; (advice-add 'helm-execute-persistent-action :around #'my-dwim-helm-find-files-navigate-forward)
  ;; (bind-key "<return>" 'helm-maybe-exit-minibuffer helm-map)
  ;; (bind-key "RET" 'helm-maybe-exit-minibuffer helm-map)
  ;; (bind-key "<return>" 'helm-execute-persistent-action helm-find-files-map)
  ;; (bind-key "<return>" 'helm-execute-persistent-action helm-read-file-map)
  ;; (bind-key "RET" 'helm-execute-persistent-action helm-find-files-map)
  ;; (bind-key "RET" 'helm-execute-persistent-action helm-read-file-map)
)


(use-package helm-imenu
  :if (and my-use-helm (not noninteractive))
  :bind ("M-g i" . my-helm-imenu)
  :config
  (defun my-helm-imenu()
    "Like `helm-imenu`, but run the dialog in full-screen"
    (interactive)
    (let ((helm-full-frame t))
      (helm-imenu)))
 
  (setq helm-imenu-delimiter " - "))


(use-package helm-man
  :if (and my-use-helm (not noninteractive))
  :defer t
  :config
  ;; allow "find man at point" for C-c h m (helm-man-woman)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))


;; (use-package helm-net
;;   :if (not noninteractive)
;;   :defer t
;;   :config
;;   (when (executable-find "curl")
;;        (setq helm-net-prefer-curl t))
;;   (setq helm-browse-url-chromium-program "x-www-browser")
;;   (setq helm-google-suggest-default-browser-function 'helm-browse-url-chromium)
;;   (setq helm-home-url "http://www.google.de")
;;   (setq helm-autoresize-mode t))


(use-package helm-ring
  :if (and my-use-helm (not noninteractive))
  :bind (:map helm-command-map
              ("g" . helm-all-mark-rings))
)


;;; Package: helm-descbinds

(use-package helm-descbinds
  :if (and my-use-helm (not noninteractive))
  :straight t
  :commands helm-descbinds
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds))) ;; used to be where-is


;;; Package: helm-flyspell

(use-package helm-flyspell
  :if (and my-use-helm (not noninteractive))
  :straight t
  :after flyspell
  ;; :command (helm-flyspell-correct)
  :bind (:map flyspell-mode-map
  			  ("C-;" . helm-flyspell-correct))
)

;;; Package: helm-swoop

(use-package helm-swoop
  :if (and my-use-helm (not noninteractive))
  :straight t
  :bind (("M-s o"   . helm-swoop)
         ("M-s b"   . helm-swoop-back-to-last-point)
		 :map isearch-mode-map
		 ("M-s o"   . helm-swoop)
		 :map helm-swoop-edit-map
		 ;; Switch to edit mode with C-c C-e, and exit edit mode with C-c C-c
		 ("C-c C-c" . helm-swoop--edit-complete)
		 :map helm-swoop-map
		 ("C-r" . helm-previous-line)
		 ("C-s" . helm-next-line)
		 :map helm-multi-swoop-map
		 ("C-r" . helm-previous-line)
		 ("C-s" . helm-next-line)
		 )
  :config
  (setq helm-swoop-split-direction 'split-window-sensibly)

  ;; always pop at bottom
  ;; from https://github.com/hatschipuh/better-helm
  (setq helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-split-window-function 'helm-default-display-buffer)
)

;;; Package: ivy's counsel

(use-package s
  :straight t
  :commands (s-trim-right s-collapse-whitespace s-suffix?))


(use-package counsel
  :if (and (not my-use-helm) (not noninteractive))
  :straight t
  :if (not noninteractive)
  :defer t
  :bind (("C-x C-f"   . counsel-find-file)
		 ("C-x g"     . counsel-git)  ;; find file in current git tree
		 ;; Help related -> this is now handled by the "helpful" package
		 ;; ("C-h f"     . counsel-describe-function)
		 ;; ("C-h v"     . counsel-describe-variable)
		 ;; ("C-h S"     . counsel-info-lookup-symbol)
		 ;; Describe
		 ("C-h f"     . counsel-describe-function)
		 ("C-h v"     . counsel-describe-variable)
		 ;; special characters
		 ("C-x 8 RET" . counsel-unicode-char)
		 ;; searching
		 ("M-s a"     . counsel-ag)
		 ("M-s g"     . counsel-git-grep)
		 ;; goto
		 ("M-g i"     . counsel-imenu)
		 ;; yank/pop, see http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
		 ("M-y"       . counsel-yank-pop)
		 )
  :config
  (when (fboundp 'helpful-callable)
	(setq counsel-describe-function-function #'helpful-callable)
	(setq counsel-describe-variable-function #'helpful-variable)))


;;; Package: js2-mode - mayor mode for JavaScript

;; If i ever work more in JavaScript, I might add more from
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html

(use-package js2-mode
  :straight t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config
  (setq js2-basic-offset 2
	js2-highlight-level 3)
  ;; we can run a nodejs REPL locally or over TRAMP, and it works out-of-the-box!
  (defalias 'run-node 'nodejs-repl))


;;; Package: keyfreq - record how often you use a keyboard command
;; Idea from http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
;;
;; Use `keyfreq-show' to see what you've used mostly.

(use-package keyfreq
  :straight t
  :if (not noninteractive)
  :defer nil
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :config
  (defun my-turnon-keyfreq-mode ()
	(interactive)
	(keyfreq-mode 1)
	(keyfreq-autosave-mode 1))

  (defun turnoff-keyfreq-mode ()
	(interactive)
	(keyfreq-mode -1)
	(keyfreq-autosave-mode -1))

  (add-to-list 'same-window-buffer-names "*frequencies*")

  (setq keyfreq-excluded-commands
		'(self-insert-command
		  org-self-insert-command
		  abort-recursive-edit
		  backward-char
		  delete-backward-char
		  forward-char
		  keyfreq-mode
		  previous-line
		  next-line
		  undefined ;; lambda
		  ))

  (setq keyfreq-file (locate-user-emacs-file "tmp/keyfreq.el"))

  (unless (file-exists-p (file-truename keyfreq-file))
	(with-temp-buffer
	  (insert "()")
	  (write-file (file-truename keyfreq-file))))

  ;; And use keyfreq-show to see how many times you used a command.
  ;; comment out below line if there is performance impact
  (my-turnon-keyfreq-mode))


;;; Package: lsp-mode - language server protocol

;; turn it on with such a .dir-locals snippet:
;;
;; ((c++-mode . ((eval . (lsp-deferred)))
;; ))

(use-package lsp-mode
  :straight t
  :diminish lsp-mode  ;; I can see if it is active in the menu!
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c o" . lsp-describe-thing-at-point)
			  ("C-c f" . lsp-format-region)
			  ("C-c a" . lsp-execute-code-action)
			  ("C-c h" . lsp-document-highlight)
			  ("C-c n" . lsp-rename)
			  ;; ("M-." . lsp-find-definition)
			  )
  :init
  (add-to-list 'safe-local-variable-values '(eval lsp-deferred))
  :config
  (require 'yasnippet)
  (setq lsp-auto-guess-root t)      ; Detect project root
  ;;(setq lsp-enable-snippet nil)   ; no yasnipped for now
  (setq lsp-prefer-flymake nil)     ; Use lsp-ui if present
  (setq lsp-eldoc-render-all nil)   ; this is now rendered via lsp-ui
  (setq lsp-enable-symbol-highlighting nil) ; don't highlight the current symbol

  (setq lsp-document-sync-method 'incremental)

  (setq lsp-keep-workspace-alive nil)
  (setq lsp-session-file (locate-user-emacs-file "tmp/lsp-session-v1"))

  ;; face for "hightlight referenced"
  (set-face-attribute 'lsp-face-highlight-textual nil
					  :background "#666" :foreground "#ffffff")
  (set-face-attribute 'lsp-face-highlight-read nil
					  :background "#666" :foreground "#ffffff" :underline nil)
  (set-face-attribute 'lsp-face-highlight-write nil
					  :background "#666" :foreground "#ffffff")

  ;; fixup the menu
  (define-key lsp-mode-menu [menu-bar lsp] nil)
  (easy-menu-remove-item lsp-mode-menu nil "Add folder to workspace")
  (easy-menu-remove-item lsp-mode-menu nil "Remove folder from workspace")
  (easy-menu-remove-item lsp-mode-menu nil "Switch to another workspace folder")
   ;; not supported by clangd
  (easy-menu-remove-item lsp-mode-menu nil "Find implementations of symbol under point")
  (easy-menu-remove-item lsp-mode-menu nil "Find type definitions of symbol under point")
  ;; no need to display this menu entry when the function is disabled
  (unless lsp-log-io
	(easy-menu-remove-item lsp-mode-menu nil "View IO logs for workspace")))


;;; Package: lsp-clients - use clangd

(use-package lsp-clients
  :defer t
  :defines (lsp-clients-clangd-args)

  :config
  (setq lsp-clients-clangd-executable "/usr/bin/clangd-10")
  ;; maybe set this from .dir-locals?
  (setq lsp-clients-clangd-args `("-j=2"
                                  "--background-index"
								  "--clang-tidy"
								  "--completion-style=bundled"
								  "--header-insertion=iwyu"
								  "--suggest-missing-includes"
                                  "--log=error"
                                  ,(concat "--compile-commands-dir=" (file-truename (locate-dominating-file "." ".git")) "build")
								  )))


;;; Package: lsp-ui - show error and documentation in a nice way

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references)
              ("M-g i" . lsp-ui-imenu)
			  )
  :config

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-include-signature t)
  ;; (setq lsp-ui-doc-border (face-foreground 'default))
  ;; (setq lsp-eldoc-enable-hover t)

  (setq lsp-ui-sideline-delay 0.5)
  (setq lsp-ui-sideline-update-mode 'point)
  (setq lsp-ui-sideline-show-hover nil) ;; I don't need info about the current symbol
  (setq lsp-ui-sideline-ignore-duplicate t)

  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  (easy-menu-define-key lsp-mode-menu "M-." (cons "Find definition" #'lsp-ui-peek-find-definitions)
						"Find definitions of symbol")

  (easy-menu-define-key lsp-mode-menu "M-?" (cons "Find references" #'lsp-ui-peek-find-references)
						"Find definitions of symbol")

  (easy-menu-remove-item lsp-mode-menu nil "Find definitions of symbol")
  ;; these two aren't supported by clangd
  (easy-menu-remove-item lsp-mode-menu nil "Find implementations of symbol under point")
  (easy-menu-remove-item lsp-mode-menu nil "Find references to symbol under point"))


;;; Package: lua-mode: mayor mode for Lua

(use-package lua-mode
  :straight t
  :mode (("\\.lua\\'" . lua-mode))
  :init
  ;; normally long lines get the same face as comments, which is quite irritating
  (defun my-lua-hook ()
	(setq-local whitespace-line-column 132))
  (add-hook 'lua-mode-hook #'my-lua-hook))


;;; Package: markdown-mode: mayor-mode for Markdown

(use-package markdown-mode
  :straight t
  :defer t
  :hook (markdown-mode . visual-line-mode)
  :mode (("\\.md\\'"       . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode)))


;;; Package: meson-mode

(use-package meson-mode
  :straight t
  :mode (("\\meson.build\\'" . meson-mode))
  :config
  (setq meson-indent-basic 4))


;;; Package: nim-mode

(use-package nim-mode
  :straight t
  )


;;; Package: notmuch

(use-package my-notmuch
  :bind ("M-g n" . my-notmuch-hello))


;;; Package: nsm - Network Security Modules

(use-package nsm
  :defer t
  :config
  (setq nsm-settings-file (locate-user-emacs-file "tmp/network-security.data")))


;;; Package: org

(use-package org
  :straight t
  :bind (("C-c l" . org-store-link)
		 ("C-c o" . org-open-at-point-global)
		 ;; This used to be org-shift{up/down}, but I'm not working
		 ;; with priorities it however also allowed to edit
		 ;; timestamps, not sure if I'm gonna miss that.
		 :map org-mode-map
		 ("C-<tab>"   . company-complete)
		 ("C-c C-x e" . my-org-set-effort)
		 ("M-p"       . org-previous-visible-heading)
		 ("M-n"       . org-next-visible-heading))
  :commands (org-babel-do-load-languages
			 org-buffer-list
			 org-open-file
			 org-set-property
			 orgstruct++-mode
             org-link-set-parameters
             org-property-get-allowed-values)

  :init
  ;; allow Shift-Cursor to mark stuff
  (setq org-replace-disputed-keys t)

  ;; modules to load together with org-mode
  (setq org-modules '(
				 ;; org-annotate-file
				 ;; org-bbdb
				 ;; org-bibtex
				 ;; org-collector
				 ;; org-docview
				 ;; org-drill
				 ;; org-eval
				 ;; org-expiry
				 ;; org-gnus
				 ;; org-habit
				 ;; org-info
				 ;; org-interactive-query
				 ;; org-irc
				 ;; org-jsinfo
				 ;; org-man
				 ;; org-mhe
				 ;; org-mouse
				 ;; org-panel
				 ;; org-protocol
				 ;; org-rmail
				 ;; org-screen
				 ;; org-toc
				 ;; org-w3m
				 ))

  (when (featurep 'straight)
	;; This section is devoted to fixing the asinine version-check
	;; handling in Org (it's not designed to handle the case where you
	;; run straight from the Git repo, apparently). This is one of the
	;; worse hacks I've ever had the misfortune to create in Emacs.

	;; First we define a function to return a proper version string
	;; based on the Git repo. (This is somewhat similar to what happens
	;; in org-fixup.el.) We should really define a function that will
	;; return the latest tag, as well, but this remains a FIXME for now.
	(defun my--org-git-version ()
	  "Return the abbreviated SHA for the Org Git repo."
	  (let ((default-directory (concat user-emacs-directory
									   "straight/repos/org/")))
		(if (executable-find "git")
			(with-temp-buffer
			  ;; Returns the shortest prefix of the SHA for HEAD that is
			  ;; unique, down to a minimum of 4 characters (see
			  ;; git-rev-parse(1)).
			  (call-process "git" nil '(t nil) nil
							"rev-parse" "--short" "HEAD")
			  (if (> (buffer-size) 0)
				  (string-trim (buffer-string))
				;; This shouldn't happen, unless somehow Org is not
				;; actually a Git repo.
				"revision unknown"))
		  ;; This also shouldn't happen, because how would you have
		  ;; gotten Org in the first place, then? But the real world
		  ;; sucks and we have to account for stuff like this.
		  "git not available")))

	;; Here we're defining `org-git-version' and `org-release' eagerly.
	;; Pay close attention here, since we actually do this multiple
	;; times. The control flow is really weird. The reason we define the
	;; functions here is that Emacs includes its own copy of Org, and
	;; these functions are autoloaded by Emacs. Now, normally the
	;; built-in autoloads are overridden by the version of Org
	;; downloaded from EmacsMirror, but since we're running straight
	;; from the Git repo, `org-git-version' and `org-release' are not
	;; generated and autoloaded. So in order to avoid the original
	;; autoloads from being triggered under any circumstances, we have
	;; to overwrite them here.
	(defalias #'org-git-version #'my--org-git-version)
	(defun org-release () "N/A") ; FIXME: replace with a real function

	;; Now, the culprit function is `org-check-version', which is
	;; defined in org-compat.el and called from org.el. The problem with
	;; this function is that if the version of Org in use is not a
	;; release version (i.e. it's running straight from the repo, as we
	;; are doing), then it prints a warning. We don't want this. The
	;; natural thought is to override `org-check-version'.
	;; Unfortunately, this is completely impossible since
	;; `org-check-version' is a macro, and org.el (which is where the
	;; macro is used) is byte-compiled, so the code of
	;; `org-check-version' is hardcoded into org.elc. The easiest way
	;; around the problem, other than doing something even more
	;; horrifying like suppressing warnings while loading Org, seems to
	;; be to *pretend* that org-version.el is available, even though it
	;; doesn't exist. Then `org-check-version' happily defines
	;; `org-git-version' and `org-release' as autoloads pointing to
	;; org-version.el. Of course, then after Org is loaded, we have to
	;; override those autoloads to make the functions point back to what
	;; we want. Right now, the definition of `org-release' generated by
	;; `org-check-version' is the same as the one used above, so we
	;; don't bother to change it. That should change, FIXME.
	(provide 'org-version)
	(with-eval-after-load 'org
	  (defalias #'org-git-version #'my--org-git-version)))

  :config
  ;; My main file
  (setq org-default-notes-file (expand-file-name "todo.org" user-emacs-directory))

  ;; Handle deletion inside elipsis
  (setq org-catch-invisible-edits 'error)

  ;; don't fold for now
  (setq org-startup-folded 'content)

  ;; Time stamp format
  (setq org-display-custom-times t)
  (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d>"))
  (setq org-time-stamp-custom-formats '("<%Y-%m-%d>"))

  ;; :bind cannot bind into a different map
  (bind-key "C-TAB"   'org-cycle org-mode-map)
  (if (and (not my-use-helm) (not noninteractive))
	  (bind-key "C-c C-j" 'counsel-org-goto org-mode-map)
	(bind-key "C-c C-j" 'helm-org-in-buffer-headings org-mode-map)) ;; was org-goto
  (bind-key "C-c k"   'org-cut-subtree org-mode-map)
  (bind-key "C-c R"   'org-reveal org-mode-map)
  ;; (bind-key "C-c t"   'org-show-todo-tree org-mode-map)

  ;; adjust level, but not for drawers/properties
  (setq org-yank-adjusted-subtrees t
		org-adapt-indentation nil)

  ;; https://emacs.stackexchange.com/questions/33064/fontify-broken-links-in-org-mode
  (org-link-set-parameters
   "file"
   :face (lambda (path) (when (not (file-remote-p path))(if (file-exists-p path) 'org-link 'org-warning))))

  ;; Hide emphasize markers like the "=" in "=foo=".
  (setq org-hide-emphasis-markers t)

  ;; New line behavior:
  ;; - add blank line after M-ENTER (org-insert-heading)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

  ;; Default modes I want to have enabled in org-mode
  ;; (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'toggle-word-wrap)
  (add-hook 'org-mode-hook #'yas-minor-mode-on)

  ;; make enter open the link
  (setq org-return-follows-link t)

  ;; some speed commands, use ? at the start of an org-header to see which one we have
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  (add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
  (add-to-list 'org-speed-commands-user '("W" widen))
  (add-to-list 'org-speed-commands-user '("k" org-cut-subtree))
  ;; (add-to-list 'org-speed-commands-user '("P" call-interactively 'org2blog/wp-post-subtree))

  ;; "!"    record time stamp
  ;; "@"    add note with time
  ;; "x/y"  use x when entering state, y when leaving state
  ;; the first letter can be used with C-c C-t
  (setq org-todo-keywords
	'((sequence "TODO(t)" "STARTED(s)" "|" "DONE(x)")
	  (sequence "WAIT(w)" "DELE(d)" "|" "CANC(c)")))

  (setq org-todo-keyword-faces
	  '(("TODO"      . (:foreground "red" :weight bold))
		("STARTED"   . (:foreground "#b70101" :weight bold))
		("DONE"      . (:foreground "forestgreen" :weight bold))
		("WAIT"      . (:foreground "orange" :weight bold))
		("DELE"      . (:foreground "forestgreen" :weight bold))
		("CANC"      . shadow)))

  ;; use extra drawer
  (setq org-log-into-drawer t)

  ;; when my day ends
  (setq org-use-effective-time t
		org-extend-today-until 17)

  ;; Resume clocking tasks when emacs is restarted
  ;; (org-clock-persistence-insinuate)

  ;; TODO creates error
  ;; (setq org-global-properties
  ;; 	'("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))

  ;; Try column with this:
  ;; (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

  ;; handle jumping into org-mode
  (setq org-show-context-detail '((default . local)))

  ;; misc refile settings
  (setq org-reverse-note-order t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  ;; (setq org-blank-before-new-entry nil)

  ;; export and open
  (defun my-org-export-to-html-and-open ()
	(interactive)
	(org-open-file (org-html-export-to-html)))
  (bind-key "<M-f7>" 'my-org-export-to-html-and-open org-mode-map)

  (setq org-imenu-depth 3)

  ;; For org-babel
  (org-babel-do-load-languages 'org-babel-load-languages
							   '((shell . t)
								 (python . t)
								 ;; (R . t)
								 ;; (ruby . t)
								 ;; (ditaa . t)
								 (dot . t)
								 ;; (octave . t)
								 (sqlite . t)
								 ;;(perl . t)
								 ))
  (setq org-confirm-babel-evaluate nil)

  (defun my-org-set-effort ()
	"More interactive replacement for org-set-effort"
	(interactive)
	;; (org-property-get-allowed-values nil org-effort-property 'table)
	(let ((effort (completing-read
				   "Effort: "
				   (org-property-get-allowed-values nil org-effort-property))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))


;;; Package: org-agenda
;; http://www.suenkler.info/docs/emacs-orgmode/

(use-package org-agenda
  :bind (("M-g a" . org-agenda)
		 ("M-g w" . org-agenda-list))
  :config
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map)
  ;; (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

  ;; Highlight current line
  (add-hook 'org-agenda-mode-hook (defun my-org-agenda-hookfunc () (hl-line-mode 1 )))

  ;; which files the agenda should consider
  (setq org-agenda-files (list org-default-notes-file))

  ;; Let date stand out
  (setq org-agenda-format-date
	"%Y-%m-%d ---------------------------------------------------------------------")

  (setq org-agenda-show-outline-path t)

  ;; colorize priorities
  (setq org-agenda-fontify-priorities
	'((65 (:foreground "Red"))
	  (66 (:foreground "Blue"))
	  (67 (:foreground "Darkgreen"))))

  ;; hide done tasks
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; normally hide the "someday" (nice-to-have) things
  (setq org-agenda-filter-preset '("-someday"))

  ;; show day schedule, not week schedule
  (setq org-agenda-span 'day)

  ;; own views
  (setq org-agenda-custom-commands
	'(("n" "Agenda and all TODO's"
	   ((agenda "")
		(alltodo "")))
	  ;; ("f" "Agenda and flagged tasks"
	  ;;  ((tags "flagged")
	  ;;   (agenda "")))
	  ("s" "Tagged 'someday'" tags "someday" ((org-agenda-filter-preset '("+someday"))
						  (org-agenda-todo-ignore-with-date nil)))
	  ))

  ;; show clock report
  ;; (setq org-agenda-start-with-clockreport-mode nil)

  ;; Keine Links, maximal bis Level 4 herunter:
  ;; (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))
  )


;;; Package: org-capture

(use-package org-capture
  :bind ("C-c r" . my-org-capture-todo)
  :commands (org-capture)
  :config
  (defun my-org-capture-todo ()
	(interactive)
	(org-capture nil "o"))
  (setq org-capture-templates
	`(("o" "Open task" entry
	   (file+headline org-default-notes-file "Unsortiert")
	   "* TODO %?\n- %u aufgenommen\n")
	  ("n" "Note" item
	   (file+headline org-default-notes-file "Infos")))))


;;; Package: org-clock

(use-package org-clock
  :bind ("C-c j" . org-clock-goto) ;; jump to current task from anywhere
  :config
  (setq org-clock-into-drawer "CLOCK")

  ;; Yes it's long... but more is better ;)
  (setq org-clock-history-length 35)

  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")

  ;; this removes clocked tasks with 0:00 duration
  ;; (setq org-clock-out-remove-zero-time-clocks t)

  ;; Don't clock out when moving task to a done state
  ;; (setq org-clock-out-when-done nil)

  ;; Save the running clock and all clock history when exiting Emacs,
  ;; load it on startup
  ;; (setq org-clock-persist t)

  ;; Disable auto clock resolution
  (setq org-clock-auto-clock-resolution nil))


;;; Package: org-list

(use-package org-list
  :commands (org-item-re)
  :config
  ;; tab changes visibility of lists like headers
  (setq org-cycle-include-plain-lists 'integrate)

  ;; count percentages of checkboxes hierarchically, note that "nil"
  ;; mean that the hierarchical statistics are *on* :-/
  (setq org-checkbox-hierarchical-statistics nil)

  ;; speed commands are fun, not only on the headers, but also on lists
  ;; see http://orgmode.org/manual/Speed-keys.html
  (defun my/org-use-speed-commands-for-headings-and-lists ()
	"Activate speed commands on list items too."
	(or (and (looking-at org-outline-regexp) (looking-back "^\**" nil))
	(save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*" nil)))))
  (setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists))


;;; Package: org-src

(use-package org-src
  :defer t
  :bind (:map org-src-mode-map
			  ;; F2 used to be save-buffer, but this will happily save a file
			  ;; "config.org[*Org Src config.org[ emacs-lisp ]*]" which is a bit
			  ;; awkward
			  ("F2"      . org-edit-source-save)
			  ;; normally I'd need C-c ' to exit, but this enables the same exit
			  ;; method I have in when doing a commit in magit.
			  ("C-c C-c" . org-edit-src-exit))
  :config
  ;; Open source editor in current window
  (setq org-src-window-setup 'current-window)
  ;; inside src block use the colors like the major mode of the src type
  (setq org-src-fontify-natively t)
  ;; inside a src block let tab act like it was in major mode of the src type
  (setq org-src-tab-acts-natively t)
  ;; don't add two indentation spaces into src blocks
  (setq org-src-preserve-indentation t))


;;; Package: ox-gfm

(use-package ox-gfm
  :straight t
  :commands (org-export-format-code-default)
  :config

  ;; The language for my source codes is ofte "emacs-lisp". However,
  ;; Hugo colorizes them weird, so I re-define org-gfm-src-block
  ;; to convert "emacs-lisp" to "lisp":
  ;;
  (defun org-gfm-src-block (src-block contents info)
	"Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."
	(let* ((lang (org-element-property :language src-block))
		   (code (org-export-format-code-default src-block info))
		   (prefix (concat "```" (if (string= lang "emacs-lisp") "lisp" lang) "\n"))
		   (suffix "```"))
	  (concat prefix code suffix))))


;;; Package: ox-publish

(use-package ox-publish
  :if (or (string= "holger" (system-name))
		  (string= "laptop" (system-name)))
  :bind ("C-c p" . my-publish)
  :commands (org-publish)
  :config
  (setq org-publish-project-alist
	'(("twbs"
	   :base-directory "~/org/"
	   :publishing-directory "~/org/twbs/"
	   :base-extension "org"
	   :recursive t
	   :publishing-function org-twbs-publish-to-html

	   ;; See http://orgmode.org/manual/Publishing-options.html#Publishing-options

	   ;; Don't emit  "Created: 2016-02-12 Fri 09:28 Emacs 24.5.1 (Org mode 8.3.3)"
	   :html-postamble nil

	   ;; This is the Table of Contents on the right side, you can turn it off
	   ;; per page with "#+OPTIONS: toc:nil"
	   :with-toc t

	   :html-use-infojs nil
	   :html-validation-link ""
	   :html-home/up-format ""
	   :html-link-up ""
	   :html-link-home ""
	   ;; :html-checkbox-type 'html   ;; use CSS to format them
	   :html-metadata-timestamp-format "%Y-%m-%d %H:%M"

	   ;; General export settings
	   :archived-trees nil
	   :headline-levels 3
	   :section-numbers nil
	   :with-author nil ;; Only one author ever
	   :with-date nil
	   :with-latex nil
	   :with-sub-superscript nil
	   )

	  ("html"
	   :base-directory "~/org/"
	   :publishing-directory "~/org/html/"
	   :base-extension "org"
	   :recursive t
	   :publishing-function org-html-publish-to-html

	   ;; see (org-html--build-head info)
	   :html-head-include-default-style nil ;; org-html-head-include-default-style
	   ;; :html-head                   ;; org-html-head
	   ;; :html-head "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\"/>"
	   ;; :html-head-extra             ;; org-html-head-extra
	   ;; :html-htmlized-css-url       ;; org-html-htmlized-css-url
	   :html-head-include-scripts nil       ;; org-html-head-include-scripts

	   ;; Don't emit  "Created: 2016-02-12 Fri 09:28 Emacs 24.5.1 (Org mode 8.3.3)"
	   :html-postamble nil

	   :with-toc nil

	   :html-use-infojs nil
	   :html-validation-link ""
	   :html-home/up-format ""
	   :html-link-up ""
	   :html-link-home ""
	   ;; :html-checkbox-type 'html   ;; use CSS to format them
	   :html-metadata-timestamp-format "%Y-%m-%d %H:%M"

	   ;; General export settings
	   :archived-trees nil
	   :headline-levels 3
	   :section-numbers nil
	   :with-author nil ;; Only one author ever
	   :with-date nil
	   :with-latex nil
	   :with-sub-superscript nil
	   )

	  ("static"
	   :base-directory "~/org/"
	   :base-extension "jpg\\|gif\\|png\\|css\\|js"
	   :recursive t
	   :publishing-directory "~/org/out/"
	   :publishing-function org-publish-attachment)

	  ("site" :components ("twbs" "html"))))
  (defun my-publish ()
	(interactive)
	(org-publish "site" t)))


;;; Package: ox-reveal

(use-package ox-reveal
  :straight t
  :commands (org-reveal-export-to-html))


;;; Package: htmlize

(use-package htmlize
  :straight t
  :defer t
  :defines (htmlize-version)
  :config
  (setq org-export-with-drawers t))


;;; Package: org2hugo

(use-package org2hugo
  :bind ("M-g h" . org2hugo))


;;; Package: pdf-occur

(use-package pdf-occur
  :commands (pdf-occur pdf-occur-global-minor-mode))


;;; Package: pdf-outline

(use-package pdf-outline
  :commands (pdf-outline))


;;; Package: pos-tip

(use-package pos-tip
  :straight t
  :defer t)


;;; Package: pug-mode: mayor mode for vibe.d templates

(use-package pug-mode
  :straight t
  :defer t
  :mode ("\\.dt\\'" . pug-mode))


;;; Package: rainbow-delimiters

(use-package rainbow-delimiters
  :straight t
  :commands (rainbow-delimiters-mode))


;;; Package: rust-mode - mayor mode for Rust

(use-package rust-mode
  :straight t
  :mode (("\\.rs\\'" . rust-mode)))


;;; Package: CANC sanityinc-tomorrow (theme)
;; https://github.com/purcell/color-theme-sanityinc-tomorrow

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :disabled t
  :after cus-edit
  :init
  (let ((custom-safe-themes t))
	(color-theme-sanityinc-tomorrow-bright))
)


;;; Package: server
;; - always start the emacs-server, except when run in daemon mode
;;
;; - already Disable prompt asking you if you want to kill a buffer
;;   with a live process attached to it.
;;   http://stackoverflow.com/questions/268088/how-to-remove-the-prompt-for-killing-emacsclient-buffers

(use-package server
  :if (not noninteractive)
  :defer nil
  :commands (server-running-p server-mode server-edit)
  :config
  (unless (or (daemonp) (server-running-p))
	(server-mode 1))
  (add-hook 'server-switch-hook 'raise-frame))

;; A good way to start emacsclient is with this line in /etc/bash.bashrc:
;; alias e="emacsclient --no-wait --alternate-editor=\"\" --create-frame"

;; May may also set the environment variables EDITOR and/or VISUAL,
;; but then you better omit the "--no-wait" option.




;;; Package: symbol-overlay - jump / manipulate to symbols
;; https://github.com/wolray/symbol-overlay

(use-package symbol-overlay
  :straight t
  :commands (symbol-overlay-mode)

  :bind (("M-p"      . symbol-overlay-jump-prev)
		 ("M-n"      . symbol-overlay-jump-next)
		 ("M-<up>"   . symbol-overlay-jump-prev)
		 ("M-<down>" . symbol-overlay-jump-next)
		 ("M-<home>" . highlight-symbol-first)
		 ("M-<end>"  . highlight-symbol-last)
		 ("M-g o"    . symbol-overlay-hydra/body))

  ;; This enables auto-highlighting. Note that the commands still
  ;; work, even when highlighting is off.
  ;; :hook ((conf-mode . symbol-overlay-mode)
  ;;        (html-mode . symbol-overlay-mode)
  ;;        (prog-mode . symbol-overlay-mode)
  ;;        (yaml-mode . symbol-overlay-mode))

  :config
  (defun highlight-symbol-first ()
	"Jump to the first location of symbol at point."
	(interactive)
	(push-mark)
	(eval
	 `(progn
		(goto-char (point-min))
		(let ((case-fold-search nil))
          (search-forward-regexp
           (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
           nil t))
		(beginning-of-thing 'symbol))))

  (defun highlight-symbol-last ()
	"Jump to the last location of symbol at point."
	(interactive)
	(push-mark)
	(eval
	 `(progn
		(goto-char (point-max))
		(let ((case-fold-search nil))
          (search-backward-regexp
           (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
           nil t)))))

  (defhydra symbol-overlay-hydra (:color pink :hint nil :timeout 5)
      "
  _p_   ^^   _b_  back         _h_  highlight  _i_  isearch
_<_   _>_    _d_  definition   _R_  remove     _Q_  query-replace
  _n_   ^^   _w_  save         ^^              _r_  rename
"
	  ("<home>" symbol-overlay-jump-first)
	  ("<end>"  symbol-overlay-jump-last)
	  ("<"      symbol-overlay-jump-first)
	  (">"      symbol-overlay-jump-last)

	  ("p"      symbol-overlay-jump-prev)
	  ("n"      symbol-overlay-jump-next)

	  ("d"      symbol-overlay-jump-to-definition)
	  ("b"      symbol-overlay-echo-mark)

  	  ("h" symbol-overlay-put :color blue)
	  ("R" symbol-overlay-remove-all :color blue)

	  ("w" symbol-overlay-save-symbol :color blue)
	  ("t" symbol-overlay-toggle-in-scope)

	  ("i" symbol-overlay-isearch-literally :color blue)
	  ("Q" symbol-overlay-query-replace :color blue)
	  ("r" symbol-overlay-rename  :color blue)
      ("q" nil)))


;;; Package: toml-mode - mayor mode for Hugo's configuration
;; This is used for Hugo's config files:

(use-package toml-mode
  :straight t
  :defer t
  :mode ("\\.toml\\'" . toml-mode))



;;; Package: undo-propose - use undo in an extra buffer
;; https://github.com/jackkamm/undo-propose-el

;; Use C-z for undo-propose in the buffer you are editing. This will
;; send you to a new temporary buffer, which is read-only except for
;; allowing undo commands. In this buffer, call undo as you normally
;; would, until you have reached your desired place in the undo
;; history.
;; 
;; When you are finished, type C-c C-c to commit the changes
;; (both in the buffer and undo-ring) back to the parent.
;;
;; Alternatively, type C-c C-s to copy the buffer but not the
;; individual undo events (squashing them into a single edit event in
;; the undo history). To cancel, type C-c C-k.
;;
;; You can also ediff the
;; proposed chain of undo’s by typing C-c C-d.

(use-package undo-propose
  :straight t
  :bind (("C-z" . undo-propose)
		 :map undo-propose-mode-map
		 ("C-z" . undo))
  :config
  ;; this makes calling undo-propose already do the first undo when entering the mode
  (add-hook 'undo-propose-entry-hook #'undo)
  )


;;; CANC Package: undo-tree - treat undo history as a tree

;; This lets you use `C-z' (undo-tree-visualize) to visually walk
;; through the changes you've made, undo back to a certain point (or
;; redo), and go down different branches.

(use-package undo-tree
  :disabled t
  :straight t
  :diminish undo-tree-mode
  :bind (("C-z" . undo-tree-visualize)
         :map undo-tree-map
         ("M-/" . undo-tree-redo))
  :commands (undo-tree-visualize global-undo-tree-mode)
  :config
  (global-undo-tree-mode 1)
  ;; Don't show Undo Tree in the mode line.
  (setq undo-tree-mode-lighter nil)
  ;; Disable undo-in-region. It sounds like an interesting feature,
  ;; but unfortunately the implementation is very buggy and regularly
  ;; causes you to lose your undo history.
  (setq undo-tree-enable-undo-in-region nil)

  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))


;;; Package: web-mode - mayor mode for HTML, CSS, JSON
;; Home page: http://web-mode.org/

(use-package web-mode
  :straight t
  :commands (web-mode web-mode-guess-engine-and-content-type)
  :mode (("\\.html\\'" . web-mode)
		 ("\\.css\\'"  . web-mode)
		 ("\\.scss\\'" . web-mode)
		 ("\\.json\\'" . web-mode)
		 )
  :defines (web-mode-engines-alist)
  :config
  ;; remove the   (nil ("<!-" . "- | -->"))  data set:
  (unless (car (car (last web-mode-engines-auto-pairs)))
   	(setq web-mode-engines-auto-pairs (butlast web-mode-engines-auto-pairs)))

  (defun my-web-mode-hook ()
	(visual-line-mode 1)
	(setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 4
		  web-mode-code-indent-offset 2
		  web-mode-indent-style 2
		  web-mode-style-padding 1
		  web-mode-script-padding 1
		  web-mode-block-padding 0
		  indent-tabs-mode t
		  tab-width 4
		  web-mode-engines-alist '(("go" . "\\.html\\'")))
	(web-mode-guess-engine-and-content-type)
	)
  (add-hook 'web-mode-hook 'my-web-mode-hook))


;;; Package: yaml-mode

(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)))


;;; Package: xref
;; https://www.emacswiki.org/emacs/EmacsTags

(use-package xref
  :defer t
  :bind ("M-*" . pop-tag-mark))


;;; Package: yasnippet

(use-package yasnippet
  :straight t
  :commands (yas-minor-mode-on yas-insert-snippet)
  :bind (("C-c s"   . yas-insert-snippet))
  :if (not noninteractive)
  :diminish yas-minor-mode

  ;; (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-reload-all)

  ;; No dropdowns please.
  (setq yas-prompt-functions '(yas-completing-prompt))

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t))


;;; Package: zeal-at-point - documentation browser
;; I got the idea from
;; https://www.reddit.com/r/emacs/comments/4xa253/browser_for_api_documentation_inside_emacs/
;; http://zealdocs.org/ and https://github.com/jinzhu/zeal-at-point
(use-package zeal-at-point
  :straight t
  :bind ("C-c d" . zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(c++-mode . "qt,c++")))


;;; CANC Package: dump-jump (goto definition)
;; With this package you can goto a definition without any setup (e.g. no
;; TAGS file is needed).

;; See https://github.com/jacktasia/dumb-jump.

;; | C-M g | goto definition                    |
;; | C-M p | back (can be called multiple times |

(use-package dumb-jump
  :disabled t
  :straight t
  :bind (("C-M-g" . dumb-jump-go)
		 ("C-M-p" . dumb-jump-back)
		 ("C-M-q" . dumb-jump-quick-look)))


;;; CANC Package: elfeed - RSS feed reader

;; | Key | Function                                              |
;; |-----+-------------------------------------------------------|
;; | RET | show article                                          |
;; | G   | update feeds                                          |
;; | b   | open in browser (browse-url)                          |
;; | q   | quit                                                  |
;; | +   | Apply TAG to all selected entries.                    |
;; | -   | Remove TAG from all selected entries.                 |
;; | S   | Set a new search filter for the elfeed-search buffer. |

;; https://github.com/skeeto/elfeed
;; Tips and Tricks: http://nullprogram.com/blog/2013/11/26/

(use-package elfeed
  :disabled t
  :straight t
  :if (string= (system-name) "desktop")
  :bind ("M-g r" . elfeed)   ; r like "RSS"
  :config
  (setq elfeed-feeds
		'(;; emacs
		  ("http://emacsredux.com/atom.xml" emacs)
		  ("http://endlessparentheses.com/atom.xml" emacs)
		  ("http://nullprogram.com/feed/" emacs)
		  ("http://planet.emacsen.org/atom.xml" emacs)
		  ("http://www.lunaryorn.com/feed.atom" emacs)
		  ("http://www.masteringemacs.org/feed/" emacs)
		  ("https://github.com/milkypostman/melpa/commits/master.atom" github emacs)
		  ("http://oremacs.com/atom.xml" emacs)
		  ("http://emacsnyc.org/atom.xml" emacs)
		  ;; ("https://www.reddit.com/r/emacs/.rss" emacs reddit)
		  ;; ("https://www.reddit.com/r/orgmode/.rss" emacs reddit)

		  ;;("http://stackexchange.com/feeds/tagsets/152198/emacs?sort=active" emacs)
		  ))

  (setq elfeed-use-curl t)
  (setq elfeed-search-filter "@1-week-ago +unread")

  ;; Entries older than 4 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
			(elfeed-make-tagger :before "4 weeks ago"
								:remove 'unread))

  ;; fetch RSS/Atom feeds in the background
  ;;
  ;; A better approach would be to have a checker function that
  ;; gets the feeds sorted by when they have last been downloaded
  ;; it would then take one (!) of them and just feed it. That
  ;; makes the idle function fast and we could increase it's
  ;; timeout from 15 minutes to a minute or so.
  (run-with-idle-timer (* 15 60) t #'elfeed-update))


;; An example on how to prune old feeds:

;; (defun elfeed-dead-feeds (years)
;;   "Return a list of feeds that haven't posted en entry in YEARS years."
;;   (cl-block
;; 	  (macroexp-let* ((living-feeds (make-hash-table :test 'equal))
;; 					  (seconds (* years 365.0 24 60 60))
;; 					  (threshold (- (float-time) seconds)))
;; 					 (with-elfeed-db-visit (entry feed)
;; 										   (let ((date (elfeed-entry-date entry)))
;; 											 (when (> date threshold)
;; 											   (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
;; 					 (cl-loop for url in (elfeed-feed-list)
;; 							  unless (gethash url living-feeds)
;; 							  collect url))))
;; (elfeed-dead-feeds 1.0)


;; Some more feeds to (eventually) check:

;; - ("http://harryrschwartz.com/atom.xml" blog)
;; - ("http://zinascii.com/writing-feed.xml" blog)
;; - ("http://githubengineering.com/atom.xml" blog)
;; - ("http://blog.smola.org/rss" blog)
;; - ("http://briancarper.net/feed" blog)
;; - ("https://kotka.de/blog/index.rss" blog)
;; - ("http://fiftyfootshadows.net/feed/" blog)
;; - ("http://blag.xkcd.com/feed/" blog)
;; - ("http://youdisappear.net/files/page1.xml" blog music)
;; - ("http://normanmaurer.me/blog.atom" blog)
;; - ("http://blog.mikemccandless.com/feeds/posts/default" elasticsearch blog)
;; - ("http://lethain.com/feeds/all/" blog)
;; - ("http://whatthefuck.computer/rss.xml" blog)
;; - ("http://feeds.feedburner.com/jamesshelley" blog)
;; - ("http://www.marco.org/rss" blog)
;; - ("http://gnuvince.wordpress.com/feed/" blog)
;; - ("http://elliotth.blogspot.com/feeds/posts/default" blog)
;; - ("http://feeds.feedburner.com/Hyperbole-and-a-half" blog)
;; - ("http://lcamtuf.blogspot.com/feeds/posts/default" blog)
;; - ("http://blog.isabel-drost.de/index.php/feed" blog)
;; - ("http://feeds2.feedburner.com/CodersTalk" blog)
;; - ("http://feeds.feedburner.com/codinghorror/" blog)
;; - ("http://lambda-the-ultimate.org/rss.xml" blog)
;; - ("http://danluu.com/atom.xml" blog)
;; - ("http://ferd.ca/feed.rss" blog)
;; - ("http://blog.fsck.com/atom.xml" blog)
;; - ("http://jvns.ca/atom.xml" blog)
;; - ("http://newartisans.com/rss.xml" blog emacs)
;; - ("http://bling.github.io/index.xml" blog emacs)
;; - ("https://rachelbythebay.com/w/atom.xml" blog)
;; - ("http://blog.nullspace.io/feed.xml" blog)
;; - ("http://www.mcfunley.com/feed/atom" blog)
;; - ("https://codewords.recurse.com/feed.xml" blog)
;; - ("http://akaptur.com/atom.xml" blog)
;; - ("http://davidad.github.io/atom.xml" blog)
;; - ("http://www.evanjones.ca/index.rss" blog)
;; - ("http://neverworkintheory.org/feed.xml" blog)
;; - ("http://blog.jessitron.com/feeds/posts/default" blog)
;; - ("http://feeds.feedburner.com/GustavoDuarte?format=xml" blog)
;; - ("http://blog.regehr.org/feed" blog)
;; - ("https://www.snellman.net/blog/rss-index.xml" blog)
;; - ("http://eli.thegreenplace.net/feeds/all.atom.xml" blog)
;; - ("https://idea.popcount.org/rss.xml" blog)
;; - ("https://aphyr.com/posts.atom" blog)
;; - ("http://kamalmarhubi.com/blog/feed.xml" blog)
;; - ("http://maryrosecook.com/blog/feed" blog)
;; - ("http://www.tedunangst.com/flak/rss" blog)
;; - ("http://yosefk.com/blog/feed" blog)
;; - ("http://www.benkuhn.net/rss/" blog)
;; - ("https://emacsgifs.github.io/feed.xml" blog emacs)


;;; CANC Package: ivy-rich
;; Make ivy-switch-buffer look nicer. Not yet as nice as helm ... but it's a start :-)
;; Link: https://github.com/Yevgnen/ivy-rich
;; Unfortunately, it doesn't work "on demand", e.g. it always loads
;; itself, dired, ivy, swiper ...

(use-package ivy-rich
  :disabled t
  :if (and (not my-use-helm) (not noninteractive))
  :straight t
  :if (not noninteractive)

  :config
  (ivy-rich-mode 1)

  :custom
  (ivy-rich-path-style 'abbrev))


;;; CANC Package: semantic

(use-package semantic
  :disabled t
  :defer t)


;;; Local package: helm-compile
(use-package helm-compile
  :if (and my-use-helm (not noninteractive))
  :defer t
  :bind (("S-<f7>" . helm-select-compile-command)
		 ("<f7>"   . helm-compile)))



;;; Local package: qt-pro - mayor mode for Qt project files
;; From  https://raw.githubusercontent.com/chriskonstad/emacs/master/elisp/qt-pro.el

(use-package qt-pro
  :defer t
  :commands (qt-pro-mode)
  :mode ("\\.pr[io]\\'" . qt-pro-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Determine automatically loaded packages

;; First we create this empty.el file:
;; and then we run emacs on this file:

;; #+BEGIN_SRC shell :tangle no
;; emacs --batch -Q -l empty.el --kill
;; #+END_SRC

;; The result will be assigned to "features-when-run-emacs-Q". And if we
;; then find the intersection of the loaded packages between this batch
;; mode invocation and a normal invocation, we know what packages got
;; loaded due to our init.el file.

(defvar features-when-run-emacs-Q '(tooltip eldoc electric
  uniquify ediff-hook vc-hooks lisp-float-type mwheel term/x-win
  x-win term/common-win x-dnd tool-bar dnd fontset image
  regexp-opt fringe tabulated-list replace newcomment text-mode
  elisp-mode lisp-mode prog-mode register page menu-bar
  rfn-eshadow isearch timer select scroll-bar mouse jit-lock
  font-lock syntax facemenu font-core term/tty-colors frame
  cl-generic cham georgian utf-8-lang misc-lang vietnamese
  tibetan thai tai-viet lao korean japanese eucjp-ms cp51932
  hebrew greek romanian slovak czech european ethiopic indian
  cyrillic chinese composite charscript charprop case-table
  epa-hook jka-cmpr-hook help simple abbrev obarray minibuffer
  cl-preloaded nadvice loaddefs button faces cus-face macroexp
  files text-properties overlay sha1 md5 base64 format env
  code-pages mule custom widget hashtable-print-readable
  backquote dbusbind inotify lcms2 dynamic-setting
  system-font-setting font-render-setting move-toolbar gtk
  x-toolkit x multi-tty make-network-process emacs))

(defun features ()
  "Show loaded features."
  (interactive)
  (with-output-to-temp-buffer "*Features*"
	(let (value)
	  (dolist (elem (cl-set-difference features features-when-run-emacs-Q))
		(unless (s-suffix? "-autoloads" (symbol-name elem))
		  (princ (format "%s\n" (symbol-name elem))))))))

