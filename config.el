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

;; Sensible default key bindings for non-evil users
(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

;; Allow scrolling up and down
(global-set-key (kbd "C-S-<up>")   (kbd "C-u 1 M-v"))
(global-set-key (kbd "C-S-<down>") (kbd "C-u 1 C-v"))



;;; Package: core/align

(defadvice my-align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'my-align-regexp)



;;; Package: core/auth-sources

(after! auth-source
  (setq auth-sources (list (concat doom-etc-dir "authinfo.gpg")
                           "~/.authinfo.gpg"
                           "~/.authinfo")))



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
      (switch-to-buffer (other-buffer (current-buffer) 1))
    (other-window -1)))

(map! "<f6>" #'my-switch-to-buffer)




;;; Package: core/calc
(after! calc
  (setq calc-angle-mode 'rad  ; radians are radians, 0..2*pi
        calc-symbolic-mode t))



;;; Package: core/cus-edit

(after! cus-edit
  ;; keep lisp names in the custom buffers, don't capitalize.
  (setq custom-unlispify-tag-names nil)
  ;; kill old buffers.
  (setq custom-buffer-done-kill t))




;;; Package: core/dictionary

(use-package! dictionary
  :custom
  (dictionary-server "dict.org")
  (dictionary-use-single-buffer t)

  :general
  ("C-c d" #'dictionary-lookup-definition)
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

  ;; Preserve hard links to the file you´re editing (this is
  ;; especially important if you edit system files)
  (backup-by-copying-when-linked t)

  ;; Just never create backup files at all
  ;; (make-backup-files nil)

  ;; Alternatively put backup files into their own directory
  (backup-directory-alist (list (cons "." (locate-user-emacs-file "tmp/bak/"))))

  :general
  ("<f2>" #'save-buffer)
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

  ;; Do less flickering be removing highlighting immediately
  (setq lazy-highlight-initial-delay 0))




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
  (setq register-preview-delay 1)
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



;;; Package: core/minibuf

(setq history-delete-duplicates t)

(setq resize-mini-windows t)

;; don't show "*Completions*" buffer
(setq completion-auto-help nil)




;;; Package: core/mule-util

(after! mule-util
  (setq truncate-string-ellipsis "…"))



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



;;; Package: core/sendmail

(after! sendmail
  (setq sendmail-program "msmtp"
        send-mail-function #'smtpmail-send-it))



;;; Package: core/shr

(after! shr
  (setq shr-color-visible-luminance-min 80
        shr-bullet "• "
        shr-folding-mode t))


;;; Package: core/simple

(after! simple
  ;; The following may be of interest to people who (a) are happy with
  ;; "C-w" and friends for killing and yanking, (b) use
  ;; "transient-mark-mode", (c) also like the traditional Unix tty
  ;; behaviour that "C-w" deletes a word backwards. It tweaks "C-w" so
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
  (setq set-mark-command-repeat-pop t))

(defun my-next-error (&optional arg)
  "Tries first flymake-goto-next-error, spell-fu-goto-next-error, then next-error."
  (interactive)
  (if (and (boundp 'flymake-mode) flymake-mode)
      (condition-case nil
          (call-interactively #'flymake-goto-next-error arg)
        (error (if (and (boundp 'spell-fu-mode) spell-fu-mode)
                   (condition-case nil
                       (call-interactively #'spell-fu-goto-next-error)
                     (error (next-error arg)))
                 (next-error arg))))))

(defun my-previous-error (&optional arg)
  "Tries first flymake-goto-previous-error, spell-fu-goto-previous-error, then previous-error."
  (interactive)
  (if (and (boundp 'flymake-mode) flymake-mode)
      (condition-case nil
          (call-interactively #'flymake-goto-prev-error arg)
        (error (if (and (boundp 'spell-fu-mode) spell-fu-mode)
                   (condition-case nil
                       (call-interactively #'spell-fu-goto-previous-error)
                     (error (previous-error arg)))
                 (previous-error arg))))))

(map! "C-x I" #'insert-buffer

      "M-SPC" #'cycle-spacing   ;; was: just-one-space

      "M-c" #'capitalize-dwim
      "M-l" #'downcase-dwim
      "M-u" #'upcase-dwim

      "M-o" #'delete-blank-lines  ; opposite of C-o

      ;; Error navigation
      "<f8>"    #'my-next-error
      "S-<f8>"  #'my-previous-error)




;;; Package: core/timer-list

;; Enable this command
(put 'list-timers 'disabled nil)



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

(map! "C-x k" #'kill-buffer-and-window)


;;; Package: core/xref

(use-package! xref
  :custom
  (xref-file-name-display 'project-relative) ;; was abs
  (xref-search-program 'ripgrep)             ;; was grep

  :config
  (advice-remove #'xref-push-marker-stack #'doom-set-jump-a)
)



;;; Package: gui/display-line-numbers

;; This removes the display of the line numbers

(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)




;;; Package: gui/minibuffer

(after! minibuffer
  (setq history-length 1000)

  (minibuffer-depth-indicate-mode 1)

  ;; Allow to type space chars in minibuffer input (for `timeclock-in',
  ;; for example).
  (define-key minibuffer-local-completion-map " " nil)
  (define-key minibuffer-local-must-match-map " " nil)

  ;; Don't insert current directory into minubuffer
  (setq insert-default-directory nil))




;;; Package: gui/nswbuff

(after! nswbuff
  (setq nswbuff-display-intermediate-buffers t
        nswbuff-exclude-buffer-regexps '("^ .*" "^\\*.*\\*")))

(map! "S-<f5>" #'nswbuff-switch-to-previous-buffer
      "S-<f6>" #'nswbuff-switch-to-next-buffer)




;;;; Package: gui/whitespace

;; (after! whitespace
;;   (setq whitespace-global-modes nil))
;; ;; unfortunately, removing doom-highlight-non-default-indentation-h from
;; ;; change-major-mode-hook didn't work, it was somehow added again so I define a
;; ;; dummy function to override doom's weird behavior of turning white-space
;; ;; mode on at unwanted times.
;; (defun doom-highlight-non-default-indentation-h ()
;;   "Dummy")

;; (map! "C-c w" #'whitespace-mode)





;;; Package: theme/font-core

;; I got the idea from here:
;; http://amitp.blogspot.de/2013/05/emacs-highlight-active-buffer.html

(defun highlight-focus:app-focus-in ()
  (global-font-lock-mode 1))

(defun highlight-focus:app-focus-out ()
  (global-font-lock-mode -1))

(add-hook 'focus-in-hook  #'highlight-focus:app-focus-in)
(add-hook 'focus-out-hook #'highlight-focus:app-focus-out)




;;; Package: theme/font-lock

(after! font-lock
  (setq font-lock-maximum-decoration 2))  ;; was t



;;; Package: theme/modus-themes
;; https://protesilaos.com/codelog/2019-08-07-emacs-modus-themes/
;; https://gitlab.com/protesilaos/modus-themes
;; https://github.com/protesilaos/modus-themes/blob/main/doc/modus-themes.org


(setq doom-theme 'modus-vivendi)

(use-package! modus-vivendi-theme
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  ;;(modus-themes-slanted-constructs t)
  (modus-themes-intense-markup t)

  :config
  ;; Make the marked region be much easier visible
  (set-face-attribute 'region nil :background "#6c6c6c")
)



;;; Package: edit/autorevert

(after! autorevert
  (setq global-auto-revert-non-file-buffers t
        auto-revert-interval 1
        auto-revert-verbose nil))



;;; Package: edit/clean-aindent-mode

;; Nice tip from tuhdo, see https://www.emacswiki.org/emacs/CleanAutoIndent
(add-hook 'prog-mode-hook #'clean-aindent-mode)




;;; Package: edit/expand-region

(after! expand-region
  (setq expand-region-reset-fast-key    "<ESC><ESC>"))

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
  (defalias 'loop 'cl-loop)
  (hydra-kurecolor/body))




;;; Package: edit/indent

;; This variable is used in indent-for-tab-command and calls and calls out to
;; completion-at-point
(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      tab-first-completion 'eol)




;;; Package: edit/newcomment

(map! "C-c c" #'comment-dwim)



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

(map! "M-p"      #'symbol-overlay-jump-prev
      "M-n"      #'symbol-overlay-jump-next
      "M-<up>"   #'symbol-overlay-jump-prev
      "M-<down>" #'symbol-overlay-jump-next
      )





;;; Package: edit/tabify
(after! tabify
  ;; only tabify initial whitespace
  (setq tabify-regexp "^\t* [ \t]+"))





;;; Package: edit/undo-tree

(after! undo-tree
  ;; Disable undo-in-region. It sounds like an interesting feature,
  ;; but unfortunately the implementation is very buggy and regularly
  ;; causes you to lose your undo history.
  (setq undo-tree-enable-undo-in-region nil)

  ;; don't save history persistently
  (setq undo-tree-auto-save-history nil)

  (setq undo-tree-visualizer-timestamps t)
  (map! "C-z" #'undo-tree-visualize)
)




;;; Package: misc/embark

;; The following keymaps are already existing, so you can just add actions to
;; them. Then position the cursor somewhere and do "C-,". To see then which
;; actions are available, run "C-h".
;;
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

(after! embark
  (message "EMBARK")
  (require 'embark-consult)

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

  ;; Now that we don't use which-keys anymore, remove this indicator
  (remove-hook 'embark-indicators #'+vertico-embark-which-key-indicator)
  (remove-hook 'embark-indicators #'embark-minimal-indicator)
  (remove-hook 'embark-indicators #'embark-highlight-indicator)
  (remove-hook 'embark-indicators #'embark-isearch-highlight-indicator)
  (add-hook 'embark-indicators #'embark-minimal-indicator 10)
  (add-hook 'embark-indicators #'embark-highlight-indicator 20)
  (add-hook 'embark-indicators #'embark-isearch-highlight-indicator 30)

  ;; This allows you to use C-; after a prefix key, e.g. "C-x C-;" to get
  ;; an embark-narrowable list of items
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Make some actions work without a special query
  (assq-delete-all 'delete-file                   embark-pre-action-hooks)
  (assq-delete-all 'delete-directory              embark-pre-action-hooks)
  (assq-delete-all 'kill-buffer                   embark-pre-action-hooks)
  (assq-delete-all 'embark-kill-buffer-and-window embark-pre-action-hooks)

  (define-key! :keymaps '(embark-collect-mode-map)
    "M-t" #'toggle-truncate-lines)

  (define-key!
    :keymaps '(embark-general-map)
    ;; this is used to cycle where it works on (e.g. file, symbol, defun)
    "C-;" #'embark-cycle
    ;; L used to be embark-collect-life, which isn't that helpful if one already uses
    ;; something like vertico or selectrum
    "L" nil)

  (define-key!
    :keymaps '(embark-variable-map)
    ;; Used to be customize-variable, but that is locked on Doom. Better use "=" to set
    ;; the variable
    "u" nil)

  (define-key!
    :keymaps '(embark-function-map embark-variable-map embark-expression-map)
    "l" #'eldoc)

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
  ;; work in a Norton Commander like mode if 2 panes are open
  (setq dired-dwim-target t))

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
  (add-hook 'helpful-mode-hook #'visual-line-mode))

(map! "<f1> h" #'helpful-at-point
      :map helpful-mode-map
      "q" #'kill-buffer-and-window)




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

(after! pdf-tools
  (add-hook 'pdf-view-mode-hook #'pdf-view-auto-slice-minor-mode)
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
  (defhydra hydra-pdftools (:color blue :hint nil)
    "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^   ↦ _W_ ↤  [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
    ("\\" hydra-master/body "back")
    ("<ESC>" nil "quit")
    ("al" pdf-annot-list-annotations)
    ("ad" pdf-annot-delete)
    ("aa" pdf-annot-attachment-dired)
    ("am" pdf-annot-add-markup-annotation)
    ("at" pdf-annot-add-text-annotation)
    ("y"  pdf-view-kill-ring-save)
    ("+" pdf-view-enlarge :color red)
    ("-" pdf-view-shrink :color red)
    ("0" pdf-view-scale-reset)
    ("H" pdf-view-fit-height-to-window)
    ("W" pdf-view-fit-width-to-window)
    ("P" pdf-view-fit-page-to-window)
    ("n" pdf-view-next-page-command :color red)
    ("p" pdf-view-previous-page-command :color red)
    ("d" pdf-view-dark-minor-mode)
    ("b" pdf-view-set-slice-from-bounding-box)
    ("r" pdf-view-reset-slice)
    ("g" pdf-view-first-page)
    ("G" pdf-view-last-page)
    ("e" pdf-view-goto-page)
    ("o" pdf-outline)
    ("s" pdf-occur)
    ("i" pdf-misc-display-metadata)
    ("u" pdf-view-revert-buffer)
    ("F" pdf-links-action-perform)
    ("f" pdf-links-isearch-link)
    ("B" pdf-history-backward :color red)
    ("N" pdf-history-forward :color red)
    ("l" image-forward-hscroll :color red)
    ("h" image-backward-hscroll :color red))

  (map! :map pdf-view-mode-map
        "?"  #'hydra-pdftools/body
        "g"  #'pdf-view-first-page
        "G"  #'pdf-view-last-page
        "l"  #'image-forward-hscroll
        "h"  #'image-backward-hscroll
        "j"  #'pdf-view-next-page
        "k"  #'pdf-view-previous-page
        "e"  #'pdf-view-goto-page
        "u"  #'pdf-view-revert-buffer
        "al" #'pdf-annot-list-annotations
        "ad" #'pdf-annot-delete
        "aa" #'pdf-annot-attachment-dired
        "am" #'pdf-annot-add-markup-annotation
        "at" #'pdf-annot-add-text-annotation
        "y"  #'pdf-view-kill-ring-save
        "i"  #'pdf-misc-display-metadata
        "s"  #'pdf-occur
        "b"  #'pdf-view-set-slice-from-bounding-box
        "r"  #'pdf-view-reset-slice))



;;; Package: modes/scratch

;; Adapted from the `scratch.el' package by Ian Eure.
(defun my-list-scratch-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           for name = (symbol-name sym)
           when (and (functionp sym)
                     (not (member sym minor-mode-list))
                     (string-match "-mode$" name)
                     (not (string-match "--" name)))
           collect name))

(defun my-scratch-buffer-setup (text &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
         (buf (format "*Scratch for %s*" major)))
    (with-current-buffer (get-buffer-create buf)
      (funcall major)
      (save-excursion
        (insert text)
        (goto-char (point-min)))
      (vertical-motion 2))
    (pop-to-buffer buf)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Scratch for ")
               (display-buffer-reuse-window display-buffer-same-window)
               (reusable-frames . visible))
             )

(defun my-scratch-buffer (&optional arg)
  "Produce a bespoke scratch buffer matching current major mode.

With optional ARG argument, prompt for a major mode
with completion.

If region is active, copy its contents to the new scratch
buffer."
  (interactive "P")
  (let ((modes (my-list-scratch-modes))
        (region (with-current-buffer (current-buffer)
                  (if (region-active-p)
                      (buffer-substring-no-properties
                       (region-beginning)
                       (region-end))
                    "")))
         (m))
    (pcase (prefix-numeric-value arg)
      (4 (progn
            (setq m (intern (completing-read "Scratch buffer mode: " modes nil t)))
            (my-scratch-buffer-setup region m)))
      (_ (my-scratch-buffer-setup region)))))


(map! "M-g s" #'my-scratch-buffer)



;;; Package: modes/spell-fu

(after! spell-fu
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "de"))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
  (spell-fu-dictionary-add
   (spell-fu-get-personal-dictionary "de" (concat spell-fu-directory "/de.pws")))
  (spell-fu-dictionary-add
   (spell-fu-get-personal-dictionary "en" (concat spell-fu-directory "/en.pws")))
)



;;; Package: completion/consult

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

  ;; use consult/vertico for completion
  (completion-in-region-function #'consult-completion-in-region)

  :general
  ;; C-c bindings (mode-specific-map)
  ("C-c h"   #'consult-history)
  ("C-c m"   #'consult-mode-command)
  ("C-c k"   #'consult-kmacro)

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
  ("M-g i"    #'consult-imenu)
  ("M-g I"    #'consult-imenu-multi)

  ;; M-s bindings (search-map)
  ("M-s e"    #'consult-isearch-history)
  ("M-s f"    #'consult-find)
  ("M-s F"    #'consult-locate)
  ("M-s g"    #'consult-grep)
  ("M-s G"    #'consult-git-grep)
  ("M-s r"    #'consult-ripgrep)
  ("M-s l"    #'consult-line)                ;; similar to swiper
  ("M-s L"    #'consult-line-multi)
  ("M-s m"    #'consult-multi-occur)
  ("M-s k"    #'consult-keep-lines)
  ("M-s u"    #'consult-focus-lines)         ;; run with "C-u M-s u" to show all lines again

  (isearch-mode-map "M-e"   #'consult-isearch-history)     ;; was: isearch-edit-string
  (isearch-mode-map "M-s e" #'consult-isearch-history)     ;; was: isearch-edit-string
  (isearch-mode-map "M-s l" #'consult-line)                ;; needed by consult-line to detect isearch
  (isearch-mode-map "M-s L" #'consult-line-multi)          ;; needed by consult-line to detect isearch

  (minibuffer-local-map "M-s" #'consult-isearch-history)   ;; was: next-matching-history-element
  (minibuffer-local-map "M-r" #'consult-isearch-history)   ;; was: previous-matching-history-element

  (compilation-mode-map       "e" #'consult-compile-error)
  (compilation-minor-mode-map "e" #'consult-compile-error)

  ;; Unfortunately, the DEL key is hijacked by vertico, so I cannot unnorrow
  ;; But we don't really need the delete-forward-char function, so we can use
  ;; that key to unnarrow.
  (consult-narrow-map "<deletechar>" #'consult--narrow-delete)     ;; was: delete-forward-char
)


;;; Package: completion/cape

;; see https://github.com/minad/cape

(use-package! cape
  ;; Bind dedicated completion commands
  :general
  ("C-c p p"  #'completion-at-point) ;; capf
  ("C-c p t"  #'complete-tag)        ;; etags
  ("C-c p d"  #'cape-dabbrev)        ;; or dabbrev-completion
  ("C-c p f"  #'cape-file)
  ("C-c p k"  #'cape-keyword)
  ("C-c p s"  #'cape-symbol)
  ("C-c p a"  #'cape-abbrev)
  ("C-c p i"  #'cape-ispell)
  ("C-c p l"  #'cape-line)
  ("C-c p w"  #'cape-dict)           ;; see cape-dict-file
  ("C-c p \\" #'cape-tex)            ;; see (describe-input-method "TeX")
  ("C-c p _"  #'cape-tex)
  ("C-c p ^"  #'cape-tex)
  ("C-c p <"  #'cape-sgml)           ;; see (describe-input-method "sgml")
  ("C-c p r"  #'cape-rfc1345)        ;; see (describe-input-method "rfc1345")

  :custom
  (cape-dabbrev-min-length 2)
  ;; (cape-dabbrev-check-other-buffers nul)

  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
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

  :custom
  (tempel-path (concat doom-private-dir "templates.el"))

  :general
  ("M-+" #'tempel-complete)    ;;  completes a template name at point in the buffer and subsequently expands the template
  ("M-*" #'tempel-insert)      ;;  selects a template by name and insert it into the current buffer

  (tempel-map "C-g" #'tempel-done)

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
    ;; only triggers on exact matches. Alternatively use `tempel-complete' if
    ;; you want to see all matches, but then Tempel will probably trigger too
    ;; often when you don't expect it.
    ;; NOTE: We add `tempel-expand' *before* the main programming mode Capf,
    ;; such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

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
   ("C-c k" #'vertico-repeat-last)
   ("C-h"   #'embark-bindings)
   :map vertico-map
   ([next] nil)     ;; was next-history-element
   ([prior] nil)   ;; was previous-history-element
   ("M-<up>"  #'next-history-element)
   ("M-<down>" #'previous-history-element))
)



;;; Package: lang/c-mode

(after! cc-mode
  (defun my-c-mode-setup ()
    ;;(eglot-ensure)

    ;; need to check the mode because I run this also at the revert hook!
    (modify-syntax-entry ?_ "w")
    (setq c-recognize-knr-p nil)

    ;; might later be changed by dtrt-indent, but this is the default for new files
    (setq indent-tabs-mode t)

    ;; use "// " for commenting in both C and C++
    (setq comment-start "// "
          comment-end "")

    (c-add-style "qt-gnu"
                 '("gnu" (c-access-key .
                        "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")))

    (if (and buffer-file-name (string-match "/linux" buffer-file-name))
       ;; only for Linux C files
       (progn (c-set-style "linux-tabs-only")
            (setq tab-width 8
              c-basic-offset 8))
      (c-set-style "qt-gnu")
      (setq tab-width 4
            c-basic-offset 4)
    ))
  (add-hook 'c-mode-hook #'my-c-mode-setup)
  (add-hook 'c++-mode-hook #'my-c-mode-setup)
  (setq-default c-electric-flag nil)
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
  ;; Replace evil-last-sexp with pp-eval-last-sexp, as this gives nicer results
  (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

  (defun my-emacs-lisp-mode-setup ()
    (interactive)
    "My emacs lisp mode setup function."
    ;; "-" is almost always part of a function- or variable-name
    (modify-syntax-entry ?- "w")

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


;; Start eglot either with (eglot-ensure) or with Doom's (lsp!)
;;
;; If you don't have a compilation database (e.g. in projects not using
;; CMake or Meson), you can use https://github.com/rizsotto/Bear

(after! eglot
  (map! :map eglot-mode-map
        "C-c R" #'eglot-rename                        ;; unused in c-mode
        "C-c a" #'eglot-code-actions                  ;; was embark-act, but that is also in C-;
        "C-c o" #'eglot-code-action-organize-imports  ;; unused in c-mode
        "C-c r" #'xref-find-references                ;; was dictionary-lookup-definition
        ;; M-g f  consult-flymake                     ;; for errors, maybe even warnings
        ;; M-.    xref-find-definitions
        )

  ;; This disables the overrides of general-override-mode-map, which steals C-c a
  (general-override-mode 0)
  (defun turn-off-eldoc-mode ()
    (eldoc-mode -1))
  (add-hook 'eglot-managed-mode-hook #'turn-off-eldoc-mode)
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
        org-startup-indented nil)
  (electric-indent-mode -1)
  ;; PlantUML
  (setq org-plantuml-jar-path "/usr/local/bin/plantuml.1.2020.16.jar")
  (org-babel-do-load-languages 'org-babel-load-languages
                                 '(plantuml . t))
  (remove-hook 'org-mode-hook #'org-superstar-mode)
  (remove-hook 'org-open-at-point-functions #'doom-set-jump-h)

  (setq org-return-follows-link t)
)



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

;; (use-package! ox-hugo
;;   :commands (org-hugo-export-wim-to-md)
;;   :after ox
;; )
(after! org
    (defun org2hugo-ensure-properties ()
    (let ((mandatory `(("EXPORT_HUGO_SECTION" . "en")
                       ("EXPORT_FILE_NAME" . "filename")
                       ("EXPORT_DATE" . ,(format-time-string "%Y-%m-%d" (org-current-time)))))
          (optional '(("EXPORT_HUGO_TAGS" . "")
                      ("EXPORT_HUGO_CATEGORIES" . "")))
          (first))

      ;; Insert path to content directory
      (unless (car (plist-get (org-export-get-environment 'hugo) :hugo-base-dir))
        (save-excursion
          (goto-char 1)
          (insert "#+HUGO_BASE_DIR: ../\n\n")))
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
    (save-window-excursion
      (unless (org2hugo-ensure-properties)
        (let ((title (org-entry-get nil "TITLE" t))
              (file "/tmp/blog.md") ;; TODO
              (blog
               ))

          ;; Create block
          (end-of-line)
          (search-backward ":EXPORT_HUGO_SECTION:")
          (org-hugo-export-wim-to-md)
          ))))

  (map! :map org-mode-map
        "C-c h" #'org2hugo)
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
    "c" #'notmuch-mua-new-mail)

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
  "M-g n" #'my-notmuch-hello ;; was next-error
  "M-g p" nil             ;; was previous-error
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


;;; This and that


(defun notmuch-poll ()
  "Run \"notmuch new\" or an external script to import mail.

Invokes `notmuch-poll-script', \"notmuch new\", or does nothing
depending on the value of `notmuch-poll-script'."
  (interactive)
  (message "Polling mail...")
  (if (stringp notmuch-poll-script)
      (unless (string-empty-p notmuch-poll-script)
        (unless (equal (call-process notmuch-poll-script nil nil) 0)
          (error "Notmuch: poll script `%s' failed!" notmuch-poll-script)))
    (notmuch-call-notmuch-process "new"))
  (message "Polling mail...done"))
