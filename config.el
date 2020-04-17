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

(add-hook 'message-mode-hook #'word-wrap-mode)

(defun 822date ()
  "Insert date at point format the RFC822 way."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y %H:%M:%S %z")))

(defun dos2unix()
  "Convert MSDOS (^M) end of line to Unix end of line."
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defalias 'sudo-edit 'doom/sudo-this-file
   "Edit currently visited file as root.")




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




;;; Package: core/cus-edit

(after! cus-edit
  ;; keep lisp names in the custom buffers, don't capitalize.
  (setq custom-unlispify-tag-names nil)
  ;; kill old buffers.
  (setq custom-buffer-done-kill t))




;;; Package: core/files

(after! files
  (setq confirm-kill-emacs nil)

  ;; Preserve hard links to the file you´re editing (this is
  ;; especially important if you edit system files)
  (setq backup-by-copying-when-linked t)

  ;; Just never create backup files at all
  ;; (make-backup-files nil)

  ;; Alternatively put backup files into their own directory
  (setq backup-directory-alist (list (cons "." (locate-user-emacs-file "tmp/bak/"))))

  ;; Make files with shebang executable
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

(map! "<f2>" #'save-buffer)




;;; Package: core/isearch

(after! isearch
  ;; Scrolling (including C-s) while searching:
  (setq isearch-allow-scroll t)

  ;; Do less flickering be removing highlighting immediately
  (setq lazy-highlight-initial-delay 0)
  (setq search-whitespace-regexp ".*"))




;;; Package: core/recentf

(after! recentf
  (setq   recentf-exclude '("^/tmp/"
                            "/\\.newsrc"
                            ".*CMakeFiles.*"
                            "bbdb$"
                            "svn-commit\\.tmp$"
                            ".*-autoloads\\.el\\'"
                            "\\.png$"
                            "COMMIT_EDITMSG" "COMMIT_EDITMSG" "TAG_EDITMSG")
          recentf-max-saved-items 1000
          recentf-auto-cleanup 300
          recentf-max-menu-items 20))




;;; Package: minibuf

(setq history-delete-duplicates t)

(setq resize-mini-windows t)

(defun my-keyboard-quit ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  ;; See: https://with-emacs.com/posts/tips/quit-current-context/
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; if we got this far just use the default so we don't miss
         ;; any upstream changes
         (keyboard-quit))))
(global-set-key [remap keyboard-quit] #'my-keyboard-quit)




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

  (setq kill-ring-max 500))

(map! "C-x I" #'insert-buffer

      "M-SPC" #'cycle-spacing   ;; was: just-one-space

      "M-c" #'capitalize-dwim
      "M-l" #'downcase-dwim
      "M-u" #'upcase-dwim

      "M-o" #'delete-blank-lines  ; opposite of C-o

      ;; Error navigation
      "<f8>"   #'next-error
      "S-<f8>"  #'previous-error)




;;; Package: core/window

(map! "C-x k" #'kill-buffer-and-window)




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




;;; Package: gui/whitespace

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
  (setq font-lock-maximum-decoration 2)
  (setq-default font-lock-multiline t))




;;; Package: theme/modus-vivendi
;; https://protesilaos.com/codelog/2019-08-07-emacs-modus-themes/
;; https://gitlab.com/protesilaos/modus-themes

(setq doom-theme 'modus-vivendi)

(use-package! modus-vivendi-theme
  :config
  (setq modus-vivendi-theme-slanted-constructs t)
  (setq modus-vivendi-theme-bold-constructs t)

  (load-theme 'modus-vivendi t)

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
Set          _sj_ ^^ brightness      _sk_ ^^ saturation      _sl_ ^^ hue
Get          _gj_ ^^ brightness      _gk_ ^^ saturation      _gl_ ^^ hue

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




;;; Package: edit/indent

;; This variable is used in indent-for-tab-command and calls and calls out to
;; completion-at-point
(setq tab-always-indent 'complete)




;;; Package: edit/newcomment

(map! "C-c c" #'comment-dwim)



;;; Package: edit/smartparens

;; I hate this package, so I don't want it. But I must define some fake functions
;; in order to mitigate errors.
;; (cl-defun sp-local-pair (modes
;;                          open
;;                          close
;;                          &key
;;                          trigger
;;                          trigger-wrap
;;                          (actions '(:add))
;;                          (when '(:add))
;;                          (unless '(:add))
;;                          (pre-handlers '(:add))
;;                          (post-handlers '(:add))
;;                          wrap
;;                          bind
;;                          insert
;;                          prefix
;;                          suffix
;;                          skip-match)
;;   "Dummy")
;; (defun turn-off-smartparens-mode ()
;;   "Dummy")
;; (defun sp-point-in-comment (&optional pos)
;;   "Dummy"
;;   nil)




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

  (setq undo-tree-visualizer-timestamps t))

(map! "C-z" #'undo-tree-visualize)




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

(map! "C-x C-d" #'dired-jump  ;; "C-x d" is dired
      :map dired-mode-map
      "q" #'dired-up-directory)





;;; Package: modes/ediff

(after! ediff
  :config
  (setq ediff-split-window-function 'split-window-vertically)

  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))




;;; Package: modes/helpful

(after! helpful
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helpful" )
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . visible))
               )
  (add-hook 'helpful-mode-hook #'visual-line-mode))

(map! "<f1> h" #'helpful-at-point)



;;; Package: modes/pdf-tools

(after! pdf-tools
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




;;; Package: completion/ivy

(after! ivy
  (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and recent files
  (setq ivy-count-format "(%d/%d) ")     ; count format, from the ivy help page

  (setq ivy-use-selectable-prompt t)     ; allow partial candidates be selected

  (setq ivy-format-function 'ivy-format-function-arrow)

  ;; Don't show ./ and ../ when finding files with ivy. Use backspace to go up.
  (setq ivy-extra-directories nil)

  (defun my-ivy-sort (X Y)
    "Similar to ivy-string<, but also takes the length of the string into consideration."
    (let ((xx (if (consp X) (car X) X))
          (yy (if (consp Y) (car Y) Y)))
      (or (< (length xx) (length yy))
          (and (= (length xx) (length yy)) (string< xx yy)))))
  ;; (sort '("c" "b" "a") #'my-ivy-sort)
  ;; (sort '("c" "b" "a" "cc" "bb" "aa") #'my-ivy-sort)
  ;; (sort '("general-define-key" "general-evil-define-key") #'my-ivy-sort)
  ;; (ivy-read "Test: " '("cc" "c" "b" "a" "bb" "aa"))
  ;; (ivy-read "Test: " '("cc" "c" "b" "a" "bb" "aa") :sort t)
  (setq ivy-sort-functions-alist
        '(
          (read-file-name-internal . ivy-sort-file-function-default)
          (t . my-ivy-sort)
          ))


  ;; Don't require order, so 'func descr' matches 'describe-function',
  ;; at least once you remove the ^ at the start of the query
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-ignore-order)))

  ;; (setq ivy-sort-functions-alist '((t . my-ivy-sort)))
  )

(map! "C-x C-b"   #'ivy-switch-buffer
      "C-c C-r"   #'ivy-resume

      "C-x C-f"   #'counsel-find-file
      "C-x 8 RET" #'counsel-unicode-char
      "M-s g"     #'counsel-git-grep
      "M-g i"     #'counsel-imenu
      "M-y"       #'counsel-yank-pop ;; see http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
      "M-s o"     #'swiper)



;;; Package: lang/c-mode

(after! cc-mode
  (defun my-c-mode-setup ()
    ;; need to check the mode because I run this also at the revert hook!
    (modify-syntax-entry ?_ "w")
    (setq c-recognize-knr-p nil)

    ;; (if (and buffer-file-name (string-match "/linux" buffer-file-name))
    ;;    ;; only for Linux C files
    ;;    (progn (c-set-style "linux-tabs-only")
    ;;         (setq tab-width 8
    ;;           c-basic-offset 8))
    ;;   (c-set-style "linux")
    ;;   (setq tab-width 4
    ;;         c-basic-offset 4))))
    )
  (add-hook 'c-mode-hook #'my-c-mode-setup)
  (add-hook 'c++-mode-hook #'my-c-mode-setup)
  (set-company-backend! '(c-mode c++-mode)
    '(company-dabbrev-code  ; all symbols of current buffer that aren't strings/code
      company-keywords      ; programming language keywords
      company-yasnippet
      ))
  )
(map! "C-ä" #'company-complete)




;;; Package: lang/compile

(after! compile
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
(add-hook 'prog-mode-hook #'my-show-trailing-whitespace)




;;; Package: lang/ivy-compile

(use-package! ivy-compile
  ;; :if (and (not my-use-helm) (not noninteractive))
  :load-path doom-private-dir
  :defer t
  :bind (("S-<f7>" . ivy-select-compile-command)
         ("<f7>"   . ivy-compile)))




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




;;; Package: lang/vc-hooks

(after! vc-hooks
  ;; Remove most back-ends from vc-mode
  (setq vc-handled-backends '(Git)))



;;; Package: org/org

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(after! org
  (setq org-directory "~/org/")
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
