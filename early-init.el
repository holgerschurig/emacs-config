;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Based on: James Cherti, https://github.com/jamescherti/minimal-emacs.d
;; SPDX-License-Identifier: GPL-3.0-or-later



;;; Directories

(when (native-comp-available-p)
  (startup-redirect-eln-cache (locate-user-emacs-file  "var/")))

(setq custom-theme-directory (locate-user-emacs-file "themes/"))
(setq custom-file (locate-user-emacs-file "custom.el"))



;;; Device type definitions

(defvar is-mac (string= system-type "darwin") "Is this running on macOS?")
(defvar is-lnx (string= system-type "gnu/linux") "Is this running on Linux?")
(defvar is-win (string= system-type "windows-nt") "Is this running under Windows?")
(defvar is-way (getenv "WAYLAND_DISPLAY") "Is this running under Wayland?")
(defvar is-x11 (and (getenv "DISPLAY") (not is-way)) "Is this running under X11?")



;;; Variables

(defvar my-debug nil
  "Non-nil to enable debug.")

(defvar my-frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")

(defvar my--default-mode-line-format mode-line-format
  "Default value of `mode-line-format'.")



;;; Package: core/advice

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)



;;; Package: core/alloc.c (garbage collection)

;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))



;;; Package: core/buffer

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)



;;; Package: core/comp (compilation and byte-compilation)

;; Suppress compiler warnings and don't inundate users with their popups.
(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors (or my-debug 'silent))
  (setopt native-comp-warning-on-missing-source my-debug)
  (setq native-comp-jit-compilation t))

(setopt byte-compile-warnings my-debug)
(setopt byte-compile-verbose my-debug)




;;; Package: core/eval

(setopt debug-on-error my-debug)



;;; Package: core/fileio

(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; Determine the state of bundled libraries using calc-loaddefs.el.
     ;; If compressed, retain the gzip handler in `file-name-handler-alist`.
     ;; If compiled or neither, omit the gzip handler during startup for
     ;; improved startup and package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Ensure the new value persists through any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist
                                file-name-handler-alist)
    ;; Remember the old value to reset it as needed.
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite to preserve any changes made
                 ;; since startup.
                 (delete-dups (append file-name-handler-alist old-value))))
              101)))



;;; Package: core/ffap

;; Don't ping things that look like domain names.
(setopt ffap-machine-p-known 'reject)



;; Package: core/font.c

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)


;;; Package: core/frame

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(unless is-mac
  (push '(menu-bar-lines . 0)   default-frame-alist))
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setopt tool-bar-mode nil)

(unless (memq window-system '(mac ns))
  ;; (menu-bar-mode -1)
  (setopt menu-bar-mode nil))



;;; Package: core/fns.c

;; Disable GUIs because theyr are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setopt use-file-dialog nil)
(setopt use-dialog-box nil)

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setopt use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))


;;; Package: core/jka-compr

(setopt jka-compr-verbose my-debug)



;;; Package: core/lread.c

;; Prefer loading newer compiled files
(setopt load-prefer-newer t)



;;; Package: core/mule (language environment)

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setopt default-input-method nil)

;; Never show the hello file
(defalias #'view-hello-file #'ignore)



;;; Package: core/package

;; Despite this being a customizable variable, we must setq here, not setopt. Otherwise
;; package.el would be loaded and elpaca would complain.
(setq package-enable-at-startup nil)



;;; Package: core/process.c

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 256 1024))  ; 256kb



;;; Package: core/scroll-bar
(setopt scroll-bar-mode nil)

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))


;;; Package: core/simple

;; By default, Emacs "updates" its ui more often than it needs to
(setopt idle-update-delay 1.0)



;;; Package: core/startup

;; Disable startup screens and messages
(setopt inhibit-startup-screen t)



;;; Package: core/tooltip

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))



;;; Package: core/warnings

;; Ignore warnings about "existing variables being aliased".
(setopt warning-suppress-types '((defvaralias) (lexical-binding)))



;;; Package: core/xdisp.c

(setopt highlight-nonselected-windows nil)

(setq frame-title-format my-frame-title-format
      icon-title-format my-frame-title-format)



;;; Package: ide/lsp

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization

(setenv "LSP_USE_PLISTS" "true")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(unless (daemonp)
  (unless noninteractive
    (progn
      ;; Disable mode-line-format during init
      (defun my--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))

      (setq-default mode-line-format nil)

      (defun my--startup-load-user-init-file (fn &rest args)
        "Around advice for startup--load-user-init-file to reset mode-line-format."
        (let (init)
          (unwind-protect
              (progn
                (apply fn args)  ; Start up as normal
                (setq init t))
            (unless init
              ;; If we don't undo inhibit-{message, redisplay} and there's an
              ;; error, we'll see nothing but a blank Emacs frame.
              (my--reset-inhibited-vars-h))
            (unless (default-toplevel-value 'mode-line-format)
              (setq-default mode-line-format
                            my--default-mode-line-format)))))

      (advice-add 'startup--load-user-init-file :around
                  #'my--startup-load-user-init-file))

    ;; Without this, Emacs will try to resize itself to a specific column size
    (setq frame-inhibit-implied-resize t)

    ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
    ;; No second pass of case-insensitive search over auto-mode-alist.
    (setq auto-mode-case-fold nil)

    ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
    ;; dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for a modest performance boost.
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)

    ;; Give up some bidirectional functionality for slightly faster re-display.
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)))


(provide 'early-init)
