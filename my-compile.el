;;; my-compile.el --- use completion with compile commands           -*- lexical-binding: t; -*-
;; Copyright (C) 2021  Holger Schurig

;; Author: Holger Schurig <holgerschurig@gmail.com>
;; Keywords: development, compile, completion
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You didn't have received a copy of the GNU General Public License
;; along with this program.  So simply see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps you in maintaining different compilation commands.
;;
;; Emacs itself comes with one compilation command preset: "make -k ".
;; Switching to another one is cumbersome, and therefore often not
;; done on-the-fly.
;;
;; However, this module uses completion to prompt you for compile commands
;; and keeps a history of previous used compile commands.
;;
;; @compile: (byte-compile-file "my-compile.el")

;;; Code:


;; Normally this is "make -k" and that's quite useless
(when (string= compile-command "make -k ")
  (setq compile-command ""))




(defvar my-compile-commands nil
  "List of previous compilation commands.

Example:

'(\"make\"
 (\"make -C ~/test\")")

(defvar my-compile-commands-max 100
  "Max amount of compile history commands")


;; automatically save our my-compile-commands
(defvar savehist-minibuffer-history-variables)
(add-to-list 'savehist-minibuffer-history-variables 'my-compile-commands)


(defun my-compile-get-commands-from-buffers ()
  "Get compilation commands from open buffers.

Searches all open buffers that have a file-name associated and
adds compile commands from to ‘my-compile-commands’.  Valid forms
for compile commands in the source code are:

- // @compile: make foo
- ## @compile: make bar
- /* @compile: make foobar
- ;; @compile: (byte-compile-file \"foo.el\")
- (setq compile-command \"make\")

Or, in words: on two characters at the beginning of a line,
followed by ' @compile: ', followed by the compilation command.

As a special case for C commands, remove ' */' at the end of the
compilation command.

As a special case for elisp, also consider '(setq compile-command
\"foo\"), this doesn't need to be at the start of the line."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^..? @compile: \\(.*\\)$" nil t)
      (let ((s (match-string-no-properties 1)))
        ;; \s- whitespace character class
        (setq s (replace-regexp-in-string "\s-*\\*/$" "" s))
        ;; (message "FOUND '%s'" s)
        (unless (member s my-compile-commands)
          ;; (message "ADD '%s'" s)
          (add-to-history 'my-compile-commands s my-compile-commands-max))))))


(require 'consult nil nil)
(require 'embark nil nil)
(when (and (featurep 'consult) (featurep 'embark))
  (defun my-compile-del (s)
    "Remove element s from my-compile-commands."
    (delete s my-compile-commands))

  (defvar-keymap embark-compile-map
    :doc "Keymap with compile commands actions."
    :parent embark-general-map
    "d" #'my-compile-del)

  (add-to-list 'embark-keymap-alist '(compile . embark-compile-map)))


(defun my-compile-select-command ()
  "Interactively select a compilation command."
  (interactive)
  (let ((completion-styles '(substring)))
    (cond ((and (featurep 'consult) (featurep 'embark))
           (consult--read my-compile-commands
                          :prompt "cmd: "
                          :category 'compile))
          (t
           (completing-read "cmd: " my-compile-commands)))))


(defun my-compile ()
  "Start a compilation.

If we haven't yet defined a compile command, a new one will be
selected with completion help."
  (interactive)
  (delete-other-windows)
  (save-some-buffers t)
  (when (string= compile-command "")
      (setq compile-command (my-compile-select-command))
      (add-to-history 'my-compile-commands compile-command my-compile-commands-max))
  (setq compile-command (replace-regexp-in-string
                         " \\(%\\)"
                         (buffer-file-name (window-buffer))
                         compile-command
                         nil nil 1))
  (if (string= (substring compile-command 0 1) "(")
      (eval (car (read-from-string compile-command)))
    (let ((default-directory (project-root (project-current nil))))
      (compile (substring-no-properties compile-command)))))


(defun my-compile-select-command-and-run ()
  "Interactively select a compilation command and execute it."
  (interactive)
  (my-compile-get-commands-from-buffers)
  (setq compile-command (my-compile-select-command))
  (add-to-history 'my-compile-commands compile-command my-compile-commands-max)
  (unless (string= compile-command "")
    (my-compile)))

(provide 'my-compile)
