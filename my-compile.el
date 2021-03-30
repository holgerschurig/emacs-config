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

The compile commands are an alist where the key is
is the command and the value is the time when it was
executed the last time.  The latter is used for sorting.

 Example:

'((\"make\" .  \"1448748904\")
  (\"make -C ~/test\" . \"1448748866\"))")


;; automatically save our my-compile-commands
(defvar savehist-minibuffer-history-variables)
(add-to-list 'savehist-minibuffer-history-variables 'my-compile-commands)


(defun my-compile-sort-command-alist ()
  "Sort ‘my-compile-commands’ by the value of their cons elements.

This sorts the entries so that recently used compile commands
are near the top."
  (setq my-compile-commands (sort my-compile-commands (lambda (x y)
                                                        (not (string< (cdr x) (cdr y)))))))

(defun my-compile-add-command (cmd)
  "Add CMD to ‘my-compile-commands’ if it isn't already in it.

It inserts the seconds since 1970 into the value."
  ;; (message "adding command '%s'" cmd)
  (unless (string= "" cmd)
    (unless (assoc cmd my-compile-commands)
      (add-to-list 'my-compile-commands (cons cmd (format-time-string "%s"))))))


(defun my-compile-del-command (cmd)
  "This deletes CMD from the ‘my-compile-commands’ list."
  (setq my-compile-commands
        (delq (assoc cmd my-compile-commands)
              my-compile-commands)))


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
        ;; (message "FOUND '%s'" s)
        (setq s (replace-regexp-in-string "\s-*\\*/$" "" s))
        (my-compile-del-command s)
        (my-compile-add-command s)))
    (goto-char (point-min))
    (while (re-search-forward "(setq compile-command \"\\(.*\\)\")" nil t)
      (let ((s (match-string-no-properties 1)))
        ;; (message "via setq '%s'" s)
        (my-compile-del-command s)
        (my-compile-add-command s)))))


(defun my-compile-default-action (cmd)
  "Default action that execute CMD."

  ;; (message "my-compile-default-action: '%s'" cmd)
  (when cmd
    (my-compile-add-command cmd)
    (setq compile-command cmd)
    (my-compile)))


(defun my-compile-del-action (cmd)
  "Action that deletes CMD from the list of commands."
  ;; (message "my-compile-del-action: '%s'" cmd)
  (my-compile-del-command cmd)
  (when (string= compile-command cmd)
    (setq compile-command nil)))


(defun my-compile-get-commands (current-input)
  (delq "" (mapcar 'car my-compile-commands)))

(defun my-compile-select-command ()
  "Interactively select a compilation command."
  (interactive)

  ;; Always get list of compilation commands and sort them
  (my-compile-get-commands-from-buffers)
  (my-compile-sort-command-alist)
  ;; my-compile-commands is now something like:
  ;; '(("make -C foo" . 1) ("ccmake && make" . 2))

  (setq compile-command
        (cond ((fboundp 'ivy-read)
               ;; http://oremacs.com/swiper/#api
               (ivy-read "cmd: "
                         #'my-compile-get-commands
                         :caller #'my-compile-select-command
                         :action #'my-compile-default-action))
              ((fboundp 'selectrum--read)
               (let ((selectrum-should-sort-p nil))
                 (selectrum--read "cmd: "
                                 #'my-compile-get-commands
                                 :default-candidate compile-command
                                 :no-move-default-candidate t
                                 :may-modify-candidates t
                                 :require-match nil
                                 )))
              (t
               (completing-read "cmd: "
                                #'my-compile-get-commands
                                :require-match nil))
              )))


(when (boundp 'ivy-read)
  (ivy-set-actions
   'my-compile-select-command
   '(("d" my-compile-del-action "delete"))))


(defun my-compile ()
  "Start a compilation.

If we haven't yet defined a compile command, a new one will be
selected with completion help."
  (interactive)
  (delete-other-windows)
  (save-some-buffers t)
  (if (string= compile-command "")
      (my-compile-select-command)
    ;; (message "compile command: %s" compile-command)
    (let ((cmd (assoc compile-command my-compile-commands)))
      (when cmd
        ;; (message "assoc: %s" (assoc compile-command my-compile-commands))
        (setcdr cmd (format-time-string "%s"))
        ;; (message "assoc: %s" (assoc compile-command my-compile-commands))
        ))
    ;; (message "compile command: %s" compile-command)
    (setq compile-command (replace-regexp-in-string
                           " \\(%\\)"
                           (buffer-file-name (window-buffer))
                           compile-command
                           nil nil 1)))
  (if (string= (substring compile-command 0 1) "(")
      (eval (car (read-from-string compile-command)))
    (let ((default-directory (or (locate-dominating-file "." ".git")
                                 (locate-dominating-file "." ".svn")
                                 (locate-dominating-file "." "CMakeLists.txt")
                                 (locate-dominating-file "." "GNUmakefile")
                                 (locate-dominating-file "." "Makefile")
                                 default-directory)))
      (cd (or (locate-dominating-file (buffer-file-name) ".git") "."))
      (compile (substring-no-properties compile-command)))))


(defun my-compile-select-command-and-run ()
  "Interactively select a compilation command and execute it."
  (interactive)
  (my-compile-select-command)
  (unless (string= compile-command "")
    (my-compile)))

(provide 'my-compile)
