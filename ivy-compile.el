;;; ivy-compile.el --- use ivy with your compile commands           -*- lexical-binding: t; -*-
(message "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
;; Copyright (C) 2017  Holger Schurig

;; Author: Holger Schurig <holgerschurig@gmail.com>
;; Keywords: development, compile, ivy
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps you in maintaining different compilation commands.
;;
;; Emacs itself comes with one compilation command preset: "make -k ".
;; Switching to another one is cumbersome, and therefore often not
;; done on-the-fly.
;;
;; However, this module uses ivy to prompt you for compile commands
;; and keeps a history of previous used compile commands.  Being ivy,
;; this allows you to easily search through your history.  There's also
;; an ivy action defined so that you
;;
;; @compile: (byte-compile-file "ivy-compile.el")

;;; Code:


;; Normally this is "make -k" and that's quite useless
(when (string= compile-command "make -k ")
  (setq compile-command ""))




(defvar ivy-compile-commands nil
  "List of previous compilation commands.

The compile commands are an alist where the key is
is the command and the value is the time when it was
executed the last time.  The latter is used for sorting.

 Example:

'((\"make\" .  \"1448748904\")
  (\"make -C ~/test\" . \"1448748866\"))")


;; automatically save our ivy-compile-commands
(defvar savehist-minibuffer-history-variables)
(add-to-list 'savehist-minibuffer-history-variables 'ivy-compile-commands)


(defun ivy-compile-sort-command-alist ()
  "Sort ‘ivy-compile-commands’ by the value of their cons elements.

This sorts the entries so that recently used compile commands
are near the top."
  (setq ivy-compile-commands (sort ivy-compile-commands (lambda (x y)
												  (not (string< (cdr x) (cdr y)))))))

(defun ivy-compile-add-command (cmd)
  "Add CMD to ‘ivy-compile-commands’ if it isn't already in it.

It inserts the seconds since 1970 into the value."
  ;; (message "adding command '%s'" cmd)
  (unless (string= "" cmd)
	(unless (assoc cmd ivy-compile-commands)
	  (add-to-list 'ivy-compile-commands (cons cmd (format-time-string "%s"))))))


(defun ivy-compile-del-command (cmd)
  "This deletes CMD from the ‘ivy-compile-commands’ list."
  (setq ivy-compile-commands
		(delq (assoc cmd ivy-compile-commands)
			  ivy-compile-commands)))


(defun ivy-compile-get-ivy-commands-from-buffers ()
  "Get compilation commands from open buffers.

Searches all open buffers that have a file-name associated and
adds compile commands from to ‘ivy-compile-commands’.  Valid forms
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
		(ivy-compile-del-command s)
		(ivy-compile-add-command s)))
	(goto-char (point-min))
	(while (re-search-forward "(setq compile-command \"\\(.*\\)\")" nil t)
	  (let ((s (match-string-no-properties 1)))
		;; (message "via setq '%s'" s)
		(ivy-compile-del-command s)
		(ivy-compile-add-command s)))))


(defun ivy-compile-default-action (cmd)
  "Default action that execute CMD."

  ;; (message "ivy-compile-default-action: '%s'" cmd)
  (when cmd
  	(ivy-compile-add-command cmd)
  	(setq compile-command cmd)
	(ivy-compile)))


(defun ivy-compile-del-action (cmd)
  "Action that deletes CMD from the list of commands."
  ;; (message "ivy-compile-del-action: '%s'" cmd)
  (ivy-compile-del-command cmd)
  (when (string= compile-command cmd)
	(setq compile-command nil)))


(defun ivy-select-compile-command ()
  "Interactively select a compilation command."
  (interactive)

  ;; Always get list of compilation commands and sort them
  (ivy-compile-get-ivy-commands-from-buffers)
  (ivy-compile-sort-command-alist)
  ;; ivy-compile-commands is now something like:
  ;; '(("make -C foo" . 1) ("ccmake && make" . 2))

  ;; http://oremacs.com/swiper/#api
  (ivy-read "cmd: " (delq "" (mapcar 'car ivy-compile-commands))
			:preselect compile-command
			:caller #'ivy-select-compile-command
			:action #'ivy-compile-default-action
			)
  )

(ivy-set-actions
 'ivy-select-compile-command
 '(("d" ivy-compile-del-action "delete")))
;;(ivy-select-compile-command)



(defun ivy-compile ()
  "Start a compilation.

If we haven't yet defined a compile command, a new one will be
selected with ivy's help."
  (interactive)
  (delete-other-windows)
  (save-some-buffers t)
  (if (string= compile-command "")
	  (ivy-select-compile-command)
	;; (message "compile command: %s" compile-command)
	(let ((cmd (assoc compile-command ivy-compile-commands)))
	  (when cmd
		;; (message "assoc: %s" (assoc compile-command ivy-compile-commands))
		(setcdr cmd (format-time-string "%s"))
		;; (message "assoc: %s" (assoc compile-command ivy-compile-commands))
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
	  (compile compile-command))))

(provide 'ivy-compile)
;;; ivy-compile.el ends here
