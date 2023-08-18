;;; calc-repl.el --- interaction mode for algebraic Emacs Calc  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
;; Copyright (C) 2022 Holger Schurig

;; Author: Holger Schurig <holgerschurig@gmail.com>
;; Maintainer: emacs-devel@gnu.org
;; Created: 25 Feb 1994
;; Keywords: lisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO
;; Provides a nice interface to evaluating Emacs Lisp expressions.
;; Input is handled by the comint package, and output is passed
;; through the pretty-printer.

;; To start: M-x calc-repl.  Type C-h m in the *calc-repl* buffer for more info.

;;; Code:

(require 'calc)
(require 'comint)

;;; User variables

(defgroup calc-repl nil
  "Interaction mode for Emacs Lisp."
  :group 'calc)


;; TODO
(defcustom calc-repl-noisy t
  "If non-nil, calc-repl will beep on error."
  :type 'boolean)

(defcustom calc-repl-prompt-read-only t
  "If non-nil, the calc-repl prompt is read only.
The read only region includes the newline before the prompt.
Setting this variable does not affect existing calc-repl runs.
This works by setting the buffer-local value of `comint-prompt-read-only'.
Setting that value directly affects new prompts in the current buffer.

If this option is enabled, then the safe way to temporarily
override the read-only-ness of calc-repl prompts is to call
`comint-kill-whole-line' or `comint-kill-region' with no
narrowing in effect.  This way you will be certain that none of
the remaining prompts will be accidentally messed up.  You may
wish to put something like the following in your init file:

\(add-hook \\='calc-repl-mode-hook
          (lambda ()
             (define-key calc-repl-map \"\\C-w\" \\='comint-kill-region)
             (define-key calc-repl-map [C-S-backspace]
               \\='comint-kill-whole-line)))

If you set `comint-prompt-read-only' to t, you might wish to use
`comint-mode-hook' and `comint-mode-map' instead of
`calc-repl-mode-hook' and `calc-repl-map'.  That will affect all comint
buffers, including calc-repl buffers.  If you sometimes use calc-repl on
text-only terminals or with `emacs -nw', you might wish to use
another binding for `comint-kill-whole-line'."
  :type 'boolean
  :version "22.1")

(defcustom calc-repl-prompt "CALC> "
  "Prompt used in calc-repl.
Setting this variable does not affect existing calc-repl runs.

Interrupting the calc-repl process with \\<calc-repl-map>\\[comint-interrupt-subjob],
and then restarting it using \\[calc-repl], makes the then current
default value affect _new_ prompts.  Unless the new prompt
differs only in text properties from the old one, calc-repl will no
longer recognize the old prompts.  However, executing \\[calc-repl]
does not update the prompt of an *calc-repl* buffer with a running process.
For calc-repl buffers that are not called `*calc-repl*', you can execute
\\[calc-repl-mode] in that calc-repl buffer to update the value,
for new prompts.  This works even if the buffer has a running process."
  :type 'string)

(defvar calc-repl-prompt-internal "CALC> "
  "Stored value of `calc-repl-prompt' in the current calc-repl buffer.
This is an internal variable used by calc-repl.  Its purpose is to
prevent a running calc-repl process from being messed up when the user
customizes `calc-repl-prompt'.")

;; TODO
;; (defcustom calc-repl-dynamic-return t
;;   "Controls whether \\<calc-repl-map>\\[calc-repl-return] has intelligent behavior in calc-repl.
;; If non-nil, \\[calc-repl-return] evaluates input for complete sexps, or inserts a newline
;; and indents for incomplete sexps.  If nil, always inserts newlines."
;;   :type 'boolean)

(defcustom calc-repl-dynamic-multiline-inputs t
  "TODO Force multiline inputs to start from column zero?
If non-nil, after entering the first line of an incomplete sexp, a newline
will be inserted after the prompt, moving the input to the next line.
This gives more frame width for large indented sexps, and allows functions
such as `edebug-defun' to work with such inputs."
  :type 'boolean)

(defcustom calc-repl-mode-hook nil
  "Hooks to be run when calc-repl (`calc-repl-mode') is started."
  ;; TODO :options '(eldoc-mode)
  :type 'hook)

;; We define these symbols (that are only used buffer-locally in calc-repl
;; buffers) this way to avoid having them be defined in the global
;; Emacs namespace.
;; (defvar *)
;; (put '* 'variable-documentation "Most recent value evaluated in calc-repl.")

;; (defvar **)
;; (put '** 'variable-documentation "Second-most-recent value evaluated in calc-repl.")

;; (defvar ***)
;; (put '*** 'variable-documentation "Third-most-recent value evaluated in calc-repl.")

;; (defvar calc-repl-match-data nil
;;   "Match data saved at the end of last command.")

;; During calc-repl evaluation, *1 is the most recent value evaluated in
;; calc-repl.  Normally identical to `*'.  However, if the working buffer
;; is an calc-repl buffer, distinct from the process buffer, then `*' gives
;; the value in the working buffer, `*1' the value in the process
;; buffer.  The intended value is only accessible during calc-repl
;; evaluation.  *2 and *3 are the same for ** and ***.
;; (defvar *1)
;; (defvar *2)
;; (defvar *3)

;;; System variables

(defvar calc-repl-working-buffer nil
  "Buffer in which calc-repl will evaluate.
This variable is buffer-local.")

(defvar calc-repl-header
  (substitute-command-keys
   "*** Welcome to calc-repl ***  Type (describe-mode) or press \
\\[describe-mode] for help.\n")
  "Message to display when calc-repl is started.")

(defvaralias 'calc-repl-mode-map 'calc-repl-map)
(defvar-keymap calc-repl-map
  :doc "Keymap for calc-repl mode."
  ;; "TAB"     #'calc-repl-tab
  ;; "RET"     #'calc-repl-return
  "M-RET"   #'calc-repl-return-for-effect
  "RET"     #'calc-repl-send-input
  "M-TAB"   #'completion-at-point       ; TODO
  ;; These bindings are from `lisp-mode-shared-map' -- can you inherit
  ;; from more than one keymap??
  "DEL"     #'backward-delete-char-untabify
  ;; Some convenience bindings for setting the working buffer
  "C-c C-b" #'calc-repl-change-working-buffer
  "C-c C-f" #'calc-repl-display-working-buffer
  "C-c C-v" #'calc-repl-print-working-buffer)

(easy-menu-define calc-repl-menu calc-repl-map
  "calc-repl mode menu."
  '("calc-repl"
    ["Change Working Buffer" calc-repl-change-working-buffer t]
    ["Display Working Buffer" calc-repl-display-working-buffer t]
    ["Print Working Buffer" calc-repl-print-working-buffer t]))

;; TODO
;; (defvar calc-repl-font-lock-keywords
;;   '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
;;      (1 font-lock-comment-face)
;;      (2 font-lock-constant-face)))
;;   "Additional expressions to highlight in calc-repl buffers.")

;;; Completion stuff

;; TODO
;; (defun calc-repl-tab ()
;;   "Indent or complete."
;;   (interactive)
;;   (if (or (eq (preceding-char) ?\n)
;;           (eq (char-syntax (preceding-char)) ?\s))
;;       (calc-repl-indent-line)
;;     (completion-at-point)))

;; TODO
;; (defun calc-repl-complete-filename nil
;;   "Dynamically complete filename before point, if in a string."
;;   (when (nth 3 (parse-partial-sexp comint-last-input-start (point)))
;;     (comint-filename-completion)))

;; TODO
;; (defun calc-repl-indent-line nil
;;   "Indent the current line as Lisp code if it is not a prompt line."
;;   (when (save-excursion (comint-bol t) (bolp))
;;     (lisp-indent-line)))



;;; Working buffer manipulation

(defun calc-repl-print-working-buffer nil
  "Print the current calc-repl working buffer's name in the echo area."
  (interactive)
  (message "The current working buffer is: %s" (buffer-name calc-repl-working-buffer)))

(defun calc-repl-display-working-buffer nil
  "Display the current calc-repl working buffer.
Don't forget that selecting that buffer will change its value of `point'
to its value of `window-point'!"
  (interactive)
  (display-buffer calc-repl-working-buffer)
  (calc-repl-print-working-buffer))

(defun calc-repl-change-working-buffer (buf)
  "TODO Change the current calc-repl working buffer to BUF.
This is the buffer in which all sexps entered at the calc-repl prompt are
evaluated."
  (interactive "bSet working buffer to: ")
  (let ((buffer (get-buffer buf)))
    (if (and buffer (buffer-live-p buffer))
        (setq calc-repl-working-buffer buffer)
      (error "No such buffer: %S" buf)))
  (calc-repl-print-working-buffer))

;;; Other bindings

(defun calc-repl-return (&optional for-effect)
  "TODO Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `calc-repl-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (calc-repl-send-input for-effect))
  ;; (if calc-repl-dynamic-return
  ;;     (let ((state
  ;;            (save-excursion
  ;;              (end-of-line)
  ;;              (parse-partial-sexp (calc-repl-pm)
  ;;                                  (point)))))
  ;;       (if (and (< (car state) 1) (not (nth 3 state)))
  ;;           (calc-repl-send-input for-effect)
  ;;         (when (and calc-repl-dynamic-multiline-inputs
  ;;                    (save-excursion
  ;;                      (beginning-of-line)
  ;;                      (looking-at-p comint-prompt-regexp)))
  ;;           (save-excursion
  ;;             (goto-char (calc-repl-pm))
  ;;             (newline 1)))
  ;;         (newline-and-indent)))
  ;; (newline))

(defun calc-repl-return-for-effect ()
  "Like `calc-repl-return', but do not print the result."
  (interactive)
  (calc-repl-return t))

(defvar calc-repl-input)

(defun calc-repl-input-sender (_proc input)
  ;; Just sets the variable calc-repl-input, which is in the scope of
  ;; `calc-repl-send-input's call.
  (setq calc-repl-input input))

(defun calc-repl-send-input (&optional for-effect)
  "Evaluate the Emacs Lisp expression after the prompt."
  (interactive)
  (let (calc-repl-input)                     ; set by calc-repl-input-sender
    (comint-send-input)                 ; update history, markers etc.
    (calc-repl-eval-input calc-repl-input for-effect)))

;;; Utility functions

;; (defun calc-repl-is-whitespace-or-comment (string)
;;   "Return non-nil if STRING is all whitespace or a comment."
;;   (or (string= string "")
;;       (string-match-p "\\`[ \t\n]*\\(?:;.*\\)*\\'" string)))

;;; Evaluation

(defun calc-repl-standard-output-impl (process)
  "Return a function to use for `standard-output' while in calc-repl eval.
The returned function takes one character as input.  Passing nil
to this function instead of a character flushes the output
buffer.  Passing t appends a terminating newline if the buffer is
nonempty, then flushes the buffer."
  ;; Use an intermediate output buffer because doing redisplay for
  ;; each character we output is too expensive.  Set up a flush timer
  ;; so that users don't have to wait for whole lines to appear before
  ;; seeing output.
  (let* ((output-buffer nil)
         (flush-timer nil)
         (flush-buffer
          (lambda ()
            (comint-output-filter
             process
             (apply #'string (nreverse output-buffer)))
            (redisplay)
            (setf output-buffer nil)
            (when flush-timer
              (cancel-timer flush-timer)
              (setf flush-timer nil)))))
    (lambda (char)
      (let (flush-now)
        (cond ((and (eq char t) output-buffer)
               (push ?\n output-buffer)
               (setf flush-now t))
              ((characterp char)
               (push char output-buffer)))
        (if flush-now
            (funcall flush-buffer)
          (unless flush-timer
            (setf flush-timer (run-with-timer 0.1 nil flush-buffer))))))))

(defun calc-repl-eval-input (input-string &optional for-effect)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  (let ((output (calc-eval input-string)))
    (setq output (concat output "\n" calc-repl-prompt-internal))
    (comint-output-filter (calc-repl-process) output)))

  ;; (let ((string input-string)        ; input expression, as a string
  ;;       form                         ; form to evaluate
  ;;       pos                          ; End posn of parse in string
  ;;       result                       ; Result, or error message
  ;;       error-type                   ; string, nil if no error
  ;;       (output "")                  ; result to display
  ;;       (wbuf calc-repl-working-buffer)   ; current buffer after evaluation
  ;;       (pmark (calc-repl-pm)))
  ;;   (unless (calc-repl-is-whitespace-or-comment string)
  ;;     (condition-case err
  ;;         (let ((rout (read-from-string string)))
  ;;           (setq form (car rout)
  ;;                 pos (cdr rout)))
  ;;       (error (setq result (error-message-string err))
  ;;              (setq error-type "Read error")))
  ;;     (unless error-type
  ;;       ;; Make sure working buffer has not been killed
  ;;       (if (not (buffer-name calc-repl-working-buffer))
  ;;           (setq result "Working buffer has been killed"
  ;;                 error-type "calc-repl Error"
  ;;                 wbuf (current-buffer))
  ;;         (if (calc-repl-is-whitespace-or-comment (substring string pos))
  ;;             ;; To correctly handle the calc-repl-local variables *,
  ;;             ;; ** and ***, we need a temporary buffer to be
  ;;             ;; current at entry to the inner of the next two let
  ;;             ;; forms.  We need another temporary buffer to exit
  ;;             ;; that same let.  To avoid problems, neither of
  ;;             ;; these buffers should be alive during the
  ;;             ;; evaluation of form.
  ;;             (let* ((*1 (bound-and-true-p *))
  ;;                    (*2 (bound-and-true-p **))
  ;;                    (*3 (bound-and-true-p ***))
  ;;                    (active-process (calc-repl-process))
  ;;                    (old-standard-output standard-output)
  ;;                    new-standard-output
  ;;                    calc-repl-temp-buffer)
  ;;               (set-match-data calc-repl-match-data)
  ;;               (save-excursion
  ;;                 (with-temp-buffer
  ;;                   (condition-case-unless-debug err
  ;;                       (unwind-protect
  ;;                           ;; The next let form creates default
  ;;                           ;; bindings for *, ** and ***.  But
  ;;                           ;; these default bindings are
  ;;                           ;; identical to the calc-repl-local
  ;;                           ;; bindings.  Hence, during the
  ;;                           ;; evaluation of form, the
  ;;                           ;; calc-repl-local values are going to be
  ;;                           ;; used in all buffers except for
  ;;                           ;; other calc-repl buffers, which override
  ;;                           ;; them.  Normally, the variables *1,
  ;;                           ;; *2 and *3 also have default
  ;;                           ;; bindings, which are not overridden.
  ;;                           (let ((* *1)
  ;;                                 (** *2)
  ;;                                 (*** *3))
  ;;                             (when (eq standard-output t)
  ;;                               (setf new-standard-output
  ;;                                     (calc-repl-standard-output-impl
  ;;                                      active-process))
  ;;                               (setf standard-output new-standard-output))
  ;;                             (kill-buffer (current-buffer))
  ;;                             (set-buffer wbuf)
  ;;                             (setq result
  ;;                                   (eval form lexical-binding))
  ;;                             (setq wbuf (current-buffer))
  ;;                             (setq
  ;;                              calc-repl-temp-buffer
  ;;                              (generate-new-buffer " *calc-repl-temp*"))
  ;;                             (set-buffer calc-repl-temp-buffer))
  ;;                         (when calc-repl-temp-buffer
  ;;                           (kill-buffer calc-repl-temp-buffer))
  ;;                         (when (eq new-standard-output standard-output)
  ;;                           (ignore-errors
  ;;                             (funcall standard-output t))
  ;;                           (setf standard-output old-standard-output)))
  ;;                     (error (setq result (error-message-string err))
  ;;                            (setq error-type "Eval error"))
  ;;                     (quit (setq result "Quit during evaluation")
  ;;                           (setq error-type "Eval error")))))
  ;;               (setq calc-repl-match-data (match-data)))
  ;;           (setq error-type "calc-repl error")
  ;;           (setq result "More than one sexp in input"))))

  ;;     ;; If the eval changed the current buffer, mention it here
  ;;     (unless (eq wbuf calc-repl-working-buffer)
  ;;       (message "current buffer is now: %s" wbuf)
  ;;       (setq calc-repl-working-buffer wbuf))

  ;;     (goto-char pmark)
  ;;     (unless error-type
  ;;       (condition-case err
  ;;           ;; Self-referential objects cause loops in the printer, so
  ;;           ;; trap quits here. May as well do errors, too
  ;;           (unless for-effect
  ;;             (let* ((aux (let ((str (eval-expression-print-format result)))
  ;;                           (if str (propertize str 'font-lock-face 'shadow)))))
  ;;               (setq output (with-temp-buffer
  ;;                              (let ((tmpbuf (current-buffer)))
  ;;                                ;; Use print settings (e.g. print-circle,
  ;;                                ;; print-gensym, etc...) from the
  ;;                                ;; right buffer!
  ;;                                (with-current-buffer calc-replbuf
  ;;                                  (cl-prin1 result tmpbuf))
  ;;                                (pp-buffer)
  ;;                                (concat (buffer-string) aux))))))
  ;;         (error
  ;;          (setq error-type "calc-repl Error")
  ;;          (setq result (format "Error during pretty-printing: %S" err)))
  ;;         (quit  (setq error-type "calc-repl Error")
  ;;                (setq result "Quit during pretty-printing"))))
  ;;     (if error-type
  ;;         (progn
  ;;           (when calc-repl-noisy (ding))
  ;;           (setq output (concat output
  ;;                                "*** " error-type " ***  "
  ;;                                result)))
  ;;       ;; There was no error, so shift the *** values
  ;;       (setq *** (bound-and-true-p **))
  ;;       (setq ** (bound-and-true-p *))
  ;;       (setq * result))
  ;;     (when (or (not for-effect) (not (equal output "")))
  ;;       (setq output (concat output "\n"))))
  ;;   (setq output (concat output calc-repl-prompt-internal))
  ;;   (comint-output-filter (calc-repl-process) output)))

;;; Process and marker utilities

(defun calc-repl-process nil
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun calc-repl-pm nil
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun calc-repl-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

;;; Input fontification

;; TODO
;; (defcustom calc-repl-fontify-input-enable t
;;   "Enable fontification of input in calc-repl buffers.
;; This variable only has effect when creating an calc-repl buffer.  Use
;; the command `comint-fontify-input-mode' to toggle fontification
;; of input in an already existing calc-repl buffer."
;;   :type 'boolean
;;   :safe 'booleanp
;;   :version "29.1")

(defcustom calc-repl-indirect-setup-hook nil
  "Hook run in an indirect buffer for input fontification.
Input fontification and indentation of an calc-repl buffer, if
enabled, is performed in an indirect buffer, whose indentation
and syntax highlighting are set up with `emacs-lisp-mode'.  In
addition to `comint-indirect-setup-hook', run this hook with the
indirect buffer as the current buffer after its setup is done.
This can be used to further customize fontification and other
behavior of the indirect buffer."
  :type 'boolean
  :safe 'booleanp
  :version "29.1")

(defun calc-repl-indirect-setup-hook ()
  "Run `calc-repl-indirect-setup-hook'."
  (run-hooks 'calc-repl-indirect-setup-hook))

;;; Major mode

(define-derived-mode calc-repl-mode comint-mode "calc-repl"
  "Major mode for interactively evaluating Emacs Lisp expressions.
Uses the interface provided by `comint-mode' (which see).

* \\[calc-repl-return] TODO inserts a newline and indents, or evaluates a
  complete expression (but see variable `calc-repl-dynamic-return').
  Inputs longer than one line are moved to the line following the
  prompt (but see variable `calc-repl-dynamic-multiline-inputs').

* \\<calc-repl-map>\\[calc-repl-send-input] evaluates the
  expression following the prompt.

* \\[calc-repl-return-for-effect] works like `calc-repl-return', except
  that it doesn't print the result of evaluating the input.  This
  functionality is useful when forms would generate voluminous
  output.

* \\[completion-at-point] completes Lisp symbols (or filenames, within strings),
  or indents the line if there is nothing to complete.

The current working buffer may be changed (with a call to `set-buffer',
or with \\[calc-repl-change-working-buffer]), and its value is preserved between successive
evaluations.  In this way, expressions may be evaluated in a different
buffer than the *calc-repl* buffer.  By default, its name is shown on the
mode line; you can always display it with \\[calc-repl-print-working-buffer], or the buffer itself
with \\[calc-repl-display-working-buffer].

During evaluations, the values of the variables `*', `**', and `***'
are the results of the previous, second previous and third previous
evaluations respectively.  If the working buffer is another calc-repl
buffer, then the values in the working buffer are used.  The variables
`*1', `*2' and `*3', yield the process buffer values.

If, at the start of evaluation, `standard-output' is t (the
default), `standard-output' is set to a special function that
causes output to be directed to the calc-repl buffer.
`standard-output' is restored after evaluation unless explicitly
set to a different value during evaluation.  You can use (princ
VALUE) or (pp VALUE) to write to the calc-repl buffer.

The behavior of calc-repl may be customized with the following variables:
* To stop beeping on error, set `calc-repl-noisy' to nil.
* If you don't like the prompt, you can change it by setting `calc-repl-prompt'.
* If you do not like that the prompt is (by default) read-only, set
  `calc-repl-prompt-read-only' to nil.
* Set `calc-repl-dynamic-return' to nil for bindings like `lisp-interaction-mode'.
* Entry to this mode runs `comint-mode-hook' and `calc-repl-mode-hook'
 (in that order).

Customized bindings may be defined in `calc-repl-map', which currently contains:
\\{calc-repl-map}"
  :syntax-table emacs-lisp-mode-syntax-table
  :after-hook
  ;; TODO
  ;; (and (null comint-use-prompt-regexp)
  ;;      calc-repl-fontify-input-enable
  ;;      (comint-fontify-input-mode))

  (setq comint-prompt-regexp (concat "^" (regexp-quote calc-repl-prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'calc-repl-input-sender)
  (setq comint-process-echoes nil)
  ;; TODO
  ;; (add-hook 'completion-at-point-functions #'comint-replace-by-expanded-history nil t)
  ;; (add-hook 'eldoc-documentation-functions
  ;;           #'elisp-eldoc-var-docstring nil t)
  ;; (add-hook 'eldoc-documentation-functions
  ;;           #'elisp-eldoc-funcall nil t)
  (setq-local calc-repl-prompt-internal calc-repl-prompt)
  (setq-local comint-prompt-read-only calc-repl-prompt-read-only)
  (setq comint-get-old-input 'calc-repl-get-old-input)
  (setq-local comint-completion-addsuffix '("/" . ""))
  (setq mode-line-process '(":%s on " (:eval (buffer-name calc-repl-working-buffer))))
  ;; Useful for `hs-minor-mode'.
  ;; TODO
  ;; (setq-local comment-start ";")
  ;; (setq-local comment-use-syntax t)
  (setq-local lexical-binding t)

  ;; TODO
  ;; (setq-local indent-line-function #'calc-repl-indent-line)
  (setq-local calc-repl-working-buffer (current-buffer))
  ;; TODO
  ;; (setq-local fill-paragraph-function #'lisp-fill-paragraph)

  ;; Value holders
  ;; (setq-local * nil)
  ;; (setq-local ** nil)
  ;; (setq-local *** nil)
  ;; (setq-local calc-repl-match-data nil)

  ;; font-lock support
  ;; TODO
  ;; (setq-local font-lock-defaults
  ;;      '(calc-repl-font-lock-keywords nil nil ((?: . "w") (?- . "w") (?* . "w"))))

  (add-hook 'comint-indirect-setup-hook
            #'calc-repl-indirect-setup-hook 'append t)
  ;; TODO
  ;; (setq comint-indirect-setup-function #'emacs-lisp-mode)

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "calc-repl" (current-buffer) "hexl")
      (file-error (start-process "calc-repl" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (calc-repl-process) nil)
    (goto-char (point-max))

    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    (setq-local comint-inhibit-carriage-motion t)

    ;; Add a silly header
    (insert calc-repl-header)
    (calc-repl-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (calc-repl-process) calc-repl-prompt-internal)
    (set-marker comint-last-input-start (calc-repl-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun calc-repl-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;;; User command

;;;###autoload
(defun calc-repl (&optional buf-name)
  "Interactively evaluate Emacs Lisp expressions.
Switches to the buffer named BUF-NAME if provided (`*calc-repl*' by default),
or creates it if it does not exist.
See `calc-repl-mode' for details."
  (interactive)
  (let (old-point
        (buf-name (or buf-name "*calc-repl*")))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create buf-name)
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (calc-repl-mode)))
    (pop-to-buffer-same-window buf-name)
    (when old-point (push-mark old-point))))

(provide 'calc-repl)

;;; calc-repl.el ends here
