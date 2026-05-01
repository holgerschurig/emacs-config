;; -*- lexical-binding: t; -*-

(defun 2C-command ()
  (interactive)
  (save-buffer))

(keymap-unset global-map "<f2>") ;; was 2C-command, becomes save-bufferr
(keymap-unset global-map "<f2>" 'remove)
(keymap-unset global-map "<f3>") ;; becomes find-sibling-file
(keymap-unset global-map "<f3>" 'remove)
(keymap-unset global-map "C-x 6") ;; was 2C-command
(keymap-unset global-map "C-x 6" 'remove)
(keymap-set global-map "<f2>" 'save-buffer)

(message "TWO COLUMNS")
(save-buffer)
(let ((inhibit-message t))
  (keyboard-quit))

;;(provide 'two-column)
