;;; my-shell --- My personal customizations for shell-mode.

;;; Commentary:

;;; Code:

(defun clear-shell-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 2))
    (comint-truncate-buffer)))

clean-buffer-list

;; Fix color in shell and make prompt read only.
(add-hook 'shell-mode-hook
          '(lambda ()
             (compilation-shell-minor-mode t)
             (local-set-key "\C-cl" 'clear-shell-buffer)
             (setq indent-tabs-mode nil)
             (setq tab-width 8)
             (ansi-color-for-comint-mode-on)
             (setq comint-prompt-read-only t)
             (setq show-trailing-whitespace nil)))

(provide 'my-shell)
