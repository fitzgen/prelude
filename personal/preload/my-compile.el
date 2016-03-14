(defun my-compile (cmd)
  ""
  (interactive
   (list (compilation-read-command compile-command)))
  (if (get-buffer "*shell*")
      (switch-to-buffer-other-window "*shell*")
    (shell))
  (clear-shell-buffer)
  (end-of-buffer)
  (comint-kill-input)
  (insert cmd)
  (end-of-line)
  (comint-send-input)
  (unless (equal cmd (eval compile-command))
    (setq compile-command cmd)))

(provide 'my-compile)
