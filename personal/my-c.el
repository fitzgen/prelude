(add-hook 'c-mode-hook
          '(lambda ()
             (linum-mode t)
             (local-set-key (kbd "C-c C-c") 'compile)
             (local-set-key (kbd "C-c g") 'gud-gdb)
             (local-set-key (kbd "C-c b") 'gud-break)
             ;; (ignore-errors (flymake-mode t))
             ))

(provide 'my-c)
