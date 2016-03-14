(require 'my-compile)

(add-hook 'c++-mode-hook
          '(lambda ()
             (linum-mode t)
             (local-set-key (kbd "C-c C-c") 'my-compile)
             (local-set-key (kbd "C-c g") 'gud-gdb)
             (local-set-key (kbd "C-c b") 'gud-break)
             (local-set-key (kbd "C-c C-l") 'pages-directory)
             ;; (ignore-errors (flymake-mode t))
             ))

(provide 'my-cpp)
