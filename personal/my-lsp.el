(require 'lsp)

(add-hook 'lsp-mode-hook
          '(lambda ()
             (lsp-modeline-code-actions-mode)
             ))

(setq lsp-signature-auto-activate t)

(provide 'my-lsp)
