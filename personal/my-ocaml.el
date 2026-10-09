;;; -*- lexical-binding: t; -*-
(use-package tuareg :ensure t)

(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mlp\\'" . tuareg-mode))

(provide 'my-ocaml)
