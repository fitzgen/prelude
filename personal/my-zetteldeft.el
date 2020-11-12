(use-package deft
  :ensure t
  :custom
  (deft-extensions '("md" "org" "txt"))
  (deft-directory (expand-file-name "~/notes"))
  (deft-use-filename-as-title t)
  (deft-recursive t))

(with-eval-after-load 'deft
  (define-key deft-mode-map
    (kbd "<tab>") 'deft-open-file-other-window))

(use-package zetteldeft
  :ensure t
  :after deft
  :config (zetteldeft-set-classic-keybindings)
  :custom
  (zetteldeft-home-id "2020-10-08-1549")
  (zetteldeft-link-indicator "[[")
  (zetteldeft-link-suffix "]]")
  (zetteldeft-title-prefix "# "))

(font-lock-add-keywords 'markdown-mode
                        `((,(concat "\\[\\["
                                    zetteldeft-id-regex
                                    "\\]\\]")
                           . font-lock-warning-face)))

(provide 'my-zetteldeft)
