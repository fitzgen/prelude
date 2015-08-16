;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list' auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list' auto-mode-alist '("\\.markdown" . markdown-mode))

(defun jimb-markdown-fill-paragraph ()
  (interactive)
  (save-excursion
    (forward-line 0)
    (unless (looking-at paragraph-start)
      (backward-paragraph))
    (if (looking-at "^:")
        (let ((colon (point)))
          (delete-region colon (1+ colon))
          (insert "\n ")
          (fill-paragraph)
          (goto-char colon)
          (delete-region colon (+ 2 colon))
          (insert ":"))
      (fill-paragraph))))

(defun jimb-markdown-mode-hook ()
  (define-key markdown-mode-map (kbd "M-q") 'jimb-markdown-fill-paragraph)
  (setq markdown-regexp-list "^\\([ \t]*\\)\\([0-9]+\\.\\|[\\*\\+-:]\\)\\([ \t]+\\)"))

(add-hook 'markdown-mode-hook 'jimb-markdown-mode-hook)

;; (add-hook 'markdown-mode-hook
;;           '(lambda ()
;;              (auto-fill-mode t)))

(provide 'my-markdown)
