(setq js-indent-level 2)

;; (defun comment-line-p ()
;;   (interactive)
;;   (nth 4 (syntax-ppss)))

;; (defun get-out-of-string ()
;;   (while (nth 3 (syntax-ppss))
;;     (backward-char)))

;; (defun find-column-of-matching-dot ()
;;   (interactive)
;;   (save-excursion
;;     (if (let ((res (equal (what-line) "Line 0")))
;;           (message "")
;;           res)
;;         nil
;;       (if (js-comment-line-p)
;;           (progn
;;             (previous-line)
;;             (find-column-of-matching-dot))
;;         (progn
;;           (get-out-of-string)
;;           (while (and (not (equal (current-column) 0))
;;                       (not (looking-at "\\.")))
;;             (backward-char))
;;           (when (not (equal (current-column) 0))
;;             (current-column)))))))

;; ;; Tern.JS integration.
;; (add-to-list 'load-path (expand-file-name "~/src/tern/emacs"))
;; (autoload 'tern-mode "tern.el" nil t)
;; (eval-after-load 'tern
;;    '(ignore-errors
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(defun maybe-enable-mozilla-eslint ()
  (interactive)
  (when (buffer-file-name)
    (let ((base (file-name-nondirectory (buffer-file-name))))
      (when (string-match "^\\([a-z]+_\\)" base)
        (setq-local flycheck-temp-prefix (match-string 1 base))))
    (let ((base-dir (locate-dominating-file (buffer-file-name)
                                            ".eslintignore")))
      (when base-dir
        (let ((eslint (expand-file-name
                       "testing/eslint/node_modules/.bin/eslint" base-dir)))
          (when (file-exists-p eslint)
            (setq-local flycheck-javascript-eslint-executable eslint)))))))

(add-hook 'js-mode-hook '(lambda ()
                           (maybe-enable-mozilla-eslint)
                           (flycheck-mode t)
                           (linum-mode t)))

;; Make js2-mode an alias for js-mode so that source which specifies
;; js2-mode doesn't mess with us.
(define-derived-mode js2-mode
  js-mode "JS Mode (alias as js2-mode)"
  "Alias for js-mode.
   \\{hypertext-mode-map}")

;; Treat Firefox .jsm JavaScript Modules as js files.
(add-to-list 'auto-mode-alist '("\\.jsm\\'" . javascript-mode))

(provide 'my-js)
