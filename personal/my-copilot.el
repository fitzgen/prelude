;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

(require 'cl)
(let ((pkg-list '(use-package
                   s
                   dash
                   editorconfig
                   company)))
  (package-initialize)
  (when-let ((to-install (map-filter (lambda (pkg _) (not (package-installed-p pkg))) pkg-list)))
    (package-refresh-contents)
    (mapc (lambda (pkg) (package-install pkg)) pkg-list)))

(use-package copilot
  :load-path (lambda () (expand-file-name "personal/copilot" user-emacs-directory)))

;; Only required the first time:
;;
;; (copilot-install-server)
;; (copilot-login)

;; Enable copilot everywhere...
(global-copilot-mode)

;; ...but then disable it in certain modes.
(defun no-copilot-mode ()
  "Helper for `no-copilot-modes'."
  (copilot-mode -1))
(defvar no-copilot-modes '(shell-mode
                           inferior-python-mode
                           eshell-mode
                           term-mode
                           vterm-mode
                           comint-mode
                           compilation-mode
                           debugger-mode
                           dired-mode-hook
                           compilation-mode-hook
                           flutter-mode-hook
                           minibuffer-mode
                           minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")
(defun my-copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or (member major-mode no-copilot-modes)
      (company--active-p)))
(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

(defun my-copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "M-C-<return>") #'my-copilot-complete-or-accept)

(defun my-copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'my-copilot-quit)

(provide 'my-copilot)
