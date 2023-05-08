(require 'page-ext)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ============================== UTILITIES ====================================

(fset 'down-and-tab
      "\C-e\C-m\C-i")

(fset 'up-and-tab
      [?\C-a return up tab])

(fset 'triple-split-window
      [?\C-x ?1 ?\C-x ?3 ?\C-x ?3 ?\C-x ?+ ?\C-x ?b return ?\C-x ?o ?\C-x ?b return ?\C-x ?o ?\C-x ?b return ?\C-x ?o ?\C-x ?o ?\C-x ?b return ?\C-x ?o ?\C-x ?o ?\C-x ?o])


;; ============================== HOTKEYS ==============================

;; Undo prelude's mucking with C-a
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)
;; Undo prelude's mucking with C-o
(global-set-key [remap other-window] 'other-window)

(define-key prelude-mode-map (kbd "C-o") 'crux-smart-open-line)
(define-key prelude-mode-map (kbd "M-o") 'crux-smart-open-line-above)

(global-set-key (kbd "M-#") 'comment-or-uncomment-region)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-x 4") 'triple-split-window)
(global-set-key (kbd "C-c C-l") 'pages-directory)

;; Make TAB cycle through completions.
(with-eval-after-load 'company
  (define-key company-active-map [tab] 'company-complete-common-or-cycle))

;; Disable prelude's `C-c d` so I can use it for zetteldeft.
(define-key prelude-mode-map (kbd "C-c d") nil)

;; ============================== SETTINGS TWEAKS ==============================

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Start the emacs server, so that programs that need to open $EDITOR open
;; emacs.
(server-start)

;; Company mode should put annotations on the right.
(setq company-tooltip-align-annotations t)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.0)

;; Keep the menu bar.
(menu-bar-mode 1)
;; Remove the tool bar.
(tool-bar-mode 0)
;; Remove the scroll bar.
(scroll-bar-mode 0)

;; Clean buffers that have not been used in a week every midnight.
(require 'midnight)
(setq clean-buffer-list-delay-general 7)

;; Scroll with a buffer of 5 lines, 1 at a time.
(setq scroll-margin 5)
(setq scroll-conservatively 1)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

;; Delete trailing whitespace on save.
(defcustom delete-trailing-whitespace-on-save t
  "Whether to delete trailing whitespace on save or not"
  :type '(boolean)
  :options '(t nil))
(add-hook 'before-save-hook
          (lambda ()
            (when delete-trailing-whitespace-on-save
              (delete-trailing-whitespace))))
(setq-default show-trailing-whitespace t)

;; Get rid of thick separators.
(fringe-mode 0)

;; Enable linum-mode for all modes except inside special buffers.
(add-hook 'change-major-mode-hook
          (lambda ()
            (unless (or (string-match "^\*.+\*$" (buffer-name))
                        (string-match "^magit:.\*$" (buffer-name)))
              (linum-mode t))))

;; Change the format of linum-mode and add a hotkey to toggle it on
;; and off.
(setq linum-format "%4d ")
(global-set-key "\M-n" 'linum-mode)

;; Highlight the line that the cursor is on.
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#557")
;; (set-face-background 'highlight "#225")
;; (set-face-background 'region "#225")
(set-face-background 'cursor "#fff")

;; Highlight matching parentheses.
(show-paren-mode t)

;; Use spaces instead of tabs, default to 4 spaces for an indent.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq c-basic-offset 4)

;; Enable backup files.
(setq make-backup-files t)
;; Enable versioning of backup files.
(setq version-control t)
;; Save all backup files to this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Show column-number in the mode line.
(column-number-mode 1)

;; Only load and save desktops to the home dir.
(setq desktop-path (list (expand-file-name "~/")))
(setq desktop-dirname (expand-file-name "~/"))
(setq desktop-base-file-name ".emacs.desktop")
(add-hook 'auto-save-hook (lambda ()
                            (desktop-save-in-desktop-dir)))
(desktop-save-mode t)

;; Wrap lines at 80 characters.
(setq-default fill-column 80)

;; Replaces default names of "foo.py<2>" with "foo.py/bar" for all duplicates.
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
;; Rename all buffers after killing a uniquified buffer. This way a uniquified
;; buffer can return to its normal name if possible.
(setq uniquify-after-kill-buffer-p t)
;; Don't mess with special buffers
(setq uniquify-ignore-buffers-re "^\\*")

;; Ensure that the emacs path is the same as the OSX path.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq split-height-threshold nil)
(setq split-width-threshold 160)

(setq completion-cycle-threshold t)

(setq pages-directory-buffer-narrowing-p t)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'flycheck)
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode))
(add-to-list 'flycheck-checkers 'proselint)

;; For Cranelift's ISLE DSL.
(add-to-list 'auto-mode-alist '("\\.isle\\'" . lisp-mode))

;; WAT/WAST.
(add-to-list 'auto-mode-alist '("\\.wat\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.wast\\'" . lisp-mode))

(provide 'config)
;;; config.el ends here
