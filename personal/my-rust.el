(require 'rustfmt)

;; Rust path
(prelude-require-packages '(rust-mode racer))
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Racer rust autocomplete config
(setq racer-rust-src-path (expand-file-name "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/"))
(setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
(eval-after-load "rust-mode" '(require 'racer))

(eval-after-load "rust-mode" '(require 'flycheck))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook
          '(lambda ()
             (linum-mode t)
             ;; (flycheck-mode t)

             (local-set-key (kbd "C-c C-c") 'my-compile)

             (racer-activate)
             (racer-turn-on-eldoc)
             (local-set-key (kbd "M-.") #'racer-find-definition)
             ;; (local-set-key (kbd "TAB") #'racer-complete-or-indent)
             ))

(setq rustfmt-bin (expand-file-name "~/.cargo/bin/rustfmt"))
;; (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

(provide 'my-rust)
