(require 'my-lsp)

(prelude-require-packages '(rust-mode
                            ;; racer
                            ))
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-c") 'my-compile)

             (linum-mode t)
             (company-mode)
             ;; (racer-activate)

             (lsp)
             (lsp-ui-mode)
             (lsp-ui-doc-enable t)

             (local-set-key (kbd "C-c C-c") 'my-compile)
             (local-set-key (kbd "C-c C-e") 'lsp-rust-analyzer-expand-macro)

             (local-set-key (kbd "M-.") #'lsp-find-definition)

             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)

             ;; Racer seems to be better at finding definitions than rust-analyzer.
             ;; (local-set-key (kbd "M-.") #'racer-find-definition)

             ;; (racer-turn-on-eldoc)
             ;; (local-set-key (kbd "TAB") #'racer-complete-or-indent)
             ))

(setq rust-rustfmt-bin (expand-file-name "~/.cargo/bin/rustfmt"))

(setq lsp-rust-server 'rust-analyzer)
(setq rust-format-show-buffer nil)

(setq
 lsp-rust-analyzer-server-command
 (expand-file-name "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))

;; Racer rust autocomplete config
;; (setq racer-rust-src-path (expand-file-name "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/"))
;; (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
;; (eval-after-load "rust-mode" '(require 'racer))

;; Rust files can start with something like `#![deny(missing_docs)]`. Emacs will
;; helpfully mark any file that starts with `#!` as executable, because it
;; assumes it is a script. Disable this behavior for Rust.
(add-function :around (symbol-function 'executable-make-buffer-file-executable-if-script-p)
              (lambda (make-executable)
                (unless (string= major-mode "rust-mode")
                  (make-executable))))

(provide 'my-rust)
