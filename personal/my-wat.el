(require 'treesit)

;; wat
(add-to-list
 'treesit-language-source-alist
 '(wat "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src"))

;; wast
(add-to-list
 'treesit-language-source-alist
 '(wast "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src"))

;; Before first usage, need to do:
;;
;;     M-x treesit-install-language-grammar
;;
;; and select `wat` and `wast` to install the shared libraries.

(autoload 'wat-ts-mode "wat-ts-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.wat\\'" . wat-ts-mode))

(autoload 'wat-ts-wast-mode "wat-ts-wast-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.wat\\'" . wat-ts-wast-mode))

(provide 'my-wat)
