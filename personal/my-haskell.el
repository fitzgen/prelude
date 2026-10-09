;;; my-haskell --- My custom haskell stuff  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-hook 'haskell-mode-hook 'flymake-hlint-load)

(provide 'my-haskell)
;;; my-haskell.el ends here
