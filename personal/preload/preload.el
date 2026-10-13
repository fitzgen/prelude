;;; -*- lexical-binding: t; -*-
;; Don't load prelude themes because zenburn (its default that it
;; can't stop from trying to load) is broken for some reason.
(setq prelude-theme nil)

;; Skip Prelude's welcome screen so that startup shows the buffers restored by
;; `desktop-save-mode' (see config.el). This must be set before Prelude's
;; `prelude-welcome' is loaded, hence living in preload.
(setq prelude-welcome-screen nil)

;; Prelude's own files lack `lexical-binding' cookies, which Emacs 31 warns
;; about, popping up `*Warnings*' on every startup. Don't display those warnings
;; (they're still logged in `*Warnings*'), but keep showing them for files in
;; `personal/'.
(dolist (file (append (directory-files prelude-dir t "\\.el\\'")
                      (directory-files-recursively prelude-core-dir "\\.el\\'")
                      (directory-files-recursively prelude-modules-dir "\\.el\\'")
                      (directory-files-recursively
                       (expand-file-name "sample" prelude-dir) "\\.el\\'")))
  (add-to-list 'warning-suppress-types
               `(files missing-lexbind-cookie ,(abbreviate-file-name file))))
