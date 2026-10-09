;;; -*- lexical-binding: t; -*-

(prelude-require-packages '(ghostel))

;; Fetch ghostel's prebuilt native module on first use instead of prompting.
(setq ghostel-module-auto-install 'download)

;; `Emacs.app` is launched outside of a login shell, so it never sees the
;; Claude-related variables exported in `~/.bashrc`, and `claude` falls back to
;; asking for `/login`. Copy them from the shell when `claude-code-ide` is first
;; loaded (rather than at startup, since this spawns a login shell).
(defvar my-claude-env-vars
  '("ANTHROPIC_API_KEY"
    "ANTHROPIC_BASE_URL"
    "ANTHROPIC_MODEL"
    "ANTHROPIC_DEFAULT_HAIKU_MODEL"
    "ANTHROPIC_DEFAULT_OPUS_MODEL"
    "ANTHROPIC_DEFAULT_SONNET_MODEL"
    "CLAUDE_CODE_EFFORT_LEVEL"
    "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS"
    "CLAUDE_CODE_FORK_SUBAGENT"
    "CLAUDE_CODE_NO_FLICKER")
  "Environment variables to copy from the login shell for Claude Code.")

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-o" . claude-code-ide-menu)
  :custom
  ;; vterm (the default) garbles Claude's TUI when its window is resized, e.g.
  ;; by splits or transient popups; ghostel is upstream's recommended backend.
  (claude-code-ide-terminal-backend 'ghostel)
  :config
  (exec-path-from-shell-copy-envs my-claude-env-vars)
  (claude-code-ide-emacs-tools-setup)
  ;; Claude's TUI pads lines with spaces, which `show-trailing-whitespace'
  ;; (enabled globally in config.el) highlights everywhere. There's no hook for
  ;; new session buffers, so disable it on the buffer this function returns.
  (advice-add 'claude-code-ide--create-terminal-session :filter-return
              #'my-claude-disable-trailing-whitespace))

(defun my-claude-disable-trailing-whitespace (buffer-and-process)
  "Turn off `show-trailing-whitespace' in a new Claude session buffer.
BUFFER-AND-PROCESS is the cons returned by
`claude-code-ide--create-terminal-session', and is returned unchanged."
  (with-current-buffer (car buffer-and-process)
    (setq-local show-trailing-whitespace nil))
  buffer-and-process)

(provide 'my-claude)
