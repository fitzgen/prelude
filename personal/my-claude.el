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
  :bind ("C-c j" . claude-code-ide-menu)
  :custom
  ;; vterm (the default) garbles Claude's TUI when its window is resized, e.g.
  ;; by splits or transient popups; ghostel is upstream's recommended backend.
  (claude-code-ide-terminal-backend 'ghostel)
  ;; Show sessions in ordinary windows (placed by `display-buffer' like any
  ;; other buffer) rather than in dedicated side windows.
  (claude-code-ide-use-side-window nil)
  :config
  (exec-path-from-shell-copy-envs my-claude-env-vars)
  (claude-code-ide-emacs-tools-setup)
  ;; There's no hook for new session buffers, so configure the buffer this
  ;; function returns.
  (advice-add 'claude-code-ide--create-terminal-session :filter-return
              #'my-claude-setup-session-buffer))

;; ghostel sends `C-g' to the terminal, where Claude opens the prompt in
;; `$EDITOR'. Make it quit as usual instead; `C-q C-g' still sends it.
(defvar-keymap my-claude-session-mode-map
  "<remap> <ghostel-send-C-g>" #'keyboard-quit)

(define-minor-mode my-claude-session-mode
  "Minor mode for Claude Code session buffers."
  :keymap my-claude-session-mode-map)

(defun my-claude-setup-session-buffer (buffer-and-process)
  "Configure a new Claude session buffer.
BUFFER-AND-PROCESS is the cons returned by
`claude-code-ide--create-terminal-session', and is returned unchanged."
  (with-current-buffer (car buffer-and-process)
    ;; Claude's TUI pads lines with spaces, which `show-trailing-whitespace'
    ;; (enabled globally in config.el) highlights everywhere.
    (setq-local show-trailing-whitespace nil)
    (my-claude-session-mode 1))
  buffer-and-process)

(provide 'my-claude)
