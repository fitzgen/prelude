;;; my-claude-sidebar.el --- Sidebar listing Claude Code sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; A side window, one per frame, that lists every live `claude-code-ide'
;; session with its status: needs input, working, done, idle or error.
;; `C-c J' shows, selects and hides it.
;;
;; claude-code-ide doesn't track turn state, so sessions it starts get extra
;; Claude Code hooks (via `--settings') that POST each hook payload to a small
;; HTTP listener in this Emacs.

;;; Code:

(require 'cl-lib)
(require 'hl-line)
(require 'subr-x)

(eval-when-compile (require 'claude-code-ide-mcp nil t))

(defvar claude-code-ide--session)
(defvar claude-code-ide-cli-path)

(declare-function claude-code-ide "claude-code-ide" ())
(declare-function claude-code-ide-rename-session "claude-code-ide" ())
(declare-function claude-code-ide--cleanup-session "claude-code-ide" (session &optional buffer-dying))
(declare-function claude-code-ide-mcp--active-sessions "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp-session-session-id "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-instance-name "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-process "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-port "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-cleanup-done "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-cli-pid "claude-code-ide-mcp" (session))
(declare-function project-prompt-project-dir "project" (&optional prompt))
(declare-function transient-append-suffix "transient" (prefix loc suffix))
(declare-function transient-get-suffix "transient" (prefix loc))

;; ================================== OPTIONS ==================================

(defgroup my-claude-sidebar nil
  "Sidebar listing Claude Code sessions."
  :group 'tools)

(defcustom my-claude-sidebar-width 35
  "Width of the sidebar window, in columns."
  :type 'natnum)

(defcustom my-claude-sidebar-side 'left
  "Side of the frame the sidebar is shown on."
  :type '(choice (const left) (const right)))

(defcustom my-claude-sidebar-auto-show-states '(needs-input error)
  "States that show the sidebar when a session enters them."
  :type '(set (const needs-input) (const working) (const done) (const error)))

(defcustom my-claude-sidebar-message-states '(needs-input error)
  "States that echo a message when a session enters them."
  :type '(set (const needs-input) (const working) (const done) (const error)))

(defcustom my-claude-sidebar-snooze-minutes 10
  "Minutes that hiding the sidebar turns off auto-show in that frame."
  :type 'number)

(defcustom my-claude-sidebar-preview-delay 0.05
  "Seconds to wait after moving in the sidebar before previewing.
Showing a terminal in a differently-sized window resizes it and makes
Claude redraw, so don't preview every row while holding `n'."
  :type 'number)

(defface my-claude-sidebar-needs-input '((t :inherit (warning bold)))
  "Face for sessions that need input.")

(defface my-claude-sidebar-working '((t :inherit success))
  "Face for working sessions.")

(defface my-claude-sidebar-done '((t :inherit font-lock-function-name-face))
  "Face for sessions that finished a turn you haven't seen.")

(defface my-claude-sidebar-idle '((t :inherit shadow))
  "Face for idle sessions.")

(defface my-claude-sidebar-error '((t :inherit error))
  "Face for failed sessions.")

(defface my-claude-sidebar-dim '((t :inherit shadow))
  "Face for the status and directory lines.")

(defconst my-claude-sidebar--states
  '((needs-input "!" "needs input" my-claude-sidebar-needs-input)
    (working "●" "working" my-claude-sidebar-working)
    (done "✓" "done" my-claude-sidebar-done)
    (idle "·" "idle" my-claude-sidebar-idle)
    (error "✗" "error" my-claude-sidebar-error))
  "Each state's glyph, status word and face, in header order.")

(defconst my-claude-sidebar--buffer-name "*Claude Sessions*")

(defconst my-claude-sidebar--tick-seconds 5
  "How often a visible sidebar redraws its elapsed times.")

;; ================================= SESSIONS ==================================

(cl-defstruct (my-claude-sidebar--entry
               (:constructor my-claude-sidebar--make-entry)
               (:copier nil))
  id session
  ;; Set only for an exited session whose terminal outlives it.
  buffer
  (state 'idle)
  (since (float-time))
  ;; Error type or exit status, shown in the error state.
  detail
  ;; Main agent's running tool.
  tool
  ;; Agent id (nil for the main agent) whose question or permission prompt
  ;; caused `needs-input', or `any' when unknown.
  blocked-on
  ;; Running subagents, as (AGENT-ID TYPE . DESCRIPTION).
  agents
  ;; Agent tool calls not yet matched to a `SubagentStart', as
  ;; (TYPE . DESCRIPTION).
  pending
  expanded
  transcript
  (transcript-pos 0)
  custom-title
  ai-title
  ;; Non-nil once any hook arrived, so that a missing `SessionEnd' means
  ;; something.
  hooked
  ended
  exited)

(defvar my-claude-sidebar--entries (make-hash-table :test 'equal)
  "Sidebar entries, keyed by claude-code-ide session id.")

(defun my-claude-sidebar--entry-for-session (session)
  "Return SESSION's entry, creating it if needed."
  (let ((id (claude-code-ide-mcp-session-session-id session)))
    (or (gethash id my-claude-sidebar--entries)
        (puthash id (my-claude-sidebar--make-entry :id id :session session)
                 my-claude-sidebar--entries))))

(defun my-claude-sidebar--live-sessions ()
  (and (fboundp 'claude-code-ide-mcp--active-sessions)
       (claude-code-ide-mcp--active-sessions)))

(defun my-claude-sidebar--sync ()
  "Add entries for new sessions and drop those of ended ones."
  (let ((live (my-claude-sidebar--live-sessions)))
    (mapc #'my-claude-sidebar--entry-for-session live)
    (maphash (lambda (id entry)
               (unless (or (memq (my-claude-sidebar--entry-session entry) live)
                           (my-claude-sidebar--entry-exited entry))
                 (remhash id my-claude-sidebar--entries)))
             my-claude-sidebar--entries)))

(defun my-claude-sidebar--forget (entry)
  (remhash (my-claude-sidebar--entry-id entry) my-claude-sidebar--entries)
  (my-claude-sidebar--schedule-refresh))

(defun my-claude-sidebar--entry-live-buffer (entry)
  (let ((buffer (or (my-claude-sidebar--entry-buffer entry)
                    (claude-code-ide-mcp-session-buffer
                     (my-claude-sidebar--entry-session entry)))))
    (and (buffer-live-p buffer) buffer)))

(defun my-claude-sidebar--entry-for-buffer (buffer)
  (cl-loop for entry being the hash-values of my-claude-sidebar--entries
           when (eq buffer (my-claude-sidebar--entry-live-buffer entry))
           return entry))

(defun my-claude-sidebar--selected-p (entry)
  "Return non-nil if ENTRY's buffer is in the selected window."
  (let ((buffer (my-claude-sidebar--entry-live-buffer entry)))
    (and buffer (eq buffer (window-buffer (selected-window))))))

(defun my-claude-sidebar--title (entry)
  (let ((session (my-claude-sidebar--entry-session entry)))
    (or (my-claude-sidebar--entry-custom-title entry)
        (my-claude-sidebar--entry-ai-title entry)
        (claude-code-ide-mcp-session-instance-name session)
        (file-name-nondirectory
         (directory-file-name
          (claude-code-ide-mcp-session-project-dir session))))))

(defun my-claude-sidebar--set-transcript (entry path)
  (setq path (expand-file-name path))
  (unless (equal path (my-claude-sidebar--entry-transcript entry))
    (setf (my-claude-sidebar--entry-transcript entry) path
          (my-claude-sidebar--entry-transcript-pos entry) 0
          (my-claude-sidebar--entry-custom-title entry) nil
          (my-claude-sidebar--entry-ai-title entry) nil)))

(defun my-claude-sidebar--find-transcript (entry)
  "Find ENTRY's transcript from its CLI's process id.
For sessions started without the hooks, which report it."
  (when-let* ((pid (claude-code-ide-mcp-session-cli-pid
                    (my-claude-sidebar--entry-session entry)))
              (file (expand-file-name (format "~/.claude/sessions/%d.json" pid)))
              ((file-exists-p file))
              (id (alist-get 'sessionId
                             (ignore-errors
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (json-parse-buffer :object-type 'alist
                                                    :null-object nil))))))
    (car (file-expand-wildcards
          (expand-file-name (format "~/.claude/projects/*/%s.jsonl" id))))))

(defun my-claude-sidebar--update-title (entry)
  "Read title records appended to ENTRY's transcript since the last call."
  (unless (my-claude-sidebar--entry-hooked entry)
    (when-let* ((file (my-claude-sidebar--find-transcript entry)))
      (my-claude-sidebar--set-transcript entry file)))
  (when-let* ((file (my-claude-sidebar--entry-transcript entry))
              (size (file-attribute-size (file-attributes file))))
    (let ((pos (my-claude-sidebar--entry-transcript-pos entry)))
      ;; The transcript was rewritten.
      (when (< size pos)
        (setq pos 0))
      (when (> size pos)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file nil pos size)
          ;; Leave a partly-written last line for next time.
          (goto-char (point-max))
          (let ((end (if (search-backward "\n" nil t) (1+ (point)) (point-min))))
            (setf (my-claude-sidebar--entry-transcript-pos entry) (+ pos end -1))
            (goto-char (point-min))
            (while (re-search-forward "\"type\":\"\\(?:custom\\|ai\\)-title\"" end t)
              (let* ((line (decode-coding-string
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))
                            'utf-8))
                     (record (ignore-errors
                               (json-parse-string line :object-type 'alist
                                                  :null-object nil))))
                (pcase (alist-get 'type record)
                  ("custom-title"
                   (setf (my-claude-sidebar--entry-custom-title entry)
                         (my-claude-sidebar--non-empty (alist-get 'customTitle record))))
                  ("ai-title"
                   (setf (my-claude-sidebar--entry-ai-title entry)
                         (my-claude-sidebar--non-empty (alist-get 'aiTitle record))))))
              (forward-line 1))))))))

(defun my-claude-sidebar--non-empty (string)
  (and (stringp string) (not (string-empty-p string)) string))

;; ================================== STATES ===================================

(defun my-claude-sidebar--set-state (entry state &optional detail)
  "Move ENTRY to STATE, alerting if that needs attention."
  ;; A turn that finishes while you're looking at it is already seen.
  (when (and (eq state 'done) (my-claude-sidebar--selected-p entry))
    (setq state 'idle))
  (setf (my-claude-sidebar--entry-detail entry) detail)
  (unless (eq state (my-claude-sidebar--entry-state entry))
    (setf (my-claude-sidebar--entry-state entry) state
          (my-claude-sidebar--entry-since entry) (float-time))
    (when (memq state '(needs-input error))
      (my-claude-sidebar--alert entry state))))

(defun my-claude-sidebar--alert (entry state)
  (unless (my-claude-sidebar--selected-p entry)
    (when (memq state my-claude-sidebar-message-states)
      (message "Claude: %s %s" (my-claude-sidebar--title entry)
               (if (eq state 'error) "failed" "needs input")))
    (when (memq state my-claude-sidebar-auto-show-states)
      (my-claude-sidebar--auto-show entry))))

(defun my-claude-sidebar--handle-event (port payload)
  "Update the session that listens on PORT for hook PAYLOAD."
  (when-let* ((session (cl-find port (my-claude-sidebar--live-sessions)
                                :key #'claude-code-ide-mcp-session-port)))
    (let* ((entry (my-claude-sidebar--entry-for-session session))
           (state (my-claude-sidebar--entry-state entry))
           (agent (alist-get 'agent_id payload))
           (tool (alist-get 'tool_name payload))
           (input (alist-get 'tool_input payload)))
      (setf (my-claude-sidebar--entry-hooked entry) t)
      ;; Subagent payloads may carry their own transcript.
      (when-let* (((not agent))
                  (path (alist-get 'transcript_path payload)))
        (my-claude-sidebar--set-transcript entry path))
      (pcase (alist-get 'hook_event_name payload)
        ("SessionStart"
         (setf (my-claude-sidebar--entry-ended entry) nil)
         (when-let* ((title (my-claude-sidebar--non-empty
                             (alist-get 'session_title payload))))
           (setf (my-claude-sidebar--entry-custom-title entry) title))
         (when (equal (alist-get 'source payload) "clear")
           (setf (my-claude-sidebar--entry-tool entry) nil
                 (my-claude-sidebar--entry-agents entry) nil
                 (my-claude-sidebar--entry-pending entry) nil)
           (my-claude-sidebar--set-state entry 'idle)))
        ("UserPromptSubmit"
         (setf (my-claude-sidebar--entry-tool entry) nil
               (my-claude-sidebar--entry-pending entry) nil)
         (my-claude-sidebar--set-state entry 'working))
        ("PreToolUse"
         (unless agent
           (setf (my-claude-sidebar--entry-tool entry) tool))
         (cond
          ((member tool '("AskUserQuestion" "ExitPlanMode"))
           (setf (my-claude-sidebar--entry-blocked-on entry) agent)
           (my-claude-sidebar--set-state entry 'needs-input))
          (t
           (when (member tool '("Agent" "Task"))
             (setf (my-claude-sidebar--entry-pending entry)
                   (append (my-claude-sidebar--entry-pending entry)
                           (list (cons (or (alist-get 'subagent_type input)
                                           "general-purpose")
                                       (alist-get 'description input))))))
           ;; A subagent's tool doesn't answer the main agent's question.
           (unless (memq state '(needs-input error))
             (my-claude-sidebar--set-state entry 'working)))))
        ((or "PostToolUse" "PostToolUseFailure")
         (when (and (not agent) (equal tool (my-claude-sidebar--entry-tool entry)))
           (setf (my-claude-sidebar--entry-tool entry) nil))
         (let ((blocked (my-claude-sidebar--entry-blocked-on entry)))
           (unless (or (eq state 'error)
                       (and (eq state 'needs-input)
                            (not (eq blocked 'any))
                            (not (equal blocked agent))))
             (my-claude-sidebar--set-state entry 'working))))
        ("PermissionRequest"
         (setf (my-claude-sidebar--entry-blocked-on entry) agent)
         (my-claude-sidebar--set-state entry 'needs-input))
        ("Notification"
         (let ((type (alist-get 'notification_type payload)))
           (cond
            ((member type '("permission_prompt" "elicitation_dialog"
                            "elicitation_url_dialog"))
             (unless (eq state 'needs-input)
               (setf (my-claude-sidebar--entry-blocked-on entry) 'any)
               (my-claude-sidebar--set-state entry 'needs-input)))
            ;; Claude has been waiting for a minute, so a turn ended without
            ;; `Stop', as when you interrupt it. Waiting on a finished turn
            ;; isn't a question, so only use this to repair `working'.
            ((and (equal type "idle_prompt") (eq state 'working))
             (setf (my-claude-sidebar--entry-tool entry) nil)
             (my-claude-sidebar--set-state entry 'idle)))))
        ("Stop"
         (setf (my-claude-sidebar--entry-tool entry) nil
               (my-claude-sidebar--entry-pending entry) nil)
         (my-claude-sidebar--set-state entry 'done))
        ("StopFailure"
         (setf (my-claude-sidebar--entry-tool entry) nil
               (my-claude-sidebar--entry-pending entry) nil)
         (my-claude-sidebar--set-state entry 'error (alist-get 'error_type payload)))
        ("SubagentStart"
         (let ((type (alist-get 'agent_type payload))
               (agents (my-claude-sidebar--entry-agents entry)))
           ;; Claude's internal agents have an empty type.
           (when (and agent (my-claude-sidebar--non-empty type)
                      (not (assoc agent agents)))
             (let ((call (assoc type (my-claude-sidebar--entry-pending entry))))
               (setf (my-claude-sidebar--entry-pending entry)
                     (delq call (my-claude-sidebar--entry-pending entry))
                     (my-claude-sidebar--entry-agents entry)
                     (append agents (list (cl-list* agent type (cdr call)))))))))
        ("SubagentStop"
         (setf (my-claude-sidebar--entry-agents entry)
               (assoc-delete-all agent (my-claude-sidebar--entry-agents entry))))
        ("SessionEnd"
         (unless (member (alist-get 'reason payload) '("clear" "resume"))
           (setf (my-claude-sidebar--entry-ended entry) t))))
      (my-claude-sidebar--schedule-refresh))))

(defun my-claude-sidebar--abnormal-exit (session)
  "Describe how SESSION's process failed, or return nil if it didn't."
  (let ((process (claude-code-ide-mcp-session-process session))
        (entry (gethash (claude-code-ide-mcp-session-session-id session)
                        my-claude-sidebar--entries)))
    (when (and process (not (process-live-p process)))
      (if (eq (process-type process) 'real)
          (let ((code (process-exit-status process)))
            (unless (zerop code)
              (format (if (eq (process-status process) 'signal) "signal %d" "exit %d")
                      code)))
        ;; ghostel's native PTY doesn't report the exit status, but a clean
        ;; exit runs the `SessionEnd' hook first.
        (and entry
             (my-claude-sidebar--entry-hooked entry)
             (not (my-claude-sidebar--entry-ended entry))
             "exited")))))

(defun my-claude-sidebar--around-cleanup (orig session &optional buffer-dying)
  "Keep a failed SESSION's row and terminal after ORIG cleans it up.
Intended as `:around' advice for `claude-code-ide--cleanup-session';
BUFFER-DYING is passed on."
  (let ((exit (and session
                   (not buffer-dying)
                   (not (claude-code-ide-mcp-session-cleanup-done session))
                   (my-claude-sidebar--abnormal-exit session))))
    (if (not exit)
        (funcall orig session buffer-dying)
      (let ((buffer (claude-code-ide-mcp-session-buffer session))
            (entry (my-claude-sidebar--entry-for-session session)))
        ;; Cleanup kills the session's buffer, but its last output explains
        ;; the failure.
        (setf (cl-struct-slot-value 'claude-code-ide-mcp-session 'buffer session) nil)
        (unwind-protect
            (funcall orig session buffer-dying)
          (setf (cl-struct-slot-value 'claude-code-ide-mcp-session 'buffer session)
                buffer))
        (setf (my-claude-sidebar--entry-exited entry) t
              (my-claude-sidebar--entry-buffer entry) buffer
              (my-claude-sidebar--entry-tool entry) nil
              (my-claude-sidebar--entry-agents entry) nil)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            ;; Don't let claude-code-ide commands target the dead session.
            (setq-local claude-code-ide--session nil)
            (add-hook 'kill-buffer-hook
                      (lambda () (my-claude-sidebar--forget entry))
                      nil t)))
        (my-claude-sidebar--set-state entry 'error exit)))
    (my-claude-sidebar--schedule-refresh)))

;; ================================ HOOK SERVER ================================

(defconst my-claude-sidebar--hook-events
  '("SessionStart" "UserPromptSubmit" "PreToolUse" "PostToolUse"
    "PostToolUseFailure" "PermissionRequest" "Notification" "Stop"
    "StopFailure" "SubagentStart" "SubagentStop" "SessionEnd"))

(defvar my-claude-sidebar--server nil)

(defun my-claude-sidebar--settings-file ()
  (expand-file-name (format "claude-sidebar-%d.json" (emacs-pid))
                    temporary-file-directory))

(defun my-claude-sidebar--ensure-server ()
  "Start the hook listener and write the settings that point at it.
Return the settings file."
  (let ((file (my-claude-sidebar--settings-file)))
    (unless (process-live-p my-claude-sidebar--server)
      (setq my-claude-sidebar--server
            (make-network-process :name "claude-sidebar-hooks"
                                  :server t :host "127.0.0.1" :service t
                                  :family 'ipv4 :coding 'binary :noquery t
                                  :filter #'my-claude-sidebar--server-filter))
      ;; claude-code-ide gives each session its own `CLAUDE_CODE_SSE_PORT',
      ;; which identifies the session. The hook must never fail or block
      ;; Claude, even when Emacs doesn't answer.
      (let ((command (format "curl -s -m 1 --noproxy '*' -o /dev/null -H 'Content-Type: application/json' -H 'Expect:' --data-binary @- http://127.0.0.1:%d/claude-hook/$CLAUDE_CODE_SSE_PORT 2>/dev/null; exit 0"
                             (process-contact my-claude-sidebar--server :service))))
        (with-temp-file file
          (insert (json-serialize
                   `((hooks
                      . ,(mapcar (lambda (event)
                                   (cons (intern event)
                                         `[((hooks . [((type . "command")
                                                       (command . ,command)
                                                       (timeout . 5))]))]))
                                 my-claude-sidebar--hook-events))))))))
    file))

(defun my-claude-sidebar--server-filter (process string)
  "Answer one HTTP request on PROCESS, then handle its hook payload."
  (let* ((data (concat (process-get process 'data) string))
         (end (string-search "\r\n\r\n" data)))
    (process-put process 'data data)
    (when end
      (let* ((head (substring data 0 end))
             (length (if (let ((case-fold-search t))
                           (string-match "^content-length: *\\([0-9]+\\)" head))
                         (string-to-number (match-string 1 head))
                       0)))
        (when (>= (- (length data) end 4) length)
          (ignore-errors
            (process-send-string
             process "HTTP/1.1 204 No Content\r\nConnection: close\r\n\r\n"))
          (delete-process process)
          (when (string-match "\\`POST /claude-hook/\\([0-9]+\\) " head)
            (with-demoted-errors "Claude sidebar: %S"
              (my-claude-sidebar--handle-event
               (string-to-number (match-string 1 head))
               (json-parse-string
                (decode-coding-string (substring data (+ end 4) (+ end 4 length))
                                      'utf-8)
                :object-type 'alist :null-object nil :false-object nil)))))))))

(defun my-claude-sidebar--add-settings-flag (command)
  "Add hooks reporting to the sidebar to Claude COMMAND.
Intended as `:filter-return' advice for
`claude-code-ide--build-claude-command'."
  (condition-case err
      (let ((flag (concat " --settings "
                          (shell-quote-argument (my-claude-sidebar--ensure-server))))
            (program claude-code-ide-cli-path))
        ;; Put it first, since `--allowedTools' takes the arguments after it.
        (if (string-prefix-p program command)
            (concat program flag (substring command (length program)))
          (concat command flag)))
    (error
     (message "Claude sidebar: no status hooks: %s" (error-message-string err))
     command)))

(defun my-claude-sidebar--delete-settings-file ()
  (ignore-errors (delete-file (my-claude-sidebar--settings-file))))

;; ================================= RENDERING =================================

(defun my-claude-sidebar--state-info (state)
  (alist-get state my-claude-sidebar--states))

(defun my-claude-sidebar--counts (separator)
  "Return each non-idle state's session count and glyph, or nil."
  (let ((counts nil))
    (maphash (lambda (_ entry)
               (cl-incf (alist-get (my-claude-sidebar--entry-state entry) counts 0)))
             my-claude-sidebar--entries)
    (mapconcat #'identity
               (cl-loop for (state glyph _ face) in my-claude-sidebar--states
                        for n = (alist-get state counts)
                        when (and n (not (eq state 'idle)))
                        collect (propertize (format "%d%s" n glyph) 'face face))
               separator)))

(defun my-claude-sidebar--age (since)
  (let ((s (max 0 (floor (- (float-time) since)))))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm" (/ s 60)))
          ((< s 86400) (format "%dh" (/ s 3600)))
          (t (format "%dd" (/ s 86400))))))

(defun my-claude-sidebar--fit (string width)
  (truncate-string-to-width string width nil nil "…"))

(defun my-claude-sidebar--sorted-entries ()
  (let ((entries (mapcar (lambda (entry)
                           (cons (downcase (my-claude-sidebar--title entry)) entry))
                         (hash-table-values my-claude-sidebar--entries))))
    (mapcar #'cdr
            (sort entries
                  (lambda (a b)
                    (or (string< (car a) (car b))
                        (and (string= (car a) (car b))
                             (string< (my-claude-sidebar--entry-id (cdr a))
                                      (my-claude-sidebar--entry-id (cdr b))))))))))

(defun my-claude-sidebar--status-line (entry width)
  (pcase-let* ((state (my-claude-sidebar--entry-state entry))
               (`(,_ ,word ,_) (my-claude-sidebar--state-info state))
               (agents (length (my-claude-sidebar--entry-agents entry)))
               (detail (cond ((> agents 0)
                              (format "%d agent%s" agents (if (= agents 1) "" "s")))
                             ((eq state 'working) (my-claude-sidebar--entry-tool entry))
                             ((eq state 'error) (my-claude-sidebar--entry-detail entry))))
               (left (concat "   " word (and detail (concat " · " detail))))
               (right (my-claude-sidebar--age (my-claude-sidebar--entry-since entry)))
               (room (- width (string-width right) 1)))
    (when (> (string-width left) room)
      (setq left (my-claude-sidebar--fit left room)))
    (propertize (concat left
                        (make-string (max 1 (- width (string-width left)
                                               (string-width right)))
                                     ?\s)
                        right)
                'face 'my-claude-sidebar-dim)))

(defun my-claude-sidebar--dir-line (entry width)
  (let ((dir (abbreviate-file-name
              (directory-file-name
               (claude-code-ide-mcp-session-project-dir
                (my-claude-sidebar--entry-session entry)))))
        (room (- width 3)))
    ;; Keep the end of the path, which tells worktrees apart.
    (when (> (string-width dir) room)
      (setq dir (concat "…" (substring dir (- (length dir) (1- room))))))
    (propertize (concat "   " dir) 'face 'my-claude-sidebar-dim)))

(defun my-claude-sidebar--insert (width)
  "Insert the sidebar contents, WIDTH columns wide."
  (let ((entries (my-claude-sidebar--sorted-entries))
        (counts (my-claude-sidebar--counts "  ")))
    (insert " " (propertize "Claude" 'face 'bold)
            (if (string-empty-p counts) "" (concat "  " counts))
            "\n"
            (propertize (concat " " (make-string (1- width) ?─))
                        'face 'my-claude-sidebar-dim)
            "\n")
    (unless entries
      (insert (propertize " No sessions (c starts one)" 'face 'my-claude-sidebar-dim)
              "\n"))
    (dolist (entry entries)
      (pcase-let* ((id (my-claude-sidebar--entry-id entry))
                   ;; Every line of the block shares this object, so the
                   ;; block is one run of the property.
                   (item (list id))
                   (agents (my-claude-sidebar--entry-agents entry))
                   (expanded (my-claude-sidebar--entry-expanded entry))
                   (`(,glyph ,_ ,face) (my-claude-sidebar--state-info
                                        (my-claude-sidebar--entry-state entry)))
                   (start (point)))
        (insert (my-claude-sidebar--fit
                 (concat " "
                         (propertize (concat (and agents (if expanded "▾ " "▸ "))
                                             glyph " "
                                             (my-claude-sidebar--title entry))
                                     'face face))
                 width)
                "\n")
        (put-text-property start (point) 'my-claude-sidebar-stop t)
        (insert (my-claude-sidebar--status-line entry width) "\n"
                (my-claude-sidebar--dir-line entry width) "\n")
        (put-text-property start (point) 'my-claude-sidebar-item item)
        (when expanded
          (pcase-dolist (`(,agent ,type . ,description) agents)
            (insert (propertize
                     (concat (my-claude-sidebar--fit
                              (concat "     "
                                      (propertize "●" 'face 'my-claude-sidebar-working)
                                      " " type
                                      (and description (concat ": " description)))
                              width)
                             "\n")
                     'my-claude-sidebar-item (list id agent)
                     'my-claude-sidebar-stop t))))
        (insert "\n")))))

(defun my-claude-sidebar--render ()
  "Redraw the sidebar, keeping each of its windows on the same row."
  (when-let* ((buffer (get-buffer my-claude-sidebar--buffer-name))
              (windows (get-buffer-window-list buffer nil t)))
    (with-current-buffer buffer
      (let* ((width (1- (apply #'min (mapcar #'window-body-width windows))))
             (saved (mapcar (lambda (window)
                              (list window
                                    (get-text-property (window-point window)
                                                       'my-claude-sidebar-item)
                                    (line-number-at-pos (window-point window))
                                    (line-number-at-pos (window-start window))))
                            windows))
             (inhibit-read-only t))
        (erase-buffer)
        (my-claude-sidebar--insert width)
        (pcase-dolist (`(,window ,item ,line ,start-line) saved)
          (let ((pos (save-excursion
                       (or (and item (my-claude-sidebar--find item))
                           (and (cdr item) (my-claude-sidebar--find (list (car item))))
                           (progn (goto-char (point-min))
                                  (forward-line (1- line))
                                  (my-claude-sidebar--snap)
                                  (point))))))
            (set-window-start window (save-excursion
                                       (goto-char (point-min))
                                       (forward-line (1- start-line))
                                       (point))
                              t)
            (set-window-point window pos)
            (set-window-parameter window 'my-claude-sidebar-item
                                  (get-text-property pos 'my-claude-sidebar-item))
            (my-claude-sidebar--highlight window)))
        ;; The selected window's point is the buffer's point.
        (when (memq (selected-window) windows)
          (goto-char (window-point (selected-window))))))))

(defvar my-claude-sidebar--mode-line-string ""
  "Mode-line indicator, kept current by `my-claude-sidebar--refresh'.")
(put 'my-claude-sidebar--mode-line-string 'risky-local-variable t)

(defun my-claude-sidebar--update-mode-line ()
  (let ((counts (my-claude-sidebar--counts " "))
        (map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'my-claude-sidebar)
    (setq my-claude-sidebar--mode-line-string
          (if (zerop (hash-table-count my-claude-sidebar--entries))
              ""
            (let ((string (concat "CC" (if (string-empty-p counts) "" " ") counts)))
              (add-text-properties 0 (length string)
                                   `(local-map ,map mouse-face mode-line-highlight
                                               help-echo "mouse-1: Claude sessions")
                                   string)
              (concat " " string " "))))
    (force-mode-line-update t)))

(add-to-list 'global-mode-string 'my-claude-sidebar--mode-line-string t)

(defun my-claude-sidebar--refresh ()
  "Bring entries, titles, the mode line and visible sidebars up to date."
  (with-demoted-errors "Claude sidebar: %S"
    (my-claude-sidebar--sync)
    (maphash (lambda (_ entry) (my-claude-sidebar--update-title entry))
             my-claude-sidebar--entries)
    (my-claude-sidebar--update-mode-line)
    (my-claude-sidebar--render)))

(defvar my-claude-sidebar--refresh-timer nil)

(defun my-claude-sidebar--schedule-refresh (&rest _)
  "Refresh soon, coalescing bursts of hook events."
  (unless (timerp my-claude-sidebar--refresh-timer)
    (setq my-claude-sidebar--refresh-timer
          (run-with-timer 0.05 nil
                          (lambda ()
                            (setq my-claude-sidebar--refresh-timer nil)
                            (my-claude-sidebar--refresh))))))

(defvar my-claude-sidebar--tick-timer nil)

(defun my-claude-sidebar--tick ()
  (if (get-buffer-window my-claude-sidebar--buffer-name t)
      (my-claude-sidebar--refresh)
    (cancel-timer my-claude-sidebar--tick-timer)
    (setq my-claude-sidebar--tick-timer nil)))

(defun my-claude-sidebar--ensure-tick ()
  (unless (timerp my-claude-sidebar--tick-timer)
    (setq my-claude-sidebar--tick-timer
          (run-with-timer my-claude-sidebar--tick-seconds
                          my-claude-sidebar--tick-seconds
                          #'my-claude-sidebar--tick))))

;; ================================ NAVIGATION =================================

(defun my-claude-sidebar--stop-p ()
  (get-text-property (line-beginning-position) 'my-claude-sidebar-stop))

(defun my-claude-sidebar--find (item)
  "Return the start of ITEM's row, or nil."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (when (and (my-claude-sidebar--stop-p)
                   (equal (get-text-property (point) 'my-claude-sidebar-item) item))
          (setq found (point)))
        (forward-line 1))
      found)))

(defun my-claude-sidebar--next-stop (direction)
  "Return the start of the next row in DIRECTION (1 or -1), or nil."
  (save-excursion
    (let (found)
      (while (and (not found) (zerop (forward-line direction)) (not (eobp)))
        (when (my-claude-sidebar--stop-p)
          (setq found (point))))
      found)))

(defun my-claude-sidebar--snap ()
  "Move point to the start of a row: the one it's in, or the nearest."
  (let ((item (get-text-property (point) 'my-claude-sidebar-item)))
    (cond ((my-claude-sidebar--stop-p) (beginning-of-line))
          (item (goto-char (my-claude-sidebar--find item)))
          (t (when-let* ((pos (or (my-claude-sidebar--next-stop 1)
                                  (my-claude-sidebar--next-stop -1))))
               (goto-char pos))))))

(defun my-claude-sidebar--highlight (window)
  "Highlight the row at WINDOW's point, in WINDOW only."
  (with-current-buffer (window-buffer window)
    (let* ((pos (window-point window))
           (overlay (or (window-parameter window 'my-claude-sidebar-overlay)
                        (let ((overlay (make-overlay 1 1)))
                          (overlay-put overlay 'face 'hl-line)
                          (overlay-put overlay 'window window)
                          (set-window-parameter window 'my-claude-sidebar-overlay overlay)
                          overlay))))
      (if (get-text-property pos 'my-claude-sidebar-item)
          (move-overlay overlay pos
                        (next-single-property-change pos 'my-claude-sidebar-item
                                                     nil (point-max))
                        (current-buffer))
        (move-overlay overlay 1 1 (current-buffer))))))

(defun my-claude-sidebar--entry-at-point ()
  (when-let* ((item (get-text-property (point) 'my-claude-sidebar-item)))
    (gethash (car item) my-claude-sidebar--entries)))

(defun my-claude-sidebar--post-command ()
  "Keep point on a row, and preview the session when it moves to another."
  (let ((window (selected-window)))
    (when (eq (window-buffer window) (current-buffer))
      (my-claude-sidebar--snap)
      (my-claude-sidebar--highlight window)
      (let ((item (get-text-property (point) 'my-claude-sidebar-item)))
        (unless (equal item (window-parameter window 'my-claude-sidebar-item))
          (set-window-parameter window 'my-claude-sidebar-item item)
          (when item
            (my-claude-sidebar--schedule-preview window item)))))))

;; ================================== PREVIEW ==================================
;;
;; A preview shows a session in the target window without selecting it. The
;; frame's `my-claude-sidebar-preview' parameter remembers what the target
;; showed before, as (WINDOW BUFFER POINT START), so that leaving the sidebar
;; any way but RET can put it back. BUFFER is nil when the preview had to
;; create WINDOW.

(defvar my-claude-sidebar--preview-timer nil)

(defun my-claude-sidebar--target-window (frame)
  "Return FRAME's most recently selected window other than the sidebar."
  (get-mru-window frame nil nil t))

(defun my-claude-sidebar--schedule-preview (window item)
  (when (timerp my-claude-sidebar--preview-timer)
    (cancel-timer my-claude-sidebar--preview-timer))
  (setq my-claude-sidebar--preview-timer
        (run-with-timer my-claude-sidebar-preview-delay nil
                        #'my-claude-sidebar--preview window item)))

(defun my-claude-sidebar--preview (window item)
  "Show ITEM's session next to the sidebar WINDOW, if it's still there."
  (setq my-claude-sidebar--preview-timer nil)
  (when-let* (((eq window (selected-window)))
              ((equal item (window-parameter window 'my-claude-sidebar-item)))
              (entry (gethash (car item) my-claude-sidebar--entries))
              (buffer (my-claude-sidebar--entry-live-buffer entry)))
    (let* ((frame (window-frame window))
           (preview (frame-parameter frame 'my-claude-sidebar-preview))
           (target (if (window-live-p (car preview))
                       (car preview)
                     (my-claude-sidebar--target-window frame))))
      (cond
       ((not target)
        (when-let* ((new (display-buffer buffer '(nil (inhibit-same-window . t)))))
          (set-frame-parameter frame 'my-claude-sidebar-preview (list new nil))))
       ((not (eq (window-buffer target) buffer))
        (unless preview
          (let ((old (window-buffer target)))
            (set-frame-parameter frame 'my-claude-sidebar-preview
                                 (list target old
                                       (set-marker (make-marker) (window-point target) old)
                                       (set-marker (make-marker) (window-start target) old)))))
        (set-window-buffer target buffer))))))

(defun my-claude-sidebar--end-preview (frame restore)
  "Stop previewing in FRAME, putting the target back if RESTORE."
  (when (timerp my-claude-sidebar--preview-timer)
    (cancel-timer my-claude-sidebar--preview-timer)
    (setq my-claude-sidebar--preview-timer nil))
  (when-let* ((preview (frame-parameter frame 'my-claude-sidebar-preview)))
    (set-frame-parameter frame 'my-claude-sidebar-preview nil)
    (pcase-let ((`(,window ,buffer ,point ,start) preview))
      (when (and restore (window-live-p window))
        (cond ((null buffer)
               (ignore-errors (delete-window window)))
              ((and (buffer-live-p buffer) (not (eq (window-buffer window) buffer)))
               (set-window-buffer window buffer)
               (set-window-point window point)
               (set-window-start window start t))))
      (when point
        (set-marker point nil)
        (set-marker start nil)))))

(defun my-claude-sidebar--window-change (frame)
  "Cancel FRAME's preview once focus leaves the sidebar, and mark sessions seen.
Intended for `window-selection-change-functions' and
`window-buffer-change-functions'."
  (let ((window (frame-selected-window frame)))
    ;; Prompting from the sidebar doesn't leave it.
    (unless (window-minibuffer-p window)
      (unless (my-claude-sidebar--window-p window)
        (my-claude-sidebar--end-preview frame t))
      (when-let* (((eq frame (selected-frame)))
                  (entry (my-claude-sidebar--entry-for-buffer (window-buffer window)))
                  ((eq (my-claude-sidebar--entry-state entry) 'done)))
        (my-claude-sidebar--set-state entry 'idle)
        (my-claude-sidebar--schedule-refresh)))))

(add-hook 'window-selection-change-functions #'my-claude-sidebar--window-change)
(add-hook 'window-buffer-change-functions #'my-claude-sidebar--window-change)

;; ================================== WINDOW ===================================

(defun my-claude-sidebar--window-p (window)
  (equal (buffer-name (window-buffer window)) my-claude-sidebar--buffer-name))

(defun my-claude-sidebar--window (&optional frame)
  (when-let* ((buffer (get-buffer my-claude-sidebar--buffer-name)))
    (get-buffer-window buffer (or frame (selected-frame)))))

(defun my-claude-sidebar--show ()
  "Show the sidebar in the selected frame, without selecting it.
Return its window."
  (let ((buffer (get-buffer-create my-claude-sidebar--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my-claude-sidebar-mode)
        (my-claude-sidebar-mode)))
    (let ((window
           (or (get-buffer-window buffer)
               ;; A large `window-min-width' would forbid the split.
               (let ((window-min-width 1))
                 (display-buffer-in-side-window
                  buffer
                  `((side . ,my-claude-sidebar-side)
                    (slot . 0)
                    (window-width . ,my-claude-sidebar-width)
                    (preserve-size . (t . nil))
                    (dedicated . t)
                    (window-parameters . ((no-other-window . t)
                                          (no-delete-other-windows . t)
                                          (mode-line-format . none))))))
               (user-error "No room for the Claude sidebar"))))
      (my-claude-sidebar--refresh)
      (my-claude-sidebar--ensure-tick)
      window)))

(defun my-claude-sidebar--hide (frame)
  "Hide FRAME's sidebar and snooze auto-showing it."
  (when-let* ((window (my-claude-sidebar--window frame)))
    (my-claude-sidebar--end-preview frame t)
    (let ((selected (eq window (frame-selected-window frame)))
          (target (my-claude-sidebar--target-window frame)))
      (delete-window window)
      (when (and selected (window-live-p target))
        (select-window target))))
  (set-frame-parameter frame 'my-claude-sidebar-snooze
                       (+ (float-time) (* 60 my-claude-sidebar-snooze-minutes))))

(defun my-claude-sidebar--auto-show (entry)
  "Show ENTRY in the selected frame's sidebar, without selecting it."
  (let* ((frame (selected-frame))
         (window (my-claude-sidebar--window frame)))
    (unless (or window
                (< (float-time)
                   (or (frame-parameter frame 'my-claude-sidebar-snooze) 0)))
      (setq window (my-claude-sidebar--show)))
    (when (and window (not (eq window (selected-window))))
      (my-claude-sidebar--refresh)
      (with-current-buffer (window-buffer window)
        (let ((item (list (my-claude-sidebar--entry-id entry))))
          (when-let* ((pos (my-claude-sidebar--find item)))
            (set-window-point window pos)
            (set-window-parameter window 'my-claude-sidebar-item item)
            (my-claude-sidebar--highlight window)))))))

;; ================================= COMMANDS ==================================

;;;###autoload
(defun my-claude-sidebar ()
  "Show and select the Claude sessions sidebar, or hide it if selected."
  (interactive)
  (require 'claude-code-ide)
  (let ((window (my-claude-sidebar--window)))
    (cond ((not window)
           (set-frame-parameter nil 'my-claude-sidebar-snooze nil)
           (select-window (my-claude-sidebar--show)))
          ((eq window (selected-window))
           (my-claude-sidebar--hide (selected-frame)))
          (t
           (select-window window)))))

(defun my-claude-sidebar-hide ()
  "Hide the sidebar."
  (interactive)
  (my-claude-sidebar--hide (selected-frame)))

(defun my-claude-sidebar-next ()
  "Move to the next session or subagent."
  (interactive)
  (when-let* ((pos (my-claude-sidebar--next-stop 1)))
    (goto-char pos)))

(defun my-claude-sidebar-previous ()
  "Move to the previous session or subagent."
  (interactive)
  (when-let* ((pos (my-claude-sidebar--next-stop -1)))
    (goto-char pos)))

(defun my-claude-sidebar--entry-or-error ()
  (or (my-claude-sidebar--entry-at-point)
      (user-error "No session here")))

(defun my-claude-sidebar-visit ()
  "Show the session at point in the target window and select it."
  (interactive)
  (let* ((buffer (or (my-claude-sidebar--entry-live-buffer
                      (my-claude-sidebar--entry-or-error))
                     (user-error "That session has no buffer")))
         (frame (selected-frame))
         (preview (frame-parameter frame 'my-claude-sidebar-preview))
         (window (if (window-live-p (car preview))
                     (car preview)
                   (my-claude-sidebar--target-window frame))))
    (my-claude-sidebar--end-preview frame nil)
    (if window
        (set-window-buffer window buffer)
      (setq window (display-buffer buffer '(nil (inhibit-same-window . t)))))
    (select-window window)))

(defun my-claude-sidebar-mouse-visit (event)
  "Visit the session clicked on in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (my-claude-sidebar--snap)
  (my-claude-sidebar-visit))

(defun my-claude-sidebar-toggle-agents ()
  "Expand or collapse the subagents of the session at point."
  (interactive)
  (let ((entry (my-claude-sidebar--entry-or-error)))
    (setf (my-claude-sidebar--entry-expanded entry)
          (not (my-claude-sidebar--entry-expanded entry)))
    (my-claude-sidebar--refresh)))

(defun my-claude-sidebar-new ()
  "Start a Claude session in a chosen project, in the target window."
  (interactive)
  (require 'project)
  (let ((dir (project-prompt-project-dir))
        (frame (selected-frame)))
    (my-claude-sidebar--end-preview frame t)
    (when-let* ((window (my-claude-sidebar--target-window frame)))
      (select-window window))
    (let ((default-directory dir)
          (display-buffer-overriding-action '(display-buffer-same-window)))
      (claude-code-ide))))

(defun my-claude-sidebar-kill ()
  "Kill the session at point, or dismiss it if it already exited."
  (interactive)
  (let* ((entry (my-claude-sidebar--entry-or-error))
         (buffer (my-claude-sidebar--entry-live-buffer entry)))
    (cond
     ((my-claude-sidebar--entry-exited entry)
      (my-claude-sidebar--forget entry)
      (when buffer
        (kill-buffer buffer)))
     ((y-or-n-p (format "Kill Claude session %s? " (my-claude-sidebar--title entry)))
      (if buffer
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer))
        (claude-code-ide--cleanup-session (my-claude-sidebar--entry-session entry)))))
    (my-claude-sidebar--refresh)))

(defun my-claude-sidebar-rename ()
  "Rename the session at point."
  (interactive)
  (let ((buffer (or (my-claude-sidebar--entry-live-buffer
                     (my-claude-sidebar--entry-or-error))
                    (user-error "That session has no buffer"))))
    ;; In its own buffer, the command targets that session.
    (with-current-buffer buffer
      (let ((current-prefix-arg nil))
        (call-interactively #'claude-code-ide-rename-session))))
  (my-claude-sidebar--refresh))

(defun my-claude-sidebar-refresh ()
  "Refresh the sidebar now."
  (interactive)
  (my-claude-sidebar--refresh))

(defvar-keymap my-claude-sidebar-mode-map
  "n" #'my-claude-sidebar-next
  "C-n" #'my-claude-sidebar-next
  "<down>" #'my-claude-sidebar-next
  "p" #'my-claude-sidebar-previous
  "C-p" #'my-claude-sidebar-previous
  "<up>" #'my-claude-sidebar-previous
  "RET" #'my-claude-sidebar-visit
  "TAB" #'my-claude-sidebar-toggle-agents
  "c" #'my-claude-sidebar-new
  "k" #'my-claude-sidebar-kill
  "r" #'my-claude-sidebar-rename
  "g" #'my-claude-sidebar-refresh
  "q" #'my-claude-sidebar-hide
  "<double-mouse-1>" #'my-claude-sidebar-mouse-visit
  "<mouse-2>" #'my-claude-sidebar-mouse-visit)

(define-derived-mode my-claude-sidebar-mode special-mode "Claude Sessions"
  "Major mode for the Claude sessions sidebar."
  (setq-local truncate-lines t
              buffer-undo-list t
              show-trailing-whitespace nil
              cursor-in-non-selected-windows nil
              ;; Exempts the sidebar from `window-min-width' when other
              ;; windows are split or balanced.
              window-size-fixed 'width
              ;; The row highlight replaces it.
              global-hl-line-mode nil)
  (add-hook 'post-command-hook #'my-claude-sidebar--post-command nil t))

(global-set-key (kbd "C-c J") #'my-claude-sidebar)

(with-eval-after-load 'claude-code-ide
  (advice-add 'claude-code-ide--build-claude-command :filter-return
              #'my-claude-sidebar--add-settings-flag)
  (advice-add 'claude-code-ide--cleanup-session :around
              #'my-claude-sidebar--around-cleanup)
  (advice-add 'claude-code-ide--start-session :after
              #'my-claude-sidebar--schedule-refresh)
  (add-hook 'kill-emacs-hook #'my-claude-sidebar--delete-settings-file))

;; These hold windows and markers, which desktop can't save.
(with-eval-after-load 'frameset
  (dolist (parameter '(my-claude-sidebar-preview my-claude-sidebar-snooze))
    (add-to-list 'frameset-filter-alist (cons parameter :never))))

(with-eval-after-load 'claude-code-ide-transient
  (with-demoted-errors "Claude sidebar: %S"
    (unless (ignore-errors (transient-get-suffix 'claude-code-ide-menu "j"))
      (transient-append-suffix 'claude-code-ide-menu "a"
        '("j" "Sidebar" my-claude-sidebar)))))

(provide 'my-claude-sidebar)
;;; my-claude-sidebar.el ends here
