;;; my-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

(require 'lsp)

(add-hook 'lsp-mode-hook
          '(lambda ()
             (lsp-modeline-code-actions-mode)
             ))

(setq lsp-signature-auto-activate t)

;; rust-analyzer can send big payloads (workspace symbols, semantic tokens,
;; inlay hints). The default 4096 makes Emacs read them a pipe-buffer at a time.
(setq read-process-output-max (* 1024 1024))

;; ============================== FILE WATCHERS ==============================
;;
;; Client-side file watching is what makes Emacs unresponsive in a big Rust
;; workspace. For `~/wasmtime' the numbers are:
;;
;;   * rust-analyzer registers 386 `workspace/didChangeWatchedFiles' glob
;;     patterns -- one set of `**/*.rs', `**/Cargo.{toml,lock}',
;;     `**/rust-analyzer.toml' and `**/*.md' per crate, and the repo has 100+
;;     crates.
;;   * lsp-mode creates ~1400 kqueue watches (one file descriptor per
;;     directory) to feed them.
;;   * For every single file change, `lsp--file-process-event' walks all of
;;     those globs and matches the path against each one twice -- once
;;     absolute, once workspace-relative. That is ~1000 regexp matches per
;;     event, in interpreted elisp, inside the `file-notify' handler, on the
;;     main thread.
;;
;; So one `git checkout' or `cargo build' turns into millions of regexp matches
;; that Emacs cannot interrupt, plus the GC churn to go with it. Three fixes
;; below, in descending order of impact.

;; --------------------------------------------------------------------------
;; 1. Let rust-analyzer watch the filesystem itself.
;;
;; rust-analyzer has a native `notify'-based watcher that runs in its own
;; process, off Emacs's thread. With client watching disabled it never sends
;; the `didChangeWatchedFiles' registration at all, so lsp-mode creates no
;; watches and never runs the matching loop -- the entire hot path above
;; disappears rather than getting cheaper.
;;
;; `custom-set-variables' rather than `setq' because `lsp-rust' is loaded
;; lazily, when the Rust client is first registered.
(custom-set-variables
 '(lsp-rust-analyzer-use-client-watching nil))

;; --------------------------------------------------------------------------
;; 2. Widen the watcher blocklist.
;;
;; Belt and braces for any other LSP client, and for the case where client
;; watching gets turned back on. Ignored *directories* never get a watch
;; created; ignored *files* get a watch but their events are dropped before the
;; expensive glob matching runs.
;;
;; `add-to-list' so this keeps lsp-mode's defaults (which already cover
;; `target', `.git', `node_modules', `.direnv' and friends) and stays
;; idempotent if this file is reloaded.
(dolist (re '(;; Wasmtime/Cranelift golden-file test suites: thousands of
              ;; .wat/.wast/.clif data files, no Rust sources.
              "[/\\\\]tests[/\\\\]disas\\'"
              "[/\\\\]tests[/\\\\]misc_testsuite\\'"
              "[/\\\\]tests[/\\\\]spec_testsuite\\'"
              "[/\\\\]tests[/\\\\]wasi-testsuite\\'"
              "[/\\\\]tests[/\\\\]component-model\\'"
              ;; The nested data directory only -- `cranelift/filetests' itself
              ;; is a crate whose `src/' we do want watched.
              "[/\\\\]filetests[/\\\\]filetests\\'"
              ;; cargo-fuzz corpora and crash artifacts. A fuzz run writes
              ;; these continuously. Anchored under `fuzz/' because
              ;; `crates/test-programs/artifacts' is a crate, not fuzz output.
              "[/\\\\]fuzz[/\\\\]corpus\\'"
              "[/\\\\]fuzz[/\\\\]artifacts\\'"
              ;; Generated coverage/benchmark output. NOTE: this also matches a
              ;; hand-written `report/' should some other project have one --
              ;; harmless, it only suppresses file watching.
              "[/\\\\]reports?\\'"
              ;; Profiling leftovers.
              "[/\\\\][^/\\\\]+\\.dSYM\\'"
              ;; In-tree cargo config/state.
              "[/\\\\]\\.cargo\\'"))
  (add-to-list 'lsp-file-watch-ignored-directories re))

(dolist (re '(;; Emacs auto-save files (`#foo.rs#'). lsp-mode's defaults cover
              ;; lockfiles (`.#foo') and backups (`foo~') but not these, and
              ;; auto-save fires every 30s / 300 chars while you type.
              "[/\\\\]#[^/\\\\]+#\\'"
              ;; Cranelift's debug worker logs, appended to on every
              ;; compilation when debug logging is on.
              "[/\\\\]cranelift\\.dbg\\.[^/\\\\]+\\'"
              ;; Build/run output that lands in the source tree.
              "[/\\\\][^/\\\\]+\\.log\\'"
              "[/\\\\][^/\\\\]+\\.cwasm\\'"
              "[/\\\\][^/\\\\]+\\.profraw\\'"
              "[/\\\\][^/\\\\]+\\.profdata\\'"))
  (add-to-list 'lsp-file-watch-ignored-files re))

;; --------------------------------------------------------------------------
;; 3. Get whatever watcher events remain off the interactive path.
;;
;; Even with a good blocklist, `lsp--file-process-event' runs synchronously
;; inside the `file-notify' handler, so a burst of changes blocks input for as
;; long as the burst takes. Queue events instead and drain the queue from an
;; idle timer in short slices, yielding as soon as there is input to process.
;;
;; Nothing is dropped -- the server still hears about every change, just a beat
;; later and without Emacs going deaf in the meantime. Duplicate events for the
;; same file are collapsed while they sit in the queue.

(defcustom my-lsp-file-event-idle-delay 0.3
  "Seconds of idle time before queued LSP file-watcher events are delivered."
  :type 'number
  :group 'lsp-mode)

(defcustom my-lsp-file-event-slice 0.05
  "Seconds to spend delivering queued LSP file-watcher events per slice.
Draining yields after this long, or as soon as there is pending input,
whichever comes first."
  :type 'number
  :group 'lsp-mode)

(defvar my-lsp--file-event-queue nil
  "FIFO of pending (ORIG SESSION ROOT EVENT) file-watcher deliveries.")

(defvar my-lsp--file-event-queue-tail nil
  "Last cons cell of `my-lsp--file-event-queue', for O(1) append.")

(defvar my-lsp--file-event-pending (make-hash-table :test 'equal)
  "Set of queue keys, used to collapse duplicate events.")

(defvar my-lsp--file-event-timer nil
  "Idle timer draining `my-lsp--file-event-queue', or nil.")

(defun my-lsp--file-event-key (root event)
  "Dedup key for EVENT under workspace ROOT."
  (list root (nth 1 event) (nth 2 event)))

(defun my-lsp--file-event-schedule (&optional delay)
  "Ensure a drain is scheduled, after DELAY seconds of idle time."
  (unless my-lsp--file-event-timer
    (setq my-lsp--file-event-timer
          (run-with-idle-timer (or delay my-lsp-file-event-idle-delay) nil
                               #'my-lsp--file-event-drain))))

(defun my-lsp--file-event-drain ()
  "Deliver queued file-watcher events, yielding to user input."
  (setq my-lsp--file-event-timer nil)
  (let ((deadline (+ (float-time) my-lsp-file-event-slice)))
    (while (and my-lsp--file-event-queue
                (not (input-pending-p))
                (< (float-time) deadline))
      (let ((entry (pop my-lsp--file-event-queue)))
        (unless my-lsp--file-event-queue
          (setq my-lsp--file-event-queue-tail nil))
        (remhash (my-lsp--file-event-key (nth 2 entry) (nth 3 entry))
                 my-lsp--file-event-pending)
        (with-demoted-errors "[my-lsp] file event failed: %S"
          (funcall (nth 0 entry) (nth 1 entry) (nth 2 entry) (nth 3 entry))))))
  (when my-lsp--file-event-queue
    ;; Back off while the user is typing; otherwise keep chewing through the
    ;; backlog promptly, a slice at a time.
    (my-lsp--file-event-schedule (if (input-pending-p) nil 0.01))))

(defun my-lsp--file-process-event-async (orig session root event)
  "Queue EVENT for later delivery by ORIG instead of processing it now.
Intended as `:around' advice for `lsp--file-process-event'."
  (let ((key (my-lsp--file-event-key root event)))
    (unless (gethash key my-lsp--file-event-pending)
      (puthash key t my-lsp--file-event-pending)
      (let ((cell (list (list orig session root event))))
        (if my-lsp--file-event-queue-tail
            (setcdr my-lsp--file-event-queue-tail cell)
          (setq my-lsp--file-event-queue cell))
        (setq my-lsp--file-event-queue-tail cell))))
  (my-lsp--file-event-schedule))

(when (fboundp 'lsp--file-process-event)
  (advice-add 'lsp--file-process-event :around
              #'my-lsp--file-process-event-async))

(provide 'my-lsp)
;;; my-lsp.el ends here
