;;; my-magit-perf.el --- Make Magit responsive when process spawning is slow -*- lexical-binding: t -*-

;;; Commentary:

;; The corporate endpoint agent on this machine hooks process creation.
;; Measured on 2026-08-25 with GIT_TRACE_PERFORMANCE:
;;
;;     git rev-parse --show-toplevel
;;       git's own work : 0.5 - 2 ms
;;       wall clock     : 40 - 800 ms  (one observed spike: 4.9 s)
;;
;; So the payload is irrelevant; every `fork'+`exec' costs 40-800ms.  A default
;; Magit status refresh spawns ~25 git processes, hence multi-second refreshes.
;;
;; Therefore the only optimizations that matter are:
;;
;;   1. Spawn fewer processes.
;;   2. Move the processes you cannot avoid OFF the interactive critical path.
;;   3. Cache answers that cannot change without a filesystem change.
;;
;; Micro-tuning git flags, or Lisp inside Magit, is pointless here.
;;
;; Everything below lives in personal config only; Prelude, Magit and diff-hl
;; are untouched.  `M-x my/magit-perf-report' explains what is active.

;;; Code:

(require 'cl-lib)

(defgroup my/magit-perf nil
  "Latency workarounds for slow process spawning."
  :group 'magit)

(defcustom my/git-batch-threshold 2
  "Batch diff-hl's git queries when more than this many buffers need them.
See section 8.  At or below it the one batched process costs about as much as
the calls it would replace, so it is not worth the extra output to parse."
  :type 'integer
  :group 'my/magit-perf)


;;;; ------------------------------------------------------------------
;;;; 1. Shave the per-spawn constant
;;;; ------------------------------------------------------------------

;; `call-process' with a bare program name searches `exec-path' (26 entries
;; here, courtesy of exec-path-from-shell), and every probe is a filesystem
;; access that the endpoint agent also intercepts.  Resolve git once, at load.
(let ((git (executable-find "git")))
  (when git
    (setq magit-git-executable git)
    (with-eval-after-load 'vc-git
      (setq vc-git-program git))))

;; `with-editor' probes for a version-matched emacsclient at load time by
;; *running* several candidates.  Point it straight at the binary that ships
;; with the running Emacs so the probe loop never happens.  Computed from
;; `invocation-directory' so this stays correct for both the Homebrew build
;; and Emacs.app.
(setq with-editor-emacsclient-executable
      (or (cl-find-if
           #'file-executable-p
           (list (expand-file-name "emacsclient" invocation-directory)
                 (expand-file-name "bin/emacsclient" invocation-directory)))
          (executable-find "emacsclient")))


;;;; ------------------------------------------------------------------
;;;; 2. Cache repository topology (the single biggest win)
;;;; ------------------------------------------------------------------

;; `magit-toplevel' is one spawn, and it is called from the mode line, from
;; `magit-file-dispatch', from `magit-get-mode-buffer', from diff-hl, and once
;; per auto-revert cycle.  Magit only caches it *within* a single refresh.
;;
;; Correctness note: the cache is keyed by `my/magit--repo-key', which is
;; derived from the dominating ".git" -- pure filesystem, no subprocess.  If a
;; .git appears or disappears anywhere on the path, the key changes and we
;; re-run git.  That makes it safe against `git init' / `git clone' /
;; `git worktree add' / repo deletion, which is the failure mode you actually
;; care about: Magit must never act on the wrong repository.

(defconst my/magit--control-dir-regexp "\\(?:\\`\\|/\\)\\.git/"
  "Matches a path that descends *into* a \".git\" directory.

A linked worktree's \".git\" is a plain file, so this never matches a
worktree root -- only control directories such as
\"<main>/.git/worktrees/<name>/\" and \"<super>/.git/modules/<name>/\".")

(defun my/magit--repo-key (dir)
  "Return a cache key identifying the repository DIR belongs to, or nil.

The obvious key -- the nearest ancestor containing a \".git\", per
`locate-dominating-file' -- is wrong for any directory *inside* a control
directory, because walking up from there finds the *enclosing* repository's
\".git\" and buckets the two together.

That is not hypothetical.  A linked worktree's COMMIT_EDITMSG lives at
\"<main>/.git/worktrees/<name>/COMMIT_EDITMSG\", so the commit message
buffer's `default-directory' is that control directory, and
`magit-commit-diff' resolves the repository from it.  With the naive key it
shares a cache entry with the main worktree -- so Magit shows you the main
worktree's staged changes while you write a linked worktree's commit
message, and would commit them against the wrong tree.  Submodule control
directories have the same shape.

Inside a control directory, key on DIR itself instead: the answer is just as
stable, it merely cannot be shared with the surrounding worktree."
  (if (string-match-p my/magit--control-dir-regexp (expand-file-name dir))
      (cons 'control-dir (expand-file-name (file-name-as-directory dir)))
    (let ((anchor (ignore-errors (locate-dominating-file dir ".git"))))
      ;; Not in a repo (or unreadable): don't cache a negative, it is the
      ;; case most likely to change under us.
      (and anchor (cons 'worktree (expand-file-name anchor))))))

(defvar my/magit-toplevel-cache (make-hash-table :test 'equal)
  "Cache of (REPO-KEY . FIND-FILE-VISIT-TRUENAME) -> `magit-toplevel' result.")

(defun my/magit-toplevel-cached (orig &optional directory)
  "Around advice for `magit-toplevel' that caches per repository root.
`find-file-visit-truename' is part of the key because `magit-toplevel'
documents that it changes the answer, and `magit-commit-message-buffer'
binds it to t and then compares results for `equal'."
  (let ((dir (or directory default-directory)))
    (if (or (not (stringp dir))
            (file-remote-p dir))
        (funcall orig directory)
      (let ((repo (my/magit--repo-key dir)))
        (if (null repo)
            (funcall orig directory)
          (let* ((key (cons repo find-file-visit-truename))
                 (hit (gethash key my/magit-toplevel-cache 'miss)))
            (if (eq hit 'miss)
                (puthash key (funcall orig directory) my/magit-toplevel-cache)
              hit)))))))

(defvar my/magit-gitdir-cache (make-hash-table :test 'equal)
  "Cache of REPO-KEY -> `magit-gitdir' result.")

(defun my/magit-gitdir-cached (orig &optional directory)
  "Around advice for `magit-gitdir', cached like `my/magit-toplevel-cached'.
`magit-gitdir' is consulted by every sequence-detecting status section
\(merge, rebase, am, sequencer, three bisect sections\), plus the mode line."
  (let ((dir (or directory default-directory)))
    (if (or (not (stringp dir))
            (file-remote-p dir))
        (funcall orig directory)
      (let ((key (my/magit--repo-key dir)))
        (if (null key)
            (funcall orig directory)
          (let ((hit (gethash key my/magit-gitdir-cache 'miss)))
            (if (eq hit 'miss)
                (puthash key (funcall orig directory) my/magit-gitdir-cache)
              hit)))))))

(with-eval-after-load 'magit-git
  (advice-add 'magit-toplevel :around #'my/magit-toplevel-cached)
  (advice-add 'magit-gitdir   :around #'my/magit-gitdir-cached))

;; --- The big one: cache repository *topology* queries -------------------
;;
;; Staging one hunk in a real repo spawns 112 processes.  A breakdown of what
;; they actually are:
;;
;;     26 x  git rev-parse --show-toplevel
;;     26 x  git rev-parse --show-cdup
;;      2 x  (30 other distinct commands, each run exactly twice)
;;
;; 52 of 112 spawns -- 46% -- are Magit re-asking "where is the repo root?".
;; It is that bad because `magit-toplevel' issues up to *four* rev-parse calls
;; per cache miss (magit-git.el:965-987): it runs --show-toplevel, then
;; --show-cdup, then --show-cdup again from a subdirectory, then
;; --show-toplevel again, all to honour `find-file-visit-truename' without
;; needlessly resolving symlinks.  Magit memoizes this in
;; `magit--refresh-cache', but that cache only lives for the duration of one
;; refresh -- and staging spans two (the apply pass and the refresh pass), plus
;; every post-refresh hook.
;;
;; These four answers describe where the repository *is*.  Nothing you do to
;; the worktree or the index can change them: staging a hunk cannot move the
;; repo root.  So unlike content queries (`diff', `status', `rev-parse HEAD'),
;; they are safe to cache for the whole session -- there is no staleness
;; window to get wrong.  `my/magit-flush-caches' clears them if a symlink
;; above a checkout is ever retargeted.
;;
;; This also, for free, defuses the per-buffer `--show-toplevel' scan that
;; `magit-save-repository-buffers' runs (magit-mode.el:1318), so Magit's
;; save-buffers behaviour can be left exactly at its stock default.
;;
;; Measured, staging one hunk:  112 spawns / 2216 ms  ->  58 spawns / 1080 ms.

(defvar my/magit-topology-cache (make-hash-table :test 'equal)
  "Cache of (DIRECTORY . REV-PARSE-FLAG) -> answer.")

(defconst my/magit-topology-args
  '("--show-toplevel" "--show-cdup" "--git-dir" "--is-bare-repository")
  "`rev-parse' flags whose answers are invariant under worktree/index changes.")

(defun my/magit-rev-parse-topology-cached (orig &rest args)
  "Cache the repository-topology `rev-parse' queries in ARGS.
ORIG is part of the key: `magit-rev-parse-true' and `magit-rev-parse-safe'
return different things for identical arguments."
  (if (and (null (cdr args))
           (member (car args) my/magit-topology-args)
           (not (file-remote-p default-directory)))
      (let* ((key (list default-directory (car args) orig))
             (hit (gethash key my/magit-topology-cache 'miss)))
        (if (eq hit 'miss)
            (puthash key (apply orig args) my/magit-topology-cache)
          hit))
    (apply orig args)))

;; `magit-rev-parse-safe' is not the only entry point: `magit-bare-repo-p'
;; goes through `magit-rev-parse-true' (magit-git.el:1081).  Cover the whole
;; family -- they all just shell out to `git rev-parse ARGS'.
(with-eval-after-load 'magit-git
  (dolist (fn '(magit-rev-parse magit-rev-parse-safe magit-rev-parse-true
                magit-rev-parse-false magit-rev-parse-p))
    (when (fboundp fn)
      (advice-add fn :around #'my/magit-rev-parse-topology-cached))))

(defvar my/truename-dir-cache (make-hash-table :test 'equal)
  "Cache of directory name -> `file-truename'.

`file-truename' resolves every path component with a separate syscall, and
recurses.  It showed up as 13% of a single \"stage one hunk\" profile.")

(defun my/truename-dir (dir)
  "Return the truename of DIR, cached, with a trailing slash."
  (let ((hit (gethash dir my/truename-dir-cache 'miss)))
    (if (eq hit 'miss)
        (puthash dir
                 (file-name-as-directory
                  (or (ignore-errors (file-truename dir)) dir))
                 my/truename-dir-cache)
      hit)))

(defun my/magit-flush-caches ()
  "Discard the `magit-toplevel' and truename caches.
Only needed if a symlink above a repository was retargeted."
  (interactive)
  (clrhash my/magit-toplevel-cache)
  (clrhash my/magit-gitdir-cache)
  (clrhash my/truename-dir-cache)
  (clrhash my/magit-topology-cache)
  (message "my/magit-perf: caches flushed"))

;; Cheap insurance: repo topology changes Magit itself performs.
(dolist (cmd '(magit-init magit-clone
                magit-worktree-checkout magit-worktree-branch
                magit-worktree-move magit-worktree-delete))
  (advice-add cmd :before (lambda (&rest _) (my/magit-flush-caches))
              '((name . my/magit-flush-caches))))


;;;; ------------------------------------------------------------------
;;;; 3. Make diff-hl's post-refresh hook cheaper -- but keep it synchronous
;;;; ------------------------------------------------------------------

;; An earlier version of this file deferred this hook onto idle timers and
;; drained it a few buffers at a time.  That was a mistake, and it is worth
;; recording why, because the reasoning looked sound.
;;
;; `diff-hl-magit-post-refresh' was the largest single item in the original
;; profile: 89 of 281 samples.  But samples are not spawns.  Each spawn blocks
;; for 40-800 ms under the endpoint agent, so one process makes a huge sample
;; count without being much *work*.  Measured directly, this hook costs the
;; same 10 spawns whether it runs synchronously or drained over idle timers --
;; and 8 of those 10 were the topology queries section 2 now caches.
;;
;; So the deferral saved nothing.  What it did do was take spawns that used to
;; block inside a command you already expected to be slow, and spread them
;; across the next several seconds of idle time -- i.e. across your typing.
;; At 40-800 ms per spawn that is strictly worse: same total cost, now paid
;; while you are trying to use the editor.  Hence "mildly unresponsive for
;; another 5-10s".  The timers are gone.
;;
;; What is genuinely worth fixing here is CPU, not process count.  Stock walks
;; every buffer calling `file-in-directory-p', which calls `file-truename' on
;; both arguments, which resolves every path component recursively: 64 of the
;; original 281 samples, 8 levels deep.  That is pure waste when a string
;; prefix test answers the question for every normal checkout.
;;
;; This replacement keeps stock's logic and ordering exactly -- same
;; `modified-files' query, same `vc-state' conditions, same
;; `vc-state-refresh'/`diff-hl-update' calls -- and only swaps the containment
;; test for a cached-truename prefix comparison.  Buffers you can actually see
;; are updated first, so the visible gutters settle before the rest.

(defconst my/diff-hl--unmodified-states '(up-to-date ignored unregistered))

(defconst my/case-insensitive-fs (memq system-type '(darwin windows-nt))
  "Non-nil when file names on this system compare case-insensitively.")

(defun my/file-relative-to-dir (file topdir)
  "Return FILE's path relative to TOPDIR, or nil if FILE is not under it.
TOPDIR must already be a truename with a trailing slash.  The fast path is a
string comparison; only unusual layouts (symlinked checkouts) pay for a
truename, and that one is cached.

The relative path is what Git wants to be asked about, so computing it here
costs nothing extra and saves the batched queries in section 8 a conversion."
  (let ((ci my/case-insensitive-fs))
    (if (string-prefix-p topdir file ci)
        (substring file (length topdir))
      (let ((d (file-name-directory file)))
        (and d
             (let ((true-d (my/truename-dir d)))
               (and (string-prefix-p topdir true-d ci)
                    (concat (substring true-d (length topdir))
                            (file-name-nondirectory file)))))))))

(defun my/file-under-dir-p (file topdir)
  "Return non-nil if FILE is under TOPDIR.
TOPDIR must already be a truename with a trailing slash."
  (and (my/file-relative-to-dir file topdir) t))

(defun my/diff-hl--update-buffer (buf modified-files)
  "Recompute diff-hl gutters for BUF, exactly as stock does."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (condition-case err
          (let* ((file buffer-file-name)
                 (backend (and file (vc-backend file))))
            (when backend
              (cond
               ((member file modified-files)
                (when (memq (vc-state file) my/diff-hl--unmodified-states)
                  (vc-state-refresh file backend))
                (diff-hl-update))
               ((not (memq (vc-state file backend)
                           my/diff-hl--unmodified-states))
                (vc-state-refresh file backend)
                (diff-hl-update)))))
        (error
         (message "my/diff-hl: %s: %S" (buffer-name buf) err))))))

(defun my/diff-hl-magit-post-refresh ()
  "Cheaper drop-in for `diff-hl-magit-post-refresh'.
Same work, same order of operations, same results.  Two things are different
from stock: the containment test avoids the recursive `file-truename' calls
that `file-in-directory-p' makes for every buffer, and the per-file `git
status' and `git diff-index' calls that every candidate buffer would make are
answered from one batched invocation each (section 8).

Buffers you can actually see are updated first, so the visible gutters settle
before the rest."
  (unless (and (bound-and-true-p diff-hl-disable-on-remote)
               (file-remote-p default-directory))
    (let ((topdir (ignore-errors (magit-toplevel))))
      (when topdir
        (let ((top (my/truename-dir topdir))
              (modified-files
               (ignore-errors
                 (magit-git-items "diff-tree" "-z" "--name-only"
                                  "-r" "HEAD~" "HEAD")))
              (candidates nil)
              (visible nil)
              (hidden nil))
          (dolist (buf (buffer-list))
            (let ((file (buffer-local-value 'buffer-file-name buf)))
              (when (and file
                         (buffer-local-value 'diff-hl-mode buf)
                         (not (buffer-modified-p buf)))
                (let ((rel (my/file-relative-to-dir file top)))
                  (when (and rel (file-exists-p file))
                    ;; Every buffer under TOPDIR still gets updated; only the
                    ;; ones this repository can actually answer for are
                    ;; batched.
                    (when (my/git--same-repo-p file top)
                      (push (list buf file rel) candidates))
                    (if (get-buffer-window buf 'visible)
                        (push buf visible)
                      (push buf hidden)))))))
          (setq candidates (nreverse candidates)
                visible    (nreverse visible)
                hidden     (nreverse hidden))
          (my/git-call-with-batched-queries
           top candidates
           (lambda ()
             (dolist (buf visible)
               (my/diff-hl--update-buffer buf modified-files))
             (dolist (buf hidden)
               (my/diff-hl--update-buffer buf modified-files)))))))))

(with-eval-after-load 'diff-hl
  (remove-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook    'magit-post-refresh-hook #'my/diff-hl-magit-post-refresh))


;;;; ------------------------------------------------------------------
;;;; 4. Spawn fewer processes per status refresh  --  OPT-IN ONLY
;;;; ------------------------------------------------------------------

;; NOTHING IN THIS SECTION IS ACTIVE BY DEFAULT.  The status buffer, the
;; revision buffer and the post-commit diff all look and behave exactly as
;; stock Magit until you turn on `my/magit-status-fast-mode'.  Sections 1, 2,
;; 3 and 5 are invisible -- they change no output, only how it is obtained --
;; and stay on unconditionally.
;;
;; MEASURED, not guessed.  Status refresh of ~/wasmtime, 40 file buffers open,
;; spawns per refresh (with the caching in section 2 already active):
;;
;;     A. nothing removed                                 28   <- DEFAULT
;;     B. push-branch header                              26
;;     C. the three tracking sections                     25
;;     D. B + C                                           23
;;     E. D + tags header                                 21   <- lean trim
;;     F. E + upstream-branch header                      20
;;     G. F + stashes                                     18
;;     H. G + recent-commits log                          10   <- fast mode
;;
;; Two things worth knowing, because they contradict the usual advice:
;;
;; 1. Savings are NOT additive, and measuring one removal at a time is
;;    misleading.  Magit answers git queries out of `magit--refresh-cache' for
;;    the duration of a refresh, so sections share results.  Removed alone, the
;;    three tracking sections save 0 -- they are riding on queries the branch
;;    headers already paid for.  Remove the headers too and they suddenly cost
;;    2-3 spawns each, because now nobody has pre-paid.  Only combinations
;;    mean anything.
;;
;; 2. `magit-insert-unpushed-to-upstream-or-recent' -- the recent-commits log --
;;    is by far the most expensive single section: 8 spawns, G -> H above.
;;    It is also the part of the status buffer most people actually read.

(defcustom my/magit-status-lean-trim
  '((magit-status-headers-hook  . magit-insert-push-branch-header)
    (magit-status-headers-hook  . magit-insert-tags-header)
    (magit-status-sections-hook . magit-insert-unpushed-to-pushremote)
    (magit-status-sections-hook . magit-insert-unpulled-from-pushremote)
    (magit-status-sections-hook . magit-insert-unpulled-from-upstream))
  "Cheap trim: drops duplicated tracking info.  Measured 28 -> 21 spawns.
Not applied unless it appears in `my/magit-status-trim'."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'my/magit-perf)

(defcustom my/magit-status-extra-trim
  '((magit-status-headers-hook  . magit-insert-upstream-branch-header)
    (magit-status-sections-hook . magit-insert-stashes)
    (magit-status-sections-hook . magit-insert-unpushed-to-upstream-or-recent))
  "Aggressive trim: costs the recent-commits log, stashes, upstream header.
Measured 21 -> 10 spawns.  Not applied unless it appears in
`my/magit-status-trim'."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'my/magit-perf)

(defcustom my/magit-status-trim
  (append my/magit-status-lean-trim my/magit-status-extra-trim)
  "Sections `my/magit-status-fast-mode' removes while it is on.
Defaults to combination H above (10 spawns).  For the middle ground, set this
to just `my/magit-status-lean-trim' (21 spawns, keeps the recent-commits log)."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'my/magit-perf)

(defcustom my/magit-status-fast-vars
  ;; Not sections, and not covered by the refresh cache: whole extra pipelines
  ;; run on demand.  `magit-revision-insert-related-refs' runs
  ;; `git branch --contains'/`--merged' every time you press RET on a commit;
  ;; `magit-commit-show-diff' shows a second full diff after `C-c C-c'.
  '((magit-revision-insert-related-refs . nil)
    (magit-commit-show-diff             . nil))
  "Variables `my/magit-status-fast-mode' overrides, restored when it is off."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'my/magit-perf)

(defun my/magit--apply-trim (trim)
  (pcase-dolist (`(,hook . ,fn) trim)
    (remove-hook hook fn)))

(defvar my/magit--pristine-hooks nil
  "Magit's own default status hooks, snapshotted before we touch them.
`remove-hook' mutates the default value of a global hook, so the original
lists must be captured up front or they cannot be restored.")

(defvar my/magit--pristine-vars nil
  "Original values of `my/magit-status-fast-vars', captured before override.")

(defun my/magit--snapshot ()
  "Capture Magit's own defaults once, before anything here modifies them."
  (require 'magit-status)
  (require 'magit-diff)
  (require 'magit-commit)
  (unless my/magit--pristine-hooks
    (setq my/magit--pristine-hooks
          (list (cons 'magit-status-headers-hook
                      (copy-sequence (default-value 'magit-status-headers-hook)))
                (cons 'magit-status-sections-hook
                      (copy-sequence (default-value 'magit-status-sections-hook))))))
  (unless my/magit--pristine-vars
    (setq my/magit--pristine-vars
          (mapcar (lambda (cell)
                    (cons (car cell) (symbol-value (car cell))))
                  my/magit-status-fast-vars))))

(defun my/magit--restore-all ()
  "Put both status hooks and the overridden variables back to Magit's own."
  (my/magit--snapshot)
  (pcase-dolist (`(,hook . ,value) my/magit--pristine-hooks)
    (set hook (copy-sequence value))
    (set-default hook (copy-sequence value)))
  (pcase-dolist (`(,var . ,value) my/magit--pristine-vars)
    (set-default var value)))

(define-minor-mode my/magit-status-fast-mode
  "Trade Magit status detail for fewer git subprocesses.

OFF (the default): Magit's UI is completely untouched -- every section,
header, related-ref lookup and post-commit diff behaves as stock.  The
invisible optimizations (executable resolution, toplevel caching, deferred
diff-hl, notification-based auto-revert) remain active either way.

ON: removes the sections in `my/magit-status-trim' and overrides
`my/magit-status-fast-vars'.  Measured 28 -> 10 spawns per refresh."
  :global t
  :lighter " magit-fast"
  :group 'my/magit-perf
  (my/magit--snapshot)
  (my/magit--restore-all)
  (when my/magit-status-fast-mode
    (my/magit--apply-trim my/magit-status-trim)
    (pcase-dolist (`(,var . ,value) my/magit-status-fast-vars)
      (set-default var value)))
  (when (derived-mode-p 'magit-status-mode)
    (magit-refresh)))

(defun my/magit-status-full ()
  "Restore every default status section and refresh once.
Equivalent to turning `my/magit-status-fast-mode' off."
  (interactive)
  (if my/magit-status-fast-mode
      (my/magit-status-fast-mode -1)
    (my/magit--restore-all)
    (when (derived-mode-p 'magit-status-mode)
      (magit-refresh)))
  (message "Magit status: full detail"))

;; `magit-git-debug' wraps every call to capture stderr separately; keep off.
(setq magit-git-debug nil)


;;;; ------------------------------------------------------------------
;;;; 5. Stop polling the filesystem behind Magit's back
;;;; ------------------------------------------------------------------

;; 66 samples (13%) of the profile were `timer-event-handler', 41 of them
;; `auto-revert-buffers' walking every buffer and stat-ing its file every 5
;; seconds.  Prelude turns on `global-auto-revert-mode', so Magit's
;; `auto-revert-buffer-list-filter' advice is bypassed entirely.
;;
;; This machine has kqueue file notification (verified) and ulimit -n is
;; 1048576, so notification-only reverting is both available and safe.
;; `auto-revert-avoid-polling' has a :set function that restarts the timer,
;; so it must go through `customize-set-variable' -- Prelude already enabled
;; the mode by the time this file loads.
(setq auto-revert-check-vc-info nil)
(setq auto-revert-verbose nil)
(customize-set-variable 'auto-revert-avoid-polling t)
(customize-set-variable 'auto-revert-interval 5)

;; Every `find-file' asks each handled backend "is this yours?".  Only Git is
;; in use here, and the rest cost stats (and for some, spawns) on every visit.
(setq vc-handled-backends '(Git))


;;;; ------------------------------------------------------------------
;;;; 6. Measurement helpers
;;;; ------------------------------------------------------------------

(defun my/magit-toggle-refresh-verbose ()
  "Toggle `magit-refresh-verbose', which reports per-section refresh timings."
  (interactive)
  (require 'magit-mode)
  (setq magit-refresh-verbose (not magit-refresh-verbose))
  (message "magit-refresh-verbose: %s" magit-refresh-verbose))

(defun my/spawn-cost (&optional n)
  "Report the average wall-clock cost of spawning a trivial process, over N runs."
  (interactive "p")
  (let* ((n (max 1 (or n 10)))
         (start (current-time)))
    (dotimes (_ n) (call-process "/usr/bin/true" nil nil nil))
    (let ((ms (/ (* 1000.0 (float-time (time-since start))) n)))
      (message "process spawn: %.1f ms average over %d runs (unhooked baseline: ~1-2 ms)"
               ms n))))

(defun my/magit-perf-report ()
  "Describe the active Magit latency workarounds."
  (interactive)
  (with-current-buffer (get-buffer-create "*magit-perf*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (format
        "Magit latency workarounds (personal/my-magit-perf.el)
=====================================================

git executable            : %s
emacsclient               : %s
topology cache            : %d entries
magit-toplevel cache      : %d entries
gitdir cache              : %d entries
truename cache            : %d entries
status sections           : %s
diff-hl post-refresh      : synchronous (cheap containment test)
diff-hl git queries       : %s
ref labels from for-each-ref : %s
savehist autosave         : %s
auto-revert-avoid-polling : %s (interval %s)
vc-handled-backends       : %s
related refs in revisions : %s
diff after commit         : %s

Commands
  M-x my/magit-status-fast-mode       trade detail for fewer spawns (off by default)
  M-x my/magit-status-full            restore every status section
  M-x my/magit-toggle-refresh-verbose per-section refresh timings
  M-x my/magit-flush-caches           drop the topology/toplevel/truename caches
  M-x my/magit-trace-spawns           count git processes for the next command
  M-x my/spawn-cost                   measure the spawn tax right now
"
        magit-git-executable
        with-editor-emacsclient-executable
        (hash-table-count my/magit-topology-cache)
        (hash-table-count my/magit-toplevel-cache)
        (hash-table-count my/magit-gitdir-cache)
        (hash-table-count my/truename-dir-cache)
        (if my/magit-status-fast-mode "fast (trimmed)" "stock (untouched)")
        (if (advice-member-p #'my/vc-git-state-batched 'vc-git-state)
            (format "batched above %d buffers" my/git-batch-threshold)
          "one process per buffer")
        (if (advice-member-p #'my/magit-rev-verify-cached 'magit-rev-verify)
            "yes" "no")
        (if (and (bound-and-true-p savehist-timer)
                 (timer--idle-delay savehist-timer))
            "on an idle timer" "on a wall-clock timer")
        auto-revert-avoid-polling
        auto-revert-interval
        vc-handled-backends
        magit-revision-insert-related-refs
        magit-commit-show-diff))
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

;;;; ------------------------------------------------------------------
;;;; 7. Ground truth: trace spawns, and a kill switch
;;;; ------------------------------------------------------------------

;; I cannot reproduce your machine: no endpoint agent, no LSP, no 40 buffers.
;; Every number in the comments above came from a batch Emacs, where a spawn
;; costs ~1 ms instead of 40-800 ms.  So use these to check the claims in your
;; real session rather than taking them on faith.

(defvar my/magit--trace nil)
(defvar my/magit--trace-depth 0)
(defvar my/magit--trace-start nil)

(defun my/magit--trace-spawn (orig &rest args)
  "Record one git process.
On a local repository `process-file' is a thin wrapper that calls
`call-process', so advising both counts every spawn twice.  The depth guard
records only the outermost frame -- one entry per actual process."
  (if (> my/magit--trace-depth 0)
      (apply orig args)
    (let ((my/magit--trace-depth 1)
          (t0 (float-time))
          (cmd (mapconcat #'identity (cl-remove-if-not #'stringp args) " ")))
      (unwind-protect (apply orig args)
        (push (cons (- (float-time) t0) cmd) my/magit--trace)))))

(defun my/magit-trace-spawns ()
  "Count and time every git process spawned by the next thing you do.
Run this, perform one Magit operation, then run it again to see the report.

The report separates time spent inside git from total elapsed time, because
those are different problems: the first one this file can attack, the second
one means something other than process spawning is blocking Emacs."
  (interactive)
  (if my/magit--trace
      (let* ((trace (nreverse my/magit--trace))
             (git-time (apply #'+ (mapcar #'car trace)))
             (wall (- (float-time) my/magit--trace-start))
             (counts (make-hash-table :test 'equal)))
        (setq my/magit--trace nil my/magit--trace-start nil)
        (advice-remove 'process-file  #'my/magit--trace-spawn)
        (advice-remove 'call-process  #'my/magit--trace-spawn)
        (advice-remove 'start-process #'my/magit--trace-spawn)
        (dolist (x trace)
          (let ((c (replace-regexp-in-string
                    "\\`.*?\\(?:diff\\.noPrefix=false \\)?" "" (cdr x))))
            (puthash c (cons (1+ (car (gethash c counts '(0 . 0))))
                             (+ (car x) (cdr (gethash c counts '(0 . 0)))))
                     counts)))
        (with-current-buffer (get-buffer-create "*magit spawn trace*")
          (let ((inhibit-read-only t) rows)
            (erase-buffer)
            (insert (format "%d git processes, %.0f ms mean\n" (length trace)
                            (if trace (* 1000 (/ git-time (length trace))) 0))
                    (format "%.1f s inside git, %.1f s elapsed total  (%.0f%% git)\n"
                            git-time wall
                            (if (> wall 0) (* 100 (/ git-time wall)) 0))
                    (format "%.1f s was NOT git -- if that is large, process spawning\nis no longer your bottleneck.\n\n"
                            (max 0 (- wall git-time))))
            (maphash (lambda (k v) (push (list (car v) (cdr v) k) rows)) counts)
            (dolist (r (sort rows (lambda (a b) (> (nth 1 a) (nth 1 b)))))
              (insert (format "%4d x %6.0f ms  %s\n" (nth 0 r) (* 1000 (nth 1 r)) (nth 2 r))))
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer))))
    (setq my/magit--trace nil
          my/magit--trace-depth 0
          my/magit--trace-start (float-time))
    (advice-add 'process-file  :around #'my/magit--trace-spawn)
    (advice-add 'call-process  :around #'my/magit--trace-spawn)
    (advice-add 'start-process :around #'my/magit--trace-spawn)
    (message "Tracing git spawns.  Do the slow thing, then M-x my/magit-trace-spawns again.")))

(defun my/magit-perf-disable ()
  "Turn every optimisation in this file off, restoring stock behaviour.
Use this to A/B against the config: if Magit is no faster with these off,
the bottleneck is somewhere I have not found."
  (interactive)
  (when (bound-and-true-p my/magit-status-fast-mode)
    (my/magit-status-fast-mode -1))
  (advice-remove 'magit-toplevel      #'my/magit-toplevel-cached)
  (advice-remove 'magit-gitdir        #'my/magit-gitdir-cached)
  (dolist (fn '(magit-rev-parse magit-rev-parse-safe magit-rev-parse-true
                magit-rev-parse-false magit-rev-parse-p))
    (when (fboundp fn) (advice-remove fn #'my/magit-rev-parse-topology-cached)))
  (remove-hook 'magit-post-refresh-hook #'my/diff-hl-magit-post-refresh)
  (add-hook    'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (advice-remove 'vc-git-state #'my/vc-git-state-batched)
  (advice-remove 'vc-git-registered #'my/vc-git-registered-batched)
  (advice-remove 'diff-hl-diff-against-reference
                 #'my/diff-hl-diff-against-reference-batched)
  (advice-remove 'magit-rev-verify #'my/magit-rev-verify-cached)
  (my/magit-flush-caches)
  (message "my/magit-perf: disabled; Magit is stock now"))

(defun my/magit-perf-enable ()
  "Re-enable the optimisations turned off by `my/magit-perf-disable'."
  (interactive)
  (advice-add 'magit-toplevel       :around #'my/magit-toplevel-cached)
  (advice-add 'magit-gitdir         :around #'my/magit-gitdir-cached)
  (dolist (fn '(magit-rev-parse magit-rev-parse-safe magit-rev-parse-true
                magit-rev-parse-false magit-rev-parse-p))
    (when (fboundp fn) (advice-add fn :around #'my/magit-rev-parse-topology-cached)))
  (remove-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook    'magit-post-refresh-hook #'my/diff-hl-magit-post-refresh)
  (advice-add 'vc-git-state :around #'my/vc-git-state-batched)
  (advice-add 'vc-git-registered :around #'my/vc-git-registered-batched)
  (advice-add 'diff-hl-diff-against-reference :around
              #'my/diff-hl-diff-against-reference-batched)
  (advice-add 'magit-rev-verify :around #'my/magit-rev-verify-cached)
  (message "my/magit-perf: enabled"))


;;;; ------------------------------------------------------------------
;;;; 8. Ask git once per question, not once per buffer
;;;; ------------------------------------------------------------------

;; Section 3 made the diff-hl post-refresh pass cheap in Lisp.  What it did
;; not touch is the process count, and that is most of what the latest
;; profile has left in it:
;;
;;   my/diff-hl-magit-post-refresh                        50
;;     vc-state-refresh -> vc-git-state -> process-file   21
;;     diff-hl-update ...                                 17
;;   diff-hl-update (rest of the same pass)               26
;;     diff-hl-changes -> vc-git-diff -> process-file     23
;;
;; Both are O(file buffers in the repository).  diff-hl asks two questions per
;; buffer and each one is its own git process:
;;
;;   git status --porcelain -z --untracked-files --ignored -- FILE
;;   git diff-index --exit-code -U0 -p HEAD -- FILE
;;
;; git answers either question for any number of paths in a single invocation,
;; and the answers are per-path, so batching loses nothing.  With 20 open
;; buffers that is 40 processes replaced by 2 -- at 40-800 ms of endpoint-agent
;; tax per spawn, the entire second half of the profile.
;;
;; The mechanism is two tables and two pieces of advice.  Each table is bound
;; only around the update loops, and only ever *answers* a question that was
;; going to be asked anyway; a miss falls through to the stock function.  So
;; the worst case of a bad prediction is an extra process, never a wrong
;; gutter -- and anything unexpected (git failing, output that cannot be
;; attributed to a requested file) abandons the whole batch rather than
;; guessing.

(eval-when-compile
  (require 'magit-git nil t)
  (require 'diff-hl nil t)
  (require 'vc-git nil t)
  (require 'vc-hg nil t))

(declare-function vc-git--git-status-to-vc-state "vc-git" (code-list))
(declare-function vc-git-diff "vc-git" (files &optional rev1 rev2 buffer async))
(declare-function magit-git-insert "magit-git" (&rest args))

(defconst my/git-batch-chunk 200
  "Maximum number of path arguments per batched git invocation.
Only a guard against blowing the argument limit in a repository with a very
large number of open buffers; one chunk is the normal case.")

(defun my/git--chunks (list)
  "Split LIST into sublists of at most `my/git-batch-chunk' elements."
  (let (chunks)
    (while list
      (let ((n my/git-batch-chunk)
            (chunk nil))
        (while (and list (> n 0))
          (push (pop list) chunk)
          (setq n (1- n)))
        (push (nreverse chunk) chunks)))
    (nreverse chunks)))

(defun my/git--path-key (path)
  "Return a hash key for repository-relative PATH.
Case-folded where the filesystem is, so that a buffer visiting \"Foo.el\"
still matches the \"foo.el\" git has recorded."
  (if my/case-insensitive-fs (downcase path) path))

(defun my/git--items-checked (&rest args)
  "Like `magit-git-items', but return the symbol `error' if git fails.
`magit-git-items' cannot tell \"no output\" from \"git blew up\", and here
that difference decides whether a file looks clean."
  (let ((env process-environment))
    (with-temp-buffer
      (setq-local process-environment env)
      (if (zerop (apply #'magit-git-insert (flatten-tree args)))
          (split-string (buffer-string) "\0" t)
        'error))))

;;; The `git status' batch.

(defvar my/vc-git--state-table nil
  "Hash of absolute file name -> `vc-state' symbol, or nil.
Bound only around the diff-hl post-refresh pass.")

(defun my/vc-git-state-batched (orig file)
  "Answer FILE's state from `my/vc-git--state-table' when it is in there."
  (let ((hit (if my/vc-git--state-table
                 (gethash (expand-file-name file)
                          my/vc-git--state-table 'miss)
               'miss)))
    (if (eq hit 'miss)
        (funcall orig file)
      hit)))

(with-eval-after-load 'vc-git
  (advice-add 'vc-git-state :around #'my/vc-git-state-batched))

(defun my/git--same-repo-p (file topdir)
  "Return non-nil if FILE's own repository root is TOPDIR.
A file inside a submodule is under TOPDIR but git answers questions about it
from the submodule, so a batch run at TOPDIR would say nothing about it -- and
\"git said nothing\" means \"clean\".  Such files keep their own processes."
  (let ((root (ignore-errors (vc-git-root file))))
    (and root (equal (my/truename-dir root) topdir))))

(defun my/vc-git-registered-batched (orig file)
  "Answer `vc-git-registered' for FILE from `my/vc-git--state-table'.
Every state the table can report except `unregistered' settles the question:
`ignored' means git is not tracking the file and never was, and anything else
means it is in the index or in HEAD.  `unregistered' is the one ambiguous
answer -- plain `??', but also the \"deleted from the index, still in HEAD and
in the working tree\" pair -- so it goes to ORIG."
  (let ((hit (if my/vc-git--state-table
                 (gethash (expand-file-name file) my/vc-git--state-table 'miss)
               'miss)))
    (cond ((memq hit '(miss unregistered)) (funcall orig file))
          ((eq hit 'ignored) nil)
          (t t))))

(with-eval-after-load 'vc-git
  (advice-add 'vc-git-registered :around #'my/vc-git-registered-batched))

(defun my/vc-git--build-state-table (topdir candidates)
  "Return a hash of absolute file name -> `vc-state' for CANDIDATES.
CANDIDATES is a list of (BUFFER FILE RELATIVE-PATH).

Runs one `git status --porcelain' for the whole set, with exactly the
arguments `vc-git-state' passes for a single file, and feeds the resulting
two-character codes to `vc-git--git-status-to-vc-state' -- so each answer is
the one the stock per-file call would have produced, including its quirks.  A
path git says nothing about is clean, which is what an empty code list means
to that function too.

Returns nil if git failed, which makes the caller fall back to the per-file
calls rather than report everything as clean."
  (let ((codes (make-hash-table :test 'equal))
        (default-directory topdir)
        (failed nil))
    (dolist (chunk (my/git--chunks (mapcar (lambda (c) (nth 2 c)) candidates)))
      (let ((items (my/git--items-checked
                    "status" "--porcelain" "-z"
                    "--untracked-files" "--ignored" "--" chunk)))
        (if (eq items 'error)
            (setq failed t)
          (while items
            (let ((item (pop items)))
              (when (> (length item) 3)
                (let ((code (substring item 0 2))
                      (path (substring item 3)))
                  ;; A rename or copy is followed by a second record holding
                  ;; the source path.  It is not a status line of its own.
                  (when (memq (aref code 0) '(?R ?C))
                    (pop items))
                  (push code (gethash (my/git--path-key path) codes)))))))))
    (unless failed
      (let ((table (make-hash-table :test 'equal)))
        (dolist (c candidates table)
          (puthash (expand-file-name (nth 1 c))
                   (vc-git--git-status-to-vc-state
                    (nreverse (gethash (my/git--path-key (nth 2 c)) codes)))
                   table))))))

;;; The `git diff-index' batch.

(defvar my/diff-hl--diff-table nil
  "Hash of absolute file name -> diff text, or nil.
Bound only around the diff-hl post-refresh pass.")

(defun my/diff-hl-diff-against-reference-batched (orig file backend buffer
                                                       &optional new-rev)
  "Serve FILE's diff from `my/diff-hl--diff-table' when it is in there.
Only the plain \"working tree against HEAD\" question is answered from the
table.  Every other combination of arguments -- an explicit revision, a
reference revision, the `git-index' variant used when
`diff-hl-show-staged-changes' is nil -- is a different diff, and goes to
ORIG."
  (let ((hit (if (and my/diff-hl--diff-table
                      (null new-rev)
                      (null diff-hl-reference-revision)
                      (bound-and-true-p diff-hl-show-staged-changes)
                      (eq backend 'Git))
                 (gethash (expand-file-name file) my/diff-hl--diff-table 'miss)
               'miss)))
    (if (eq hit 'miss)
        (funcall orig file backend buffer new-rev)
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert hit)))
      ;; Stock ends with a bare BUFFER and `diff-hl-changes' stores that.
      buffer)))

(with-eval-after-load 'diff-hl
  (advice-add 'diff-hl-diff-against-reference :around
              #'my/diff-hl-diff-against-reference-batched))

(defun my/diff-hl--dirty (candidates state-table)
  "Return the CANDIDATES diff-hl is about to run `git diff' for.
Those are the ones whose refreshed state `diff-hl-modified-p' accepts.
Guessing wrong is cheap in both directions: a needless path in the batch, or
one stock call that was going to happen anyway."
  (let (dirty)
    (dolist (c candidates (nreverse dirty))
      (when (memq (gethash (expand-file-name (nth 1 c)) state-table)
                  '(edited conflict))
        (push c dirty)))))

(defconst my/git--ascii-path-regexp "\\`[[:ascii:]]+\\'")

(defun my/git--plain-path-p (rel)
  "Return non-nil if git prints REL unquoted in a `diff --git' header.
Non-ASCII, quotes, backslashes and control characters all get C-style
quoting, which this file does not try to undo; such files keep their own
process."
  (and (string-match-p my/git--ascii-path-regexp rel)
       (not (string-match-p "[\"\\\\[:cntrl:]]" rel))))

(defun my/diff-hl--build-diff-table (topdir dirty)
  "Return a hash of absolute file name -> diff text for DIRTY.

One `git diff-index' for the whole set, run through `vc-git-diff' inside
`diff-hl-with-diff-switches' so the command and its switches (notably -U0)
are byte-for-byte the ones diff-hl would have used per file, then split on
the `diff --git' headers.  Every requested file is seeded with the empty
string, because \"git printed no hunk for it\" is a real answer.

Returns nil -- meaning \"fall back to one process per file\" -- if git failed
or if any chunk of output cannot be attributed to a requested file.  A diff
served to the wrong buffer would be a wrong gutter, so anything unrecognised
abandons the batch instead of guessing."
  (let ((batchable (cl-remove-if-not
                    (lambda (c) (my/git--plain-path-p (nth 2 c)))
                    dirty))
        (table (make-hash-table :test 'equal))
        (abs (make-hash-table :test 'equal))
        (headers (make-hash-table :test 'equal))
        (ok t))
    (when batchable
      (dolist (c batchable)
        (let ((rel (nth 2 c))
              (file (expand-file-name (nth 1 c))))
          (puthash file "" table)
          (puthash rel file abs)
          ;; The two header spellings git uses, with and without diff.noprefix.
          (puthash (concat "diff --git a/" rel " b/" rel) rel headers)
          (puthash (concat "diff --git " rel " " rel) rel headers)))
      (dolist (chunk (my/git--chunks batchable))
        (when ok
          (with-temp-buffer
            (setq default-directory topdir)
            (unless (condition-case nil
                        (progn
                          (diff-hl-with-diff-switches
                           ;; Repository-relative paths, not absolute: TOPDIR
                           ;; is a truename and the buffer file names may not
                           ;; be, and `vc-do-command' relativises against
                           ;; `default-directory' without noticing.
                           (vc-git-diff (mapcar (lambda (c) (nth 2 c)) chunk)
                                        nil nil (current-buffer) nil))
                          t)
                      (error nil))
              (setq ok nil))
            (when ok
              (let ((case-fold-search nil)
                    (starts nil))
                (goto-char (point-min))
                (while (re-search-forward "^diff --git " nil t)
                  (push (match-beginning 0) starts))
                (let ((bounds (append (nreverse starts) (list (point-max)))))
                  (while (and ok (cdr bounds))
                    (let* ((beg (car bounds))
                           (end (cadr bounds))
                           (line (buffer-substring-no-properties
                                  beg (save-excursion
                                        (goto-char beg)
                                        (line-end-position))))
                           (rel (gethash line headers)))
                      (if (null rel)
                          (setq ok nil)
                        (puthash (gethash rel abs)
                                 (buffer-substring-no-properties beg end)
                                 table)))
                    (setq bounds (cdr bounds))))))))))
    (and ok batchable table)))

(defun my/git-call-with-batched-queries (topdir candidates fn)
  "Call FN with diff-hl's per-file git queries answered from batched output.
CANDIDATES is a list of (BUFFER FILE RELATIVE-PATH).  If a table cannot be
built it is left nil and FN runs against entirely stock behaviour."
  (let* ((my/vc-git--state-table
          (and (> (length candidates) my/git-batch-threshold)
               (ignore-errors
                 (my/vc-git--build-state-table topdir candidates))))
         (my/diff-hl--diff-table
          (and my/vc-git--state-table
               (ignore-errors
                 (my/diff-hl--build-diff-table
                  topdir
                  (my/diff-hl--dirty candidates my/vc-git--state-table))))))
    (funcall fn)))


;;;; ------------------------------------------------------------------
;;;; 9. One `for-each-ref' instead of a `rev-parse' per ref label
;;;; ------------------------------------------------------------------

;; `magit-log-wash-rev' -> `magit-format-ref-labels' is 21 samples, and it
;; runs once per commit in the log section.  For every ref decorating a
;; commit it asks git whether a tag of that name exists:
;;
;;   (magit-rev-verify (concat "refs/tags/" name))   ; git rev-parse --verify
;;
;; `magit-rev-verify' goes through `magit-git-string', which *is*
;; refresh-cached, so repeats within one refresh are free -- but each distinct
;; refname is still its own process.  `git for-each-ref' lists every ref and
;; its object id in one, and the listing is exhaustive: a name absent from it
;; does not exist, which is exactly what `rev-parse --verify' reports by
;; returning nil.  For an annotated tag both give the tag object's id.
;;
;; Restricted to full "refs/..." names made of ordinary characters, because
;; those are the ones `for-each-ref' enumerates verbatim.  Anything carrying
;; revision syntax (~ ^ : @{} ..) asks git to *resolve* something, which a
;; listing cannot answer, so those still spawn.  Short names are deliberately
;; not handled either: resolving "main" runs git's disambiguation order over
;; refs/, refs/tags/, refs/heads/, refs/remotes/..., and getting that subtly
;; wrong would put the wrong label on the wrong commit.
;;
;; The table lives in Magit's own per-refresh cache, so it lasts exactly one
;; `magit-refresh' and cannot go stale inside one.

(defconst my/magit--plain-refname-regexp "\\`refs/[A-Za-z0-9._/+-]+\\'"
  "Refnames this file is willing to answer from a `for-each-ref' listing.")

(defun my/magit--ref-table ()
  "Return a hash of full refname -> object id for the current repository.
Computed once per `magit-refresh'."
  (magit--with-refresh-cache (cons default-directory 'my/ref-table)
    (let ((table (make-hash-table :test 'equal)))
      (dolist (line (magit-git-lines "for-each-ref"
                                     "--format=%(refname) %(objectname)"))
        (let ((sep (string-search " " line)))
          (when sep
            (puthash (substring line 0 sep) (substring line (1+ sep)) table))))
      table)))

(defun my/magit-rev-verify-cached (orig rev)
  "Answer REV from `my/magit--ref-table' when it is a plain full refname."
  (if (and magit--refresh-cache
           (stringp rev)
           (string-match-p my/magit--plain-refname-regexp rev)
           (not (file-remote-p default-directory)))
      (gethash rev (my/magit--ref-table))
    (funcall orig rev)))

(with-eval-after-load 'magit-git
  (advice-add 'magit-rev-verify :around #'my/magit-rev-verify-cached))


;;;; ------------------------------------------------------------------
;;;; 10. Keep unrelated timers out of the middle of a Magit command
;;;; ------------------------------------------------------------------

;; 54 of 431 samples in the profile are `timer-event-handler', and 16 of those
;; are `savehist-autosave'.  Staging is asynchronous -- Magit waits on the git
;; process -- and timers run during that wait, so savehist serialises every
;; history variable with `prin1' right in the middle of the lock-up.  It does
;; that whether or not the result differs from what is on disk.
;;
;; `savehist-mode' schedules it with `run-with-timer', a wall-clock timer.
;; Nothing about the work wants that; an idle timer does the same job at a
;; moment when you are not waiting for anything.
;;
;; The trade-off, stated plainly: history is now written after a minute of
;; genuine idleness rather than every minute regardless.  `savehist-autosave'
;; is still on `kill-emacs-hook', so a normal exit saves either way; only a
;; crash during a long uninterrupted stretch of work loses more than before.

(defun my/savehist-use-idle-timer ()
  "Run savehist's autosave from an idle timer instead of a wall-clock one."
  (when (timerp savehist-timer)
    (cancel-timer savehist-timer))
  (setq savehist-timer
        (and savehist-autosave-interval
             (run-with-idle-timer savehist-autosave-interval t
                                  #'savehist-autosave))))

(with-eval-after-load 'savehist
  (add-hook 'savehist-mode-hook #'my/savehist-use-idle-timer)
  (when (bound-and-true-p savehist-mode)
    (my/savehist-use-idle-timer)))


(provide 'my-magit-perf)
;;; my-magit-perf.el ends here
