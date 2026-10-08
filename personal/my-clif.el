;;; my-clif.el --- Major mode for Cranelift's CLIF text format  -*- lexical-binding: t; -*-

;;; Commentary:

;; Syntax highlighting for `.clif' files: Cranelift's IR text format, as
;; accepted by `cranelift/reader' and emitted by
;; `cranelift/codegen/src/write.rs'.  Also knows about the
;; `test'/`set'/`target'/`feature' preamble and the FileCheck directives
;; used throughout `cranelift/filetests'.

;;; Code:

(defgroup clif nil
  "Major mode for Cranelift's CLIF text format."
  :group 'languages
  :prefix "clif-")

(defcustom clif-indent-offset 4
  "Indentation of a function body, matching the CLIF printer."
  :type 'integer
  :group 'clif)

;;; Keyword tables
;;
;; Opcodes are the generated `Opcode' name table; everything else is an
;; identifier the reader's parser matches on by name.

(eval-and-compile

  (defconst clif--opcodes
    '("atomic_cas" "atomic_load" "atomic_rmw" "atomic_store" "avg_round"
      "band" "bitcast" "bitrev" "bitselect" "blendv" "bmask" "bnot" "bor"
      "br_table" "brif" "bswap" "bxor" "call" "call_indirect" "ceil" "cls"
      "clz" "ctz" "debugtrap" "dynamic_stack_addr" "extract_vector"
      "extractlane" "f128const" "f16const" "f32const" "f64const" "fabs"
      "fadd" "fcmp" "fcopysign" "fcvt_from_sint" "fcvt_from_uint"
      "fcvt_to_sint" "fcvt_to_sint_sat" "fcvt_to_uint" "fcvt_to_uint_sat"
      "fdemote" "fdiv" "fence" "floor" "fma" "fmax" "fmin" "fmul" "fneg"
      "fpromote" "fsub" "func_addr" "fvdemote" "fvpromote_low"
      "get_exception_handler_address" "get_frame_pointer" "get_pinned_reg"
      "get_return_address" "get_stack_pointer" "iabs" "iadd" "iadd_pairwise"
      "icmp" "iconcat" "iconst" "imul" "ineg" "insertlane" "ireduce" "ishl"
      "isplit" "istore16" "istore32" "istore8" "isub" "jump" "load" "nearest"
      "nop" "popcnt" "return" "return_call" "return_call_indirect" "rotl"
      "rotr" "sadd_overflow" "sadd_overflow_cin" "sadd_sat"
      "scalar_to_vector" "sdiv" "select" "select_spectre_guard"
      "sequence_point" "set_pinned_reg" "sextend" "shuffle" "sload16"
      "sload16x4" "sload32" "sload32x2" "sload8" "sload8x8" "smax" "smin"
      "smul_overflow" "smulhi" "snarrow" "splat" "sqmul_round_sat" "sqrt"
      "srem" "sshr" "ssub_overflow" "ssub_overflow_bin" "ssub_sat"
      "stack_addr" "stack_switch" "store" "swiden_high" "swiden_low"
      "swizzle" "symbol_value" "tls_value" "trap" "trapnz" "trapz" "trunc"
      "try_call" "try_call_indirect" "uadd_overflow" "uadd_overflow_cin"
      "uadd_overflow_trap" "uadd_sat" "udiv" "uextend" "uload16" "uload16x4"
      "uload32" "uload32x2" "uload8" "uload8x8" "umax" "umin" "umul_overflow"
      "umulhi" "unarrow" "urem" "ushr" "usub_overflow" "usub_overflow_bin"
      "usub_sat" "uunarrow" "uwiden_high" "uwiden_low" "vall_true"
      "vany_true" "vconst" "vhigh_bits" "x86_cvtt2dq" "x86_pmaddubsw"
      "x86_pmulhrsw" "x86_pshufb")
    "CLIF opcodes.")

  (defconst clif--preamble-keywords
    '("test" "set" "target" "feature" "function")
    "Keywords introducing a top-level item or test directive.")

  (defconst clif--test-commands
    '("alias-analysis" "cat" "compile" "domtree" "inline" "interpret"
      "optimize" "print-cfg" "run" "safepoint" "unwind" "verifier")
    "Subtests namable in a `test' line, from `cranelift/filetests'.")

  (defconst clif--declaration-keywords
    '(;; Stack slot kinds.
      "explicit_slot" "explicit_dynamic_slot"
      ;; Global value kinds.
      "vmctx" "load" "iadd_imm" "symbol" "dyn_scale_target_const"
      ;; Function/global value flags.
      "colocated" "tls" "patchable"
      ;; Signature argument purposes and extensions.
      "normal" "sret" "sarg" "uext" "sext"
      ;; Misc. preamble and instruction annotations.
      "stack_limit" "align" "key" "cold" "stack_map" "default" "context")
    "Keywords appearing in declarations and instruction annotations.")

  (defconst clif--calling-conventions
    '("fast" "tail" "system_v" "windows_fastcall" "apple_aarch64"
      "probestack" "winch" "preserve_all")
    "Calling convention names.")

  (defconst clif--condition-codes
    '(;; Integer condition codes.
      "eq" "ne" "sgt" "sge" "slt" "sle" "ugt" "uge" "ult" "ule"
      ;; Float condition codes (the unsigned int codes double as the
      ;; unordered-or-* float codes).
      "ord" "uno" "one" "ueq" "lt" "le" "gt" "ge")
    "`icmp'/`fcmp' condition codes.")

  (defconst clif--memory-flags
    '("notrap" "aligned" "readonly" "little" "big" "can_move")
    "Flags on load/store instructions.")

  (defconst clif--trap-codes
    '("stk_ovf" "heap_oob" "int_ovf" "int_divz" "bad_toint")
    "Well-known trap codes.  User trap codes are spelled `userN'.")

  (defconst clif--atomic-rmw-ops
    '("add" "sub" "and" "nand" "or" "xor" "xchg"
      "umin" "umax" "smin" "smax")
    "Operations accepted by `atomic_rmw'.")

  (defconst clif--entity-prefixes
    '("v" "block" "ss" "dss" "dt" "gv" "mt" "const" "fn" "sig" "u"
      "userextname" "extable" "tag" "region" "ret" "exn")
    "Prefixes of numbered entity references, e.g. the `gv' in `gv3'.")

  (defconst clif--filecheck-directives
    '("check" "sameln" "nextln" "unordered" "regex" "not" "run" "print")
    "FileCheck directives recognized inside comments in filetests."))

;;; Syntax table

(defvar clif-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; `;' comments run to end of line.
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings are multi-line and have no escape character, so `\' is
    ;; ordinary punctuation rather than an escape.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "." table)
    ;; `%' introduces a name, and is part of the symbol it names.
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?_ "_" table)
    ;; Function bodies are braced; signatures, block params and call
    ;; arguments are parenthesized; jump and exception tables are bracketed.
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (dolist (char '(?@ ?# ?: ?, ?= ?+ ?- ?* ?! ?. ?< ?>))
      (modify-syntax-entry char "." table))
    table)
  "Syntax table for `clif-mode'.")

;;; Font lock
;;
;; Order matters: font-lock does not override an earlier match, so the more
;; specific patterns come first.

(defconst clif-font-lock-keywords
  (let ((opcodes (regexp-opt clif--opcodes 'symbols))
        (preamble (regexp-opt clif--preamble-keywords 'symbols))
        (test-commands (regexp-opt clif--test-commands 'symbols))
        (declarations (regexp-opt clif--declaration-keywords 'symbols))
        (call-convs (regexp-opt clif--calling-conventions 'symbols))
        (cond-codes (regexp-opt clif--condition-codes 'symbols))
        (mem-flags (regexp-opt clif--memory-flags 'symbols))
        (trap-codes (regexp-opt clif--trap-codes 'symbols))
        (atomic-ops (regexp-opt clif--atomic-rmw-ops 'symbols))
        (entities (regexp-opt clif--entity-prefixes))
        (directives (regexp-opt clif--filecheck-directives)))
    `(
      ;; FileCheck directives in filetest comments.  These override the
      ;; comment face that the syntax table already applied.
      (,(concat "^[ \t]*;[ \t]*" directives ":")
       0 font-lock-preprocessor-face t)

      ;; The test preamble: `test <subtest> [options]', `target <isa> [flags]',
      ;; `set <name>=<value>' and `feature "<name>"'.
      (,(concat "^[ \t]*\\(test\\)[ \t]+" test-commands)
       (1 font-lock-keyword-face)
       (2 font-lock-builtin-face))
      ("^[ \t]*\\(target\\)[ \t]+\\(.*\\)$"
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face))
      ("^[ \t]*\\(set\\)\\_>"
       (1 font-lock-keyword-face)
       ;; Anchored: highlight each `name' in the `name=value' pairs that follow.
       ("\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)=" nil nil
        (1 font-lock-variable-name-face)))

      ;; `function %name' and the `%name' operand of `symbol'/`fn0 = ...'.
      (,(concat preamble "\\(?:[ \t]+\\(%\\(?:\\sw\\|\\s_\\)*\\)\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ("%\\(?:\\sw\\|\\s_\\)*" . font-lock-function-name-face)

      ;; A block label, which the printer puts at column zero.
      ("^[ \t]*\\(block[0-9]+\\)\\_>" 1 font-lock-function-name-face)

      ;; Types: i8..i128, f16..f128, and their SIMD vector forms.
      ("\\_<[if]\\(?:8\\|16\\|32\\|64\\|128\\)\\(?:x[0-9]+\\)?\\_>"
       . font-lock-type-face)

      ;; Numbered entity references: v0, block3, ss1, gv2, extable0, ...
      (,(concat "\\_<" entities "[0-9]+\\_>") . font-lock-variable-name-face)

      ;; Source locations (@00c7) and hex sequences (#89af).
      ("@[0-9a-fA-F]+\\_>" . font-lock-constant-face)
      ("#[0-9a-fA-F]+\\_>" . font-lock-constant-face)

      (,opcodes . font-lock-keyword-face)
      (,declarations . font-lock-keyword-face)
      (,call-convs . font-lock-builtin-face)
      (,cond-codes . font-lock-builtin-face)
      (,mem-flags . font-lock-builtin-face)
      (,atomic-ops . font-lock-builtin-face)
      (,trap-codes . font-lock-constant-face)
      ("\\_<user[0-9]+\\_>" . font-lock-constant-face)

      ;; Numeric immediates, including hex floats and NaN payloads.
      ("\\_<s?NaN\\(?::0x[0-9a-fA-F_]+\\)?\\_>" . font-lock-constant-face)
      ("[-+]?\\_<Inf\\_>" . font-lock-constant-face)
      ("[-+]?\\_<0x[0-9a-fA-F_]+\\(?:\\.[0-9a-fA-F_]*\\)?\\(?:[pP][-+]?[0-9]+\\)?"
       . font-lock-constant-face)
      ("[-+]?\\_<[0-9][0-9_]*\\(?:\\.[0-9_]*\\)?" . font-lock-constant-face)))
  "Font lock keywords for `clif-mode'.")

;;; Indentation

(defun clif--desired-indentation ()
  "Return the column this line should be indented to."
  (save-excursion
    (back-to-indentation)
    (cond
     ;; Leave comments where they are.  Filetests deliberately put `; check:'
     ;; directives at column zero inside function bodies.
     ((looking-at-p ";") (current-column))
     ;; Block labels and the closing brace sit at the left margin.
     ((looking-at-p "block[0-9]+\\_>") 0)
     ((looking-at-p "}") 0)
     ;; Everything else inside a function body is indented one step; anything
     ;; outside one is top level.
     ((> (car (syntax-ppss)) 0) clif-indent-offset)
     (t 0))))

(defun clif-indent-line ()
  "Indent the current line of CLIF."
  (interactive)
  (let ((indent (clif--desired-indentation))
        (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (indent-line-to indent)
    (when (> offset 0)
      (goto-char (+ (point) offset)))))

;;; Mode

;;;###autoload
(define-derived-mode clif-mode prog-mode "CLIF"
  "Major mode for editing Cranelift IR (CLIF) files.

\\{clif-mode-map}"
  :syntax-table clif-mode-syntax-table
  (setq-local font-lock-defaults '(clif-font-lock-keywords))
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local indent-line-function #'clif-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width clif-indent-offset)
  (setq-local imenu-generic-expression
              '((nil "^function[ \t]+\\(%\\(?:\\sw\\|\\s_\\)*\\)" 1))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.clif\\'" . clif-mode))

(provide 'my-clif)

;;; my-clif.el ends here
