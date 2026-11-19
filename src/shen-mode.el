;;; shen-mode.el --- Major mode for editing Shen source files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ethan Hawk
;; SPDX-License-Identifier: BSD-3-Clause

;; Author: Ethan Hawk <ethhawk@iu.edu>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/echawk/shen-elisp

;;; Commentary:

;; A major mode for Shen (https://shenlanguage.org) files, written to
;; the lexical rules of the Shen/Elisp port in this repository
;; (reader grammar in `kernel/sources/reader.shen'):
;;
;;   \\ ...       one-line comment
;;   \\* ... *\\  block comment, nestable
;;   "..."        string; `c#N;' is a control character, there is no
;;                backslash escape
;;   (X ...)      grouping / function application
;;   [X | Y]      list; `|' is the cons separator
;;   @p @s @v ... Shen symbols (pattern vars, state, vectors)
;;   *name*       state variables
;;
;; The mode is self-contained: it never requires `shen-elisp'.  When
;; `shen-elisp' happens to be loaded in the same Emacs (typically
;; booted via `shen-elisp-boot'), the mode transparently upgrades:
;; `shen-completion-at-point' offers every function and state
;; variable defined in the running Shen image (kernel, standard
;; library, and your own definitions) with the arity as an
;; annotation, eldoc shows the arity of the function at point, and
;; the evaluation commands work.
;;
;; `shen-completion-at-point' is a plain `completion-at-point'
;; function, so it works with corfu, company, or built-in minibuffer
;; completion, unmodified.
;;
;; To use:
;;
;;   (use-package shen-mode
;;     :load-path "~/src/shen-el/src"
;;     :commands (shen-mode))
;;
;; which puts files ending in `.shen' into `shen-mode' automatically.

;;; Code:

(defgroup shen-mode nil
  "Major mode for editing Shen source files."
  :group 'languages
  :prefix "shen-")

;;;; Keywords

(defconst shen-mode--top-level-keywords
  '("define" "defcc" "declare" "defprolog" "datatype" "package"
    "defmacro" "def-abstract-type" "defrecord" "defclamp"
    "enable-type-theory" "disable-type-theory" "load" "include"
    "set" "tc" "factorise" "preclude-all-but")
  "Top-level Shen form keywords.")

(defconst shen-mode--arrows
  '("->" "<-" "<--" "<-->")
  "Pattern and rule arrows.")

(defconst shen-mode--booleans '("true" "false")
  "Boolean literals.")

(defconst shen-mode--special-forms
  '("do" "if" "let" "lambda" "freeze" "thaw" "trap-error" "cond"
    "and" "or" "not" "fail" "skip" "error" "simple-error" "type"
    "value" "set" "cons" "hd" "tl" "eval-kl" "eval" "input+")
  "Shen special forms used in expressions.")

(defconst shen-mode--reserved-names
  (append (list "where" "where-")
          shen-mode--top-level-keywords
          shen-mode--booleans
          shen-mode--special-forms
          shen-mode--arrows)
  "Names always offered by completion.")

(defconst shen-mode--definition-keywords
  '("define" "defprolog" "defmacro" "defcc" "defclamp"
    "def-abstract-type" "defrecord")
  "Keywords that introduce a named definition.")

(defconst shen-mode--def-words
  (append shen-mode--definition-keywords
          '("datatype" "package" "declare"))
  "Every keyword that starts a top-level definition form.")

;;;; Syntax table

(defun shen-mode--make-syntax-table ()
  "Build the syntax table for Shen source.

Parens are the secondary list (grouping / application), square
brackets the primary list (with `|' as the cons separator).
Backslash starts a comment: `\\\\' is a one-line comment and
`\\\\*' a nestable block comment (the nestable flag goes on
`*'; backslash itself carries the comment-start flag).  `|' is
punctuation so a cons spanning lines does not glue its elements.
The reader word characters from `misc?' in
`kernel/sources/reader.shen' (`= - * / + _ ? $ ! @ ~ . > < & % ' #
`') are word characters; backslash is not."
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\( "( " tbl)
    (modify-syntax-entry ?\) ")" tbl)
    (modify-syntax-entry ?\[ "(]" tbl)
    (modify-syntax-entry ?\] ")[ " tbl)
    (modify-syntax-entry ?\" "\"" tbl)
    (dolist (c '(?\; ?\| ?\, ?\{ ?\} ?\:))
      (modify-syntax-entry c " " tbl))
    (dolist (c '(?\= ?\- ?\/ ?\+ ?\_ ?\? ?\$ ?\! ?\@ ?\~ ?\. ?\<
                  ?\> ?\& ?\% ?\' ?\# ?\`))
      (modify-syntax-entry c "w" tbl))
    (modify-syntax-entry ?\\ ". 124b" tbl)
    (modify-syntax-entry ?\* "w 23n" tbl)
    (modify-syntax-entry ?\n "> b" tbl)
    tbl))

;;;; Font lock

(defun shen-mode--font-lock-keywords ()
  "Return the `font-lock-keywords' value for the current buffer."
  (list
   ;; Top-level keywords.
   (list (concat "\\_<" (regexp-opt shen-mode--top-level-keywords) "\\_>")
         0 'font-lock-keyword-face)
   ;; The name on a definition line, with or without a leading paren.
   (list (concat "^[\\t ()]*\\(?:"
                 (regexp-opt shen-mode--definition-keywords)
                 "\\)\\s-+\\(\\w+\\)\\_>")
         1 'font-lock-function-name-face)
   ;; Pattern arrows and the prolog clause arrow.
   (list (concat "\\_<\\(" (regexp-opt shen-mode--arrows) "\\)_>")
         1 'font-lock-constant-face)
   ;; `where' and `where-' clauses.
   (list "\\_<where-?\\_>" 0 'font-lock-keyword-face)
   ;; Booleans.
   (list (concat "\\_<\\(" (regexp-opt shen-mode--booleans) "\\)_>")
         1 'font-lock-constant-face)
   ;; Special forms.
   (list (concat "\\_<\\(" (regexp-opt shen-mode--special-forms) "\\)_>")
         1 'font-lock-builtin-face)
   ;; State variables: *name*.
   (list "\\*\\w+\\*" 0 'font-lock-variable-face)
   ;; Shen symbols: a leading @, a word, an optional trailing !
   (list "@\\w+!?" 0 'font-lock-variable-face)
   ;; The `|' cons separator, dimmed.
   (list "\\s-|\\s-" 0 'font-lock-constant-face)))

;;;; Indentation

(defun shen-mode-indent-line ()
  "Indent the current line as Shen source.

A line that is just `]' (optionally preceded by `|') lines up
with the opening `['.  A line inside a list indents one level
deeper than the line containing the opening bracket.  A line that
starts a comment copies the indentation of the next non-blank
line.  Lines inside a string or a block comment are left alone."
  (save-excursion
    (beginning-of-line)
    (back-to-indentation)
    (let ((ppss (syntax-ppss)))
      (cond
       ((nth 10 ppss)
        nil)                                 ; inside a string
       ((nth 4 ppss)
        nil)                                 ; inside a comment
       ((looking-at-p "\\\\")
        ;; Line that starts a comment: copy the next line's indent.
        (save-excursion
          (forward-line 1)
          (while (and (not (eobp)) (looking-at-p "^\\s-*$"))
            (forward-line 1))
          (unless (eobp)
            (indent-to (current-indentation)))))
       (t
        (let ((closing (looking-at-p "\\s-|\\s-*\\]\\|\\s-\\]")))
          (indent-to
           (cond
            (closing
             (nth 1 ppss))                    ; align with the opener
            ((nth 1 ppss)
             (+ (save-excursion
                  (goto-char (nth 1 ppss))
                  (current-indentation))
                shen-mode-indent))
            (t 0)))))))))

;;;; Completion and eldoc

(defun shen-mode--image-names ()
  "Return an alist of (NAME . ANNOTATION) from the running Shen image.

Every key of the Shen property vector is a defined name, in its
canonical spelling as the writer prints it.  Kernel internals
(names starting with `shen.') are excluded.  The annotation is
the arity, when known and non-negative.  Returns an empty list
if `shen-elisp' is not loaded or not booted."
  (let ((pv (when (boundp 'shen/*property-vector*)
              (symbol-value 'shen/*property-vector*)))
        names)
    (when (hash-table-p pv)
      (maphash (lambda (k v)
                 (let ((name (and (symbolp k) (symbol-name k))))
                   (when (and name (not (string-prefix-p "shen." name))
                              (not (string= name "<>")))
                     (let* ((arity (cdr (assoc 'arity v)))
                            (ann (when (and (integerp arity)
                                            (>= arity 0))
                                   (format "(%d)" arity))))
                       (push (cons name ann) names)))))
               pv))
    names))

(defun shen-mode--buffer-names ()
  "Return an alist of the names defined in the current buffer."
  (let (names)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^[\\t ()]*\\(?:"
                      (regexp-opt
                       (append shen-mode--definition-keywords
                               '("datatype" "package")))
                      "\\)\\s-+\\(\\w+\\)")
              nil t)
        (push (cons (match-string 1) nil) names))
      (goto-char (point-min)))
    names))

(defun shen-mode--all-names ()
  "Return the full alist of (NAME . ANNOTATION) for this buffer."
  (let ((seen (make-hash-table :test #'equal)) result)
    (dolist (entry (append (shen-mode--image-names)
                           (shen-mode--buffer-names)
                           (mapcar (lambda (n) (cons n "keyword"))
                                   shen-mode--reserved-names)))
      (unless (gethash (car entry) seen)
        (puthash (car entry) t seen)
        (push entry result)))
    (nreverse result)))

(defun shen-completion-at-point (&optional pt)
  "Complete a Shen name at or before point PT.

A `completion-at-point' function: completes the (partial) name
ending at PT (or `point' when PT is nil) from the names defined
in the current buffer, the reserved keywords, and, when
`shen-elisp' is loaded in this Emacs, the names defined in the
running Shen image (with the arity as an annotation).  Suitable
for use with corfu, company, or the built-in minibuffer
completion."
  (save-excursion
    (when pt
      (goto-char pt))
    (let ((pos (point))
          (ppss (syntax-ppss)))
      (and (not (bobp))
           (not (nth 4 ppss))                ; not in a comment
           (not (nth 3 ppss))                ; not in a string
           (memq (char-syntax (char-before pos)) '(?w ?_))
           (save-excursion
             (skip-syntax-backward "w")
             (when (/= (point) pos)
               (let* ((entries (shen-mode--all-names))
                      (annotations (make-hash-table :test #'equal)))
                 (dolist (entry entries)
                   (puthash (car entry) (cdr entry) annotations))
                 (list (point) pos (mapcar #'car entries)
                       :annotation-function
                       (lambda (name)
                         (let ((ann (gethash name annotations)))
                           (when ann (concat " " ann))))))))))))

(defun shen-eldoc (name)
  "Eldoc: the arity of the Shen name NAME, from the running image."
  (let ((pv (when (boundp 'shen/*property-vector*)
              (symbol-value 'shen/*property-vector*))))
    (when (hash-table-p pv)
      (let* ((k (intern-soft name))
             (arity (when k (cdr (assoc 'arity (gethash k pv nil))))))
        (when (and (integerp arity) (>= arity 0))
          (format "(%d)" arity))))))

;;;; Interactive commands

(defun shen-eval-region (start end)
  "Evaluate the Shen code between START and END in this Emacs."
  (interactive "r")
  (if (fboundp #'shen-elisp-eval-region)
      (shen-elisp-eval-region start end)
    (user-error
     "shen-elisp is not loaded; add it to load-path and run `shen-elisp-boot'")))

(defun shen-eval-defun ()
  "Evaluate the Shen definition at or before point."
  (interactive)
  (let ((start (save-excursion
                 (re-search-backward
                  (concat "^[\\t ]*\\(?:([\\t ]*\\)?\\(?:"
                          (regexp-opt shen-mode--def-words)
                          "\\)\\s-+\\w") nil t))))
    (unless start
      (user-error "No Shen definition found"))
    (let ((end (save-excursion
                 (goto-char start)
                 (if (looking-at-p "[\\t ]*(")
                     (scan-sexps (point) 1)
                   (or (save-excursion
                         (forward-line 1)
                         (re-search-forward "^[^\\t \n]" nil t))
                       (point-max))))))
      (unless end
        (user-error "Shen definition has unmatched parentheses"))
      (shen-eval-region start end))))

(defun shen-send-region (start end)
  "Send the region between START and END to a `shen-repl' buffer."
  (interactive "r")
  (require 'comint)
  (let ((text (buffer-substring-no-properties start end))
        (repl (get-buffer "*shen*")))
    (unless (and repl (get-buffer-process repl))
      (user-error "No Shen REPL is running; start one with `shen-repl'"))
    (with-current-buffer repl
      (comint-send-string (get-buffer-process repl) (concat text "\n")))))

;;;; Mode setup

(defcustom shen-mode-indent 2
  "The amount of additional indentation per Shen list level."
  :type 'integer
  :group 'shen-mode)

;;;###autoload
(define-derived-mode shen-mode prog-mode "Shen"
  "Major mode for editing Shen source files.

\\{shen-mode-map}"
  (set-syntax-table (shen-mode--make-syntax-table))
  (setq-local comment-start "\\\\")
  (setq-local comment-end "")
  (setq-local comment-start-skip "^\\\\\\\\")
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'shen-mode-indent-line)
  (setq-local font-lock-defaults
              '(shen-mode--font-lock-keywords))
  (setq-local eldoc-documentation-function
              (lambda (_symbol _overlay)
                (let ((w (thing-at-point 'word)))
                  (when w (shen-eldoc w)))))
  (unless (memq #'shen-completion-at-point completion-at-point-functions)
    (setq-local completion-at-point-functions
                (cons #'shen-completion-at-point
                      completion-at-point-functions))))

(define-key shen-mode-map (kbd "C-c C-e") #'shen-eval-region)
(define-key shen-mode-map (kbd "C-c C-d") #'shen-eval-defun)
(define-key shen-mode-map (kbd "C-c C-s") #'shen-send-region)

(add-to-list 'auto-mode-alist '("\\.shen$" . shen-mode))

(provide 'shen-mode)

;;; shen-mode.el ends here
