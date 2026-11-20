;;; shen-el.el --- Shen for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Ethan Hawk
;; SPDX-License-Identifier: BSD-3-Clause

;; Author: Ethan Hawk <ethhawk@iu.edu>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, lisp
;; URL: https://github.com/echawk/shen-elisp

;;; Commentary:

;; Shen (https://shenlanguage.org) running natively in Emacs.  The Shen
;; kernel is compiled from KLambda into ordinary Emacs Lisp by
;; `shen-el-compiler.el'; this file loads it, initialises it, and
;; provides the command-line launcher, an interactive REPL (`shen-repl')
;; and an Emacs Lisp API (`shen-elisp-eval-string').

;;; Code:

(require 'cl-lib)
(require 'shen-el-primitives)

(defgroup shen-elisp nil
  "Shen for Emacs Lisp."
  :group 'languages
  :prefix "shen-elisp-")

(defconst shen-elisp-version "0.1.0"
  "Version of the Shen/Elisp port, reported as Shen's `*port*'.")

(defcustom shen-elisp-max-lisp-eval-depth 200000
  "Value of `max-lisp-eval-depth' while Shen code runs.
Shen relies on deep recursion; Emacs's default is far too small."
  :type 'integer)

(defconst shen-elisp-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory of the Shen/Elisp installation.")

(defconst shen-elisp-kernel-files
  '("toplevel" "core" "sys" "dict" "sequent" "yacc" "reader" "prolog"
    "track" "load" "writer" "macros" "declarations" "types" "t-star" "init"
    "extension-features" "extension-expand-dynamic" "extension-launcher"
    "extension-programmable-pattern-matching" "extension-namespaces"
    "extension-type-annotations" "stlib")
  "Compiled kernel files, in load order.")

;;;; Standard streams

(defun shen-elisp--stdin-reader ()
  "Read the next line of standard input in batch mode, or nil at EOF."
  (condition-case nil
      (concat (read-from-minibuffer "") "\n")
    (error nil)))

(defun shen-elisp-make-standard-streams ()
  "Create the standard input, output and error streams for batch use."
  (list (shen-elisp-make-stream :name "stdin" :direction 'in :char t
                                :reader #'shen-elisp--stdin-reader)
        (shen-elisp-make-stream :name "stdout" :direction 'out :char t
                                :writer (lambda (s) (princ s)))
        (shen-elisp-make-stream :name "stderr" :direction 'out :char t
                                :writer (lambda (s)
                                          (princ s #'external-debugging-output)))))

(defun shen-elisp--set-standard-streams (streams)
  (setq shen/*stinput* (nth 0 streams)
        shen/*stoutput* (nth 1 streams)
        shen/*sterror* (nth 2 streams)))

;;;; Boot

(defvar shen-elisp--booted nil)

(defun shen-elisp--os ()
  (pcase system-type
    ('darwin "macOS") ('gnu/linux "Linux") ('windows-nt "Windows")
    ((or 'berkeley-unix 'gnu/kfreebsd) "BSD") (_ (symbol-name system-type))))

(defun shen-elisp--exit (code)
  "Leave Shen with exit status CODE."
  (if noninteractive
      (kill-emacs (if (integerp code) code 0))
    (throw 'shen-elisp-exit code)))

(defun shen/shen-elisp.exit (code) (shen-elisp--exit code))

(defun shen-elisp--initialise-port ()
  "Register the port's own functions with the kernel."
  (shen/update-lambda-table 'shen-elisp.exit 1)
  (shen/declare 'shen-elisp.exit '(number --> unit))
  (shen-elisp-eval-string
   "(defmacro shen-elisp.exit-macro [shen-elisp.exit] -> [shen-elisp.exit 0])"))

(defun shen-elisp-boot ()
  "Load and initialise the Shen kernel, once."
  (unless shen-elisp--booted
    (let ((max-lisp-eval-depth (max max-lisp-eval-depth shen-elisp-max-lisp-eval-depth))
          (compiled (expand-file-name "compiled" shen-elisp-root)))
      (let ((eln (expand-file-name "eln/" shen-elisp-root)))
        (when (and (boundp 'native-comp-eln-load-path) (file-directory-p eln))
          (add-to-list 'native-comp-eln-load-path eln)))
      (add-to-list 'load-path compiled)
      (dolist (file shen-elisp-kernel-files)
        (require (intern (concat "shen-kernel-" file))))
      (require 'shen-el-overwrite)
      (setq shen/*language* "Emacs Lisp"
            shen/*implementation* "GNU Emacs"
            shen/*release* emacs-version
            shen/*port* shen-elisp-version
            shen/*porters* "Ethan Hawk"
            shen/*os* (shen-elisp--os))
      (shen-elisp--set-standard-streams (shen-elisp-make-standard-streams))
      ;; The initialisation sequence the kernel's porting guide prescribes.
      (shen/shen.initialise)
      (shen/shen.x.features.initialise '(shen/elisp))
      (shen/stlib.initialise)
      (shen/shen.x.programmable-pattern-matching.initialise)
      (shen/shen.x.namespaces.initialise)
      (shen/update-lambda-table 'shen.x.type-annotations.annotate-kl 1)
      (shen-elisp--initialise-port)
      (setq shen-elisp--booted t)))
  t)

;;;; Emacs Lisp API

(defun shen-elisp-eval-string (string)
  "Evaluate the Shen code in STRING and return the value of the last form."
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth shen-elisp-max-lisp-eval-depth))
        (result nil))
    (dolist (form (shen/read-from-string string) result)
      (setq result (shen/eval form)))))

(defun shen-elisp-eval (string)
  "Boot Shen if needed, evaluate STRING and return the result."
  (shen-elisp-boot)
  (shen-elisp-eval-string string))

;;;; Command line

(defun shen-elisp-main ()
  "Run the Shen launcher on the remaining command-line arguments."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (when (equal (car args) "--") (pop args))
    (setq max-lisp-eval-depth shen-elisp-max-lisp-eval-depth)
    (shen-elisp-boot)
    (condition-case err
        (let ((result (shen/shen.x.launcher.launch-shen (cons "shen" args))))
          (shen/shen.x.launcher.default-handle-result result)
          (kill-emacs (if (memq (car-safe result) '(error unknown-arguments)) 1 0)))
      (error
       (ignore-errors
         (princ (format "\n!!! FATAL error: %s\nExiting Shen.\n"
                        (shen-elisp-error-message err))
                #'external-debugging-output))
       (kill-emacs 1)))))

;;;; Interactive commands

(defcustom shen-elisp-program (expand-file-name "bin/shen" shen-elisp-root)
  "Launcher used by `shen-repl'."
  :type 'file)

;;;###autoload
(defun shen-repl ()
  "Run a Shen REPL in a comint buffer."
  (interactive)
  (require 'comint)
  (pop-to-buffer (make-comint "shen" shen-elisp-program nil "repl")))

;;;###autoload
(defun shen-elisp-eval-region (start end)
  "Evaluate the Shen code between START and END in this Emacs."
  (interactive "r")
  (let ((value (shen-elisp-eval (buffer-substring-no-properties start end))))
    (message "%s" (shen/shen.app value "" 'shen.s))
    value))

;;;###autoload
(defun shen-elisp-load-file (file)
  "Load the Shen source FILE into this Emacs."
  (interactive "fShen file: ")
  (shen-elisp-boot)
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth shen-elisp-max-lisp-eval-depth)))
    (shen/load (expand-file-name file))))

(provide 'shen-el)

;;; shen-el.el ends here
