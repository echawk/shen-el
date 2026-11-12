;;; build.el --- Build Shen/Elisp from KLambda  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Ethan Hawk
;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:

;; Run from the repository root:
;;
;;   emacs -Q --batch -l scripts/build.el -f shen-elisp-build-translate
;;   emacs -Q --batch -l scripts/build.el -f shen-elisp-build-compile
;;
;; `translate' compiles klambda/*.kl to compiled/*.el.  `compile'
;; byte-compiles src/ and compiled/, and native-compiles them when Emacs
;; supports it and SHEN_ELISP_NATIVE is not "0".

;;; Code:

(defconst shen-elisp-build-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path (expand-file-name "src" shen-elisp-build-root))
(add-to-list 'load-path (expand-file-name "compiled" shen-elisp-build-root))

(require 'bytecomp)
(require 'comp nil t)
(require 'shen-elisp-compiler)

(setq max-lisp-eval-depth 20000)

(defconst shen-elisp-build-kernel-files
  '("toplevel" "core" "sys" "dict" "sequent" "yacc" "reader" "prolog"
    "track" "load" "writer" "macros" "declarations" "types" "t-star" "init"
    "extension-features" "extension-expand-dynamic" "extension-launcher"
    "extension-programmable-pattern-matching" "extension-namespaces"
    "extension-type-annotations" "stlib")
  "KL files of the kernel and bundled libraries, in load order.")

(defun shen-elisp-build--path (dir name ext)
  (expand-file-name (concat name ext) (expand-file-name dir shen-elisp-build-root)))

(defun shen-elisp-build-translate ()
  "Translate every kernel KL file into compiled/."
  (let* ((files shen-elisp-build-kernel-files)
         (forms (mapcar (lambda (f)
                          (shen-elisp-read-kl-file
                           (shen-elisp-build--path "klambda" f ".kl")))
                        files))
         (arities (make-hash-table :test 'eq))
         (shen-elisp-compiling-kernel t)
         (shen-elisp-compiler-arities arities))
    (dolist (fs forms) (shen-elisp-collect-arities fs arities))
    (make-directory (expand-file-name "compiled" shen-elisp-build-root) t)
    (dolist (f files)
      (let ((start (float-time)))
        (shen-elisp-compile-kl-file
         (shen-elisp-build--path "klambda" f ".kl")
         (shen-elisp-build--path "compiled" (concat "shen-kernel-" f) ".el")
         (intern (concat "shen-kernel-" f)))
        (message "Translated %s.kl (%.2fs)" f (- (float-time) start))))))

(defun shen-elisp-build--sources ()
  (append
   (mapcar (lambda (f) (shen-elisp-build--path "src" f ".el"))
           '("shen-elisp-compiler" "shen-elisp-primitives"))
   (mapcar (lambda (f) (shen-elisp-build--path "compiled" (concat "shen-kernel-" f) ".el"))
           shen-elisp-build-kernel-files)
   (mapcar (lambda (f) (shen-elisp-build--path "src" f ".el"))
           '("shen-elisp-overwrite" "shen-elisp"))))

(defun shen-elisp-build-compile ()
  "Byte-compile, and if possible native-compile, the sources."
  (let ((native (and (fboundp 'native-comp-available-p)
                     (native-comp-available-p)
                     (not (equal (getenv "SHEN_ELISP_NATIVE") "0"))))
        (byte-compile-warnings '(not free-vars unresolved docstrings lexical
                                     make-local suspicious))
        (failed nil))
    (when native
      (setq native-comp-eln-load-path
            (cons (expand-file-name "eln/" shen-elisp-build-root)
                  native-comp-eln-load-path)))
    (dolist (file (shen-elisp-build--sources))
      (let ((start (float-time)))
        (unless (byte-compile-file file)
          (push file failed))
        (when native
          (condition-case err
              (let ((native-comp-speed 2)
                    (native-comp-debug 0)
                    (native-comp-verbose 0))
                (native-compile file))
            (error (message "Native compilation of %s failed: %S" file err))))
        (load (file-name-sans-extension file) nil t)
        (message "Compiled %s (%.2fs)" (file-name-nondirectory file)
                 (- (float-time) start))))
    (when failed
      (error "Byte compilation failed: %S" failed))))

;;; build.el ends here
