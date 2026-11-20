;;; shen-el-overwrite.el --- Native replacements for kernel functions  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Ethan Hawk
;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:

;; The kernel defines everything above the KL primitives in Shen.  Some of
;; those definitions have direct Emacs Lisp equivalents that are much
;; faster; the porting guide calls these "peephole optimisations" and
;; places them in an overwrite file.  Each replacement keeps the behaviour
;; of the kernel definition it replaces.  This file is loaded after the
;; compiled kernel.

;;; Code:

(require 'shen-el-primitives)

(defvar shen/*property-vector*)
(defvar shen/*hush*)
(defvar shen/*stoutput*)
(defvar shen/*stinput*)

;;;; Dictionaries: hash tables under Shen equality

(defun shen/shen.dict (size)
  (if (and (numberp size) (>= size 1))
      (shen-elisp-make-dict size)
    (shen-elisp-simple-error
     (format "invalid initial dict size: %s" (shen-elisp-str size)))))

(defun shen/shen.dict? (x) (shen-elisp-boolean (hash-table-p x)))

(defun shen/shen.dict-count (dict) (hash-table-count dict))

(defun shen/shen.dict-> (dict key value)
  (puthash key value dict)
  value)

(defun shen/shen.<-dict (dict key)
  (let ((value (gethash key dict 'shen-elisp--missing)))
    (if (eq value 'shen-elisp--missing)
        (shen-elisp-simple-error
         (concat "value " (shen/shen.app key " not found in dict\n" 'shen.a)))
      value)))

(defun shen/shen.dict-rm (dict key)
  (remhash key dict)
  key)

(defun shen/shen.dict-fold (f dict init)
  (let ((acc init))
    (maphash (lambda (k v)
               (setq acc (funcall (funcall (funcall (shen-elisp-callable f) k) v) acc)))
             dict)
    acc))

(defun shen/shen.dict-keys (dict) (hash-table-keys dict))

(defun shen/shen.dict-values (dict) (hash-table-values dict))

;;;; Property lists

(defun shen/get (key property dict)
  (let ((entry (gethash key dict 'shen-elisp--missing)))
    (if (eq entry 'shen-elisp--missing)
        (shen-elisp-simple-error
         (shen/shen.app key (concat " has no attributes: "
                                    (shen/shen.app property "\n" 'shen.s))
                        'shen.a))
      (let ((pair (shen-elisp-assoc property entry)))
        (if pair
            (cdr pair)
          (shen-elisp-simple-error
           (concat "attribute "
                   (shen/shen.app property
                                  (concat " not found for "
                                          (shen/shen.app key "\n" 'shen.s))
                                  'shen.s))))))))

(defun shen/put (key property value dict)
  (puthash key (shen/shen.assoc-set property value (gethash key dict nil)) dict)
  value)

(defun shen/unput (key property dict)
  (puthash key (shen/shen.assoc-rm property (gethash key dict nil)) dict)
  key)

;;;; Predicates

(defun shen/element? (x list) (shen-elisp-boolean (shen-elisp-element-p x list)))
(defun shen/symbol? (x) (shen-elisp-boolean (shen-elisp-symbol-p x)))
(defun shen/variable? (x) (shen-elisp-boolean (shen-elisp-variable-p x)))
(defun shen/boolean? (x) (shen-elisp-boolean (shen-elisp-boolean-p x)))
(defun shen/integer? (x) (shen-elisp-boolean (shen-elisp-integer-p x)))
(defun shen/shen.pvar? (x) (shen-elisp-boolean (shen-elisp-pvar-p x)))
(defun shen/tuple? (x) (shen-elisp-boolean (shen-elisp-tuple-p x)))
(defun shen/bound? (x) (shen-elisp-boolean (shen-elisp-bound-p x)))
(defun shen/shen.+string? (x)
  (shen-elisp-boolean (and (stringp x) (> (length x) 0))))

;;;; Vectors and tuples

(defun shen/@p (x y) (vector 'shen.tuple x y))

(defun shen/shen.make-prolog-variable (ticket) (vector 'shen.pvar ticket))

(defun shen/vector (n)
  (let ((v (shen-elisp-absvector (1+ n))))
    (aset v 0 n)
    v))

(defun shen/<-vector (vector n)
  (if (eql n 0)
      (shen-elisp-simple-error "cannot access 0th element of a vector\n")
    (let ((x (aref vector n)))
      (if (eq x 'shen.fail!)
          (shen-elisp-simple-error "vector element not found\n")
        x))))

(defun shen/vector-> (vector n x)
  (if (eql n 0)
      (shen-elisp-simple-error "cannot access 0th element of a vector\n")
    (aset vector n x)
    vector))

(defun shen/shen.lazyderef (x bindings)
  (let (v)
    (while (and (shen-elisp-pvar-p x)
                (not (eq (setq v (aref bindings (aref x 1))) 'shen.-null-)))
      (setq x v))
    x))

;;;; Strings and I/O

(defun shen/shen.str->bytes (string)
  (append string nil))

(defun shen/shen.bytes->string (bytes)
  (concat bytes))

(defun shen/explode (x)
  (mapcar #'string (shen/shen.app x "" 'shen.a)))

(defun shen/read-file-as-bytelist (path)
  (append (shen-elisp-read-file-bytes (shen-elisp-resolve-path path)) nil))

(defun shen/read-file-as-string (path)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents (shen-elisp-resolve-path path)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun shen/shen.my-read-byte (stream)
  (shen-elisp-read-code stream))

(defun shen/pr (string stream)
  (if (and (eq shen/*hush* 'true)
           (or (eq stream shen/*stoutput*) (eq stream shen/*stinput*)))
      string
    (shen-elisp-write-string string stream)))

(defun shen/shen.sysfunc? (symbol)
  (shen-elisp-boolean
   (and (symbolp symbol) symbol
        (or (shen-elisp--elisp-symbol symbol)
            (shen-elisp-element-p
             symbol (shen/get 'shen 'shen.external-symbols shen/*property-vector*))))))

;; `elisp.F' calls are host calls of unknown arity: leave them uncurried.
(defvar shen-elisp--kernel-shen-call? (symbol-function 'shen/shen.shen-call?))

(defun shen/shen.shen-call? (symbol)
  (if (and (symbolp symbol) symbol (shen-elisp--elisp-symbol symbol))
      'true
    (funcall shen-elisp--kernel-shen-call? symbol)))

(provide 'shen-el-overwrite)

;;; shen-el-overwrite.el ends here
