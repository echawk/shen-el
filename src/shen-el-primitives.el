;;; shen-el-primitives.el --- KLambda primitives for Shen/Elisp  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Ethan Hawk
;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:

;; The KL primitive instruction set, in the representation used by the
;; compiler in `shen-el-compiler.el':
;;
;;   Shen            Emacs Lisp
;;   ----            ----------
;;   symbol          symbol (Shen `nil' is `shen--nil')
;;   true/false      the symbols `true' and `false'
;;   []              nil
;;   cons            cons
;;   number          fixnum, bignum or float
;;   string          string
;;   absvector       vector
;;   function        closure or byte-code function
;;   exception       `shen-elisp-exception' record
;;   stream          `shen-elisp-stream' record
;;   dictionary      hash table using `shen-elisp-equal-p'
;;
;; Small primitives are `defsubst's so the byte compiler opens them at
;; their call sites in compiled kernel code.  Every primitive is also
;; available as an ordinary function `shen/NAME', for first-class use.

;;; Code:

(require 'cl-lib)
(require 'shen-el-compiler)

(define-error 'shen-error "Shen error")

;;;; Errors

(cl-defstruct (shen-elisp-exception
               (:constructor shen-elisp--make-exception (condition))
               (:copier nil))
  "A Shen exception, wrapping an Emacs Lisp error condition."
  condition)

(defsubst shen-elisp-simple-error (message)
  "Signal a Shen error with the string MESSAGE."
  (signal 'shen-error (list message)))

(defun shen-elisp-exception (condition)
  "Return the Shen exception for the Emacs Lisp error CONDITION."
  (shen-elisp--make-exception condition))

(defun shen-elisp-error-message (condition)
  "Return the message of the Emacs Lisp error CONDITION, as Shen shows it."
  (pcase condition
    (`(shen-error ,(and message (pred stringp))) message)
    (`(void-variable ,(and var (pred symbolp)))
     (let ((name (symbol-name var)))
       (if (string-prefix-p "shen/" name)
           (format "variable %s has no value\n" (substring name 5))
         (error-message-string condition))))
    (`(void-function ,(and fn (pred symbolp)))
     (let ((name (symbol-name fn)))
       (if (string-prefix-p "shen/" name)
           (format "%s is not defined\n" (substring name 5))
         (error-message-string condition))))
    (_ (error-message-string condition))))

(defun shen-elisp-error-to-string (exception)
  "Return the message of the Shen EXCEPTION."
  (if (shen-elisp-exception-p exception)
      (shen-elisp-error-message (shen-elisp-exception-condition exception))
    (shen-elisp-simple-error
     (format "%s is not an exception\n" (shen-elisp-str exception)))))

;;;; Booleans and equality

(defsubst shen-elisp-boolean (x)
  "Return the Shen boolean for the Emacs Lisp boolean X."
  (if x 'true 'false))

(defsubst shen-elisp-boolean-p (x)
  (or (eq x 'true) (eq x 'false)))

(defsubst shen-elisp-num-eq (x n)
  "Shen equality of X and the number N."
  (and (numberp x) (= x n)))

(defsubst shen-elisp-equal-p (x y)
  "Non-nil if X and Y are equal in the sense of Shen's `='."
  (or (eq x y) (shen-elisp--equal x y)))

(defun shen-elisp--equal (x y)
  "Shen equality of X and Y, once they are known not to be `eq'."
  (cond
   ((numberp x) (and (numberp y) (= x y)))
   ((stringp x) (and (stringp y) (string= x y)))
   ((consp x)
    (and (consp y)
         (let ((same t))
           (while (and same (consp x) (consp y))
             (setq same (shen-elisp-equal-p (car x) (car y))
                   x (cdr x)
                   y (cdr y)))
           (and same (shen-elisp-equal-p x y)))))
   ((vectorp x)
    (and (vectorp y)
         (= (length x) (length y))
         (let ((i 0) (n (length x)) (same t))
           (while (and same (< i n))
             (setq same (shen-elisp-equal-p (aref x i) (aref y i))
                   i (1+ i)))
           same)))
   (t nil)))

(defun shen-elisp--hash (x &optional depth)
  "Hash X consistently with `shen-elisp-equal-p'."
  (cond
   ((symbolp x) (sxhash-eq x))
   ((integerp x) (sxhash-eql x))
   ((floatp x)
    (if (and (= x (ffloor x)) (< (abs x) 1.0e18))
        (sxhash-eql (truncate x))
      (sxhash-eql x)))
   ((stringp x) (sxhash-equal x))
   ((and (consp x) (< (or depth 0) 4))
    (logxor (* 31 (shen-elisp--hash (car x) (1+ (or depth 0))))
            (shen-elisp--hash (cdr x) (1+ (or depth 0)))))
   ((and (vectorp x) (< (or depth 0) 4) (> (length x) 0))
    (logxor (length x) (shen-elisp--hash (aref x 0) (1+ (or depth 0)))))
   ((vectorp x) (length x))
   (t 0)))

(define-hash-table-test 'shen-elisp-equal #'shen-elisp-equal-p #'shen-elisp--hash)

(defun shen-elisp-element-p (x list)
  "Non-nil if X is a member of LIST under Shen equality."
  (if (and (symbolp x) x)
      (memq x list)
    (let ((found nil))
      (while (and list (not found))
        (unless (consp list)
          (shen-elisp-simple-error "attempt to find an element in a non-list\n"))
        (setq found (shen-elisp-equal-p x (car list))
              list (cdr list)))
      found)))

(defun shen-elisp-assoc (key alist)
  "Return the first pair in ALIST whose car is KEY under Shen equality."
  (if (and (symbolp key) key)
      (assq key alist)
    (cl-assoc key alist :test #'shen-elisp-equal-p)))

;;;; Type predicates

(defsubst shen-elisp-integer-p (x)
  (or (integerp x)
      (and (floatp x) (= x (ffloor x)) (not (isnan x)))))

(defsubst shen-elisp-pvar-p (x)
  (and (vectorp x) (> (length x) 0) (eq (aref x 0) 'shen.pvar)))

(defsubst shen-elisp-tuple-p (x)
  (and (vectorp x) (> (length x) 0) (eq (aref x 0) 'shen.tuple)))

(defconst shen-elisp--misc-chars "=-*/+_?$!@~.><&%'#`"
  "Non-alphanumeric characters allowed in Shen symbols (`shen.misc?').")

(defun shen-elisp--alpha-char-p (c)
  (or (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?Z))
      (seq-contains-p shen-elisp--misc-chars c)))

(defun shen-elisp--analyse-name (name first-p)
  "Non-nil if NAME is non-empty, its first char satisfies FIRST-P and the
rest are Shen alphanumerics."
  (and (> (length name) 0)
       (funcall first-p (aref name 0))
       (cl-loop for i from 1 below (length name)
                for c = (aref name i)
                always (or (shen-elisp--alpha-char-p c)
                           (and (>= c ?0) (<= c ?9))))))

(defun shen-elisp-symbol-p (x)
  "Native `symbol?': non-nil if X is a Shen symbol."
  (and (symbolp x) x
       (not (eq x 'true)) (not (eq x 'false))
       (let ((cached (get x 'shen-elisp-symbol-p)))
         (when (null cached)
           (setq cached
                 (if (or (memq x '({ } \: \; \,))
                         (shen-elisp--analyse-name (shen-elisp-symbol-name x)
                                                   #'shen-elisp--alpha-char-p))
                     'yes 'no))
           (put x 'shen-elisp-symbol-p cached))
         (eq cached 'yes))))

(defun shen-elisp-variable-p (x)
  "Native `variable?': non-nil if X is a Shen variable."
  (and (symbolp x) x
       (let ((cached (get x 'shen-elisp-variable-p)))
         (when (null cached)
           (setq cached
                 (if (shen-elisp--analyse-name
                      (shen-elisp-symbol-name x)
                      (lambda (c) (and (>= c ?A) (<= c ?Z))))
                     'yes 'no))
           (put x 'shen-elisp-variable-p cached))
         (eq cached 'yes))))

;;;; Numbers

(defun shen-elisp-divide (x y)
  "Shen division: exact when both are integers and Y divides X."
  (cond ((and (integerp x) (integerp y))
         (if (zerop (% x y)) (/ x y) (/ (float x) y)))
        ((and (numberp y) (zerop y)) (signal 'arith-error (list x y)))
        (t (/ (float x) y))))

;;;; Strings and symbols

(defsubst shen-elisp-pos (string n)
  (string (aref string n)))

(defsubst shen-elisp-tlstr (string)
  (if (and (stringp string) (> (length string) 0))
      (substring string 1)
    (shen-elisp-simple-error "tlstr requires a non-empty string\n")))

(defsubst shen-elisp-string->n (string)
  (if (and (stringp string) (> (length string) 0))
      (aref string 0)
    (shen-elisp-simple-error "string->n requires a non-empty string\n")))

(defun shen-elisp-number->string (n)
  (if (floatp n)
      (let ((s (number-to-string n)))
        (if (string-suffix-p ".0" s) s s))
    (number-to-string n)))

(defun shen-elisp-str (x)
  "Return the string representation of the Shen atom X."
  (cond
   ((null x)
    (shen-elisp-simple-error
     "[] is not an atom in Shen; str cannot convert it to a string.\n"))
   ((symbolp x) (shen-elisp-symbol-name x))
   ((numberp x) (shen-elisp-number->string x))
   ((stringp x) (concat "\"" x "\""))
   ((shen-elisp-stream-p x)
    (format "#<stream %s>" (or (shen-elisp-stream-name x) "")))
   ((shen-elisp-exception-p x)
    (format "#<exception %s>"
            (shen-elisp-error-message (shen-elisp-exception-condition x))))
   ((functionp x) "#<function>")
   ((hash-table-p x) "#<dictionary>")
   ((consp x)
    (shen-elisp-simple-error
     (format "%S is not an atom; str cannot convert it to a string.\n" x)))
   (t (format "%S" x))))

;;;; Globals

(defun shen-elisp-value (symbol)
  "Return the value of the Shen global SYMBOL."
  (let ((name (shen-elisp-name symbol)))
    (if (boundp name)
        (symbol-value name)
      (shen-elisp-simple-error
       (format "variable %s has no value\n" (shen-elisp-str symbol))))))

(defun shen-elisp-set (symbol value)
  "Set the Shen global SYMBOL to VALUE."
  (unless (and (symbolp symbol) symbol)
    (shen-elisp-simple-error
     (format "%s is not a symbol\n" (shen-elisp-str symbol))))
  (set (shen-elisp-name symbol) value))

(defun shen-elisp-bound-p (symbol)
  (and (symbolp symbol) symbol (boundp (shen-elisp-name symbol))))

;;;; Vectors

(defconst shen-elisp-max-absvector-size (expt 2 26)
  "Largest absvector `absvector' will allocate.")

(defsubst shen-elisp-fail () 'shen.fail!)

(defun shen-elisp-absvector (n)
  "Return a new absvector of N elements, each the fail object."
  (if (and (natnump n) (<= n shen-elisp-max-absvector-size))
      (make-vector n 'shen.fail!)
    (shen-elisp-simple-error
     (format "absvector size %s out of range (0..%d)\n"
             n shen-elisp-max-absvector-size))))

(defsubst shen-elisp-address-set (vector n value)
  (aset vector n value)
  vector)

(defun shen-elisp-address-ref-or (vector n missing)
  (if (and (vectorp vector) (natnump n) (< n (length vector)))
      (aref vector n)
    missing))

(defun shen-elisp-vector-ref-or (vector n missing)
  (if (and (vectorp vector) (natnump n) (> n 0) (< n (length vector)))
      (let ((x (aref vector n)))
        (if (eq x 'shen.fail!) missing x))
    missing))

;;;; Dictionaries

(defun shen-elisp-make-dict (&optional size)
  (make-hash-table :test 'shen-elisp-equal :size (max 16 (or size 16))))

(defun shen-elisp-dict-ref-or (dict key missing)
  (if (hash-table-p dict) (gethash key dict missing) missing))

(defun shen-elisp-get-or (key property dict missing)
  "Value of KEY's PROPERTY in DICT, or MISSING."
  (let ((entry (shen-elisp-dict-ref-or dict key missing)))
    (if (eq entry missing)
        missing
      (let ((pair (shen-elisp-assoc property entry)))
        (if pair (cdr pair) missing)))))

;;;; Functions

(defun shen-elisp-curry (function arity &optional args)
  "Return FUNCTION as a curried closure expecting ARITY more arguments.
ARGS, in reverse order, have already been supplied."
  (if (<= arity 0)
      (apply function (reverse args))
    (lambda (x) (shen-elisp-curry function (1- arity) (cons x args)))))

(defun shen-elisp--resolve (symbol)
  "Return a function to call when the Shen symbol SYMBOL is applied."
  (let ((name (and symbol (shen-elisp-name symbol))))
    (unless (and name (fboundp name))
      (shen-elisp-simple-error
       (format "%s is not a function\n" (shen-elisp-str symbol))))
    (let ((arity (func-arity name)))
      (if (and (eql (car arity) (cdr arity)) (> (car arity) 1))
          (shen-elisp-curry name (car arity))
        name))))

(defsubst shen-elisp-callable (f)
  "Return F as an Emacs Lisp function; a Shen symbol names a function."
  (if (symbolp f) (shen-elisp--resolve f) f))

;;;; Streams

(cl-defstruct (shen-elisp-stream
               (:constructor shen-elisp-make-stream)
               (:copier nil))
  "A Shen stream.
DIRECTION is `in' or `out'.  CHAR is non-nil for character streams.
An input stream reads TEXT from POS, refilling it with READER, a function
returning the next chunk of input or nil at end of file.  An output
stream passes strings to WRITER, or collects CHUNKS until it is closed."
  name direction char (text "") (pos 0) reader writer chunks closed)

(defun shen-elisp--stream-fill (stream)
  "Ensure STREAM has unread input; return nil at end of file."
  (or (< (shen-elisp-stream-pos stream) (length (shen-elisp-stream-text stream)))
      (let* ((reader (shen-elisp-stream-reader stream))
             (more (and reader (funcall reader))))
        (when more
          (setf (shen-elisp-stream-text stream) more
                (shen-elisp-stream-pos stream) 0)
          (> (length more) 0)))))

(defun shen-elisp-read-code (stream)
  "Read the next byte or character code from STREAM; -1 at end of file."
  (when (shen-elisp-stream-closed stream)
    (shen-elisp-simple-error "attempt to read from a closed stream\n"))
  (if (shen-elisp--stream-fill stream)
      (let ((pos (shen-elisp-stream-pos stream)))
        (setf (shen-elisp-stream-pos stream) (1+ pos))
        (aref (shen-elisp-stream-text stream) pos))
    -1))

(defun shen-elisp-read-byte (stream)
  (shen-elisp-read-code stream))

(defun shen-elisp-read-unit-string (stream)
  (let ((c (shen-elisp-read-code stream)))
    (if (eql c -1) "" (string c))))

(defun shen-elisp-write-string (string stream)
  "Write STRING to STREAM and return it."
  (when (shen-elisp-stream-closed stream)
    (shen-elisp-simple-error "attempt to write to a closed stream\n"))
  (let ((writer (shen-elisp-stream-writer stream)))
    (if writer
        (funcall writer string)
      (push string (shen-elisp-stream-chunks stream))))
  string)

(defun shen-elisp-write-byte (byte stream)
  (if (shen-elisp-stream-writer stream)
      (shen-elisp-write-string (string byte) stream)
    (push (unibyte-string byte) (shen-elisp-stream-chunks stream)))
  byte)

(defvar shen/*home-directory*)

(defun shen-elisp-resolve-path (path)
  "Resolve PATH against Shen's `*home-directory*'."
  (if (or (file-name-absolute-p path) (not (boundp 'shen/*home-directory*)))
      path
    (concat shen/*home-directory* path)))

(defun shen-elisp-read-file-bytes (path)
  "Return the contents of PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun shen-elisp-open (path direction)
  "Open the file PATH for DIRECTION, `in' or `out'."
  (let ((file (shen-elisp-resolve-path path)))
    (pcase direction
      ('in
       (unless (file-readable-p file)
         (shen-elisp-simple-error (format "cannot open %s\n" file)))
       (shen-elisp-make-stream :name file :direction 'in
                               :text (shen-elisp-read-file-bytes file)))
      ('out
       (unless (file-writable-p file)
         (shen-elisp-simple-error (format "cannot open %s for writing\n" file)))
       (shen-elisp-make-stream :name file :direction 'out))
      (_ (shen-elisp-simple-error "invalid stream direction\n")))))

(defun shen-elisp-close (stream)
  "Close STREAM, writing a file stream's contents to its file."
  (unless (shen-elisp-stream-closed stream)
    (when (and (eq (shen-elisp-stream-direction stream) 'out)
               (null (shen-elisp-stream-writer stream)))
      (let ((coding-system-for-write 'no-conversion)
            (chunks (reverse (shen-elisp-stream-chunks stream))))
        (with-temp-file (shen-elisp-stream-name stream)
          (set-buffer-multibyte nil)
          (dolist (chunk chunks)
            (insert (if (multibyte-string-p chunk)
                        (encode-coding-string chunk 'utf-8)
                      chunk))))))
    (setf (shen-elisp-stream-closed stream) t
          (shen-elisp-stream-chunks stream) nil))
  nil)

;;;; Time

(defun shen-elisp-get-time (kind)
  (pcase kind
    ('run (if (fboundp 'current-cpu-time)
              (let ((ct (current-cpu-time)))
                (/ (float (car ct)) (cdr ct)))
            (float-time)))
    ('real (float-time))
    ('unix (truncate (float-time)))
    (_ (shen-elisp-simple-error
        (format "get-time does not understand the parameter %s\n"
                (shen-elisp-str kind))))))

;;;; Evaluation

(defvar shen-elisp-compile-definitions t
  "Non-nil means `eval-kl' byte-compiles the functions it defines.")

(defun shen-elisp-install-defun (compiled)
  "Evaluate the compiled `defun' form COMPILED, byte-compiling it."
  (let ((name (nth 1 compiled)))
    (eval compiled t)
    (when shen-elisp-compile-definitions
      (let ((byte-compile-warnings nil)
            (inhibit-message t)
            (message-log-max nil))
        (ignore-errors (byte-compile name))))
    name))

(defun shen-elisp-eval-kl (form)
  "Compile and evaluate the KL FORM."
  (if (eq (car-safe form) 'defun)
      (progn
        (shen-elisp-install-defun (shen-elisp-compile-toplevel form))
        (nth 1 form))
    (eval (shen-elisp-compile-toplevel form) t)))

;;;; First-class primitives

(defmacro shen-elisp--define-primitives (&rest specs)
  "Define `shen/NAME' for each (NAME ARGS BODY) in SPECS."
  `(progn
     ,@(mapcar (pcase-lambda (`(,name ,args ,body))
                 `(defun ,(shen-elisp-name name) ,args ,body))
               specs)))

(shen-elisp--define-primitives
 (+ (x y) (+ x y))
 (- (x y) (- x y))
 (* (x y) (* x y))
 (/ (x y) (shen-elisp-divide x y))
 (> (x y) (shen-elisp-boolean (> x y)))
 (< (x y) (shen-elisp-boolean (< x y)))
 (>= (x y) (shen-elisp-boolean (>= x y)))
 (<= (x y) (shen-elisp-boolean (<= x y)))
 (= (x y) (shen-elisp-boolean (shen-elisp-equal-p x y)))
 (number? (x) (shen-elisp-boolean (numberp x)))
 (string? (x) (shen-elisp-boolean (stringp x)))
 (cons? (x) (shen-elisp-boolean (consp x)))
 (absvector? (x) (shen-elisp-boolean (vectorp x)))
 (cons (x y) (cons x y))
 (hd (x) (car x))
 (tl (x) (cdr x))
 (str (x) (shen-elisp-str x))
 (pos (s n) (shen-elisp-pos s n))
 (tlstr (s) (shen-elisp-tlstr s))
 (cn (a b) (concat a b))
 (n->string (n) (string n))
 (string->n (s) (shen-elisp-string->n s))
 (intern (s) (shen-elisp-intern s))
 (set (s v) (shen-elisp-set s v))
 (value (s) (shen-elisp-value s))
 (simple-error (s) (shen-elisp-simple-error s))
 (error-to-string (e) (shen-elisp-error-to-string e))
 (absvector (n) (shen-elisp-absvector n))
 (<-address (v n) (aref v n))
 (address-> (v n x) (shen-elisp-address-set v n x))
 (eval-kl (x) (shen-elisp-eval-kl x))
 (get-time (k) (shen-elisp-get-time k))
 (type (x _) x)
 (open (p d) (shen-elisp-open p d))
 (close (s) (shen-elisp-close s))
 (read-byte (s) (shen-elisp-read-byte s))
 (write-byte (b s) (shen-elisp-write-byte b s))
 (char-stream? (s) (shen-elisp-boolean (and (shen-elisp-stream-p s)
                                            (shen-elisp-stream-char s))))
 (read-unit-string (s) (shen-elisp-read-unit-string s))
 (write-string (x s) (shen-elisp-write-string x s))
 (if (c a b) (if (eq c 'true) a b))
 (and (a b) (shen-elisp-boolean (and (eq a 'true) (eq b 'true))))
 (or (a b) (shen-elisp-boolean (or (eq a 'true) (eq b 'true))))
 (do (_ b) b)
 (thaw (f) (funcall f))
 (freeze (x) (lambda () x)))

(provide 'shen-el-primitives)

;;; shen-el-primitives.el ends here
