;;; shen-elisp-compiler.el --- KLambda to Emacs Lisp compiler  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Ethan Hawk
;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:

;; This file reads KLambda (KL) and translates it into ordinary Emacs Lisp.
;; The output depends only on the small runtime in
;; `shen-elisp-primitives.el'; it contains no interpreter or trampoline.
;;
;; Naming
;;
;;   A KL function F becomes the Emacs Lisp function `shen/F', and a KL
;;   global *G* becomes the variable `shen/*G*'.  Emacs Lisp is a Lisp-2, so
;;   both live on the same symbol without clashing, and the prefix keeps the
;;   kernel's `append', `reverse', `eval', `load' etc. away from Emacs' own.
;;   KL symbols used as data are plain Emacs Lisp symbols.  The one exception
;;   is the Shen symbol `nil', which would read as the empty list; it is
;;   represented by `shen--nil' (see `shen-elisp-intern').
;;
;; Booleans
;;
;;   Shen's `true' and `false' are the symbols `true' and `false'.  Tests are
;;   compiled in a separate "test" context which yields a native Emacs Lisp
;;   boolean, so `(if (cons? X) ...)' becomes `(if (consp X) ...)' rather than
;;   comparing a reified Shen boolean.
;;
;; Tail calls
;;
;;   Emacs Lisp has no tail-call elimination.  Self tail calls, which carry
;;   almost all of the kernel's iteration, are compiled into a `while' loop;
;;   other calls use the host stack (see `shen-elisp-max-lisp-eval-depth').
;;
;; Applications
;;
;;   A call to a known function with all of its arguments is a direct call.
;;   Partial applications become curried closures, over-applications call
;;   the result with `funcall'.  Applying a variable uses `funcall' through
;;   `shen-elisp-callable', which resolves a Shen symbol to its function.

;;; Code:

(require 'cl-lib)

;;;; Symbols

(defconst shen-elisp-nil-symbol 'shen--nil
  "Emacs Lisp symbol that represents the Shen symbol `nil'.")

(defsubst shen-elisp-intern (string)
  "Return the Shen symbol named STRING."
  (if (string= string "nil") shen-elisp-nil-symbol (intern string)))

(defsubst shen-elisp-symbol-name (symbol)
  "Return the Shen name of SYMBOL."
  (if (eq symbol shen-elisp-nil-symbol) "nil" (symbol-name symbol)))

(defun shen-elisp-name (symbol)
  "Return the Emacs Lisp symbol naming the Shen function or global SYMBOL.
The mapping is cached on SYMBOL's property list."
  (or (get symbol 'shen-elisp-name)
      (put symbol 'shen-elisp-name
           (intern (concat "shen/" (shen-elisp-symbol-name symbol))))))

;;;; Reader

(defconst shen-elisp--number-regexp
  (rx bos (? (any "+-"))
      (or (: (+ digit) (? "." (+ digit))) (: "." (+ digit)))
      (? (any "eE") (? (any "+-")) (+ digit))
      eos)
  "Tokens matching this regexp are KL numbers.")

(defun shen-elisp--read-token (token)
  (if (string-match-p shen-elisp--number-regexp token)
      (string-to-number token)
    (shen-elisp-intern token)))

(defun shen-elisp-read-kl-buffer ()
  "Read every KL form from point to the end of the current buffer."
  (let ((stack (list nil)))
    (while (progn (skip-chars-forward " \t\n\r\f") (not (eobp)))
      (let ((c (char-after)))
        (cond
         ((eq c ?\()
          (forward-char)
          (push nil stack))
         ((eq c ?\))
          (forward-char)
          (when (null (cdr stack))
            (error "Unbalanced `)' in KL source at %d" (point)))
          (let ((form (nreverse (pop stack))))
            (push form (car stack))))
         ((eq c ?\")
          (let ((start (1+ (point))))
            (forward-char)
            (unless (search-forward "\"" nil t)
              (error "Unterminated string in KL source at %d" start))
            (push (buffer-substring-no-properties start (1- (point)))
                  (car stack))))
         (t
          (let ((start (point)))
            (skip-chars-forward "^ \t\n\r\f()\"")
            (push (shen-elisp--read-token
                   (buffer-substring-no-properties start (point)))
                  (car stack)))))))
    (when (cdr stack)
      (error "Unbalanced `(' in KL source"))
    (nreverse (car stack))))

(defun shen-elisp-read-kl-string (string)
  "Return the list of KL forms in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (shen-elisp-read-kl-buffer)))

(defun shen-elisp-read-kl-file (file)
  "Return the list of KL forms in FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (goto-char (point-min))
    (shen-elisp-read-kl-buffer)))

;;;; Compiler state

(defvar shen-elisp-compiler-arities nil
  "Hash table from KL function names to arities, or nil.
Bound while compiling kernel files, where the functions being referenced
are not yet defined in the running image.")

(defvar shen-elisp-compiling-kernel nil
  "Non-nil while compiling the kernel and its bundled libraries.
Enables rewrites that assume kernel code's use of `trap-error' only
guards a missing value (see `shen-elisp--compile-trap-error').")

(defvar shen-elisp--self nil
  "Name of the KL function whose body is being compiled.")
(defvar shen-elisp--self-params nil
  "Emacs Lisp variables that a self tail call assigns.")
(defvar shen-elisp--self-recurs nil
  "Set to non-nil when a self tail call has been emitted.")
(defvar shen-elisp--counter 0)

(defconst shen-elisp--recur 'shen-elisp--recur
  "Value of a self tail call; tells the enclosing loop to go round again.")

(defconst shen-elisp-primitive-arities
  '((+ . 2) (- . 2) (* . 2) (/ . 2) (> . 2) (< . 2) (>= . 2) (<= . 2) (= . 2)
    (number? . 1) (string? . 1) (cons? . 1) (absvector? . 1)
    (cons . 2) (hd . 1) (tl . 1)
    (str . 1) (pos . 2) (tlstr . 1) (cn . 2) (n->string . 1) (string->n . 1)
    (intern . 1) (set . 2) (value . 1) (simple-error . 1) (error-to-string . 1)
    (absvector . 1) (<-address . 2) (address-> . 3)
    (eval-kl . 1) (get-time . 1) (type . 2)
    (open . 2) (close . 1) (read-byte . 1) (write-byte . 2)
    (char-stream? . 1) (read-unit-string . 1) (write-string . 2)
    (if . 3) (and . 2) (or . 2) (freeze . 1) (thaw . 1)
    (trap-error . 2) (lambda . 2) (let . 3) (do . 2))
  "Arities of KL primitives.")

(defun shen-elisp-function-arity (name)
  "Return the arity of the KL function NAME, or -1 when it is unknown."
  (or (and shen-elisp-compiler-arities
           (gethash name shen-elisp-compiler-arities))
      (alist-get name shen-elisp-primitive-arities)
      (and (fboundp 'shen/arity)
           (boundp 'shen/*property-vector*)
           (let ((arity (shen/arity name)))
             (and (integerp arity) (>= arity 0) arity)))
      (let ((fn (shen-elisp-name name)))
        (and (fboundp fn)
             (let ((arity (func-arity fn)))
               (and (eql (car arity) (cdr arity)) (car arity)))))
      -1))

;;;; Variables

(defun shen-elisp--fresh (base)
  (intern (format "%s--%d" base (cl-incf shen-elisp--counter))))

(defun shen-elisp--local-name (symbol)
  "Return an Emacs Lisp variable name for the KL variable SYMBOL.
KL variables keep their names unless Emacs Lisp cannot bind them
lexically (constants, keywords and special variables)."
  (if (or (memq symbol '(t nil))
          (keywordp symbol)
          (special-variable-p symbol)
          (string-prefix-p "shen-elisp--" (symbol-name symbol)))
      (shen-elisp--fresh (if (keywordp symbol)
                             (substring (symbol-name symbol) 1)
                           symbol))
    symbol))

(defsubst shen-elisp--local (symbol env)
  (cdr (assq symbol env)))

(defun shen-elisp--mentions-p (form symbol)
  "Non-nil if SYMBOL occurs anywhere in the Emacs Lisp FORM."
  (cond ((eq form symbol) t)
        ((consp form)
         (or (shen-elisp--mentions-p (car form) symbol)
             (shen-elisp--mentions-p (cdr form) symbol)))
        (t nil)))

(defun shen-elisp--kl-closure-p (form)
  "Non-nil if the KL FORM creates a closure."
  (and (consp form)
       (or (memq (car form) '(lambda freeze))
           (cl-some #'shen-elisp--kl-closure-p (cdr form)))))

;;;; Literals

(defun shen-elisp--quote (object)
  "Return a form evaluating to OBJECT."
  (if (or (and (symbolp object) (not (memq object '(nil t)))
               (not (keywordp object)))
          (consp object))
      (list 'quote object)
    object))

(defun shen-elisp--constant-p (form)
  "Non-nil if the compiled FORM is a constant."
  (or (not (or (symbolp form) (consp form)))
      (memq form '(nil t))
      (keywordp form)
      (eq (car-safe form) 'quote)))

(defun shen-elisp--constant-value (form)
  (if (eq (car-safe form) 'quote) (cadr form) form))

;;;; Expressions

(defun shen-elisp-compile-expression (expr &optional env tail)
  "Compile the KL expression EXPR into Emacs Lisp.
ENV maps KL variables to Emacs Lisp variables.  TAIL is non-nil when EXPR
is in tail position of the function being compiled."
  (cond
   ((symbolp expr)
    (cond ((null expr) nil)
          ((shen-elisp--local expr env))
          (t (shen-elisp--quote expr))))
   ((atom expr) expr)
   (t (shen-elisp--compile-form (car expr) (cdr expr) expr env tail))))

(defun shen-elisp--compile-args (args env)
  (mapcar (lambda (arg) (shen-elisp-compile-expression arg env)) args))

(defun shen-elisp--boolean (test)
  "Turn the Emacs Lisp boolean form TEST into a Shen boolean."
  (pcase test
    ('t ''true)
    ('nil ''false)
    (`(not ,inner) `(if ,inner 'false 'true))
    (_ `(if ,test 'true 'false))))

(defun shen-elisp--compile-form (op args expr env tail)
  (pcase expr
    (`(let ,var ,value ,body)
     (shen-elisp--compile-let var value body env tail))
    ;; Closure bodies are never in tail position of the enclosing function.
    (`(lambda ,var ,body)
     (let ((name (shen-elisp--local-name var)))
       `(lambda (,name)
          ,(shen-elisp-compile-expression body (cons (cons var name) env)))))
    (`(freeze ,body)
     `(lambda () ,(shen-elisp-compile-expression body env)))
    (`(thaw ,thunk)
     `(funcall ,(shen-elisp-compile-expression thunk env)))
    (`(if ,test ,then ,else)
     (shen-elisp--make-if (shen-elisp-compile-test test env)
                          (shen-elisp-compile-expression then env tail)
                          (shen-elisp-compile-expression else env tail)))
    (`(cond . ,clauses)
     (shen-elisp--compile-cond clauses env tail))
    (`(and ,a ,b)
     (shen-elisp--make-if (shen-elisp-compile-test a env)
                          (shen-elisp-compile-expression b env tail)
                          ''false))
    (`(or ,a ,b)
     (shen-elisp--make-if (shen-elisp-compile-test a env)
                          ''true
                          (shen-elisp-compile-expression b env tail)))
    (`(do ,_ ,_) (shen-elisp--compile-do expr env tail))
    ((and `(cons ,_ ,_) (guard (not (shen-elisp--local 'cons env))))
     (shen-elisp--compile-cons expr env))
    (`(trap-error ,body ,handler)
     (shen-elisp--compile-trap-error body handler env tail))
    (`(type ,value ,_) (shen-elisp-compile-expression value env tail))
    (`(value ,(and sym (pred symbolp)))
     (if (or (null sym) (shen-elisp--local sym env))
         `(shen-elisp-value ,(shen-elisp-compile-expression sym env))
       (shen-elisp-name sym)))
    (`(set ,(and sym (pred symbolp)) ,value)
     (if (or (null sym) (shen-elisp--local sym env))
         `(shen-elisp-set ,(shen-elisp-compile-expression sym env)
                          ,(shen-elisp-compile-expression value env))
       `(setq ,(shen-elisp-name sym)
              ,(shen-elisp-compile-expression value env))))
    (`(defun ,name ,params ,body)
     (shen-elisp-compile-defun name params body))
    (`(elisp. ,(and code (pred stringp)))
     (car (read-from-string code)))
    (_
     (cond
      ((and (symbolp op) op (not (shen-elisp--local op env)))
       (shen-elisp--compile-call op args env tail))
      (t
       (shen-elisp--nest-funcall
        `(shen-elisp-callable ,(shen-elisp-compile-expression op env))
        (shen-elisp--compile-args args env)))))))

(defun shen-elisp--compile-do (expr env tail)
  "Compile a chain of `do' forms without recursing on its length."
  (let ((forms nil))
    (while (and (eq (car-safe expr) 'do) (= (length expr) 3))
      (push (nth 1 expr) forms)
      (setq expr (nth 2 expr)))
    (let ((body (list (shen-elisp-compile-expression expr env tail))))
      (dolist (form forms)
        (let ((c (shen-elisp-compile-expression form env)))
          (cond ((or (shen-elisp--constant-p c) (symbolp c)))
                ((eq (car-safe c) 'progn) (setq body (append (cdr c) body)))
                (t (push c body)))))
      (if (cdr body) `(progn ,@body) (car body)))))

(defun shen-elisp--compile-cons (expr env)
  "Compile a chain of `cons' forms without recursing on its length."
  (let ((elements nil))
    (while (and (eq (car-safe expr) 'cons) (= (length expr) 3))
      (push (shen-elisp-compile-expression (nth 1 expr) env) elements)
      (setq expr (nth 2 expr)))
    (let ((tail (shen-elisp-compile-expression expr env)))
      (setq elements (nreverse elements))
      (cond
       ((and (shen-elisp--constant-p tail)
             (cl-every #'shen-elisp--constant-p elements))
        (shen-elisp--quote (nconc (mapcar #'shen-elisp--constant-value elements)
                                  (shen-elisp--constant-value tail))))
       ((null tail) `(list ,@elements))
       ((null (cdr elements)) `(cons ,(car elements) ,tail))
       (t `(nconc (list ,@elements) ,tail))))))

(defun shen-elisp--make-if (test then else)
  (cond ((eq test t) then)
        ((null test) else)
        ((and (equal then ''true) (equal else ''false))
         (shen-elisp--boolean test))
        ((and (eq (car-safe else) 'if) (= (length else) 4))
         `(cond (,test ,then) ,@(shen-elisp--cond-clauses else)))
        ((eq (car-safe else) 'cond)
         `(cond (,test ,then) ,@(cdr else)))
        (t `(if ,test ,then ,else))))

(defun shen-elisp--cond-clauses (form)
  (pcase form
    (`(if ,test ,then ,else)
     (cons (list test then)
           (pcase else
             ((and `(if . ,_) (guard (= (length else) 4)))
              (shen-elisp--cond-clauses else))
             (`(cond . ,clauses) clauses)
             (_ (list (list t else))))))))

(defun shen-elisp--compile-let (var value body env tail)
  (let* ((name (shen-elisp--local-name var))
         (init (shen-elisp-compile-expression value env))
         (inner (shen-elisp-compile-expression body (cons (cons var name) env) tail)))
    (pcase inner
      (`(let ,bindings . ,rest)
       `(let* ((,name ,init) ,@bindings) ,@rest))
      (`(let* ,bindings . ,rest)
       `(let* ((,name ,init) ,@bindings) ,@rest))
      (_ `(let ((,name ,init)) ,inner)))))

(defun shen-elisp--compile-cond (clauses env tail)
  (let ((compiled nil) (exhaustive nil))
    (while (and clauses (not exhaustive))
      (pcase-let ((`(,test ,body) (car clauses)))
        (let ((ctest (shen-elisp-compile-test test env)))
          (cond ((null ctest))
                (t (push (list ctest (shen-elisp-compile-expression body env tail))
                         compiled)
                   (when (eq ctest t) (setq exhaustive t))))))
      (setq clauses (cdr clauses)))
    (unless exhaustive
      (push '(t (shen-elisp-simple-error "condition failure")) compiled))
    (setq compiled (nreverse compiled))
    (if (eq (caar compiled) t)
        (cadar compiled)
      `(cond ,@compiled))))

(defun shen-elisp--handler-parts (handler)
  "If HANDLER is a KL lambda, return (VAR . BODY)."
  (pcase handler
    (`(lambda ,(and var (pred symbolp)) ,body) (cons var body))))

(defun shen-elisp--kl-mentions-p (form symbol)
  (cond ((eq form symbol) t)
        ((consp form) (cl-some (lambda (f) (shen-elisp--kl-mentions-p f symbol)) form))))

(defun shen-elisp--compile-trap-error (body handler env tail)
  (let* ((parts (shen-elisp--handler-parts handler))
         (var (car parts))
         (hbody (cdr parts))
         (uses-error (and parts (shen-elisp--kl-mentions-p hbody var))))
    ;; The protected body is not in tail position: a loop iteration must
    ;; not escape the handler.  The handler body runs after unwinding.
    (or
     ;; In kernel code, `(trap-error (F ...) (lambda E Default))' only
     ;; guards a missing value; test for it instead of raising and catching.
     (and parts (not uses-error)
          (shen-elisp--compile-value-or
           body env (lambda () (shen-elisp-compile-expression hbody env tail))))
     (if parts
         (let* ((name (if uses-error (shen-elisp--local-name var) nil))
                (henv (if uses-error (cons (cons var name) env) env))
                (hform (shen-elisp-compile-expression hbody henv tail)))
           `(condition-case ,name
                ,(shen-elisp-compile-expression body env)
              (error
               ,@(if uses-error
                     `((setq ,name (shen-elisp-exception ,name)) ,hform)
                   (list hform)))))
       (let ((err (shen-elisp--fresh "err")))
         `(condition-case ,err
              ,(shen-elisp-compile-expression body env)
            (error (funcall (shen-elisp-callable
                             ,(shen-elisp-compile-expression handler env))
                            (shen-elisp-exception ,err)))))))))

(defun shen-elisp--compile-value-or (body env default-fn)
  "Compile a kernel lookup BODY falling back to DEFAULT-FN's form, or nil."
  (when (and shen-elisp-compiling-kernel
             (pcase body
               (`(value ,(pred symbolp)) t)
               (`(,(or 'get '<-vector '<-address 'shen.<-dict) . ,_) t)))
   (let ((default (funcall default-fn)))
    (let ((missing ''shen-elisp--missing))
      (pcase body
        (`(value ,(and sym (pred symbolp)))
         (if (and sym (not (shen-elisp--local sym env)))
             (let ((gname (shen-elisp-name sym)))
               `(if (boundp ',gname) ,gname ,default))
           (let ((s (shen-elisp--fresh "sym")))
             `(let ((,s (shen-elisp-name ,(shen-elisp-compile-expression sym env))))
                (if (boundp ,s) (symbol-value ,s) ,default)))))
        (`(,(and op (or 'get '<-vector '<-address 'shen.<-dict)) . ,args)
         (let ((v (shen-elisp--fresh "v"))
               (fn (pcase op
                     ('get 'shen-elisp-get-or)
                     ('<-vector 'shen-elisp-vector-ref-or)
                     ('<-address 'shen-elisp-address-ref-or)
                     ('shen.<-dict 'shen-elisp-dict-ref-or))))
           `(let ((,v (,fn ,@(shen-elisp--compile-args args env) ,missing)))
              (if (eq ,v ,missing) ,default ,v)))))))))

;;;; Tests
(defconst shen-elisp--predicates
  '((cons? 1 . consp)
    (number? 1 . numberp)
    (string? 1 . stringp)
    (absvector? 1 . vectorp)
    (empty? 1 . null)
    (integer? 1 . shen-elisp-integer-p)
    (boolean? 1 . shen-elisp-boolean-p)
    (symbol? 1 . shen-elisp-symbol-p)
    (variable? 1 . shen-elisp-variable-p)
    (shen.pvar? 1 . shen-elisp-pvar-p)
    (tuple? 1 . shen-elisp-tuple-p)
    (element? 2 . shen-elisp-element-p)
    (> 2 . >) (< 2 . <) (>= 2 . >=) (<= 2 . <=))
  "KL predicates and the Emacs Lisp predicates used in test position.
Each entry is (NAME ARITY . ELISP-PREDICATE).")


(defun shen-elisp-compile-test (expr &optional env)
  "Compile the KL expression EXPR to an Emacs Lisp boolean form."
  (pcase expr
    ('true t)
    ('false nil)
    (`(and ,a ,b)
     (shen-elisp--make-and (shen-elisp-compile-test a env)
                           (shen-elisp-compile-test b env)))
    (`(or ,a ,b)
     (let ((ta (shen-elisp-compile-test a env))
           (tb (shen-elisp-compile-test b env)))
       (cond ((eq ta t) t) ((null ta) tb)
             (t `(or ,@(if (eq (car-safe ta) 'or) (cdr ta) (list ta))
                     ,@(if (eq (car-safe tb) 'or) (cdr tb) (list tb)))))))
    (`(not ,a)
     (let ((ta (shen-elisp-compile-test a env)))
       (cond ((eq ta t) nil) ((null ta) t) (t `(not ,ta)))))
    (`(if ,test ,then ,else)
     (shen-elisp--make-if (shen-elisp-compile-test test env)
                          (shen-elisp-compile-test then env)
                          (shen-elisp-compile-test else env)))
    (`(let ,var ,value ,body)
     (let* ((name (shen-elisp--local-name var)))
       `(let ((,name ,(shen-elisp-compile-expression value env)))
          ,(shen-elisp-compile-test body (cons (cons var name) env)))))
    (`(= ,a ,b) (shen-elisp--compile-equal a b env))
    (`(,(and op (pred symbolp)) . ,args)
     (let ((pred (and (not (shen-elisp--local op env))
                      (alist-get op shen-elisp--predicates))))
       (if (and pred (= (length args) (car pred)))
           (cons (cdr pred) (shen-elisp--compile-args args env))
         `(eq ,(shen-elisp-compile-expression expr env) 'true))))
    (_ `(eq ,(shen-elisp-compile-expression expr env) 'true))))

(defun shen-elisp--make-and (a b)
  (cond ((null a) nil) ((eq a t) b) ((eq b t) a)
        (t `(and ,@(if (eq (car-safe a) 'and) (cdr a) (list a))
                 ,@(if (eq (car-safe b) 'and) (cdr b) (list b))))))


(defun shen-elisp--compile-equal (a b env)
  (let ((ca (shen-elisp-compile-expression a env))
        (cb (shen-elisp-compile-expression b env)))
    (when (and (shen-elisp--constant-p ca) (not (shen-elisp--constant-p cb)))
      (cl-rotatef ca cb))
    (if (not (shen-elisp--constant-p cb))
        `(shen-elisp-equal-p ,ca ,cb)
      (let ((k (shen-elisp--constant-value cb)))
        (cond ((null k) `(null ,ca))
              ((symbolp k) `(eq ,ca ,cb))
              ((stringp k) `(equal ,ca ,cb))
              ((fixnump k) `(shen-elisp-num-eq ,ca ,cb))
              (t `(shen-elisp-equal-p ,ca ,cb)))))))

;;;; Calls

(defconst shen-elisp--open-coded
  '((cons . cons) (hd . car) (tl . cdr)
    (+ . +) (- . -) (* . *) (/ . shen-elisp-divide)
    (cn . concat) (tlstr . shen-elisp-tlstr) (pos . shen-elisp-pos)
    (n->string . string) (string->n . shen-elisp-string->n)
    (str . shen-elisp-str) (intern . shen-elisp-intern)
    (absvector . shen-elisp-absvector)
    (<-address . aref) (address-> . shen-elisp-address-set)
    (error-to-string . shen-elisp-error-to-string)
    (simple-error . shen-elisp-simple-error)
    (eval-kl . shen-elisp-eval-kl)
    (value . shen-elisp-value) (set . shen-elisp-set)
    (fail . shen-elisp-fail)
    (length . length) (reverse . reverse))
  "KL functions translated to direct calls of Emacs Lisp functions.
Each entry is (KL-NAME . ELISP-FUNCTION).  Arities must agree.")

(defun shen-elisp--compile-call (op args env tail)
  (let* ((arity (if (and (eq op shen-elisp--self) shen-elisp--self-params)
                    (length shen-elisp--self-params)
                  (shen-elisp-function-arity op)))
         (nargs (length args)))
    (cond
     ;; Partial application.
     ((and (> arity nargs) (> nargs -1) (>= arity 0))
      (shen-elisp--partial op arity (shen-elisp--compile-args args env)))
     ;; Over-application: call with ARITY arguments, apply the rest.
     ((and (>= arity 0) (< arity nargs))
      (shen-elisp--nest-funcall
       (shen-elisp--compile-call op (cl-subseq args 0 arity) env nil)
       (shen-elisp--compile-args (nthcdr arity args) env)))
     ;; Predicates in value position.
     ((let ((pred (alist-get op shen-elisp--predicates)))
        (and pred (= nargs (car pred))))
      (shen-elisp--boolean (shen-elisp-compile-test (cons op args) env)))
     ((eq op '=)
      (shen-elisp--boolean (shen-elisp--compile-equal (car args) (cadr args) env)))
     ;; Self tail call: loop instead of recursing.
     ((and tail (eq op shen-elisp--self) shen-elisp--self-params
           (= nargs (length shen-elisp--self-params)))
      (setq shen-elisp--self-recurs t)
      (shen-elisp--recur-form (shen-elisp--compile-args args env)))
     (t
      (let ((cargs (shen-elisp--compile-args args env)))
        (shen-elisp--optimise-call op cargs))))))

(defun shen-elisp--cxr-path (form)
  "If FORM is a `car'/`cdr' chain, return (PATH . ARG), e.g. (\"ad\" . x)."
  (let ((name (and (consp form) (symbolp (car form)) (= (length form) 2)
                   (symbol-name (car form)))))
    (and name (string-match-p "\\`c[ad]\\{1,4\\}r\\'" name)
         (cons (substring name 1 -1) (cadr form)))))

(defun shen-elisp--cxr (letter arg)
  "Return (cXr ARG) for LETTER ?a or ?d, folding into `cadr' and friends."
  (let ((inner (shen-elisp--cxr-path arg)))
    (if (and inner (< (length (car inner)) 4))
        (list (intern (format "c%c%sr" letter (car inner))) (cdr inner))
      (list (if (eq letter ?a) 'car 'cdr) arg))))

(defun shen-elisp--optimise-call (op cargs)
  (pcase (cons op cargs)
    (`(hd ,x) (shen-elisp--cxr ?a x))
    (`(tl ,x) (shen-elisp--cxr ?d x))
    (`(+ 1 ,x) `(1+ ,x))
    (`(+ ,x 1) `(1+ ,x))
    (`(- ,x 1) `(1- ,x))
    (`(cons ,x nil) `(list ,x))
    (`(cons ,x (list . ,rest)) `(list ,x ,@rest))
    ((and `(cons ,x ,y)
          (guard (and (shen-elisp--constant-p x) (shen-elisp--constant-p y))))
     (shen-elisp--quote (cons (shen-elisp--constant-value x)
                              (shen-elisp--constant-value y))))
    (`(thaw ,x) `(funcall ,x))
    (`(type ,x ,_) x)
    (`(elisp. ,code) (car (read-from-string code)))
    (_
     (let ((open (alist-get op shen-elisp--open-coded)))
       (cond
        (open (cons open cargs))
        ((shen-elisp--elisp-symbol op) (cons (shen-elisp--elisp-symbol op) cargs))
        (t (cons (shen-elisp-name op) cargs)))))))

(defun shen-elisp--elisp-symbol (op)
  "If OP is an `elisp.NAME' interop symbol, return the symbol NAME."
  (let ((name (symbol-name op)))
    (and (> (length name) 6)
         (string-prefix-p "elisp." name)
         (intern (substring name 6)))))

(defun shen-elisp--recur-form (cargs)
  "Assign CARGS to the loop variables and request another iteration."
  (let* ((params shen-elisp--self-params)
         (pairs (cl-loop for p in params for a in cargs
                         unless (eq p a) collect (cons p a)))
         ;; Plain `setq' is safe when no value mentions an earlier target.
         (sequential
          (cl-loop for tail on pairs
                   never (cl-some (lambda (later)
                                    (shen-elisp--mentions-p (cdr later) (caar tail)))
                                  (cdr tail)))))
    (cond
     ((null pairs) `',shen-elisp--recur)
     (sequential
      `(progn (setq ,@(cl-loop for (p . a) in pairs nconc (list p a)))
              ',shen-elisp--recur))
     (t
      (let ((temps (mapcar (lambda (pair) (shen-elisp--fresh (car pair))) pairs)))
        `(let ,(cl-mapcar (lambda (tmp pair) (list tmp (cdr pair))) temps pairs)
           (setq ,@(cl-mapcan (lambda (tmp pair) (list (car pair) tmp)) temps pairs))
           ',shen-elisp--recur))))))

(defun shen-elisp--nest-funcall (fn cargs)
  (if (null cargs)
      `(funcall ,fn)
    (let ((form fn))
      (dolist (arg cargs form)
        (setq form `(funcall ,form ,arg))))))

(defun shen-elisp--partial (op arity cargs)
  "Curried closure for OP of ARITY applied to the compiled CARGS."
  (let* ((bound (mapcar (lambda (_) (shen-elisp--fresh "x")) cargs))
         (free (cl-loop repeat (- arity (length cargs))
                        collect (shen-elisp--fresh "y")))
         (call (shen-elisp--optimise-call op (append bound free)))
         (body call))
    (dolist (y (reverse free))
      (setq body `(lambda (,y) ,body)))
    (if bound
        `(let ,(cl-mapcar #'list bound cargs) ,body)
      body)))

;;;; Definitions

(defun shen-elisp-compile-defun (name params body)
  "Compile the KL definition (defun NAME PARAMS BODY) into a `defun' form."
  (let* ((shen-elisp--counter 0)
         (closures (shen-elisp--kl-closure-p body))
         (locals (mapcar #'shen-elisp--local-name params))
         ;; With closures in the body, loop through separate variables so
         ;; that each iteration's closures see that iteration's arguments.
         (loop-vars (if closures
                        (mapcar (lambda (v) (intern (format "%s--arg" v))) locals)
                      locals))
         (env (cl-mapcar #'cons params locals))
         (shen-elisp--self name)
         (shen-elisp--self-params loop-vars)
         (shen-elisp--self-recurs nil)
         (cbody (shen-elisp-compile-expression body env t)))
    `(defun ,(shen-elisp-name name) ,loop-vars
       ,(if (not shen-elisp--self-recurs)
            (if closures
                `(let ,(cl-mapcar #'list locals loop-vars) ,cbody)
              cbody)
          (let ((result 'shen-elisp--result))
            `(let (,result)
               (while (eq (setq ,result
                                ,(if closures
                                     `(let ,(cl-mapcar #'list locals loop-vars)
                                        ,cbody)
                                   cbody))
                          ',shen-elisp--recur))
               ,result))))))

(defun shen-elisp-compile-toplevel (form)
  "Compile the top-level KL FORM."
  (let ((shen-elisp--counter 0)
        (shen-elisp--self nil))
    (shen-elisp-compile-expression form nil)))

;;;; Files

(defun shen-elisp-collect-arities (forms &optional table)
  "Record in TABLE the arity of each `defun' in FORMS and return TABLE."
  (let ((table (or table (make-hash-table :test 'eq))))
    (dolist (form forms table)
      (pcase form
        (`(defun ,name ,params . ,_) (puthash name (length params) table))))))

(defun shen-elisp--globals (form &optional acc)
  "Collect the global variable names referenced by the compiled FORM."
  (cond ((and (symbolp form)
              (string-prefix-p "shen/*" (symbol-name form)))
         (cl-pushnew form acc))
        ((eq (car-safe form) 'quote) acc)
        ((consp form)
         (while (consp form)
           (setq acc (shen-elisp--globals (car form) acc))
           (setq form (cdr form)))
         (if form (shen-elisp--globals form acc) acc))
        (t acc)))

(defvar shen-elisp-pretty-print t
  "Non-nil means compiled files are pretty-printed.")

(defun shen-elisp-write-forms (forms)
  "Insert the compiled FORMS into the current buffer, one per paragraph."
  (let ((print-escape-newlines nil)
        (print-escape-nonascii nil)
        (print-escape-multibyte nil)
        (print-length nil)
        (print-level nil)
        (print-circle nil)
        (print-quoted t))
    (dolist (form forms)
      (if shen-elisp-pretty-print
          (pp form (current-buffer))
        (prin1 form (current-buffer))
        (insert "\n"))
      (insert "\n"))))


(defun shen-elisp-compile-kl-file (source target &optional feature preamble)
  "Compile the KL file SOURCE into the Emacs Lisp file TARGET.
FEATURE is provided at the end of TARGET.  PREAMBLE is a list of forms
inserted after the `require' of the runtime."
  (let* ((forms (shen-elisp-read-kl-file source))
         (license (and (stringp (car forms)) (pop forms)))
         (compiled (mapcar #'shen-elisp-compile-toplevel forms))
         (globals (sort (shen-elisp--globals compiled)
                        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
         (base (file-name-nondirectory target)))
    (with-temp-file target
      (setq buffer-file-coding-system 'utf-8-unix)
      (insert (format ";;; %s --- Shen `%s', compiled from KLambda  -*- lexical-binding: t; -*-\n\n"
                      base (file-name-base source)))
      (when license
        (dolist (line (split-string (string-trim license) "\n"))
          (insert (string-trim-right (concat ";; " line)) "\n"))
        (insert "\n"))
      (insert ";; Derived from the Shen kernel sources,\n"
              ";; Copyright (c) 2010-2026 Mark Tarver; BSD 3-Clause License.\n"
              ";; See kernel/LICENSE.txt.\n\n"
              ";;; Commentary:\n\n"
              (format ";; Generated by shen-elisp from %s.  Do not edit.\n\n"
                      (file-name-nondirectory source))
              ";;; Code:\n\n"
              "(require 'shen-elisp-primitives)\n\n")
      (shen-elisp-write-forms preamble)
      (when globals
        (dolist (g globals)
          (insert (format "(defvar %s)\n" (prin1-to-string g))))
        (insert "\n"))
      (shen-elisp-write-forms compiled)
      (when feature
        (insert (format "(provide '%s)\n\n" feature)))
      (insert (format ";;; %s ends here\n" base)))
    target))

(provide 'shen-el-compiler)

;;; shen-elisp-compiler.el ends here
