;;; shen-mode-tests.el --- ert tests for shen-mode completion  -*- lexical-binding: t; -*-

;; Unit tests for `shen-completion-at-point'.  Two halves:
;;
;;   - gate and buffer-local completion: no Shen runtime needed;
;;   - live-image completion: boots the real Shen kernel
;;     (run `make build' first) and checks that completion sees the
;;     standard library, its arities, and that eval works.
;;
;; Run with:
;;   emacs --batch -l test/shen-mode-tests.el
;; (the file self-runs under `--batch'.)

(require 'ert)

;; Walk up from this file until "src/shen-mode.el" is found, so the
;; tests run from a checkout of any depth or via any launch method.
(add-to-list 'load-path
 (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'shen-mode)

(defun shen-test--buffer (text)
  "Return a fresh shen-mode buffer containing TEXT."
  (with-current-buffer (get-buffer-create (generate-new-buffer-name "shen-test"))
    (shen-mode)
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (current-buffer)))

(defun shen-test--marker-after (buf text)
  "In BUF, return a marker right after the first occurrence of TEXT."
  (with-current-buffer buf
    (let ((m (make-marker)))
      (goto-char (point-min))
      (unless (search-forward text nil t)
        (error "Text not found: %s" text))
      (when (eq (char-before) ?\)) (backward-char))
      (set-marker m (point) buf)
      m)))

(defun shen-test--capf (buf m)
  "Run shen-completion-at-point at marker M in BUF.

Returns (PREFIX . CANDIDATES) or nil when the CAPF refuses."
  (with-current-buffer buf
    (let ((r (shen-completion-at-point m)))
      (if r
          (let ((pts (nth 0 r))
                (end (nth 1 r))
                (table (nth 2 r)))
            (cons (buffer-substring pts end)
                  (mapcar (lambda (name)
                            (cons name
                                  (string-trim-left
                                   (or (funcall (plist-get (nthcdr 3 r)
                                                           :annotation-function)
                                                name) ""))))
                          (all-completions (buffer-substring pts end) table))))
        nil))))

(defun shen-test--names (r)
  "The candidate names from a `shen-test--capf' result."
  (mapcar #'car (cdr r)))

;;;; Gate tests: when the CAPF must refuse

(ert-deftest shen-capf-refuses-at-line-start ()
  "No completion when point is at the start of a line."
  (let* ((b (shen-test--buffer "abc\nxyz"))
        (m (make-marker)))
    (with-current-buffer b
      (goto-char (line-beginning-position 2))
      (set-marker m (point) b))
    (should (null (shen-test--capf b m)))))

(ert-deftest shen-capf-refuses-in-comment ()
  "No completion inside a \\ line comment."
  (let* ((b (shen-test--buffer "abc\n\\\\\\ myfun\n"))
        (m (make-marker)))
    (with-current-buffer b
      (search-forward "myfun")
      (set-marker m (point) b))
    (should (null (shen-test--capf b m)))))

(ert-deftest shen-capf-refuses-in-string ()
  "No completion inside a string literal."
  (let* ((b (shen-test--buffer "(pr \"myfun\")\n"))
        (m (make-marker)))
    (with-current-buffer b
      (search-forward "myfun")
      (set-marker m (point) b))
    (should (null (shen-test--capf b m)))))

(ert-deftest shen-capf-refuses-after-whitespace ()
  "No completion when the previous character is whitespace."
  (let* ((b (shen-test--buffer "(  myfun)\n"))
        (m (make-marker)))
    (with-current-buffer b
      (search-forward "(  ")
      (set-marker m (point) b))
    (should (null (shen-test--capf b m)))))

;;;; Buffer-local completion (no Shen runtime)

(ert-deftest shen-capf-completes-buffer-defined-names ()
  "A name defined in the buffer completes its own prefix."
  (let* ((b (shen-test--buffer "(define myfun\n  [A | B] -> A\n  [A] -> A)\n(myf)\n"))
        (m (shen-test--marker-after b "(myf)")))
    (let ((r (shen-test--capf b m)))
      (should r)
      (should (equal (car r) "myf"))
      (should (member "myfun" (shen-test--names r))))))

(ert-deftest shen-capf-offers-reserved-words ()
  "Top-level keywords are offered for their prefixes."
  (let* ((b (shen-test--buffer "(de\n"))
        (m (shen-test--marker-after b "(de")))
    (let ((r (shen-test--capf b m)))
      (should r)
      (dolist (kw '("define" "defcc" "defmacro" "defprolog"))
        (should (member kw (shen-test--names r)))))))

(ert-deftest shen-capf-bounds-select-the-typed-word ()
  "The returned bounds select exactly the word being typed."
  (let* ((b (shen-test--buffer "(string.len\n"))
        (m (shen-test--marker-after b "string.len")))
    (with-current-buffer b
      (let ((r (shen-completion-at-point m)))
        (should r)
        (should (= (nth 0 r)
                   (save-excursion
                     (goto-char (shen-test--marker-after b "string.len"))
                     (search-backward "string.len")
                     (point))))
        (should (= (nth 1 r) (marker-position m)))))))

;;;; Live-image completion (requires the compiled kernel)

(ert-deftest shen-capf-reflects-running-image ()
  "After `shen-elisp-boot', a standard library name completes."
  (unless (fboundp #'shen-elisp-boot)
    (signal 'ert-test-skipped (list "shen-elisp not loadable")))
  (shen-elisp-boot)
  (let* ((b (shen-test--buffer "(string.len\n"))
        (m (shen-test--marker-after b "string.len")))
    (let ((r (shen-test--capf b m)))
      (should r)
      (should (member "string.length" (shen-test--names r))))
    (should (equal (shen-eldoc "string.length") "(1)"))))

(ert-deftest shen-capf-annotates-arity ()
  "Image candidates carry their arity as the annotation."
  (unless (fboundp #'shen-elisp-boot)
    (signal 'ert-test-skipped (list "shen-elisp not loadable")))
  (shen-elisp-boot)
  (let* ((b (shen-test--buffer "(app\n"))
        (m (shen-test--marker-after b "(app")))
    (let ((r (shen-test--capf b m)))
      (should r)
      (should (member (cons "append" "(2)") (cdr r)))
      (should (member (cons "approx" "(1)") (cdr r))))))

(ert-deftest shen-capf-mixes-buffer-and-image ()
  "Buffer-local names and image names appear in one candidate list."
  (unless (fboundp #'shen-elisp-boot)
    (signal 'ert-test-skipped (list "shen-elisp not loadable")))
  (shen-elisp-boot)
  (let* ((b (shen-test--buffer "(define myfun\n  X -> X)\n(myf)\n"))
        (m (shen-test--marker-after b "(myf)")))
    (let ((r (shen-test--capf b m)))
      (should r)
      (should (member "myfun" (shen-test--names r)))
      (with-current-buffer b
        (should (> (length (shen-mode--all-names)) 100))))))

(ert-deftest shen-tab-completes-string-len ()
  "The user-visible tab: try-completion of `string.len' gives the full name."
  (unless (fboundp #'shen-elisp-boot)
    (signal 'ert-test-skipped (list "shen-elisp not loadable")))
  (shen-elisp-boot)
  (let* ((b (shen-test--buffer "(string.len\n"))
        (m (shen-test--marker-after b "string.len")))
    (with-current-buffer b
      (let ((r (shen-completion-at-point m)))
        (should r)
        (let ((completed
               (try-completion (buffer-substring (nth 0 r) (nth 1 r))
                               (nth 2 r))))
          (should (equal completed "string.length")))))))

(ert-deftest shen-capf-registered-in-mode ()
  "`shen-mode' adds its CAPF to `completion-at-point-functions'."
  (let* ((b (shen-test--buffer "(abc)\n")))
    (with-current-buffer b
      (should (member #'shen-completion-at-point
                      completion-at-point-functions)))))

(ert-deftest shen-eval-region-works-on-booted-image ()
  "C-c C-e evaluates a definition into the running image."
  (unless (fboundp #'shen-elisp-boot)
    (signal 'ert-test-skipped (list "shen-elisp not loadable")))
  (shen-elisp-boot)
  (let* ((b (shen-test--buffer "(define shen-test-fun\n  X -> (+ X 1))\n")))
    (with-current-buffer b
      (shen-eval-region (point-min) (point-max)))
    (let ((result (shen-elisp-eval "(shen-test-fun 41)")))
      (should (numberp result))
      (should (= result 42)))))

(ert-run-tests-batch-and-exit)

;;; shen-mode-tests.el ends here
