;;; run-tests.el --- driver for the shen-mode ert suite  -*- lexical-binding: t; -*-
;; Evaluates shen-mode-tests.el from a scratch buffer instead of
;; reading it as a load file, which sidesteps a batch-mode quirk of
;; this Emacs build where the *load* buffer dies mid-read on
;; sizeable load files.
;;
;; Run with:  emacs --batch -l test/run-tests.el

(add-to-list 'load-path
 (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'shen-mode)
(require 'shen-el)
(require 'ert)

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "shen-mode-tests.el"
                     (file-name-directory load-file-name)))
  (eval-buffer))

;;; run-tests.el ends here
