;;; cram-mode.el --- major mode for editing cram.

;;; Copyright (C) 2016, by Volodymyr Vitvitskyi

;; Author: Volodymyr Vitvitskyi ( contact.volodymyr@gmail.com )
;; Version: 2016.5.6
;; Created: 18 Apr 2016
;; Keywords: languages
;; Homepage: https://github.com/signalpillar/cram-mode

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Syntax highlight for cram file.
;; https://pypi.python.org/pypi/cram

;;; Code:

(setq cram-highlights
      '(("^  [\\$>] .*$" . font-lock-string-face)
        ("^.* \(\\(glob\\|re\\|no-eol\\|esc\\)\)$" . font-lock-keyword-face)
        ("^  .*$" . font-lock-defaults)
        (".*" . font-lock-comment-face)))

(defcustom cram-executable "cram"
  "cram executable path")

(defcustom cram-indent 2
  "Number of spaces to use for indentation")

(defun cram-get-debug-cmdline (fname)
  (format "%s -d %s" cram-executable fname))

(defun cram-run-buffer-in-debug ()
  "Run cram against a file in debug mode (-d)."
  (interactive)
  (shell-command (cram-get-debug-cmdline (buffer-file-name))))

(defun cram-run-region-in-debug (start end)
  "Run cram (in debug) against region and output insert below selection."
  (interactive "r")
  (let ((dest (make-temp-file mode-name)))
    (write-region start end dest)
    (insert (shell-command-to-string (cram-get-debug-cmdline dest)) " \n"))) 

(define-derived-mode cram-mode text-mode
  (setq font-lock-defaults '(cram-highlights))
  (setq mode-name "cram"))

(define-key cram-mode-map (kbd "C-c C-b") 'cram-run-buffer-in-debug)
(define-key cram-mode-map (kbd "C-c C-r") 'cram-run-region-in-debug)

(add-to-list 'auto-mode-alist '("\\.t\\'" . cram-mode))

;; Local Variables:
;; coding: utf-8
;; End:

(provide 'cram-mode)
;;; cram-mode.el ends here
