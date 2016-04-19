;;; cram-mode.el --- major mode for editing cram.

;;; Copyright (C) 2016, by Volodymyr Vitvitskyi

;; Author: Volodymyr Vitvitskyi ( contact.volodymyr@gmail.com )
;; Version: 2016.5.3
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

(setq mode-name "cram")

(define-derived-mode cram-mode text-mode
  (setq font-lock-defaults '(cram-highlights))
  (setq mode-name "cram"))

(add-to-list 'auto-mode-alist '("\\.t\\'" . cram-mode))

;; Local Variables:
;; coding: utf-8
;; End:

(provide 'cram-mode)
;;; cram-mode.el ends here
