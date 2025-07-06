;;; elpf.el --- Emacs Lisp prefix insertion          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: convenience, lisp, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Add an abbrev for inserting the prefix for the current Emacs Lisp file.

;;; Code:

(defconst elpf--prefix-fallback "unknown"
  "Library name fallback.
Returned by ‘elpf--prefix’ when buffer is not
associated with a file.")

(defvar-local elpf-prefix nil
  "Prefix for current buffer.
This is inserted by ‘elpf-insert-prefix’.
If nil, the name of the library file is used.")
;;;###autoload
(put 'elpf-prefix 'safe-local-variable #'stringp)

(defun elpf--prefix ()
  "Return symbol prefix for the current buffer."
  (if elpf-prefix
      elpf-prefix
    (setq elpf-prefix (cond
                       (buffer-file-name (file-name-base buffer-file-name))
                       (t elpf--prefix-fallback)))))

;;;###autoload
(defun elpf-set-prefix (value)
  "Set prefix for current buffer to VALUE.
See variable ‘elpf-prefix’."
  (interactive "MSet prefix to: ")
  (setq-local elpf-prefix value))

;;;###autoload
(defun elpf-insert-prefix ()
  "Insert Elisp symbol prefix at point.
Inserts ‘elpf-prefix’ if it is non-nil, otherwise the
name of the library file is used."
  (interactive)
  (insert (elpf--prefix)))

;;;###autoload
(with-eval-after-load 'elisp-mode
  (define-abbrev emacs-lisp-mode-abbrev-table
    "pf" "" #'elpf-insert-prefix :system t))

(provide 'elpf)
;;; elpf.el ends here
