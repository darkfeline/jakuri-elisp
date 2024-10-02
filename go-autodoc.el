;;; go-autodoc.el --- Go auto doc comment            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: languages, local

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

;; Go auto doc comment

;;; Code:

(eval-when-compile 'cl-lib)

;;;###autoload
(defun go-autodoc-insert ()
  "Insert name for the Go documentation comment at point."
  (cl-destructuring-bind (kind ident) (go-autodoc--identifier)
    (cond
     ((string= kind "type") (insert (if (string-match "^[aeiouAEIOU]" ident)
                                        (format "An %s" ident)
                                      (format "A %s" ident))))
     ((string= kind "package") (insert (format "Package %s" ident)))
     (t (insert ident)))))

(defun go-autodoc--identifier ()
  "Return identifier for the documentation comment at point.
\(KIND IDENT)"
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (cond
     ((looking-at "type")
      (forward-sexp)
      (list "type" (buffer-substring-no-properties (progn (forward-sexp) (point))
                                                   (progn (backward-sexp) (point)))))
     ((looking-at "var")
      (forward-sexp)
      (list "var" (buffer-substring-no-properties (progn (forward-sexp) (point))
                                                  (progn (backward-sexp) (point)))))
     ((looking-at "const")
      (forward-sexp)
      (list "const" (buffer-substring-no-properties (progn (forward-sexp) (point))
                                                    (progn (backward-sexp) (point)))))
     ((looking-at "func")
      (forward-sexp 2)
      (backward-sexp)
      (when (looking-at "\(")
        (forward-sexp))
      (list "func" (buffer-substring-no-properties (progn (forward-sexp) (point))
                                                   (progn (backward-sexp) (point)))))
     ((looking-at "package")
      (forward-sexp)
      (list "package" (buffer-substring-no-properties (progn (forward-sexp) (point))
                                                      (progn (backward-sexp) (point)))))
     ;; Nested declarations
     ((looking-at (rx (group-n 1 (1+ word))))
      (list "nested" (match-string-no-properties 1)))
     (t (error "No autodoc identifier found")))))

;;;###autoload
(with-eval-after-load 'go-mode
  (defvar go-mode-abbrev-table)
  (define-abbrev go-mode-abbrev-table
    "cm" "// " #'go-autodoc-insert :system t))

(provide 'go-autodoc)
;;; go-autodoc.el ends here
