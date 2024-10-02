;;; jakuri.el --- personal code (public)             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Allen Li

;; Author: Allen Li <ayatane@google.com>
;; Keywords: local

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

;; Personal code, the public bits.

;;; Code:

;;;###autoload
(defun jakuri-sort-uniq (beg end)
  "Sort and uniq lines between BEG and END."
  (interactive "r")
  (shell-command-on-region beg end "sort | uniq" t t))

;;;###autoload
(defun jakuri-stable-uniq (beg end)
  "Remove duplicate lines between BEG and END without sorting."
  (interactive "r")
  (let ((seen (make-hash-table :test 'equal)))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (atomic-change-group
          (while (not (eobp))
            (let ((line (thing-at-point 'line t)))
              (if (gethash line seen)
                  (delete-region (point) (save-excursion (forward-line) (point)))
                (puthash line t seen)
                (forward-line)))))))))

;;;###autoload
(defun jakuri-toggle-quotes (beg end)
  "Toggle single and double quotes in region BEG to END."
  (interactive "r")
  (translate-region beg end (let ((table (make-char-table 'translation-table)))
                              (aset table ?' ?\")
                              (aset table ?\" ?')
                              table)))

(provide 'jakuri)
;;; jakuri.el ends here
