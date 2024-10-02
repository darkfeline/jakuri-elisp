;;; abbrevedit.el --- abbrev editor                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
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

;; A better abbrev editor than the builtin `edit-abbrevs'.
;; Leverages `tabulated-list-mode' and handles properties like `:fixed-case' properly.
;;
;; Usage is simple.  Run `abbrevedit' to open a buffer displaying all abbrevs.
;; Use `abbrevedit-edit' (bound to "e" by default) to edit the abbrev field at
;; point.

;;; Code:

;;;###autoload
(defun abbrevedit ()
  "Open abbrev editor."
  (interactive)
  (pop-to-buffer (get-buffer-create "*abbrevedit*"))
  (abbrevedit-mode)
  (setq tabulated-list-entries #'abbrevedit--all-table-entries)
  (tabulated-list-print))

;;;###autoload
(defun abbrevedit-edit ()
  "Edit abbrev at point."
  (interactive)
  (let* ((col (abbrevedit--col))
         (coli (tabulated-list--column-number col))
         (entry (tabulated-list-get-entry))
         (val (aref entry coli))
         (newval (read-from-minibuffer (format "Edit %s (default %s): " col val) nil nil nil nil val)))
    ;; Clear previous abbrev in case we're renaming abbrev or table.
    (abbrevedit--delete-entry entry)
    (aset entry coli newval)
    (abbrevedit--save-entry entry)
    ;; Update display with new value.
    ;; Not sure if needed?
    (tabulated-list-set-col col newval)))

(defvar abbrevedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?e] #'abbrevedit-edit)
    map)
  "Keymap for `abbrevedit-mode'.")

(define-derived-mode abbrevedit-mode tabulated-list-mode "Abbrevedit"
  "Mode for editing abbrevs."
  (setq tabulated-list-format [("Table" 24 nil) ("Name" 5 nil) ("Expand" 16 nil) ("Hook" 16 nil) ("Props" 0 nil)])
  (tabulated-list-init-header))

(defun abbrevedit--all-table-entries ()
  "Return tabulated entries for `abbrevedit'."
  (mapcan #'abbrevedit--table-entries
          abbrev-table-name-list))

(defun abbrevedit--table-entries (table)
  "Return tabulated entries for TABLE."
  (mapcar (lambda (abbrev)
            (list table
                  (vector (symbol-name table)
                    (nth 0 abbrev) ; name
                    (nth 1 abbrev) ; expansion
                    (prin1-to-string (nth 2 abbrev)) ; hook
                    (prin1-to-string (nthcdr 3 abbrev)))))
          (abbrevedit--table-abbrevs table)))

(defun abbrevedit--table-abbrevs (table)
  "Return abbrevs in TABLE as a list of lists.
TABLE is a symbol."
  (eval (nth 2 (read (with-temp-buffer
                 (insert-abbrev-table-description table)
                 (buffer-string))))))

(defun abbrevedit--delete-entry (entry)
  "Delete abbrev ENTRY."
  (define-abbrev (symbol-value (intern (aref entry 0))) (aref entry 1) nil))

(defun abbrevedit--save-entry (entry)
  "Save abbrev ENTRY."
  (apply #'define-abbrev (symbol-value (intern (aref entry 0))) (aref entry 1) (aref entry 2)
    (read (aref entry 3)) (read (aref entry 4))))

(defun abbrevedit--col ()
  "Return column name."
  (get-text-property (point) 'tabulated-list-column-name))

(provide 'abbrevedit)
;;; abbrevedit.el ends here
