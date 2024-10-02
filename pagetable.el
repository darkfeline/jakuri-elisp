;;; pagetable.el --- Display table of contents for page breaks  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: convenience

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

;; Displays a table of contents for the pages separated by page breaks
;; in a buffer.

;;; Code:

(defvar-local pagetable--master-buffer nil
  "Master buffer for `pagetable' buffers.")

;;;###autoload
(defun pagetable (&optional buffer)
  "Displays the page table for the current buffer."
  (interactive (list (current-buffer)))
  (pop-to-buffer (get-buffer-create (format "*pagetable %s*" (buffer-name buffer))))
  (pagetable-mode)
  (setq pagetable--master-buffer buffer
        tabulated-list-entries #'pagetable--entries)
  (tabulated-list-print))

(defvar pagetable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-m] #'pagetable-jump)
    (define-key map [?\C-o] #'pagetable-show)
    map)
  "Keymap for `pagetable-mode'.")

(define-derived-mode pagetable-mode tabulated-list-mode "Pagetable"
  "Mode for viewing page tables."
  (setq tabulated-list-format [("L" 6 nil) ("Title" 0 nil)])
  (tabulated-list-init-header))

(defun pagetable-jump ()
  "Jump to page at point."
  (interactive)
  (let ((m (tabulated-list-get-id)))
    (pop-to-buffer (marker-buffer m))
    (widen)
    (goto-char m)
    (narrow-to-page)))

(defun pagetable-show ()
  "Show page at point."
  (interactive)
  (let ((m (tabulated-list-get-id)))
    (with-selected-window (display-buffer (marker-buffer m)
                                          '(display-buffer-reuse-window
                                            display-buffer-use-some-window
                                            (inhibit-same-window . t)))
      (widen)
      (goto-char m)
      (narrow-to-page))))

(defsubst pagetable--entry-at-point ()
  "Returns the page entry at point."
  (vector (number-to-string (line-number-at-pos nil t)) (string-trim-right (thing-at-point 'line))))

(defun pagetable--entries ()
  "Function for `tabulated-list-entries'."
  (with-current-buffer pagetable--master-buffer
    (goto-char (point-min))
    (let (entries)
      (while (not (eobp))
        (when (eolp)
          (forward-char))
        (push (list (point-marker) (pagetable--entry-at-point))
              entries)
        (forward-page))
      (nreverse entries))))

(provide 'pagetable)
;;; pagetable.el ends here
