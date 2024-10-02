;;; stackview.el --- view marker stack               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Allen Li

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

;; View marker stacks.

;;; Code:

(require 'ring)
(eval-when-compile (require 'subr-x))

(defvar-local stackview-marker-ring nil
  "Marker ring for `stackview-mode'.")

;;;###autoload
(defun stackview-mark-ring ()
  "Show mark ring marker stack."
  (interactive)
  (let ((buf (current-buffer)))
    (pop-to-buffer (get-buffer-create "*stackview mark*"))
    (stackview-mode)
    (setq tabulated-list-entries
          (lambda ()
            (stackview--marker-entries (buffer-local-value 'mark-ring buf))))
    (tabulated-list-print)))

;;;###autoload
(defun stackview-xref ()
  "Show xref marker stack."
  (interactive)
  (require 'xref)
  (pop-to-buffer (get-buffer-create "*stackview xref*"))
  (stackview-mode)
  (setq tabulated-list-entries
        (lambda ()
          (stackview--marker-entries (let ((x (funcall xref-history-storage)))
                                       (append (cdr x) (car x))))))
  (tabulated-list-print))

(defun stackview-jump ()
  "Jump to marker at point."
  (interactive)
  (let ((m (tabulated-list-get-id)))
    (pop-to-buffer (marker-buffer m))
    (goto-char m)))

(defun stackview-show ()
  "Show marker at point."
  (interactive)
  (let ((m (tabulated-list-get-id)))
    (with-selected-window (display-buffer (marker-buffer m)
                                          '(display-buffer-reuse-window
                                            display-buffer-use-some-window
                                            (inhibit-same-window . t)))
      (goto-char m)
      (recenter))))

(defvar stackview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-m] #'stackview-jump)
    (define-key map [?\C-o] #'stackview-show)
    map)
  "Keymap for `stackview-mode'.")

(define-derived-mode stackview-mode tabulated-list-mode "Stackview"
  "Mode for viewing marker stacks."
  (setq tabulated-list-format [("Buffer" 15 nil) ("L" 6 nil) ("Line" 0 nil)])
  (tabulated-list-init-header))

(defun stackview--marker-entries (markers)
  "Return a list for `tabulated-list-entries' for MARKERS."
  (let (entries)
    (dolist (m markers (nreverse entries))
      (push (list m (stackview--marker-entry m)) entries))))

(defun stackview--marker-entry (marker)
  "Return `tabulated-list-mode' entry for MARKER."
  (let ((b (marker-buffer marker)))
    (if b
        (with-current-buffer b
          (save-excursion
            (goto-char (marker-position marker))
            (vector (buffer-name b)
                    ;; This is faster (probably) than `line-number-at-pos'.
                    (format-mode-line "%l" nil nil (marker-buffer marker))
                    (string-trim-right (thing-at-point 'line)))))
      (vector "<no buffer>"
              "0"
              ""))))

(provide 'stackview)
;;; stackview.el ends here
