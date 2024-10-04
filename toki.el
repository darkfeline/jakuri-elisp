;;; toki.el --- Time                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Allen Li

;; Author: Allen Li <ayatane@felesatra.moe>
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

;; Time

;;; Code:

(defvar toki-time-formats
  '(("ISO 8601 date" . "%F")
    ("ISO 8601 datetime" . "%FT%T%z")
    ("ISO 8601 week" . "%G-W%V")
    ("ISO 8601 weekdate" . "%G-W%V-%u")
    ("time" . "%T")
    ("date and weekday" . "%F %a"))
  "An alist of time formats.

\(name . format-string)

See ‘format-time-string’ for format string.")

(defvar toki--time-format-history nil
  "History for format time inputs.")

;;;###autoload
(defun toki-insert-format-time (string)
  "Insert formatted time at point.
When called interactively, prompts for a preset format
from ‘toki-time-formats’.
When called from Lisp, STRING is a string to pass to ‘format-time-string’."
  (interactive (list (toki--read-time-format)))
  (insert (format-time-string string)))

(defun toki--read-time-format ()
  "Read a time format preset."
  (let* ((names (mapcar #'car toki-time-formats))
         (prompt (format "Insert time format (default %s): " (car names)))
         (input (completing-read
                 prompt names
                 nil t nil
                 'toki--time-format-history (car names))))
    (cdr (assoc input toki-time-formats))))

;;;###autoload
(defun toki-update-date-at-point ()
  "Update date at point to today."
  (interactive)
  (atomic-change-group
    (let ((bounds (toki--bounds-of-date-at-point)))
      (delete-region (car bounds) (cdr bounds)))
    (insert (format-time-string "%F"))))

;;;###autoload
(defun toki-increment-date-at-point (days)
  "Increment date at point by DAYS."
  (interactive "p")
  (seq-let (_sec _min _hour day mon year _dow _dst _tz)
      (parse-time-string (thing-at-point 'symbol t))
    (setq day (+ day days))
    (atomic-change-group
      (let ((bounds (toki--bounds-of-date-at-point)))
        (delete-region (car bounds) (cdr bounds)))
      (insert (format-time-string "%F" (encode-time 0 0 0 day mon year))))))

;;;###autoload
(defun toki-increment-month-at-point (months)
  "Increment date at point by MONTHS."
  (interactive "p")
  (seq-let (_sec _min _hour day mon year _dow _dst _tz)
      (parse-time-string (thing-at-point 'symbol t))
    (setq mon (+ mon months))
    (atomic-change-group
      (let ((bounds (toki--bounds-of-date-at-point)))
        (delete-region (car bounds) (cdr bounds)))
      (insert (format-time-string "%F" (encode-time 0 0 0 day mon year))))))

(defun toki--bounds-of-date-at-point ()
  "Return cons of bounds of date at point.
Date should be in ISO 8601 format.
Return nil if no date at point."
  (save-excursion
    (let ((bounds (cons (progn
                          (skip-chars-backward "-0-9")
                          (point))
                        (progn
                          (skip-chars-forward "-0-9")
                          (point)))))
      (if (= (car bounds) (cdr bounds)) nil bounds))))

(provide 'toki)
;;; toki.el ends here
