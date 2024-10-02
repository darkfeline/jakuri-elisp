;;; clipmag.el --- clipboard magazine                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Allen Li

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

;; Clipboard magazine.

;;; Code:

(eval-when-compile (require 'subr-x))

(defun clipmag-feed-and-cycle ()
  "Type current line into a selected X window and cycle to next line."
  (interactive)
  (let ((text (string-trim (thing-at-point 'line))))
    (call-process "xdotool" nil nil nil
                  "selectwindow"
                  "windowfocus" "--sync"
                  "type" (concat text "\n")))
  (forward-line 1))

(defvar clipmag-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-tab] #'clipmag-feed-and-cycle)
    map))

;;;###autoload
(define-minor-mode clipmag-mode
  "Minor mode for clipboard magazine.

\\{clipmag-mode-map}"
  :lighter " clipmag"
  :keymap clipmag-mode-map)

(provide 'clipmag)
;;; clipmag.el ends here
