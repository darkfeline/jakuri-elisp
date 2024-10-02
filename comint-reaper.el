;;; comint-reaper.el --- Kill comint buffers when their process dies  -*- lexical-binding: t; -*-

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

;; Kill comint buffers when their process dies

;;; Code:

(defvar comint-reaper-lighter " Rpr"
  "Lighter for `comint-reaper-mode'.")

(defvar-local comint-reaper--original-sentinel nil
  "Original process sentinel.")

;;;###autoload
(define-minor-mode comint-reaper-mode
  "Kill the buffer when its process dies."
  :lighter comint-reaper-lighter
  (let ((proc (get-buffer-process (current-buffer))))
    (if comint-reaper-mode
      (progn
        (setq comint-reaper--original-sentinel (process-sentinel proc))
        (set-process-sentinel proc #'comint-reaper--sentinel))
      (set-process-sentinel proc comint-reaper--original-sentinel))))

(defun comint-reaper--sentinel (proc event)
  "Sentinel for `comint-reaper-mode'."
  (funcall comint-reaper--original-sentinel proc event)
  (unless (process-live-p proc)
    (kill-buffer (process-buffer proc))))

(provide 'comint-reaper)
;;; comint-reaper.el ends here
