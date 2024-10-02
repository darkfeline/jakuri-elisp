;;; gttap.el --- goto thing at point                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: hypermedia, local

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

;; goto thing at point

;;; Code:

;;;###autoload
(defun gttap-dwim (&optional arg)
  "Go to thing at point.
This also intelligently handles an active region.
ARG is the prefix argument, which is passed to the handler."
  (interactive "P")
  (if (use-region-p)
      (gttap-region (region-beginning) (region-end) arg)
    (gttap arg)))

;;;###autoload
(defun gttap-region (beg end &optional arg)
  "Go to things in region BEG to END.
ARG is the prefix argument, which is passed to the handler."
  (interactive "rP")
  (setq deactivate-mark t)
  (goto-char beg)
  (push-mark)
  (while (< (point) end)
    (gttap arg)
    (move-beginning-of-line 1)
    (forward-line)))

(defvar gttap-functions '(browse-url-at-point)
  "Functions called by `gttap' to try to handle thing at point.
If a function raises an error, the next function is tried.")

;;;###autoload
(defun gttap (&optional arg)
  "Go to thing at point.
ARG is the prefix argument, which is passed to the handler."
  (interactive "P")
  (unless (catch 'ret
            (dolist (f gttap-functions)
              (condition-case nil
                  (progn
                    (funcall f arg)
                    (throw 'ret t))
                ((debug t)))))
    (user-error "No suitable gttap handler")))

(provide 'gttap)
;;; gttap.el ends here
