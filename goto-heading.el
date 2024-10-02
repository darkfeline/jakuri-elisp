;;; goto-heading.el --- Org protocol goto heading    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Allen Li

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

;; Implements a new Org protocol for going to a specific heading by ID.
;; To enable, add `goto-heading' to `org-protocol-protocol-alist'.
;; The command `goto-heading-get-link' provides an easy way to
;; generate the corresponding Org protocol link for the current head.

;;; Code:

;;;###autoload
(defun goto-heading (arg)
  "Org protocol handler for navigating to Org headings.
ARG is a property list."
  (org-id-goto (plist-get arg :id))
  (raise-frame))

;;;###autoload
(defun goto-heading-get-link ()
  "Put Org protocol link to current heading in kill ring."
  (interactive)
  (kill-new (format "org-protocol://goto-heading?id=%s" (org-id-get-create))))

(provide 'goto-heading)
;;; goto-heading.el ends here
