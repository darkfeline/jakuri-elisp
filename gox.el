;;; gox.el --- Go mode extensions                    -*- lexical-binding: t; -*-

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

;; Go mode extensions

;;; Code:

(defun gox-current-package-name ()
  "Return the current Go package name.
If package name not found, return nil."
  (catch 'ret
    (dolist (f (gox--directory-go-files default-directory))
      (with-temp-buffer
        (insert-file-contents-literally f)
        (when (search-forward-regexp "^package " nil t)
          (throw 'ret (buffer-substring (point)
                                        (save-excursion (search-forward-regexp "$") (point)))))))
    nil))

;;;###autoload
(with-eval-after-load 'go-mode
  (define-abbrev go-mode-abbrev-table
    "pac" "package" #'gox--insert-package-name :system t))

;;;###autoload
(defun gox--insert-package-name ()
  "Insert the current Go package name.
Used by abbrev."
  (let ((name (or (gox-current-package-name)
                  (file-name-nondirectory (directory-file-name default-directory)))))
    (when name
      (insert " ")
      (insert name))))

;;;###autoload
(defun gox-rewrite-package-name (name)
  "Rewrite the package name in all Go files in the directory to NAME."
  (interactive "sPackage name: ")
  (save-current-buffer
    (dolist (f (gox--directory-go-files default-directory))
      (find-file f)
      (gox-rewrite-buffer-package-name name))))

(defun gox--directory-go-files (dir)
  "Return list of Go files in DIR."
  (directory-files dir t "\\`[^.].*\\.go\\'" t))

(defun gox-rewrite-buffer-package-name (name)
  "Set the package name in the current Go buffer to NAME."
  (save-window-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward-regexp "^package " nil t)
        (delete-region (point) (save-excursion (search-forward-regexp "$") (point)))
        (insert name)))))

(provide 'gox)
;;; gox.el ends here
