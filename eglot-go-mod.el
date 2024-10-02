;;; eglot-go-mod.el --- eglot Go module support      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: languages

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

;; Adds support for eglot and project recognizing Go module roots for running gopls.

;;; Code:

(eval-when-compile 'cl-generic)
(eval-when-compile 'cl-macs)

(defvar project-find-functions)
(defvar elgot-lsp-context)

(cl-defstruct eglot-go-mod-project
  "Project instance for Go modules."
  root)

(cl-defmethod project-root ((project eglot-go-mod-project))
  (eglot-go-mod-project-root project))

;;;###autoload
(defun eglot-go-mod-find (dir)
  "Function for `project-find-functions' that recognizes Go modules for `eglot'."
  (when (and (boundp 'eglot-lsp-context) eglot-lsp-context)
    (let ((root (locate-dominating-file dir "go.mod")))
      (when root
        (make-eglot-go-mod-project :root root)))))

;;;###autoload
(with-eval-after-load 'project
  (push #'eglot-go-mod-find project-find-functions))

(provide 'eglot-go-mod)
;;; eglot-go-mod.el ends here
