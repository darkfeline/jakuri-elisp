;;; jakuri.el --- personal code (public)             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Version: 0.1.0
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

;; Personal code, the public bits.

;;; Code:


;;; Text editing

;;;###autoload
(defun jakuri-sort-uniq (beg end)
  "Sort and uniq lines between BEG and END."
  (interactive "r")
  (shell-command-on-region beg end "sort | uniq" t t))

;;;###autoload
(defun jakuri-stable-uniq (beg end)
  "Remove duplicate lines between BEG and END without sorting."
  (interactive "r")
  (let ((seen (make-hash-table :test 'equal)))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (atomic-change-group
          (while (not (eobp))
            (let ((line (thing-at-point 'line t)))
              (if (gethash line seen)
                  (delete-region (point) (save-excursion (forward-line) (point)))
                (puthash line t seen)
                (forward-line)))))))))

;;;###autoload
(defun jakuri-toggle-quotes (beg end)
  "Toggle single and double quotes in region BEG to END."
  (interactive "r")
  (translate-region beg end (let ((table (make-char-table 'translation-table)))
                              (aset table ?' ?\")
                              (aset table ?\" ?')
                              table)))


;;; Comint/shell

;;;###autoload
(defun jakuri-shell (dir)
  "Open a shell in DIR.
If current directory is remote, use the home directory instead."
  (interactive (list (if (file-remote-p default-directory)
                         (expand-file-name "~")
                       default-directory)))
  (let ((default-directory dir))
    (shell (generate-new-buffer-name "*shell*"))))


;;; Buffer and files

;;;###autoload
(defun jakuri-sudo-find-buffer-file ()
  "Find current buffer's file with TRAMP sudo."
  (interactive)
  (find-file (format "/sudo::%s" (if buffer-file-name buffer-file-name default-directory))))

;;;###autoload
(defun jakuri-kill-directory-files ()
  "Save current directory files to kill ring."
  (interactive)
  (kill-new (string-join
             (directory-files default-directory nil (rx (or (not (any ?.))
                                                            (= 3 anything))))
             "\n")))

;;;###autoload
(defun jakuri-kill-buffer-file-name ()
  "Put the buffer file name in the kill ring."
  (interactive)
  (kill-new (cond
             ((eq major-mode 'dired-mode)
              (when (boundp 'dired-directory)
                dired-directory))
             (t (buffer-file-name)))))


;;; Packages

;;;###autoload
(defun jakuri-package-recompile-all ()
  "Byte-compile all installed packages.
This is meant to be used only in the case the byte-compiled files
are invalid due to changed byte-code, macros or the like.

Patched for `https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-02/msg00611.html'."
  (interactive)
  (pcase-dolist (`(_ ,pkg-desc) package-alist)
    (with-demoted-errors "Error while recompiling: %S"
      (package-recompile pkg-desc))))

;;;###autoload
(defun jakuri-package-rebuild ()
  "Rebuild all package stuff."
  (interactive)
  (package-initialize)
  (package-quickstart-refresh)
  (jakuri-package-recompile-all))


;;; System integration

;;;###autoload
(defun jakuri-tail-interprogram-paste ()
  "Insert text from ‘interprogram-paste-function’ until quitted."
  (interactive)
  (while t
    (insert (jakuri--wait-for-clipboard))
    (insert "\n")
    (recenter)))

(defun jakuri--wait-for-clipboard ()
  "Wait for and return text from system clipboard."
  (funcall interprogram-cut-function "")
  (with-temp-message "Waiting for clipboard, C-g to stop"
    (catch 'ret
      (while t
        (let ((new (funcall interprogram-paste-function)))
          (if (and (not (null new))
                   (not (equal new "")))
              (throw 'ret new)))
        (sit-for 0.05 t)))))

;;;###autoload
(defun jakuri-reset-reintegrate ()
  "Reset reintegrate bindings."
  (interactive)
  (setq browse-url-browser-function #'browse-url-default-browser
        interprogram-cut-function nil
        interprogram-paste-function nil))


;;; Misc

;;;###autoload
(defun jakuri-ediff-pacnew ()
  "Ediff current buffer file with corresponding \".pacnew\"."
  (interactive)
  (let ((f buffer-file-name))
    (if f
        (ediff f (concat f ".pacnew"))
      (user-error "Buffer has no file"))))

(provide 'jakuri)
;;; jakuri.el ends here
