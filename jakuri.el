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

;;;###autoload
(defun jakuri-remove-region-readonly (beg end)
  "Remove the read-only property from the region BEG to END."
  (interactive "r")
  (let ((inhibit-read-only t))
    (remove-text-properties beg end '(read-only t))))


;;; Elisp

;;;###autoload
(defun jakuri-insert-uncompiled-functions ()
  "Insert uncompiled functions at point."
  (interactive)
  (let (f)
    (mapatoms (lambda (s)
                (when (jakuri--uncompiled-function-p (symbol-function s))
                  (push (symbol-name s) f))))
    (setq f (cl-sort f #'string<))
    (dolist (s f)
      (insert s "\n"))))

(defun jakuri--uncompiled-function-p (f)
  "Non-nil if F is an uncompiled function object."
  (and (functionp f)
       (not (byte-code-function-p f))
       (not (subrp f))
       (not (symbolp f))))

;;;###autoload
(defun jakuri-expand-custom-set ()
  "Expand `custom-set-variables' and the like.
This is used to expand `custom-set-variables' forms to enable better
profiling startup times."
  (interactive)
  (down-list)
  (let ((setfunc (buffer-substring (point) (progn (forward-sexp) (point)))))
    (forward-sexp)
    (backward-sexp)
    (let ((start (point)))
      (while (condition-case nil
                 (progn (forward-sexp) t)
               (scan-error nil))
        (backward-sexp)
        (insert "(" setfunc " ")
        (forward-sexp)
        (insert ")"))
      (let ((text (buffer-substring start (point))))
        (delete-region start (point))
        (backward-up-list)
        (delete-region (point) (progn (forward-sexp) (point)))
        (let ((start (point)))
          (insert text)
          (indent-region start (point)))))))

;;;###autoload
(defun jakuri-custom-eval-set-variable ()
  "Evaluate ‘customize’ variable setting around point."
  (interactive)
  (save-excursion
    (backward-up-list)
    (seq-let (sym val) (read (current-buffer))
      (funcall #'custom-set-variables (list sym val))
      (message "%S" val))))


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

;;;###autoload
(defun jakuri-disable-comint-expansion ()
  "Disable comint expansion, e.g. of history \"!\"."
  (interactive)
  (defvar comint-input-autoexpand)
  (setq-local comint-input-autoexpand nil)
  (defvar shell-input-autoexpand)
  (setq-local shell-input-autoexpand nil))

;;;###autoload
(defun jakuri-shell-set-ssh-agent (&optional agent)
  "Set SSH_AUTH_SOCK to AGENT in the current shell.
If AGENT is nil, uses the value from the environment."
  (interactive)
  (unless agent
    (setq agent (getenv "SSH_AUTH_SOCK")))
  (jakuri-shell-exec (format "export SSH_AUTH_SOCK=%s" (shell-quote-argument agent))))

(defun jakuri-shell-exec (command)
  "Run COMMAND in current shell buffer."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc))
         (started-at-pmark (= (point) (marker-position pmark))))
    (save-excursion
      (goto-char pmark)
      ;; If the process echoes commands, don't insert a fake command in
      ;; the buffer or it will appear twice.
      (unless comint-process-echoes
        (insert command) (insert "\n"))
      (sit-for 0)                       ; force redisplay
      (comint-send-string proc command)
      (comint-send-string proc "\n")
      (set-marker pmark (point)))
    (if started-at-pmark (goto-char (marker-position pmark)))))


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

;;;###autoload
(defun jakuri-delete-empty-dirs (dir)
  "Recursively delete empty directories in DIR, without recursive calls."
  (interactive "DParent directory to delete empty dirs: ")
  (let ((dirs (list dir)))
    (while dirs
      (let ((dir (pop dirs)))
        (dolist (file (directory-files dir t))
          (unless (member file '("." ".."))
            (let ((path (expand-file-name file dir)))
              (if (file-directory-p path)
                  (push path dirs)))))
        (when (<= (length (directory-files dir t)) 2)
          (delete-directory dir))))))


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
        interprogram-cut-function #'gui-select-text
        interprogram-paste-function #'gui-selection-value))

;;;###autoload
(defun jakuri-paste-from-x ()
  "Paste from X (for Wayland Emacs)."
  (interactive)
  (insert (shell-command-to-string "xclip -selection clipboard -out")))


;;; Debug

;;;###autoload
(defun jakuri-debug-kill-emacs ()
  "Add debug to `kill-emacs-hook'."
  (let* ((f "/tmp/kill-emacs.log")
         (makelogger (lambda (s)
                       (lambda ()
                         (write-region (format "%s %s\n" (format-time-string "%FT%T%z") s)
                                       nil f t))))
         newhook)
    (push (funcall makelogger "Starting `kill-emacs-hook'") newhook)
    (dolist (x kill-emacs-hook)
      (push (funcall makelogger (if (symbolp x)
                                    (format "Starting `%s'" (symbol-name x))
                                  (format "Starting non-symbol %S" x)))
            newhook)
      (push x newhook))
    (push (funcall makelogger "Finished `kill-emacs-hook'") newhook)
    (setq kill-emacs-hook (nreverse newhook))))

;;;###autoload
(defmacro jakuri-dbg (format x)
  "Print X using FORMAT and return X.
Used for debugging."
  (declare (debug (stringp form)))
  (let ((sym (make-symbol "sym")))
    `(let ((,sym ,x))
       (message ,format ,sym)
       ,sym)))


;;; Git

;;;###autoload
(defun jakuri-find-gitdir ()
  "Find git directory."
  (interactive)
  (require 'magit)
  (dired (magit-gitdir)))


;;; Misc

;;;###autoload
(defun jakuri-ediff-pacnew ()
  "Ediff current buffer file with corresponding \".pacnew\"."
  (interactive)
  (let ((f buffer-file-name))
    (if f
        (ediff f (concat f ".pacnew"))
      (user-error "Buffer has no file"))))

(defun jakuri-delete-orphaned-elc-files (dir)
  (dolist (path (directory-files-recursively dir "\\.elc$"))
    (unless (file-exists-p (substring path 0 -1))
      (delete-file path))))

;;;###autoload
(defun jakuri-cleanup-elpa ()
  "Clean up orphaned dirs in elpa directory."
  (interactive)
  (jakuri-delete-orphaned-elc-files package-user-dir)
  (jakuri-delete-empty-dirs package-user-dir))

(provide 'jakuri)
;;; jakuri.el ends here
