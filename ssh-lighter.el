;;; ssh-lighter.el --- SSH mode line indicator         -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: mode-line, local

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

;; Mode line indicator when running in an SSH session.

;; Example usage:
;;
;;   (require 'ssh-lighter)
;;   (add-to-list 'mode-line-misc-info 'ssh-lighter)
;;
;; This will display an indicator like "SSH" or "SSH:hostname" in the mode
;; line when running in an SSH session (detected via environment variables
;; such as SSH_TTY).
;;
;; To include the short hostname in the indicator:
;;
;;   (setq ssh-lighter-show-hostname t)
;;
;; To customize the faces used for selected and unselected windows:
;;
;;   (set-face-attribute 'ssh-lighter-selected nil
;;                       :foreground "gold" :background "gray40")
;;   (set-face-attribute 'ssh-lighter-unselected nil
;;                       :foreground "gold")

;;; Code:

(defgroup ssh-lighter nil
  "Mode line indicator for SSH sessions."
  :group 'mode-line)

(defface ssh-lighter-selected
  '((t :foreground "gold" :background "gray40"))
  "Face for SSH mode line indicator when window is selected."
  :group 'ssh-lighter)

(defface ssh-lighter-unselected
  '((t :foreground "gold"))
  "Face for SSH mode line indicator when window is unselected."
  :group 'ssh-lighter)

(defcustom ssh-lighter-show-hostname nil
  "Non-nil means include the short hostname in the SSH mode line indicator."
  :type 'boolean
  :group 'ssh-lighter)

(defun ssh-lighter--in-ssh-p ()
  "Return non-nil if running in an SSH session."
  (or (getenv "SSH_TTY")
      (getenv "SSH_CLIENT")
      (getenv "SSH_CONNECTION")))

(defun ssh-lighter--short-hostname ()
  "Return the short system hostname."
  (let ((host (system-name)))
    (if (stringp host)
        (car (split-string host "\\."))
      "")))

(defun ssh-lighter--face ()
  "Return the appropriate face for the SSH mode line indicator."
  (if (if (fboundp 'mode-line-window-selected-p)
          (mode-line-window-selected-p)
        (eq (selected-window) (get-buffer-window)))
      'ssh-lighter-selected
    'ssh-lighter-unselected))

(defun ssh-lighter--eval ()
  "Return the formatted SSH indicator for the mode line, or nil if not in SSH."
  (when (ssh-lighter--in-ssh-p)
    (let ((text (if ssh-lighter-show-hostname
                    (format "SSH:%s" (ssh-lighter--short-hostname))
                  "SSH")))
      (concat (propertize text 'face (ssh-lighter--face)) " "))))

;;;###autoload
(defvar ssh-lighter
  '(:eval (ssh-lighter--eval))
  "Mode line construct for the SSH indicator.

Add this symbol to `mode-line-misc-info' or `mode-line-format' to display
the SSH indicator when running in an SSH session.  For example:

  (add-to-list \\='mode-line-misc-info \\='ssh-lighter)")
(put 'ssh-lighter 'risky-local-variable t)

(provide 'ssh-lighter)
;;; ssh-lighter.el ends here
