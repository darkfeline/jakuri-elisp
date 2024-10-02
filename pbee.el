;;; pbee.el --- Mode for Python bees      -*- lexical-binding: t; -*-

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

;; Minor mode for programming bees

;;; Code:

(require 'python)

(defgroup pbee nil
  "Customization group for pbee."
  :group 'local)

(defvar-local pbee-masking-enabled nil
  "Whether Python Bee masking is enabled.")

(defface pbee-masked '((default :weight bold :background "gray"))
  "Face for masking in `pbee-mode'.")

(defun pbee-enable-masking (&optional arg)
  "Enables masking.
Prefix ARG enables if positive, disables if negative, toggles if nil."
  (interactive (list (or current-prefix-arg 'toggle)))
  (cond
   ;; Toggle
   ((eq arg 'toggle)
    (if pbee-masking-enabled
        (pbee-enable-masking -1)
      (pbee-enable-masking 1)))
   ;; Enable
   ((or (null arg) (> arg 0))
    (setq pbee-masking-enabled t)
    (setq mode-name "PBee[Masked]")
    (font-lock-mode -1))
   ;; Disable
   ((< arg 0)
    (setq pbee-masking-enabled nil)
    (setq mode-name "PBee")
    (font-lock-mode 1)
    (remove-text-properties (point-min) (point-max) '(display nil face nil)))
   (t (error "Invalid argument %S" arg))))

(defvar pbee-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-m] #'pbee-enable-masking)
    map))

;;;###autoload
(define-derived-mode pbee-mode
  python-mode "PBee"
  "Python Bee mode"
  (add-hook 'post-command-hook #'pbee-post-command nil t))

(defun pbee-post-command ()
  "Runs in `post-command-hook'."
  (when pbee-masking-enabled
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (pbee--mask-line)
        (forward-line 1)))
    (pbee--reveal-line-end)))

(defun pbee--mask-line ()
  "Mask current line."
  (let ((beg (save-excursion (move-beginning-of-line 1) (point)))
        (end (save-excursion (move-end-of-line 1) (point))))
    (put-text-property beg end 'display (list (make-string 12 ?X)))
    (put-text-property beg end 'face 'pbee-masked)))

(defun pbee--reveal-line-end ()
  "Mask all but last char in current line."
  (let ((beg (save-excursion (move-beginning-of-line 1) (point)))
        (end (save-excursion (move-end-of-line 1) (point))))
    (when (< beg end)
      (remove-text-properties beg end '(display nil face nil))
      (put-text-property beg (1- end) 'display (list (make-string 11 ?X)))
      (put-text-property beg (1- end) 'face 'pbee-masked))))

(provide 'pbee)
;;; pbee.el ends here
