;;; orgx.el --- Org extensions                       -*- lexical-binding: t; -*-

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

;; Org extensions.

;;; Code:

(require 'org)
(eval-when-compile (require 'subr-x))

(defsubst orgx-current-level ()
  "Return the logical level of the current entry (ignoring `org-odd-levels')."
  (let ((lv (org-current-level)))
    (when lv (org-reduced-level lv))))

;;;###autoload
(defun orgx-promote-children ()
  "Promote all children of entry."
  (interactive)
  (org-map-entries #'org-promote
                   (format "LEVEL>=%s" (1+ (orgx-current-level)))
                   'tree))

;;;###autoload
(defun orgx-raise-to-top ()
  "Raise current entry before all of its siblings."
  (interactive)
  (org-preserve-local-variables
   (let ((ins-point (make-marker))
	 (col (current-column))
	 beg end txt folded)
     ;; Select the tree
     (org-back-to-heading)
     (setq beg (point))
     (save-match-data
       (save-excursion (outline-end-of-heading)
		       (setq folded (org-invisible-p)))
       (progn (org-end-of-subtree nil t)
	      (unless (eobp) (backward-char))))
     (outline-next-heading)
     (setq end (point))
     ;; Push mark here.
     (push-mark end)
     (goto-char beg)
     ;; Find insertion point
     (let (p)
       (while (and (setq p (org-get-previous-sibling)) (looking-at org-outline-regexp))
         (move-marker ins-point p)))
     (goto-char ins-point)
     (setq txt (buffer-substring beg end))
     (org-save-markers-in-region beg end)
     (delete-region beg end)
     (org-remove-empty-overlays-at beg)
     (unless (= beg (point-min)) (org-fold-region (1- beg) beg nil 'outline))
     (unless (bobp) (org-fold-region (1- (point)) (point) nil 'outline))
     (and (not (bolp)) (looking-at "\n") (forward-char 1))
     (let ((bbb (point)))
       (insert-before-markers txt)
       (org-reinstall-markers-in-region bbb)
       (move-marker ins-point bbb))
     (or (bolp) (insert "\n"))
     (goto-char ins-point)
     (org-skip-whitespace)
     (if folded
	 (org-fold-subtree t)
       (org-fold-show-entry)
       (org-fold-show-children))
     (org-clean-visibility-after-subtree-move)
     ;; move back to the initial column we were at
     (move-to-column col))))

;;;###autoload
(defun orgx-uniq ()
  "Remove duplicate subheadings, preserving order."
  (interactive)
  (let ((seen (make-hash-table :test 'equal))
        (removed 0))
    (org-map-entries
     (lambda ()
       (let ((heading (org-get-heading t t t t)))
         (if (not (gethash heading seen))
             (puthash heading t seen)
           (org-cut-subtree)
           (org-backward-heading-same-level 1)
           (setq removed (1+ removed)))))
     (format "LEVEL=%s" (1+ (orgx-current-level)))
     'tree)
    (message "Removed %d duplicates" removed)))

;;;###autoload
(defun orgx-pull-child-tags ()
  "Pull tags from child headings."
  (interactive)
  (let (tags)
    (org-map-entries
     (lambda ()
       (dolist (tag (org-get-tags nil t))
         (push tag tags)))
     nil 'tree)
    (org-set-tags (delete-dups (nreverse tags)))))

;;;###autoload
(defun orgx-uniq-merge-tags (&optional print-message)
  "Remove duplicate subheadings, preserving order, and merge tags.
If PRINT-MESSAGE is non-nil, the number of duplicate headings merged
is printed.  The value is the number of duplicate headings merged."
  (interactive "p")
  (let ((seen (make-hash-table :test 'equal))
        (tags (make-hash-table :test 'equal))
        (merged 0)
        (level (1+ (orgx-current-level))))
    (org-map-entries
     (lambda ()
       (let ((heading (org-get-heading t t t t)))
         (puthash heading (append (gethash heading tags)
                                  (org-get-tags nil t))
                  tags)
         (if (not (gethash heading seen))
             (puthash heading t seen)
           (org-cut-subtree)
           (when (org-at-heading-p)
             (setq org-map-continue-from
                   (save-excursion
                     (org-backward-heading-same-level 1)
                     (end-of-line)
                     (point))))
           (setq merged (1+ merged)))))
     (format "LEVEL=%s" level) 'tree)
    (org-map-entries
     (lambda ()
       (let ((heading (org-get-heading t t t t)))
         (org-set-tags (delete-dups (gethash heading tags)))))
     (format "LEVEL=%s" level) 'tree)
    (when print-message
      (message "Merged %d duplicates" merged))
    merged))

;;;###autoload
(defun orgx-add-id ()
  "Add and kill ID to Org entry."
  (interactive)
  (kill-new (org-id-get-create)))

;;;###autoload
(defun orgx-archive-closed-older-than (datespec)
  "Archive entries older than DATESPEC."
  (interactive (list (org-read-date nil nil nil "Archive older than"
                                    (org-read-date nil t "-2w"))))
  (org-map-entries (lambda ()
                     (org-archive-subtree-default)
                     (setq org-map-continue-from (max (point-min) (1- (point)))))
                   (format "LEVEL=%s+CLOSED<\"<%s>\"" (1+ (or (orgx-current-level) 0)) datespec)
                   (when (orgx-current-level) 'tree)))

(provide 'orgx)
;;; orgx.el ends here
