;;; keeper.el --- keeper file support                -*- lexical-binding: t; -*-

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

;; keeper file support

;;; Code:

(defgroup keeper nil
  "Customization group for keeper."
  :group 'files)


(defconst keeper--lower "abcdefghijklmnopqrstuvwxyz")
(defconst keeper--upper (upcase keeper--lower))
(defconst keeper--letters (concat keeper--lower keeper--upper))
(defconst keeper--digits "0123456789")
(defconst keeper--account-part-chars (concat keeper--letters keeper--digits "_"))
(defconst keeper--account-chars (concat keeper--account-part-chars ":"))

(defconst keeper--account-pattern
  (rx upper (0+ (any alnum "_")) ":" (0+ (any alnum ":_"))))
(defconst keeper--date-pattern
  (rx (1+ num) "-" (1+ num) "-" (1+ num)))
(defconst keeper--unit-pattern
  (rx (1+ upper)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kpr\\'". keeper-mode))

(defconst keeper--entry-keywords-pattern
  (regexp-opt '("unit" "balance" "tx" "disable" "account" "treebal") 'words))

(defconst keeper--end-keyword-pattern
  "\\bend\\b")

(defvar keeper--keywords
  `((,keeper--end-keyword-pattern . 'keeper-end)
    (,keeper--entry-keywords-pattern . 'keeper-keyword)
    (,keeper--account-pattern . 'keeper-account)
    (,keeper--unit-pattern . 'keeper-unit)))

(defvar keeper--syntax
  '(("#" . "<")
    ("\n" . ">")))

(defvar keeper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-c] #'keeper-copy-entry)
    map))

;;;###autoload
(define-derived-mode keeper-mode prog-mode "Keeper"
  "Major mode for editing keeper files."
  (setq font-lock-defaults `(keeper--keywords nil nil ,keeper--syntax)
        comment-start "#")
  (setq-local completion-at-point-functions '(keeper--complete)))

(defun keeper--forward-end (&optional arg)
  (let ((arg (if arg arg 1)))
    (dotimes (_ arg)
      (re-search-forward keeper--end-keyword-pattern))))

(defun keeper--backward-entry (&optional arg)
  (let ((arg (if arg arg 1)))
    (dotimes (_ arg)
      (re-search-backward keeper--entry-keywords-pattern))))

;;;###autoload
(defun keeper-copy-entry ()
  "Copy current keeper entry."
  (interactive)
  (let ((text (save-excursion
                (keeper--backward-entry)
                (let ((beg (point)))
                  (keeper--forward-end)
                  (buffer-substring-no-properties beg (point))))))
    (goto-char (point-max))
    (save-excursion (insert text))
    (save-excursion
      (when (re-search-forward keeper--date-pattern nil t)
        (toki-update-date-at-point)))))


;;; Completion
(defun keeper--complete ()
  "Completion at point function."
  (let ((beg (save-excursion
               (skip-chars-backward keeper--account-part-chars)
               (point)))
        (end (save-excursion
               (skip-chars-forward keeper--account-part-chars)
               (point)))
        (prefix (keeper--account-parts
                 (buffer-substring-no-properties
                  (save-excursion
                    (skip-chars-backward keeper--account-chars)
                    (point))
                  (point)))))
    (list beg end
          (completion-table-dynamic
           (lambda (_)
             (let ((candidates (keeper--complete-part (butlast prefix) (keeper--buffer-accounts))))
               (remhash (car (last prefix)) candidates)
               candidates))))))

(defun keeper--account-parts (account)
  "Return the parts of ACCOUNT."
  (split-string account ":"))

(defun keeper--complete-part (prefix accounts)
  "Return hash table for completing PREFIX with ACCOUNTS.
PREFIX is returned by `keeper--account-parts'."
  (let ((candidates (make-hash-table :test 'equal)))
    (maphash (lambda (k _)
               (let ((q prefix))
                 (catch 'break
                   (dolist (p (keeper--account-parts k))
                     (cond
                      ((null q)
                       (puthash p t candidates)
                       (throw 'break nil))
                      ((string= p (car q)) (setq q (cdr q)))
                      (t (throw 'break nil)))))))
             accounts)
    candidates))

(defun keeper--buffer-accounts (&optional disabled)
  "Return a hash table of all of the accounts in the current buffer.
If DISABLED is non-nil, also include disabled accounts."
  (let ((accounts (make-hash-table :test 'equal)))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp keeper--account-pattern nil t)
          (puthash (match-string-no-properties 0) t accounts))
        (unless disabled
          (goto-char (point-min))
          (while (search-forward-regexp (concat "disable"
                                                " +"
                                                keeper--date-pattern
                                                " +"
                                                "\\("
                                                keeper--account-pattern
                                                "\\)")
                                        nil t)
            (remhash (match-string-no-properties 1) accounts)))))
    accounts))


;; Faces

(defface keeper-keyword '((default :foreground "cyan" :weight extra-bold))
  "Face used for \".\" in `keeper-mode'.")

(defface keeper-end '((default :foreground "violet" :weight extra-bold))
  "Face used for \".\" in `keeper-mode'.")

(defface keeper-account '((default :foreground "pale green"))
  "Face used for accounts in `keeper-mode'.")

(defface keeper-unit '((default :foreground "deep sky blue"))
  "Face used for accounts in `keeper-mode'.")

(provide 'keeper)
;;; keeper.el ends here
