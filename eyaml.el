;;; eyaml.el --- eyaml support                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: tools, local

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

;; Support for encrypting/decrypting eyaml.

;;; Code:

(defvar eyaml-public-key "./keys/public_key.pkcs7.pem"
  "Public key to use for eyaml.")

(defvar eyaml-private-key "./keys/private_key.pkcs7.pem"
  "Private key to use for eyaml.")

;;;###autoload
(defun eyaml-insert-encrypted (string)
  "Insert STRING encrypted for eyaml.
The key to use is passed using ‘eyaml-public-key’."
  (interactive "MString to encrypt: ")
  (insert ">\n")
  (save-restriction
    (narrow-to-region (point) (point))
    (insert string)
    (eyaml-encrypt-region (point-min) (point-max) t)
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun eyaml-encrypt-region (beg end &optional block)
  "Encrypt region between BEG and END.
If BLOCK is non-nil, encrypt in block format.  Otherwise, encrypt
in string format.  When called interactively, BLOCK is non-nil.

The key to use is passed using ‘eyaml-public-key’."
  (interactive "r\np")
  (call-process-region
   beg end "eyaml" t t nil
   "encrypt"
   "--pkcs7-public-key" eyaml-public-key
   "--stdin" "-o" (if block "block" "string")))

;;;###autoload
(defun eyaml-decrypt-region (beg end)
  "Decrypt region between BEG and END.
The key to use is passed using ‘eyaml-private-key’."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]+" nil t)
        (replace-match ""))
      (call-process-region
       (point-min) (point-max) "eyaml" t t nil
       "decrypt"
       "--pkcs7-private-key" eyaml-private-key
       "--pkcs7-public-key" eyaml-public-key
       "--stdin")
      ;; Delete trailing newline
      (delete-char -1))))

(provide 'eyaml)
;;; eyaml.el ends here
