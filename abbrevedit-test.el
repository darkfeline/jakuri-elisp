;;; abbrevedit-test.el --- abbrevedit tests          -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Allen Li

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

;; abbrevedit tests

;;; Code:

(require 'abbrevedit)

(ert-deftest abbrevedit-test--table-abbrevs ()
  (let ((global-abbrev-table (make-abbrev-table)))
    (define-abbrev global-abbrev-table "azusa" "is a good girl")
    (should (equal (abbrevedit--table-abbrevs 'global-abbrev-table)
                   '(("azusa" "is a good girl" nil :count 0))))))

(ert-deftest abbrevedit-test--save-entry ()
  (let ((global-abbrev-table (make-abbrev-table)))
    (abbrevedit--save-entry ["global-abbrev-table" "azusa" "is a good girl" "nil" "(:count 0)"])
    (should (equal (read (with-temp-buffer
                           (insert-abbrev-table-description 'global-abbrev-table)
                           (buffer-string)))
                   '()))))

(provide 'abbrevedit-test)
;;; abbrevedit-test.el ends here
