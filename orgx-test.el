;;; orgx-test.el --- orgx tests                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Allen Li

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

;; orgx tests

;;; Code:

(require 'orgx)
(require 'ert)

(ert-deftest orgx-raise-to-top ()
  (with-temp-buffer
    (org-mode)
    (save-excursion (insert "* Casualty
foo
* Top
bar
** ayanami
oni
** unicorn
bad
** laffey
alcohol
* Casualty 2
bar
"))
    (search-forward "laffey")
    (orgx-raise-to-top)
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "* Casualty
foo
* Top
bar
** laffey
alcohol
** ayanami
oni
** unicorn
bad
* Casualty 2
bar
"))))

(ert-deftest orgx-uniq-merge-tags/simple ()
  (let ((org-tags-column 0))
    (with-temp-buffer
      (org-mode)
      (save-excursion (insert "* Top
** foo :foo:
** foo :bar:
"))
      (should (= (orgx-uniq-merge-tags) 1))
      (should (string= (buffer-substring-no-properties (point-min) (point-max))
                       "* Top
** foo :foo:bar:
")))))

(ert-deftest orgx-uniq-merge-tags/multiple ()
  (let ((org-tags-column 0))
    (with-temp-buffer
      (org-mode)
      (save-excursion (insert "* Top
** foo :foo:
** spam :spam:
** foo :bar:
** spam :eggs:
"))
      (should (= (orgx-uniq-merge-tags) 2))
      (should (string= (buffer-substring-no-properties (point-min) (point-max))
                       "* Top
** foo :foo:bar:
** spam :spam:eggs:
")))))

(ert-deftest orgx-uniq-merge-tags/multiple-with-unique ()
  (let ((org-tags-column 0))
    (with-temp-buffer
      (org-mode)
      (save-excursion (insert "* Top
** niimi :kms:
** foo :foo:
** spam :spam:
** ayanami :ijn:
** foo :bar:
** spam :eggs:
** foo :baz:
** laffey :uss:
"))
      (should (= (orgx-uniq-merge-tags) 3))
      (should (string= (buffer-substring-no-properties (point-min) (point-max))
                       "* Top
** niimi :kms:
** foo :foo:bar:baz:
** spam :spam:eggs:
** ayanami :ijn:
** laffey :uss:
")))))

(ert-deftest orgx-uniq-merge-tags/dupe-tags ()
  (let ((org-tags-column 0))
    (with-temp-buffer
      (org-mode)
      (save-excursion (insert "* Top
** foo :foo:
** foo :bar:foo:
"))
      (should (= (orgx-uniq-merge-tags) 1))
      (should (string= (buffer-substring-no-properties (point-min) (point-max))
                       "* Top
** foo :foo:bar:
")))))

(provide 'orgx-test)
;;; orgx-test.el ends here
