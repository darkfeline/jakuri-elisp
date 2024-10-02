;;; jakuri-test.el --- jakuri.el tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Allen Li

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

;; jakuri.el tests

;;; Code:
(require 'ert)
(require 'jakuri)

(ert-deftest jakuri-test-sort-uniq ()
  (with-temp-buffer
    (insert "spam
eggs
foo
eggs
")
    (jakuri-sort-uniq (point-min) (point-max))
    (should (string= (buffer-string) "eggs
foo
spam
"))))

(ert-deftest jakuri-test-stable-uniq ()
  (with-temp-buffer
    (insert "spam
eggs
foo
eggs
")
    (jakuri-stable-uniq (point-min) (point-max))
    (should (string= (buffer-string) "spam
eggs
foo
"))))

(ert-deftest jakuri-test-stable-uniq/compare-with-newline ()
  (with-temp-buffer
    (insert "spam
eggs
foo
eggs
eggs")
    (jakuri-stable-uniq (point-min) (point-max))
    (should (string= (buffer-string) "spam
eggs
foo
eggs"))))

(provide 'jakuri-test)
;;; jakuri-test.el ends here
