;;; gox-test.el --- gox tests                        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Allen Li

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

;; gox tests

;;; Code:

(require 'ert)
(require 'gox)

(ert-deftest gox-rewrite-buffer-package-name ()
  (with-temp-buffer
    (insert "// License

// Package blah blah
package blah

func main() {}
")
    (gox-rewrite-buffer-package-name "ayanami")
    (should (string= (buffer-string) "// License

// Package blah blah
package ayanami

func main() {}
"))))

(provide 'gox-test)
;;; gox-test.el ends here
