;;; go-autodoc-test.el --- go-autodoc tests          -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'ert)
(require 'go-autodoc)

(defmacro go-autodoc-test--with-buffer (text &rest body)
  "Set up a test buffer for `go-autodoc-test'.
TEXT is the buffer text.  \"@\" marks point.
BODY is then evaluated."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (go-mode)
     (insert ,text)
     (search-backward "@" nil)
     (delete-char 1)
     ,@body))


;;; Simple declarations
(ert-deftest go-autodoc--identifier/var ()
  (go-autodoc-test--with-buffer
   "\
// @
var Foo = 1"
   (should (equal (go-autodoc--identifier) '("var" "Foo")))))

(ert-deftest go-autodoc--identifier/const ()
  (go-autodoc-test--with-buffer
   "\
// @
const Foo = 1"
   (should (equal (go-autodoc--identifier) '("const" "Foo")))))

(ert-deftest go-autodoc--identifier/func ()
  (go-autodoc-test--with-buffer
   "\
// @
func Foo() {"
   (should (equal (go-autodoc--identifier) '("func" "Foo")))))

(ert-deftest go-autodoc--identifier/type ()
  (go-autodoc-test--with-buffer
   "\
// @
type Foo string"
   (should (equal (go-autodoc--identifier) '("type" "Foo")))))

(ert-deftest go-autodoc--identifier/type-alias ()
  (go-autodoc-test--with-buffer
   "\
// @
type Foo = Bar"
   (should (equal (go-autodoc--identifier) '("type" "Foo")))))

(ert-deftest go-autodoc--identifier/method ()
  (go-autodoc-test--with-buffer
   "\
// @
func (b *Bar) Foo() {"
   (should (equal (go-autodoc--identifier) '("func" "Foo")))))


;;; Nested declarations
(ert-deftest go-autodoc--identifier/nested-var ()
  (go-autodoc-test--with-buffer
      "\
var (
	Foo = 1
	// @
	Bar = 2
)"
    (should (equal (go-autodoc--identifier) '("nested" "Bar")))))

(ert-deftest go-autodoc--identifier/nested-const-continuation ()
  (go-autodoc-test--with-buffer
   "\
const (
	Foo = iota
	// @
	Bar
)"
   (should (equal (go-autodoc--identifier) '("nested" "Bar")))))

(ert-deftest go-autodoc--identifier/nested-method ()
  (go-autodoc-test--with-buffer
   "\
type Foo interface (
	Foo()
	// @
	Bar()
)"
   (should (equal (go-autodoc--identifier) '("nested" "Bar")))))

(ert-deftest go-autodoc--identifier/struct-field ()
  (go-autodoc-test--with-buffer
   "\
type Foo struct (
	foo string
	// @
	bar string
)"
   (should (equal (go-autodoc--identifier) '("nested" "bar")))))

(ert-deftest go-autodoc--identifier/struct-field-func ()
  (go-autodoc-test--with-buffer
   "\
type Foo struct (
	foo string
	// @
	bar func(string)
	baz string
)"
   (should (equal (go-autodoc--identifier) '("nested" "bar")))))


;;; Package declarations
(ert-deftest go-autodoc--identifier/package ()
  (go-autodoc-test--with-buffer
   "\
// @
package foo"
   (should (equal (go-autodoc--identifier) '("package" "foo")))))


;;; Misc
(ert-deftest go-autodoc--identifier/multi-line-comment ()
  (go-autodoc-test--with-buffer
      "\
// @
// some stuff
var Foo = 1"
    (should (equal (go-autodoc--identifier) '("var" "Foo")))))

(provide 'go-autodoc-test)
;;; go-autodoc-test.el ends here
