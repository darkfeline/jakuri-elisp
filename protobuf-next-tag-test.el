;;; protobuf-next-tag-test.el --- Tests              -*- lexical-binding: t; -*-

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

;; tests

;;; Code:

(require 'ert)
(require 'protobuf-next-tag)

(defmacro protobuf-next-tag-test--with-buffer (text &rest body)
  "Set up a test buffer.
TEXT is the buffer text.  \"@\" marks point.
BODY is then evaluated."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (protobuf-mode)
     (insert ,text)
     (search-backward "@" nil)
     (delete-char 1)
     ,@body))

(ert-deftest protobuf-next-tag--max-tag/simple-v2 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  required int foo = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/simple-v3 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  int foo = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/with-next-tag-v2 ()
  (protobuf-next-tag-test--with-buffer
      "\
// NEXT TAG: 5
message Foo {
  required int foo = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/with-next-tag-v3 ()
  (protobuf-next-tag-test--with-buffer
      "\
// NEXT TAG: 5
message Foo {
  int foo = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/underscore-field-v2 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  required int foo_bar = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/underscore-field-v3 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  int foo_bar = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/dotted-type-v2 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  required ijn.Ayanami foo = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/dotted-type-v3 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  ijn.Ayanami foo = 1;@
}"
    (should (= (protobuf-next-tag--max-tag) 1))))

(ert-deftest protobuf-next-tag--max-tag/comments-v2 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  // some comment
  required int foo = 1;@
  // some comment
  required int foo = 2;
}"
    (should (= (protobuf-next-tag--max-tag) 2))))

(ert-deftest protobuf-next-tag--max-tag/comments-v3 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  // some comment
  int foo = 1;@
  // some comment
  int foo = 2;
}"
    (should (= (protobuf-next-tag--max-tag) 2))))

(ert-deftest protobuf-next-tag--max-tag/skipping-numbers-v2 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  required int foo = 1;@
  required int foo = 4;
}"
    (should (= (protobuf-next-tag--max-tag) 4))))

(ert-deftest protobuf-next-tag--max-tag/skipping-numbers-v3 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
   foo = 1;@
  int foo = 4;
}"
    (should (= (protobuf-next-tag--max-tag) 4))))

(ert-deftest protobuf-next-tag--max-tag/empty-v2 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
@
}"
    (should (= (protobuf-next-tag--max-tag) 0))))

(ert-deftest protobuf-next-tag--max-tag/empty-v3 ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
@
}"
    (should (= (protobuf-next-tag--max-tag) 0))))

(ert-deftest protobuf-next-tag--max-tag/enum ()
  (protobuf-next-tag-test--with-buffer
      "\
enum Foo {
SOME_VALUE = 5;@
}"
    (should (= (protobuf-next-tag--max-tag) 5))))

(ert-deftest protobuf-next-tag--max-tag/enum-empty ()
  (protobuf-next-tag-test--with-buffer
      "\
enum Foo {
@
}"
    (should (= (protobuf-next-tag--max-tag) -1))))

(ert-deftest protobuf-next-tag--max-tag/oneof-max-inside ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  int foo = 3;
  oneof bar {
    int laffey = 5;
    int ayanami =@
  }
}"
    (should (= (protobuf-next-tag--max-tag) 5))))

(ert-deftest protobuf-next-tag--max-tag/oneof-max-inside ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  int foo = 5;
  oneof bar {
    int laffey = 3;
    int ayanami =@
  }
}"
    (should (= (protobuf-next-tag--max-tag) 5))))

(ert-deftest protobuf-next-tag--max-tag/map ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  map<string, Foo> foo = 5;
  int ayanami =@
}"
    (should (= (protobuf-next-tag--max-tag) 5))))

(ert-deftest protobuf-next-tag--max-tag/nested ()
  (protobuf-next-tag-test--with-buffer
      "\
message Foo {
  int foo = 3;
  int ayanami =@
  message Nahida {
    int ion = 5;
  }
}"
    (should (= (protobuf-next-tag--max-tag) 3))))

(provide 'protobuf-next-tag-test)
;;; protobuf-next-tag-test.el ends here
