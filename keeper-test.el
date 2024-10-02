;;; keeper-test.el --- keeper.el tests               -*- lexical-binding: t; -*-

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

;; keeper.el tests

;;; Code:

(require 'keeper)
(require 'ert)

(ert-deftest keeper--account-parts ()
  (should (equal (keeper--account-parts "Foo:Bar:Baz") '("Foo" "Bar" "Baz"))))

(ert-deftest keeper--account-parts/empty-last ()
  (should (equal (keeper--account-parts "Foo:Bar:") '("Foo" "Bar" ""))))

(ert-deftest keeper--complete-part ()
  (should (keeper-test--hash-equal
           (keeper--complete-part
            '()
            #s(hash-table test equal data ("AL:IJN:Ayanami" t "AL:USS:Laffey" t)))
           '("AL"))))

(ert-deftest keeper--complete-part/subsequent-part ()
  (should (keeper-test--hash-equal
           (keeper--complete-part
            '("AL")
            #s(hash-table test equal data ("AL:IJN:Ayanami" t "AL:USS:Laffey" t)))
           '("IJN" "USS"))))

(ert-deftest keeper--buffer-accounts ()
  (with-temp-buffer
    (insert "AL:IJN:Ayanami
AL:USS:Laffey
USD
unit tx
")
    (should (keeper-test--hash-equal (keeper--buffer-accounts)
                                     '("AL:IJN:Ayanami" "AL:USS:Laffey")))))

(defun keeper-test--hash-equal (table items)
  "Return non-nil if the keys of TABLE is exactly ITEMS."
  (and (= (hash-table-count table)
          (length items))
       (catch 'ret
         (dolist (i items)
           (unless (gethash i table)
             (throw 'ret nil)))
         t)))

(provide 'keeper-test)
;;; keeper-test.el ends here
