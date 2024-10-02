;;; eyaml-test.el --- eyaml tests                    -*- lexical-binding: t; -*-

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

;; eyaml tests.

;;; Code:

(require 'ert)
(require 'eyaml)

(defconst eyaml--pkgdir (file-name-directory load-file-name))

(ert-deftest eyaml/encrypt-and-decrypt-region/string ()
  (skip-unless (executable-find "eyaml"))
  (let ((eyaml-private-key
         (expand-file-name "keys/private_key.pkcs7.pem"
                           eyaml--pkgdir))
        (eyaml-public-key
         (expand-file-name "keys/public_key.pkcs7.pem"
                           eyaml--pkgdir)))
   (with-temp-buffer
    (insert "foo bar")
    (eyaml-encrypt-region (point-min) (point-max))
    (eyaml-decrypt-region (point-min) (point-max))
    (should (string= (buffer-string) "foo bar")))))

(ert-deftest eyaml/encrypt-and-decrypt-region/block ()
  (skip-unless (executable-find "eyaml"))
  (let ((eyaml-private-key
         (expand-file-name "keys/private_key.pkcs7.pem"
                           eyaml--pkgdir))
        (eyaml-public-key
         (expand-file-name "keys/public_key.pkcs7.pem"
                           eyaml--pkgdir)))
   (with-temp-buffer
    (insert "foo bar")
    (eyaml-encrypt-region (point-min) (point-max) t)
    (eyaml-decrypt-region (point-min) (point-max))
    (should (string= (buffer-string) "foo bar")))))

(provide 'eyaml-test)
;;; eyaml-test.el ends here
