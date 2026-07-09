;;; ssh-lighter-test.el --- ssh-lighter.el tests     -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Allen Li

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

;; ssh-lighter.el tests.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ssh-lighter)

(ert-deftest ssh-lighter-test-not-in-ssh ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "SSH_TTY" nil)
    (setenv "SSH_CLIENT" nil)
    (setenv "SSH_CONNECTION" nil)
    (should-not (ssh-lighter--in-ssh-p))
    (should-not (ssh-lighter--eval))))

(ert-deftest ssh-lighter-test-in-ssh ()
  (let ((process-environment (copy-sequence process-environment))
        (ssh-lighter-show-hostname nil))
    (setenv "SSH_TTY" "/dev/pts/0")
    (should (ssh-lighter--in-ssh-p))
    (let ((result (ssh-lighter--eval)))
      (should (string= result "SSH "))
      (should (eq (get-text-property 0 'face result) 'ssh-lighter-selected))
      (should-not (get-text-property 3 'face result)))))

(ert-deftest ssh-lighter-test-in-ssh-with-hostname ()
  (let ((process-environment (copy-sequence process-environment))
        (ssh-lighter-show-hostname t))
    (setenv "SSH_CLIENT" "10.0.0.1 50000 22")
    (cl-letf (((symbol-function 'system-name) (lambda () "host.example.com")))
      (let ((result (ssh-lighter--eval)))
        (should (string= result "SSH:host "))
        (should (eq (get-text-property 0 'face result) 'ssh-lighter-selected))
        (should-not (get-text-property 8 'face result))))))

(ert-deftest ssh-lighter-test-unselected-face ()
  (let ((process-environment (copy-sequence process-environment))
        (ssh-lighter-show-hostname nil))
    (setenv "SSH_CONNECTION" "10.0.0.1 50000 10.0.0.2 22")
    (cl-letf (((symbol-function 'mode-line-window-selected-p) (lambda () nil)))
      (let ((result (ssh-lighter--eval)))
        (should (eq (get-text-property 0 'face result) 'ssh-lighter-unselected))))))

(provide 'ssh-lighter-test)
;;; ssh-lighter-test.el ends here
