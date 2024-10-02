;;; python-hanging-indent.el --- Python hanging indent  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: languages, local

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

;; Python hanging indent.

;;; Code:

(require 'python)

(defgroup python-hanging-indent nil
  "Python hanging indent."
  :group 'python)

(defcustom python-hanging-indent-levels 1
  "Number of ‘python-indent-offset’ to use for hanging indentation."
  :type 'integer
  :safe 'integerp)
;;;###autoload
(put 'python-hanging-indent-levels 'safe-local-variable 'integerp)

;;;###autoload
(defun python-hanging-indent--calculate (func)
  "Advice for ‘python-indent--calculate-indentation’.
FUNC is the original function."
  (save-restriction
    (widen)
    (save-excursion
      (pcase (python-indent-context)
        (`(:inside-paren-newline-start . ,start)
         (goto-char start)
         (+ (current-indentation)
            (* python-hanging-indent-levels python-indent-offset)))
        (_ (funcall func))))))

;;;###autoload
(with-eval-after-load 'python
  (advice-add 'python-indent--calculate-indentation :around #'python-hanging-indent--calculate))

(provide 'python-hanging-indent)
;;; python-hanging-indent.el ends here
