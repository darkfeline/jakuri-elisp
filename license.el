;;; license.el --- insert licenses            -*- lexical-binding: t; -*-

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

;; Convenient license insertion.

;;; Code:

;;;###autoload
(defun license-insert-copyright ()
  "Insert copyright header."
  (interactive)
  (insert (format "Copyright (C) %s  %s\n" (format-time-string "%Y") (user-full-name))))

(defmacro license--comment-inserted (&rest body)
  "Comment the region between point before and after evaluating BODY."
  (declare (indent 0))
  (let ((beg (gensym "beg")))
   `(let ((,beg (point-marker)))
     ,@body
     (comment-region ,beg (point)))))

;;;###autoload
(defun license-insert-apache ()
  "Insert Apache license header."
  (interactive)
  (license--comment-inserted
    (license-insert-copyright)
    (insert "\n")
    (insert "Licensed under the Apache License, Version 2.0 (the \"License\");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an \"AS IS\" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
")))

;;;###autoload
(defun license-insert-gpl (&optional name)
  "Insert GPL license header for project named NAME."
  (interactive "MProject name: ")
  (license--comment-inserted
    (license-insert-copyright)
    (insert "\n")
    (if (string= name "")
        (setq name "This program")
      (insert (format "This file is part of %s.\n\n" name)))
    (insert (format "%s is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

%s is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with %s.  If not, see <http://www.gnu.org/licenses/>.
" name name name))))

;;;###autoload
(defun license-insert-chromium ()
  "Insert Chromium license header."
  (interactive)
  (license--comment-inserted
   (insert (format "Copyright %s The Chromium Authors
Use of this source code is governed by a BSD-style license that can be
found in the LICENSE file.
" (format-time-string "%Y")))))

(provide 'license)
;;; license.el ends here
