;;; protobuf-next-tag.el --- Find next tag in protobuf  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Allen Li

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

;; Find next tag in protobuf

;;; Code:

;;;###autoload
(defun protobuf-next-tag ()
  "Insert next tag for protobuf."
  (interactive)
  (let ((max (protobuf-next-tag--max-tag)))
    (insert (number-to-string (1+ max)))
    (protobuf-next-tag--set-next-tag (+ 2 max))))

;;;###autoload
(defun protobuf-next-tag-update ()
  "Update \"NEXT TAG:\" for the current message."
  (interactive)
  (unless (protobuf-next-tag--update)
    (user-error "No \"NEXT TAG:\"")))

(defun protobuf-next-tag--update ()
  "Update \"NEXT TAG:\" for the current message.
The value is the next tag as an int if the NEXT TAG cookie is
present, else the value is nil."
  (interactive)
  (let ((max (protobuf-next-tag--max-tag)))
    (protobuf-next-tag--set-next-tag (1+ max))))

(defun protobuf-next-tag--max-tag ()
  "Return the max tag number used in the context at point.
Return 0 if there are no tags yet (-1 for enums)."
  (save-excursion
    (backward-up-list)
    (let ((end (save-excursion (forward-sexp) (point)))
          (decltype (save-excursion (backward-sexp 2) (thing-at-point 'sexp t))))
      (pcase decltype
        ("oneof" (max (protobuf-next-tag--max-tag-in-message)
                      (protobuf-next-tag--max-tag)))
        ("message" (protobuf-next-tag--max-tag-in-message))
        ("enum"
         (let ((max -1))
           (while (re-search-forward (rx bol (0+ blank)
                                         ;; Identifier
                                         (1+ (in alnum "_")) (0+ (in space "\n"))
                                         "=" (0+ (in space "\n"))
                                         (group (1+ num)))
                                     end t)
             (setq max (max max (string-to-number (match-string 1)))))
           max))
        (_ (error "Unknown protobuf declaration type %S" decltype))))))

(defmacro protobuf-next-tag--save-contents (&rest body)
  "Run body and restore current buffer contents afterward."
  (declare (indent 0) (debug (form body)))
  (let ((x (gensym)))
    `(catch ',x
       (atomic-change-group
         ,@body
         (throw ',x nil)))))

(defun protobuf-next-tag--max-tag-in-message ()
  "Returns the max tag in the current message.
Assumes point is at the beginning of the block."
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (save-excursion (forward-sexp) (point)))
      (let ((max 0))
        (protobuf-next-tag--save-contents
          ;; Delete nested messages and enums.
          (save-excursion
            (while (re-search-forward
                    (rx bol (0+ blank)
                        (or "message" "enum") (1+ (in space "\n"))
                        ;; Identifier
                        (1+ (in alnum "_" ".")) (1+ (in space "\n"))
                        "{")
                    nil t)
              (delete-region (point) (save-excursion
                                       (re-search-forward
                                        (rx bol (0+ blank) "}")
                                        nil)))))
          (while (re-search-forward
                  (let* ((type-name '(1+ (in alnum "_" ".")))
                         (type-map `(: "map<" (0+ blank) ,type-name (0+ blank) "," (0+ blank) ,type-name (0+ blank) ">"))
                         (type `(or ,type-name ,type-map)))
                    (rx-to-string `(seq bol (0+ blank)
                                        (or (: "required" (1+ (in space "\n")))
                                            (: "optional" (1+ (in space "\n")))
                                            (: "repeated" (1+ (in space "\n")))
                                            "")
                                        ;; Type
                                        ,type (1+ (in space "\n"))
                                        ;; Identifier
                                        (1+ (in alnum "_")) (0+ (in space "\n"))
                                        "=" (0+ (in space "\n"))
                                        (group (1+ num)))
                                  :nogroup))
                  nil t)
            (setq max (max max (string-to-number (match-string 1))))))
        max))))

(defun protobuf-next-tag--set-next-tag (next)
  "Set \"NEXT TAG:\" comment to NEXT if present.
The value is NEXT if comment is present, else the value is nil."
  (save-excursion
    (backward-up-list)
    (forward-line -1)
    (when (search-forward "NEXT TAG: " (line-end-position) t)
      (kill-line)
      (insert (number-to-string next))
      next)))

;;;###autoload
(defun protobuf-next-tag-finish-field ()
  "Finish a protobuf field after \"type name =\"."
  (insert " ")
  (protobuf-next-tag)
  (insert ";")
  (protobuf-next-tag--update))

(provide 'protobuf-next-tag)
;;; protobuf-next-tag.el ends here
