;;; jakuri-gptel-adds.el --- gptel additions (tools, presets, etc)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Allen Li

;; Author: Allen Li <ayatane@google.com>
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

(require 'gptel)

(defun jakuri-gptel-adds--obsidian-cli (callback args)
  "Run obsidian-cli command with ARGS."
  (let* ((output-buffer (generate-new-buffer " *gptel-agent-obsidian*"))
         (proc (make-process
                :name "gptel-agent-obsidian"
                :buffer output-buffer
                :command (append (list "obsidian-cli") args nil)
                :connection-type 'pipe
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (let* ((exit-code (process-exit-status process))
                           (output (with-current-buffer (process-buffer process)
                                     (buffer-string))))
                      (kill-buffer (process-buffer process))
                      (funcall callback
                               (if (zerop exit-code)
                                   output
                                 (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
                                         exit-code output)))))))))
    proc))

(gptel-make-tool
 :name "obsidian-cli"
 :description "Run obsidian-cli command. Prefer using this over any shell tools to make user confirmation easier."
 :function #'jakuri-gptel-adds--obsidian-cli
 :args '(( :name "args"
           :type array
           :items
           ( :type string
             :description "Argument to obsidian-cli command.  This is NOT interpreted by a shell.")))
 :category "jakuri"
 :async t
 :confirm t)

(provide 'jakuri-gptel-adds)
;;; jakuri-gptel-adds.el ends here
