;;; jakuri-gptel.el --- gptel additions (tools, presets, etc)  -*- lexical-binding: t; -*-

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

(defun jakuri-gptel--obsidian (callback args)
  "Run obsidian-cli command with ARGS."
  (let* ((output-buffer (generate-new-buffer " *gptel-agent-obsidian*"))
         (proc (make-process
                :name "gptel-agent-obsidian"
                :buffer output-buffer
                :command (append (list "obsidian") args nil)
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
 :description "Run obsidian CLI command. Prefer using this over any shell tools to make user confirmation easier."
 :function #'jakuri-gptel--obsidian
 :args '(( :name "args"
           :type array
           :items
           ( :type string
             :description "Argument to obsidian command.  This is NOT interpreted by a shell.")))
 :category "jakuri"
 :async t
 :confirm t)

;;;###autoload
(defun jakuri-gptel-load-mcp-json (file)
  "Load MCP servers from JSON FILE and return a value for `mcp-hub-servers'."
  (let* ((data (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist
                                    :array-type 'list
                                    :null-object nil
                                    :false-object nil)))
         (mcp-servers (cdr (assoc 'mcpServers data)))
         result)
    (dolist (server mcp-servers (nreverse result))
      (let* ((name (symbol-name (car server)))
             (config (cdr server))
             (command (cdr (assoc 'command config)))
             (args (cdr (assoc 'args config)))
             (url (cdr (assoc 'url config)))
             (env (cdr (assoc 'env config)))
             (token (cdr (assoc 'token config)))
             (headers (cdr (assoc 'headers config)))
             (roots (cdr (assoc 'roots config)))
             (timeout (cdr (assoc 'timeout config)))
             plist)
        (when command (setq plist (plist-put plist :command command)))
        (when args (setq plist (plist-put plist :args args)))
        (when url (setq plist (plist-put plist :url url)))
        (when env
          (let (env-plist)
            (dolist (kv env)
              (setq env-plist (plist-put env-plist
                                         (intern (concat ":" (symbol-name (car kv))))
                                         (cdr kv))))
            (setq plist (plist-put plist :env env-plist))))
        (when token (setq plist (plist-put plist :token token)))
        (when headers
          (let (headers-alist)
            (dolist (kv headers)
              (push (cons (symbol-name (car kv)) (cdr kv)) headers-alist))
            (setq plist (plist-put plist :headers (nreverse headers-alist)))))
        (when roots (setq plist (plist-put plist :roots roots)))
        (when timeout (setq plist (plist-put plist :timeout timeout)))
        (push (cons name plist) result)))))

(provide 'jakuri-gptel)
;;; jakuri-gptel.el ends here
