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

(defun jakuri-gptel--parse-mcp-json ()
  "Parse MCP JSON config from current buffer starting at point.

Move point after the end of the value.

Format is specified in URL
`https://gofastmcp.com/integrations/mcp-json-configuration'.

The return value is suitable for `mcp-hub-servers'.
"
  (let* ((data (json-parse-buffer :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil))
         (mcp-servers (cdr (assoc 'mcpServers data)))
         result)
    (dolist (server mcp-servers (nreverse result))
      (let* ((name (symbol-name (car server)))
             (config (cdr server))
             (command (cdr (assoc 'command config)))
             (args (cdr (assoc 'args config)))
             (env (cdr (assoc 'env config)))
             plist)
        (when command (setq plist (plist-put plist :command command)))
        (when args (setq plist (plist-put plist :args args)))
        (when env
          (let (env-plist)
            (dolist (kv env)
              (setq env-plist (plist-put env-plist
                                         (intern (concat ":" (symbol-name (car kv))))
                                         (cdr kv))))
            (setq plist (plist-put plist :env env-plist))))
        (push (cons name plist) result)))))

;;;###autoload
(defun jakuri-gptel-load-mcp-json (file)
  "Load MCP servers from JSON FILE and return a value for `mcp-hub-servers'.

Format is specified in URL
`https://gofastmcp.com/integrations/mcp-json-configuration'."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (jakuri-gptel--parse-mcp-json)))

(provide 'jakuri-gptel)
;;; jakuri-gptel.el ends here
