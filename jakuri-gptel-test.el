;;; jakuri-gptel-test.el --- jakuri-gptel.el tests  -*- lexical-binding: t; -*-

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

;; jakuri-gptel.el tests

;;; Code:

(require 'ert)
(require 'jakuri-gptel)

(defconst jakuri-gptel-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest jakuri-gptel-load-mcp-json ()
  (let* ((test-file (expand-file-name "testdata/mcp-config.json" jakuri-gptel-test--dir))
         (result (jakuri-gptel-load-mcp-json test-file)))
    (should (equal result
                   '(("server1"
                      :command "npx"
                      :args ("-y" "@server/something")
                      :env (:API_KEY "secret")))))))

(ert-deftest jakuri-gptel--parse-mcp-json ()
  (should (equal (with-temp-buffer
                   (insert "{}")
                   (goto-char (point-min))
                   (jakuri-gptel--parse-mcp-json))
                 nil))
  (should (equal (with-temp-buffer
                   (insert "{\"mcpServers\": {\"server1\": {\"command\": \"npx\", \"args\": [\"-y\", \"@server/something\"], \"env\": {\"API_KEY\": \"secret\"}}}}")
                   (goto-char (point-min))
                   (jakuri-gptel--parse-mcp-json))
                 '(("server1"
                    :command "npx"
                    :args ("-y" "@server/something")
                    :env (:API_KEY "secret")))))
  (should (equal (with-temp-buffer
                   (insert "{\"mcpServers\": {\"server2\": {\"command\": \"python\"}}}")
                   (goto-char (point-min))
                   (jakuri-gptel--parse-mcp-json))
                 '(("server2"
                    :command "python")))))

(provide 'jakuri-gptel-test)
;;; jakuri-gptel-test.el ends here
