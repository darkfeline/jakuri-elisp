;;; ssh-session-manager.el --- Manage SSH sessions   -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: comm, processes, tools, local

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

;; General-purpose Emacs UI for inspecting and managing incoming and
;; outgoing SSH sessions, including port forwards and ControlMaster
;; multiplexed connections.
;;
;; Entry points:
;;
;;   M-x ssh-session-manager
;;     Open the `*SSH Sessions*' buffer (`tabulated-list-mode') showing
;;     all incoming (`sshd' / `sshd-session') and outgoing (`ssh')
;;     sessions with peer/host info, TTY, health/hung indicators,
;;     uptime, idle time, and port forwards.
;;
;;   M-x ssh-session-manager-list-forwards
;;     Open the `*SSH Port Forwards*' buffer (`tabulated-list-mode')
;;     showing every TCP, Unix socket, X11, and agent forward across
;;     all sessions.
;;
;;   M-x ssh-session-manager-connect
;;     Start a new outgoing ControlMaster SSH session (with optional
;;     port forwards) using completion from `~/.ssh/config'.
;;
;; Key capabilities:
;;
;; - Identify hung or stale incoming connections (TCP Send-Q, active
;;   retransmission or unanswered keepalive timers, closing TCP states,
;;   stuck process states, TTY idle time) and terminate them safely
;;   (with extra protection against killing the current Emacs session).
;; - Inspect port forwards (local `-L', remote `-R', dynamic `-D',
;;   Unix streamlocal sockets, X11, and agent forwarding) across both
;;   incoming and outgoing connections.  Supports optional privileged
;;   socket inspection (`!' to toggle sudo/TRAMP) for exact kernel PID
;;   attribution of non-dumpable incoming `sshd-session' processes.
;; - Dynamically add (`a'), cancel (`r'), and edit (`e') port forwards
;;   on outgoing ControlMaster sessions via `ssh -O forward/cancel',
;;   with automatic rollback if editing a forward fails.
;; - Inspect detailed ControlMaster connection info (`ssh -O conninfo')
;;   and open channels (`ssh -O channels'), check master health (`c'),
;;   stop accepting new clients (`P'), or gracefully exit (`E').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-method "tramp" (cl-x))
(declare-function tramp-file-name-user "tramp" (cl-x))
(declare-function tramp-file-name-host "tramp" (cl-x))


;;; Customization & Faces

(defgroup ssh-session-manager nil
  "Manage incoming and outgoing SSH sessions."
  :group 'processes
  :group 'comm)

(defcustom ssh-session-manager-ssh-program "ssh"
  "Path or name of the OpenSSH client executable."
  :type 'string
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-ps-program "ps"
  "Path or name of the `ps' executable."
  :type 'string
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-ss-program "ss"
  "Path or name of the `ss' executable."
  :type 'string
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-lsof-program "lsof"
  "Path or name of the `lsof' executable (fallback when `ss' is absent)."
  :type 'string
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-show-all-users nil
  "Non-nil means show SSH sessions for all users, not just current user."
  :type 'boolean
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-use-sudo nil
  "Non-nil means run socket inspection with elevated privileges.
On Linux, incoming `sshd-session' processes drop privileges and are
marked non-dumpable (`PR_SET_DUMPABLE=0'), preventing unprivileged
`ss' or `lsof' from mapping their sockets to PIDs.  Enabling this
allows exact per-PID port-forward attribution for incoming sessions."
  :type 'boolean
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-sudo-method 'tramp
  "Method used when `ssh-session-manager-use-sudo' is non-nil.
`tramp' runs socket commands via TRAMP `/sudo::/' (prompting via
Emacs if needed and supporting remote multi-hop buffers).
`sudo' invokes `sudo -n' directly without prompting."
  :type '(choice (const :tag "TRAMP /sudo::/" tramp)
                 (const :tag "Non-interactive sudo -n" sudo))
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-query-ssh-config t
  "Non-nil means query `ssh -G' for configured forwards and control paths.
This reveals `LocalForward', `RemoteForward', `DynamicForward', and
`ControlPath' directives defined in `~/.ssh/config' rather than on the
command line.  Results are cached per host."
  :type 'boolean
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-idle-warning-threshold (* 24 3600)
  "Seconds of TTY idle time before flagging an incoming session as stale.
Set to nil to disable idle-time staleness warnings."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-confirm-kill t
  "Non-nil means prompt for confirmation before signaling sessions.
Killing the current Emacs SSH session always prompts regardless of
this setting."
  :type 'boolean
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-auto-refresh-interval nil
  "Seconds between automatic refreshes in `*SSH Sessions*', or nil."
  :type '(choice (const :tag "Manual refresh only" nil)
                 (number :tag "Seconds"))
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-control-directory "~/.ssh/cm"
  "Default directory for new ControlMaster sockets created by connect.
Used by `ssh-session-manager-connect' when no explicit `ControlPath'
is configured for the destination."
  :type 'directory
  :group 'ssh-session-manager)

(defcustom ssh-session-manager-config-file "~/.ssh/config"
  "Path to the user's OpenSSH client configuration file."
  :type 'file
  :group 'ssh-session-manager)

(defface ssh-session-manager-incoming
  '((t :inherit font-lock-type-face))
  "Face for incoming SSH session indicators."
  :group 'ssh-session-manager)

(defface ssh-session-manager-current
  '((t :inherit font-lock-type-face :weight bold :underline t))
  "Face for the incoming SSH session hosting the current Emacs."
  :group 'ssh-session-manager)

(defface ssh-session-manager-outgoing
  '((t :inherit success))
  "Face for outgoing SSH session indicators."
  :group 'ssh-session-manager)

(defface ssh-session-manager-master
  '((t :inherit success :weight bold))
  "Face for outgoing ControlMaster SSH session indicators."
  :group 'ssh-session-manager)

(defface ssh-session-manager-hung
  '((t :inherit error :weight bold))
  "Face for hung or unhealthy SSH session status indicators."
  :group 'ssh-session-manager)

(defface ssh-session-manager-stale
  '((t :inherit warning))
  "Face for stale/long-idle SSH session status indicators."
  :group 'ssh-session-manager)

(defface ssh-session-manager-forward-local
  '((t :inherit font-lock-function-name-face))
  "Face for local (`-L') port forwards."
  :group 'ssh-session-manager)

(defface ssh-session-manager-forward-remote
  '((t :inherit font-lock-constant-face))
  "Face for remote (`-R') port forwards."
  :group 'ssh-session-manager)

(defface ssh-session-manager-forward-dynamic
  '((t :inherit font-lock-builtin-face))
  "Face for dynamic (`-D') SOCKS port forwards."
  :group 'ssh-session-manager)

(defface ssh-session-manager-forward-other
  '((t :inherit shadow))
  "Face for X11, agent, and active forwarded connections."
  :group 'ssh-session-manager)

(defface ssh-session-manager-section-heading
  '((t :inherit bold :height 1.05))
  "Face for section headings in the session inspect buffer."
  :group 'ssh-session-manager)

(defface ssh-session-manager-label
  '((t :inherit font-lock-keyword-face))
  "Face for field labels in the session inspect buffer."
  :group 'ssh-session-manager)


;;; Data Structures

(cl-defstruct (ssh-session-manager-forward
               (:constructor ssh-session-manager-forward-create)
               (:copier ssh-session-manager-forward-copy))
  "Description of a port or socket forward on an SSH session."
  type         ; local, remote, dynamic, x11, agent, active
  proto        ; tcp or unix
  bind-addr    ; string or nil (host/IP or local/remote unix socket path)
  bind-port    ; integer or nil
  target-host  ; string or nil (host/IP or target unix socket path)
  target-port  ; integer or nil
  spec         ; canonical OpenSSH -L/-R/-D spec string, or nil
  source       ; cli, config, ss, runtime, environ, inferred
  active-p)    ; non-nil if confirmed listening/active via socket scan

(cl-defstruct (ssh-session-manager-session
               (:constructor ssh-session-manager-session-create)
               (:copier ssh-session-manager-session-copy))
  "Description of an active incoming or outgoing SSH session."
  pid           ; integer
  ppid          ; integer
  direction     ; in or out
  role          ; session, notty, sftp, master, persist, client, mux-client, proxy
  user          ; string (OS process owner)
  remote-user   ; string or nil (outgoing login name)
  host          ; string (destination host or incoming client peer)
  port          ; integer or nil (explicit outgoing port)
  peer-addr     ; string or nil (IP:port)
  local-addr    ; string or nil (IP:port)
  tty           ; string or nil (e.g. "pts/0", "notty", "sftp")
  state         ; string (process STAT from ps)
  tcp-state     ; string or nil (ESTAB, CLOSE-WAIT, etc.)
  send-q        ; integer or nil
  recv-q        ; integer or nil
  tcp-timer     ; plist (:kind STR :time STR :retries INT) or nil
  elapsed       ; integer seconds
  idle          ; integer seconds or nil
  control-path  ; string or nil
  forwards      ; list of `ssh-session-manager-forward'
  children      ; list of (PID . COMMAND)
  command       ; string (full command line)
  argv          ; list of strings
  current-p     ; non-nil if session hosts current Emacs
  hung-reasons  ; list of strings (hard hang indicators)
  stale-reasons ; list of strings (soft/idle warnings)
  inferred-p)   ; non-nil if incoming forwards were inferred without sudo

(cl-defstruct (ssh-session-manager--proc
               (:constructor ssh-session-manager--proc-create))
  "Raw process record from `ps'."
  pid ppid user state elapsed tty command argv)

(cl-defstruct (ssh-session-manager--sock
               (:constructor ssh-session-manager--sock-create))
  "Raw socket record from `ss' or `lsof'."
  proto       ; tcp or unix
  state       ; LISTEN, ESTAB, CLOSE-WAIT, etc.
  recv-q      ; integer or nil
  send-q      ; integer or nil
  local-addr  ; IP string or unix path
  local-port  ; integer or nil
  peer-addr   ; IP string or unix path or nil
  peer-port   ; integer or nil
  pids        ; list of integer PIDs from users:(...)
  uid         ; integer or nil
  timer)      ; plist (:kind STR :time STR :retries INT) or nil


;;; State & History Variables

(defvar ssh-session-manager-forward-history nil
  "Minibuffer history for port-forward specifications.")

(defvar ssh-session-manager-host-history nil
  "Minibuffer history for SSH destination hosts.")

(defvar ssh-session-manager--runtime-forwards (make-hash-table :test 'equal)
  "Runtime forward ledger keyed by control-path string.
Each value is a plist `(:added LIST :canceled LIST)' where LIST
elements are `ssh-session-manager-forward' structs (for `:added') or
`(TYPE . SPEC)' cons cells (for `:canceled').")

(defvar ssh-session-manager--ssh-config-cache (make-hash-table :test 'equal)
  "Cache of `ssh -G' results keyed by `(HOST PORT USER . REMOTE-PREFIX)'.")

(defvar-local ssh-session-manager--sessions nil
  "List of `ssh-session-manager-session' objects in the current buffer.")

(defvar-local ssh-session-manager--unattributed-forwards nil
  "List of unattributed user-owned non-dumpable forwards.")

(defvar-local ssh-session-manager--marks nil
  "Alist of `(PID . MARK-CHAR)' for marked rows in `*SSH Sessions*'.")

(defvar-local ssh-session-manager--filter nil
  "Active filter plist in `*SSH Sessions*'.
Supported keys: `:direction', `:forwards-only', `:hung-only',
`:masters-only', `:user', `:regexp'.")

(defvar-local ssh-session-manager--all-users nil
  "Buffer-local override for showing all users' SSH sessions.")

(defvar-local ssh-session-manager--sudo nil
  "Buffer-local override for privileged socket inspection.")

(defvar-local ssh-session-manager--refresh-timer nil
  "Buffer-local auto-refresh timer.")

(defvar-local ssh-session-manager--forward-rows nil
  "List of `(SESSION . FORWARD)' entries in `*SSH Port Forwards*'.")

(defvar-local ssh-session-manager--forwards-filter-pid nil
  "When non-nil, restrict `*SSH Port Forwards*' to this session PID.")

(defvar-local ssh-session-manager--inspect-pid nil
  "PID being inspected in `*SSH Session: ...*'.")

(defvar-local ssh-session-manager--source-buffer nil
  "Originating `*SSH Sessions*' buffer for auxiliary views.")

(defconst ssh-session-manager--buffer-name "*SSH Sessions*"
  "Default buffer name for `ssh-session-manager'.")

(defconst ssh-session-manager--forwards-buffer-name "*SSH Port Forwards*"
  "Default buffer name for `ssh-session-manager-list-forwards'.")


;;; Pure Parsing & Formatting Utilities

(defun ssh-session-manager--parse-etime (str)
  "Parse `ps' elapsed time STR (`[[DD-]HH:]MM:SS') into seconds."
  (save-match-data
    (let ((s (string-trim (or str ""))))
      (cond
       ((string-match
         (rx bos
             (optional (group (+ digit)) "-")
             (optional (group (+ digit)) ":")
             (group (+ digit)) ":"
             (group (+ digit)) eos)
         s)
        (let ((days (if (match-string 1 s) (string-to-number (match-string 1 s)) 0))
              (hours (if (match-string 2 s) (string-to-number (match-string 2 s)) 0))
              (mins (string-to-number (match-string 3 s)))
              (secs (string-to-number (match-string 4 s))))
          (+ (* days 86400) (* hours 3600) (* mins 60) secs)))
       ((string-match-p (rx bos (+ digit) eos) s)
        (string-to-number s))
       (t 0)))))

(defun ssh-session-manager--format-duration (seconds)
  "Format SECONDS as a compact human-readable duration."
  (cond
   ((null seconds) "-")
   ((< seconds 0) "0s")
   ((< seconds 60) (format "%ds" seconds))
   ((< seconds 3600)
    (format "%dm%02ds" (/ seconds 60) (% seconds 60)))
   ((< seconds 86400)
    (format "%dh%02dm" (/ seconds 3600) (/ (% seconds 3600) 60)))
   (t
    (format "%dd%02dh" (/ seconds 86400) (/ (% seconds 86400) 3600)))))

(defun ssh-session-manager--normalize-ip (addr)
  "Normalize IP string ADDR (strip brackets and `::ffff:' prefix)."
  (when addr
    (let ((s (string-trim addr)))
      (when (and (string-prefix-p "[" s) (string-suffix-p "]" s))
        (setq s (substring s 1 -1)))
      (when (string-prefix-p "::ffff:" s t)
        (setq s (substring s 7)))
      s)))

(defun ssh-session-manager--loopback-p (addr)
  "Return non-nil if ADDR denotes a loopback address."
  (let ((norm (ssh-session-manager--normalize-ip addr)))
    (or (null norm)
        (member norm '("127.0.0.1" "::1" "localhost")))))

(defun ssh-session-manager--wildcard-p (addr)
  "Return non-nil if ADDR denotes a wildcard bind address."
  (let ((norm (ssh-session-manager--normalize-ip addr)))
    (member norm '("" "*" "0.0.0.0" "::"))))

(defun ssh-session-manager--format-host-port (host port)
  "Format HOST and PORT as `host:port', bracketing IPv6 addresses."
  (let ((h (ssh-session-manager--normalize-ip host)))
    (cond
     ((and h port)
      (if (string-match-p ":" h)
          (format "[%s]:%d" h port)
        (format "%s:%d" h port)))
     (port (number-to-string port))
     (h h)
     (t ""))))

(defun ssh-session-manager--split-colon-respecting-brackets (str)
  "Split STR on `:' outside square brackets `[...]'."
  (let ((len (length str))
        (i 0)
        (depth 0)
        (start 0)
        parts)
    (while (< i len)
      (let ((ch (aref str i)))
        (cond
         ((eq ch ?\[) (setq depth (1+ depth)))
         ((eq ch ?\]) (setq depth (max 0 (1- depth))))
         ((and (eq ch ?:) (= depth 0))
          (push (substring str start i) parts)
          (setq start (1+ i)))))
      (setq i (1+ i)))
    (push (substring str start len) parts)
    (nreverse parts)))

(defun ssh-session-manager--numeric-port-p (str)
  "Return integer port if STR is a valid TCP port number, else nil."
  (when (and (stringp str) (string-match-p (rx bos (+ digit) eos) str))
    (let ((n (string-to-number str)))
      (when (and (>= n 0) (<= n 65535))
        n))))

(defun ssh-session-manager--unix-path-p (str)
  "Return non-nil if STR represents a Unix socket path."
  (and (stringp str) (string-match-p "/" str)))

(defun ssh-session-manager--normalize-config-forward-spec (type raw-spec)
  "Normalize space-separated `ssh_config' forward RAW-SPEC for TYPE.
For example, `\"127.0.0.1:8080 localhost:80\"' becomes
`\"127.0.0.1:8080:localhost:80\"'."
  (let ((s (string-trim raw-spec)))
    (if (and (memq type '(local remote))
             (string-match (rx bos (group (+ (not space)))
                               (+ space)
                               (group (+ (not space))) eos)
                           s))
        (concat (match-string 1 s) ":" (match-string 2 s))
      s)))

(defun ssh-session-manager--parse-forward-spec (type raw-spec &optional source)
  "Parse an OpenSSH forward RAW-SPEC of TYPE (`local', `remote', `dynamic').
Optional SOURCE tags the resulting `ssh-session-manager-forward'.
Return an `ssh-session-manager-forward' struct, or nil if invalid."
  (let* ((spec (ssh-session-manager--normalize-config-forward-spec
                type (or raw-spec "")))
         (parts (ssh-session-manager--split-colon-respecting-brackets spec)))
    (pcase type
      ('dynamic
       (pcase parts
         (`(,port-s)
          (when-let* ((port (ssh-session-manager--numeric-port-p port-s)))
            (ssh-session-manager-forward-create
             :type 'dynamic :proto 'tcp
             :bind-addr nil :bind-port port
             :spec spec :source source)))
         (`(,addr ,port-s)
          (when-let* ((port (ssh-session-manager--numeric-port-p port-s)))
            (ssh-session-manager-forward-create
             :type 'dynamic :proto 'tcp
             :bind-addr (if (string-empty-p addr)
                            "*"
                          (ssh-session-manager--normalize-ip addr))
             :bind-port port
             :spec spec :source source)))
         (_ nil)))
      ((or 'local 'remote)
       (pcase parts
         ;; [bind:]port:host:hostport (4 parts)
         (`(,baddr ,bport-s ,thost ,tport-s)
          (let ((bport (ssh-session-manager--numeric-port-p bport-s))
                (tport (ssh-session-manager--numeric-port-p tport-s)))
            (when (and bport tport (not (string-empty-p thost)))
              (ssh-session-manager-forward-create
               :type type :proto 'tcp
               :bind-addr (if (string-empty-p baddr)
                              "*"
                            (ssh-session-manager--normalize-ip baddr))
               :bind-port bport
               :target-host (ssh-session-manager--normalize-ip thost)
               :target-port tport
               :spec spec :source source))))
         ;; 3 parts:
         ;;   bind:port:/remote/sock  OR
         ;;   /local/sock:host:port   OR
         ;;   port:host:hostport
         (`(,a ,b ,c)
          (cond
           ((ssh-session-manager--unix-path-p c)
            (when-let* ((bport (ssh-session-manager--numeric-port-p b)))
              (ssh-session-manager-forward-create
               :type type :proto 'tcp
               :bind-addr (if (string-empty-p a)
                              "*"
                            (ssh-session-manager--normalize-ip a))
               :bind-port bport
               :target-host c
               :spec spec :source source)))
           ((ssh-session-manager--unix-path-p a)
            (when-let* ((tport (ssh-session-manager--numeric-port-p c)))
              (ssh-session-manager-forward-create
               :type type :proto 'unix
               :bind-addr a
               :target-host (ssh-session-manager--normalize-ip b)
               :target-port tport
               :spec spec :source source)))
           (t
            (let ((bport (ssh-session-manager--numeric-port-p a))
                  (tport (ssh-session-manager--numeric-port-p c)))
              (when (and bport tport (not (string-empty-p b)))
                (ssh-session-manager-forward-create
                 :type type :proto 'tcp
                 :bind-addr nil :bind-port bport
                 :target-host (ssh-session-manager--normalize-ip b)
                 :target-port tport
                 :spec spec :source source))))))
         ;; 2 parts:
         ;;   /local/sock:/remote/sock  OR
         ;;   port:/remote/sock         OR
         ;;   (-R only) bind:port (remote SOCKS)
         (`(,a ,b)
          (cond
           ((and (ssh-session-manager--unix-path-p a)
                 (ssh-session-manager--unix-path-p b))
            (ssh-session-manager-forward-create
             :type type :proto 'unix
             :bind-addr a :target-host b
             :spec spec :source source))
           ((and (ssh-session-manager--numeric-port-p a)
                 (ssh-session-manager--unix-path-p b))
            (ssh-session-manager-forward-create
             :type type :proto 'tcp
             :bind-addr nil
             :bind-port (ssh-session-manager--numeric-port-p a)
             :target-host b
             :spec spec :source source))
           ((and (eq type 'remote)
                 (ssh-session-manager--numeric-port-p b))
            (ssh-session-manager-forward-create
             :type 'remote :proto 'tcp
             :bind-addr (if (string-empty-p a)
                            "*"
                          (ssh-session-manager--normalize-ip a))
             :bind-port (ssh-session-manager--numeric-port-p b)
             :target-host "SOCKS"
             :spec spec :source source))
           (t nil)))
         ;; 1 part: (-R only) port (remote SOCKS)
         (`(,a)
          (when (and (eq type 'remote)
                     (ssh-session-manager--numeric-port-p a))
            (ssh-session-manager-forward-create
             :type 'remote :proto 'tcp
             :bind-addr nil
             :bind-port (ssh-session-manager--numeric-port-p a)
             :target-host "SOCKS"
             :spec spec :source source)))
         (_ nil)))
      (_ nil))))

(defun ssh-session-manager--forward-bind-display (fwd)
  "Return human-readable bind endpoint string for FWD."
  (pcase (ssh-session-manager-forward-type fwd)
    ('x11 (or (ssh-session-manager-forward-bind-addr fwd) "X11"))
    ('agent (or (ssh-session-manager-forward-bind-addr fwd) "agent"))
    (_
     (if (eq (ssh-session-manager-forward-proto fwd) 'unix)
         (or (ssh-session-manager-forward-bind-addr fwd) "unix")
       (let ((addr (ssh-session-manager-forward-bind-addr fwd))
             (port (ssh-session-manager-forward-bind-port fwd)))
         (cond
          ((and addr port)
           (ssh-session-manager--format-host-port addr port))
          (port (number-to-string port))
          (addr addr)
          (t "?")))))))

(defun ssh-session-manager--forward-target-display (fwd)
  "Return human-readable target endpoint string for FWD."
  (pcase (ssh-session-manager-forward-type fwd)
    ('dynamic "SOCKS")
    ('x11 "X11")
    ('agent "ssh-agent")
    (_
     (let ((host (ssh-session-manager-forward-target-host fwd))
           (port (ssh-session-manager-forward-target-port fwd)))
       (cond
        ((and host port)
         (ssh-session-manager--format-host-port host port))
        (host host)
        (t nil))))))

(defun ssh-session-manager--forward-type-tag (type)
  "Return short display tag for forward TYPE."
  (pcase type
    ('local "L")
    ('remote "R")
    ('dynamic "D")
    ('x11 "X11")
    ('agent "Agent")
    ('active "Act")
    (_ "?")))

(defun ssh-session-manager--forward-face (type)
  "Return display face for forward TYPE."
  (pcase type
    ('local 'ssh-session-manager-forward-local)
    ('remote 'ssh-session-manager-forward-remote)
    ('dynamic 'ssh-session-manager-forward-dynamic)
    (_ 'ssh-session-manager-forward-other)))

(defun ssh-session-manager--forward-summary (fwd)
  "Return concise propertized summary string for FWD."
  (let* ((type (ssh-session-manager-forward-type fwd))
         (tag (ssh-session-manager--forward-type-tag type))
         (inferred (if (eq (ssh-session-manager-forward-source fwd) 'inferred) "?" ""))
         (text
          (pcase type
            ('x11 (format "X11(%s)" (or (ssh-session-manager-forward-bind-addr fwd) "on")))
            ('agent "Agent")
            ('dynamic
             (format "D:%s%s"
                     (ssh-session-manager--forward-bind-display fwd)
                     inferred))
            ('active
             (format "Act->%s"
                     (or (ssh-session-manager--forward-target-display fwd) "?")))
            (_
             (let ((bind (ssh-session-manager--forward-bind-display fwd))
                   (target (ssh-session-manager--forward-target-display fwd)))
               (if target
                   (format "%s:%s->%s%s" tag bind target inferred)
                 (format "%s:%s%s" tag bind inferred)))))))
    (propertize text 'face (ssh-session-manager--forward-face type))))

(defun ssh-session-manager--forward-cli-flag (type)
  "Return the OpenSSH CLI flag (`-L', `-R', `-D') for TYPE."
  (pcase type
    ('local "-L")
    ('remote "-R")
    ('dynamic "-D")
    (_ (user-error "Forward type %s cannot be controlled via ssh -O" type))))

(defun ssh-session-manager--forward-key (fwd)
  "Return comparison key for deduplicating or canceling FWD."
  (list (ssh-session-manager-forward-type fwd)
        (ssh-session-manager-forward-proto fwd)
        (let ((a (ssh-session-manager-forward-bind-addr fwd)))
          (if (ssh-session-manager--loopback-p a) 'loopback a))
        (ssh-session-manager-forward-bind-port fwd)
        (ssh-session-manager-forward-target-host fwd)
        (ssh-session-manager-forward-target-port fwd)))


;;; Parsing `ps`, `ssh` argv, `ss`, `lsof`, and `/proc`

(defun ssh-session-manager--parse-ps-output (output)
  "Parse `ps -ewwo pid=,ppid=,user:32=,stat=,etime=,tty=,args=' OUTPUT.
Return a list of `ssh-session-manager--proc' structs."
  (let (procs)
    (dolist (line (split-string output "\n" t))
      (when (string-match
             (rx bos (* space)
                 (group (+ digit)) (+ space)      ; 1: pid
                 (group (+ digit)) (+ space)      ; 2: ppid
                 (group (+ (not space))) (+ space) ; 3: user
                 (group (+ (not space))) (+ space) ; 4: stat
                 (group (+ (not space))) (+ space) ; 5: etime
                 (group (+ (not space))) (+ space) ; 6: tty
                 (group (+ anything)) eos)        ; 7: args
             line)
        (let ((pid-s (match-string 1 line))
              (ppid-s (match-string 2 line))
              (user (match-string 3 line))
              (state (match-string 4 line))
              (etime (match-string 5 line))
              (tty (match-string 6 line))
              (args (match-string 7 line)))
          (push (ssh-session-manager--proc-create
                 :pid (string-to-number pid-s)
                 :ppid (string-to-number ppid-s)
                 :user user
                 :state state
                 :elapsed (ssh-session-manager--parse-etime etime)
                 :tty (unless (string= tty "?") tty)
                 :command (string-trim-right args))
                procs))))
    (nreverse procs)))

(defconst ssh-session-manager--ssh-opts-with-arg
  "BbcDEeFIiJLlmOoPpQRSWw"
  "Single-character `ssh' options that take an argument.")

(defun ssh-session-manager--parse-ssh-option (opt-str)
  "Parse a `-o' OPT-STR (`Key=Val' or `Key Val') into `(DOWNCASED-KEY . VAL)'."
  (let ((s (string-trim opt-str)))
    (when (string-match
           (rx bos (group (+ (not (any space ?=))))
               (or (seq (* space) ?= (* space))
                   (+ space))
               (group (* anything)) eos)
           s)
      (cons (downcase (match-string 1 s))
            (let ((v (string-trim (match-string 2 s))))
              (if (and (>= (length v) 2)
                       (or (and (string-prefix-p "\"" v) (string-suffix-p "\"" v))
                           (and (string-prefix-p "'" v) (string-suffix-p "'" v))))
                  (substring v 1 -1)
                v))))))

(defun ssh-session-manager--parse-ssh-destination (dest)
  "Parse DEST (`[user@]host' or `ssh://[user@]host[:port]').
Return a plist `(:user USER :host HOST :port PORT)'."
  (cond
   ((null dest) nil)
   ((string-match
     (rx bos "ssh://"
         (optional (group (+ (not (any ?@ ?/)))) "@")
         (group (or (seq "[" (+ (not (any ?\]))) "]")
                    (+ (not (any ?: ?/)))))
         (optional ":" (group (+ digit)))
         (* anything) eos)
     dest)
    (let ((u (match-string 1 dest))
          (h (match-string 2 dest))
          (p (match-string 3 dest)))
      (list :user u
            :host (ssh-session-manager--normalize-ip h)
            :port (when p (string-to-number p)))))
   ((string-match (rx bos (group (+ (not ?@))) "@" (group (+ anything)) eos) dest)
    (let ((u (match-string 1 dest))
          (h (match-string 2 dest)))
      (list :user u
            :host (ssh-session-manager--normalize-ip h)
            :port nil)))
   (t
    (list :user nil :host (ssh-session-manager--normalize-ip dest) :port nil))))

(defun ssh-session-manager--parse-ssh-argv (argv)
  "Parse outgoing `ssh' ARGV list into a plist of extracted parameters.
Returns nil if ARGV is an ephemeral control/query invocation (`-O',
`-G', `-V', `-Q')."
  (let ((args (cdr argv))
        (i 0)
        (len (length (cdr argv)))
        (stop-opts nil)
        (skip nil)
        forwards
        control-path
        remote-user
        port
        proxy-target
        master-flag
        destination
        remote-cmd)
    (while (and (< i len) (not skip))
      (let ((arg (nth i args)))
        (cond
         (stop-opts
          (if destination
              (push arg remote-cmd)
            (setq destination arg)))
         ((string= arg "--")
          (setq stop-opts t))
         ((and (not destination)
               (> (length arg) 1)
               (eq (aref arg 0) ?-))
          (let ((j 1)
                (alen (length arg)))
            (while (and (< j alen) (not skip))
              (let ((ch (aref arg j)))
                (if (cl-find ch ssh-session-manager--ssh-opts-with-arg)
                    (let ((val (if (< (1+ j) alen)
                                   (substring arg (1+ j))
                                 (setq i (1+ i))
                                 (when (< i len) (nth i args)))))
                      (setq j alen)
                      (pcase ch
                        ((or ?O ?Q) (setq skip t))
                        (?L (when-let* ((f (ssh-session-manager--parse-forward-spec
                                            'local val 'cli)))
                              (push f forwards)))
                        (?R (when-let* ((f (ssh-session-manager--parse-forward-spec
                                            'remote val 'cli)))
                              (push f forwards)))
                        (?D (when-let* ((f (ssh-session-manager--parse-forward-spec
                                            'dynamic val 'cli)))
                              (push f forwards)))
                        (?S (unless (member val '("none" ""))
                              (setq control-path val)))
                        (?l (setq remote-user val))
                        (?p (setq port (ssh-session-manager--numeric-port-p val)))
                        (?W (setq proxy-target val))
                        (?o
                         (when-let* ((kv (ssh-session-manager--parse-ssh-option (or val ""))))
                           (pcase (car kv)
                             ("localforward"
                              (when-let* ((f (ssh-session-manager--parse-forward-spec
                                              'local (cdr kv) 'cli)))
                                (push f forwards)))
                             ("remoteforward"
                              (when-let* ((f (ssh-session-manager--parse-forward-spec
                                              'remote (cdr kv) 'cli)))
                                (push f forwards)))
                             ("dynamicforward"
                              (when-let* ((f (ssh-session-manager--parse-forward-spec
                                              'dynamic (cdr kv) 'cli)))
                                (push f forwards)))
                             ("controlpath"
                              (unless (member (downcase (cdr kv)) '("none" ""))
                                (setq control-path (cdr kv))))
                             ("controlmaster"
                              (when (member (downcase (cdr kv))
                                            '("yes" "auto" "ask" "autoask"))
                                (setq master-flag t)))
                             ("user"
                              (unless remote-user (setq remote-user (cdr kv))))
                             ("port"
                              (unless port
                                (setq port (ssh-session-manager--numeric-port-p
                                            (cdr kv))))))))))
                  ;; Boolean option flag.
                  (pcase ch
                    ((or ?G ?V) (setq skip t))
                    (?M (setq master-flag t)))
                  (setq j (1+ j)))))))
         (t
          (if destination
              (push arg remote-cmd)
            (setq destination arg)
            ;; Stop option parsing once destination is seen (OpenSSH behavior).
            (setq stop-opts t)))))
      (setq i (1+ i)))
    (unless skip
      (let* ((dest-info (ssh-session-manager--parse-ssh-destination destination))
             (host (plist-get dest-info :host))
             (ruser (or remote-user (plist-get dest-info :user)))
             (rport (or port (plist-get dest-info :port))))
        (list :host host
              :remote-user ruser
              :port rport
              :control-path control-path
              :master-flag master-flag
              :proxy-target proxy-target
              :forwards (nreverse forwards)
              :remote-cmd (nreverse remote-cmd))))))

(defun ssh-session-manager--classify-proc (proc)
  "Classify raw PROC as an incoming or outgoing SSH session, or nil.
Return a partial `ssh-session-manager-session' struct if PROC is an
SSH session process, otherwise nil."
  (let* ((cmd (ssh-session-manager--proc-command proc))
         (argv (or (ssh-session-manager--proc-argv proc)
                   (split-string-and-unquote cmd)))
         (argv0 (or (car argv) ""))
         (base0 (file-name-nondirectory argv0)))
    (cond
     ;; 1. Incoming OpenSSH session:
     ;;    OpenSSH >= 9.8: "sshd-session: user@pts/0", "sshd-session: user@notty",
     ;;                    "sshd-session: user@internal-sftp", "sshd-session: user"
     ;;    OpenSSH <  9.8: "sshd: user@pts/0", "sshd: user@notty", etc.
     ;;    Skip listeners and privsep monitors ([priv], [postauth], [net], [accepted]).
     ((string-match
       (rx bos (or "sshd-session" "sshd") ": "
           (group (+ (not (any space ?@ ?\[ ?/))))
           (optional "@" (group (+ (not space))))
           (* space) eos)
       cmd)
      (let* ((sess-user (match-string 1 cmd))
             (suffix (match-string 2 cmd))
             (role (cond
                    ((equal suffix "notty") 'notty)
                    ((and suffix (string-match-p "sftp" suffix)) 'sftp)
                    (t 'session)))
             (tty (cond
                   ((and suffix (not (member suffix '("notty")))) suffix)
                   ((equal suffix "notty") "notty")
                   (t (ssh-session-manager--proc-tty proc)))))
        (ssh-session-manager-session-create
         :pid (ssh-session-manager--proc-pid proc)
         :ppid (ssh-session-manager--proc-ppid proc)
         :direction 'in
         :role role
         :user (ssh-session-manager--proc-user proc)
         :remote-user sess-user
         :host "?"
         :tty tty
         :state (ssh-session-manager--proc-state proc)
         :elapsed (ssh-session-manager--proc-elapsed proc)
         :command cmd
         :argv argv)))

     ;; 2. Outgoing ControlPersist background master:
     ;;    "ssh: /path/to/control.sock [mux]"
     ((string-match (rx bos "ssh: " (group (+ anything)) " [mux]" (* space) eos) cmd)
      (let ((ctl (match-string 1 cmd)))
        (ssh-session-manager-session-create
         :pid (ssh-session-manager--proc-pid proc)
         :ppid (ssh-session-manager--proc-ppid proc)
         :direction 'out
         :role 'persist
         :user (ssh-session-manager--proc-user proc)
         :host (file-name-nondirectory ctl)
         :tty "mux"
         :state (ssh-session-manager--proc-state proc)
         :elapsed (ssh-session-manager--proc-elapsed proc)
         :control-path ctl
         :command cmd
         :argv argv)))

     ;; 3. Outgoing ssh client process.
     ((and (string= base0 "ssh")
           ;; Ignore bracketed kernel/zombie strings like "[ssh] <defunct>".
           (not (string-prefix-p "[" cmd)))
      (when-let* ((parsed (ssh-session-manager--parse-ssh-argv argv)))
        (let ((host (plist-get parsed :host))
              (proxy (plist-get parsed :proxy-target)))
          (when (or host (plist-get parsed :control-path))
            (ssh-session-manager-session-create
             :pid (ssh-session-manager--proc-pid proc)
             :ppid (ssh-session-manager--proc-ppid proc)
             :direction 'out
             :role (cond
                    (proxy 'proxy)
                    ((plist-get parsed :master-flag) 'master)
                    (t 'client))
             :user (ssh-session-manager--proc-user proc)
             :remote-user (plist-get parsed :remote-user)
             :host (or host "?")
             :port (plist-get parsed :port)
             :tty (ssh-session-manager--proc-tty proc)
             :state (ssh-session-manager--proc-state proc)
             :elapsed (ssh-session-manager--proc-elapsed proc)
             :control-path (plist-get parsed :control-path)
             :forwards (plist-get parsed :forwards)
             :command cmd
             :argv argv)))))

     (t nil))))

(defun ssh-session-manager--parse-ss-endpoint (endpoint)
  "Split `ss' TCP ENDPOINT (`addr:port') into `(ADDR . PORT-OR-NIL)'."
  (save-match-data
    (when (and endpoint (string-match (rx bos (group (+ anything)) ":" (group (+ (not ?:))) eos)
                                      endpoint))
      (let ((raw-addr (match-string 1 endpoint))
            (port-s (match-string 2 endpoint)))
        (cons (ssh-session-manager--normalize-ip raw-addr)
              (ssh-session-manager--numeric-port-p port-s))))))

(defun ssh-session-manager--parse-ss-pids (line)
  "Extract list of integer PIDs from `users:(...)' in `ss' LINE."
  (save-match-data
    (let ((pos 0)
          pids)
      (while (string-match (rx "pid=" (group (+ digit))) line pos)
        (let ((pid (string-to-number (match-string 1 line))))
          (unless (memq pid pids)
            (push pid pids)))
        (setq pos (match-end 0)))
      (nreverse pids))))

(defun ssh-session-manager--parse-ss-timer (line)
  "Extract `timer:(KIND,TIME,RETRIES)' from `ss' LINE as a plist."
  (save-match-data
    (when (string-match
           (rx "timer:("
               (group (+ (not (any ?, ?\))))) ","
               (group (+ (not (any ?, ?\))))) ","
               (group (+ digit)) ")")
           line)
      (list :kind (match-string 1 line)
            :time (match-string 2 line)
            :retries (string-to-number (match-string 3 line))))))

(defun ssh-session-manager--parse-ss-tcp (output)
  "Parse `ss -tnapHeoO' OUTPUT into a list of `ssh-session-manager--sock'."
  (let (socks)
    (dolist (line (split-string output "\n" t))
      (when (string-match
             (rx bos (group (+ (not space))) (+ space) ; 1: State
                 (group (+ digit)) (+ space)           ; 2: Recv-Q
                 (group (+ digit)) (+ space)           ; 3: Send-Q
                 (group (+ (not space))) (+ space)     ; 4: Local
                 (group (+ (not space)))               ; 5: Peer
                 (group (* anything)) eos)             ; 6: Rest
             line)
        (let* ((state (match-string 1 line))
               (recv-q (string-to-number (match-string 2 line)))
               (send-q (string-to-number (match-string 3 line)))
               (local-s (match-string 4 line))
               (peer-s (match-string 5 line))
               (rest (match-string 6 line))
               (local (ssh-session-manager--parse-ss-endpoint local-s))
               (peer (ssh-session-manager--parse-ss-endpoint peer-s))
               (pids (ssh-session-manager--parse-ss-pids rest))
               (uid (when (string-match (rx word-start "uid:" (group (+ digit))) rest)
                      (string-to-number (match-string 1 rest))))
               (timer (ssh-session-manager--parse-ss-timer rest)))
          (when local
            (push (ssh-session-manager--sock-create
                   :proto 'tcp
                   :state state
                   :recv-q recv-q
                   :send-q send-q
                   :local-addr (car local)
                   :local-port (cdr local)
                   :peer-addr (car peer)
                   :peer-port (cdr peer)
                   :pids pids
                   :uid uid
                   :timer timer)
                  socks)))))
    (nreverse socks)))

(defun ssh-session-manager--parse-ss-unix (output)
  "Parse `ss -xapHO' OUTPUT into a list of `ssh-session-manager--sock'."
  (let (socks)
    (dolist (line (split-string output "\n" t))
      (when (string-match
             (rx bos (or "u_str" "u_dgr" "u_seq") (+ space)
                 (group (+ (not space))) (+ space) ; 1: State
                 (group (+ digit)) (+ space)       ; 2: Recv-Q
                 (group (+ digit)) (+ space)       ; 3: Send-Q
                 (group (+ (not space))) (+ space) ; 4: Local path or *
                 (group (+ (not space))) (+ space) ; 5: Local inode
                 (group (+ (not space))) (+ space) ; 6: Peer path or *
                 (group (+ (not space)))           ; 7: Peer inode
                 (group (* anything)) eos)         ; 8: Rest
             line)
        (let* ((state (match-string 1 line))
               (recv-q (string-to-number (match-string 2 line)))
               (send-q (string-to-number (match-string 3 line)))
               (lpath (match-string 4 line))
               (ppath (match-string 6 line))
               (rest (match-string 8 line))
               (pids (ssh-session-manager--parse-ss-pids rest)))
          (push (ssh-session-manager--sock-create
                 :proto 'unix
                 :state state
                 :recv-q recv-q
                 :send-q send-q
                 :local-addr (unless (string= lpath "*") lpath)
                 :peer-addr (unless (string= ppath "*") ppath)
                 :pids pids)
                socks))))
    (nreverse socks)))

(defun ssh-session-manager--parse-lsof-output (output)
  "Parse `lsof -nP -iTCP -U -F pntPT' OUTPUT into `ssh-session-manager--sock's.
Fallback parser for systems without `ss'."
  (let ((pid nil)
        (ftype nil)
        (name nil)
        (tcp-state nil)
        socks)
    (cl-flet ((flush ()
                (when (and pid name)
                  (cond
                   ((member ftype '("IPv4" "IPv6"))
                    (if (string-match (rx bos (group (+ (not space))) "->"
                                          (group (+ (not space))) eos)
                                      name)
                        (let* ((loc-s (match-string 1 name))
                               (peer-s (match-string 2 name))
                               (loc (ssh-session-manager--parse-ss-endpoint loc-s))
                               (peer (ssh-session-manager--parse-ss-endpoint peer-s)))
                          (when loc
                            (push (ssh-session-manager--sock-create
                                   :proto 'tcp
                                   :state (or (and tcp-state (upcase tcp-state)) "ESTAB")
                                   :local-addr (car loc) :local-port (cdr loc)
                                   :peer-addr (car peer) :peer-port (cdr peer)
                                   :pids (list pid))
                                  socks)))
                      (when-let* ((loc (ssh-session-manager--parse-ss-endpoint name)))
                        (push (ssh-session-manager--sock-create
                               :proto 'tcp
                               :state (if (and tcp-state (string-match-p "LISTEN" tcp-state))
                                          "LISTEN"
                                        (or tcp-state "LISTEN"))
                               :local-addr (car loc) :local-port (cdr loc)
                               :pids (list pid))
                              socks))))
                   ((and (string= ftype "unix")
                         (string-prefix-p "/" name))
                    (push (ssh-session-manager--sock-create
                           :proto 'unix
                           :state "LISTEN"
                           :local-addr (replace-regexp-in-string " type=.*\\'" "" name)
                           :pids (list pid))
                          socks))))
                (setq ftype nil name nil tcp-state nil)))
      (dolist (line (split-string output "\n" t))
        (let ((tag (aref line 0))
              (val (substring line 1)))
          (pcase tag
            (?p (flush) (setq pid (string-to-number val)))
            (?f (flush))
            (?t (setq ftype val))
            (?n (setq name val))
            (?T (when (string-prefix-p "ST=" val)
                  (let ((st (substring val 3)))
                    (setq tcp-state (if (string= st "ESTABLISHED") "ESTAB" st))))))))
      (flush))
    (nreverse socks)))

(defun ssh-session-manager--parse-proc-environ (raw)
  "Parse NUL-delimited `/proc/<pid>/environ' RAW string into an alist."
  (let (alist)
    (dolist (entry (split-string (or raw "") "\0" t))
      (when (string-match (rx bos (group (+ (not ?=))) "=" (group (* anything)) eos)
                          entry)
        (push (cons (match-string 1 entry) (match-string 2 entry)) alist)))
    alist))

(defun ssh-session-manager--parse-ssh-G-output (output)
  "Parse `ssh -G' OUTPUT into a plist of forwards and connection details."
  (let (forwards control-path hostname user port)
    (dolist (line (split-string (or output "") "\n" t))
      (when (string-match (rx bos (group (+ (not space))) (+ space) (group (+ anything)) eos)
                          line)
        (let ((key (downcase (match-string 1 line)))
              (val (string-trim (match-string 2 line))))
          (pcase key
            ("hostname" (setq hostname val))
            ("user" (setq user val))
            ("port" (setq port (ssh-session-manager--numeric-port-p val)))
            ("controlpath"
             (unless (member (downcase val) '("none" ""))
               (setq control-path
                     (if (and (string-prefix-p "~" val)
                              (not (ssh-session-manager--remote-prefix)))
                         (expand-file-name val)
                       val))))
            ("localforward"
             (when-let* ((f (ssh-session-manager--parse-forward-spec 'local val 'config)))
               (push f forwards)))
            ("remoteforward"
             (when-let* ((f (ssh-session-manager--parse-forward-spec 'remote val 'config)))
               (push f forwards)))
            ("dynamicforward"
             (when-let* ((f (ssh-session-manager--parse-forward-spec 'dynamic val 'config)))
               (push f forwards)))))))
    (list :hostname hostname
          :user user
          :port port
          :control-path control-path
          :forwards (nreverse forwards))))

(defun ssh-session-manager--parse-ssh-config-hosts (text)
  "Extract concrete `Host' aliases from ssh_config TEXT."
  (let (hosts)
    (dolist (line (split-string (or text "") "\n" t))
      (let ((trimmed (string-trim line)))
        (when (string-match (rx bos (or "Host" "host" "HOST") (+ space) (group (+ anything)) eos)
                            trimmed)
          (dolist (pat (split-string (match-string 1 trimmed)))
            (unless (or (string-match-p "[*?!=]" pat)
                        (string-prefix-p "#" pat)
                        (member pat hosts))
              (push pat hosts))))))
    (nreverse hosts)))

(defun ssh-session-manager--normalize-mux-socket-path (path &optional file-exists-fn)
  "Normalize OpenSSH atomic-bind ControlPath PATH.
OpenSSH binds `<control_path>.<16-random-alnum>' and renames it to
`<control_path>', leaving the original suffix visible in `ss -x'.
FILE-EXISTS-FN defaults to `file-exists-p'.
Return `(NORMALIZED-PATH . RENAMED-P)'."
  (let ((exists-fn (or file-exists-fn #'file-exists-p)))
    (if (and path
             (string-match
              (rx bos (group (+ anything)) "." (= 16 alnum) eos)
              path))
        (let ((base (match-string 1 path)))
          (if (and (not (funcall exists-fn path))
                   (funcall exists-fn base))
              (cons base t)
            (cons path nil)))
      (cons path nil))))


;;; System Data Collection Layer (Testable & TRAMP-Aware)

(defun ssh-session-manager--remote-prefix ()
  "Return TRAMP remote prefix for `default-directory', or nil."
  (file-remote-p default-directory))

(defun ssh-session-manager--host-path (abs-path)
  "Prefix ABS-PATH with the active TRAMP remote prefix, if any."
  (let ((prefix (ssh-session-manager--remote-prefix)))
    (if prefix (concat prefix abs-path) abs-path)))

(defun ssh-session-manager--sudo-directory ()
  "Return a TRAMP directory string for privileged execution."
  (let ((prefix (ssh-session-manager--remote-prefix)))
    (if prefix
        (progn
          (require 'tramp)
          (let ((vec (tramp-dissect-file-name default-directory)))
            (format "/%s:%s%s|sudo::/"
                    (tramp-file-name-method vec)
                    (if (tramp-file-name-user vec)
                        (concat (tramp-file-name-user vec) "@")
                      "")
                    (tramp-file-name-host vec))))
      "/sudo::/")))

(defun ssh-session-manager--run-command (program args &optional sudo)
  "Run PROGRAM with ARGS and return `(EXIT-CODE STDOUT STDERR)'.
When SUDO is non-nil, run with elevated privileges according to
`ssh-session-manager-sudo-method'."
  (let ((stderr-file (make-temp-file "ssh-session-manager-err-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((default-directory
                  (if (and sudo (eq ssh-session-manager-sudo-method 'tramp))
                      (ssh-session-manager--sudo-directory)
                    default-directory))
                 (prog (if (and sudo (eq ssh-session-manager-sudo-method 'sudo))
                           "sudo"
                         program))
                 (final-args (if (and sudo (eq ssh-session-manager-sudo-method 'sudo))
                                 (append (list "-n" program) args)
                               args))
                 (rc (condition-case err
                         (apply #'process-file prog nil (list t stderr-file) nil final-args)
                       (error
                        (with-temp-file stderr-file
                          (insert (error-message-string err)))
                        -1)))
                 (stdout (buffer-string))
                 (stderr (if (file-readable-p stderr-file)
                             (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-string))
                           "")))
            (list rc stdout stderr)))
      (ignore-errors (delete-file stderr-file)))))

(defun ssh-session-manager--read-proc-cmdline (pid)
  "Read `/proc/PID/cmdline' as an argv list, or nil if unavailable."
  (let ((path (ssh-session-manager--host-path (format "/proc/%d/cmdline" pid))))
    (when (file-readable-p path)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents-literally path)
            (let ((parts (split-string (buffer-string) "\0" t)))
              (when parts parts)))
        (error nil)))))

(defun ssh-session-manager--read-proc-environ (pid)
  "Read `/proc/PID/environ' as an alist, or nil if unreadable."
  (let ((path (ssh-session-manager--host-path (format "/proc/%d/environ" pid))))
    (when (file-readable-p path)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents-literally path)
            (ssh-session-manager--parse-proc-environ (buffer-string)))
        (error nil)))))

(defun ssh-session-manager--read-tty-idle (tty)
  "Return idle seconds for TTY (e.g. `\"pts/0\"'), or nil."
  (when (and tty (not (member tty '("notty" "mux" "sftp" "?"))))
    (let* ((dev-path (if (string-prefix-p "/" tty) tty (concat "/dev/" tty)))
           (full (ssh-session-manager--host-path dev-path))
           (attrs (ignore-errors (file-attributes full))))
      (when attrs
        (let ((atime (file-attribute-access-time attrs)))
          (when atime
            (max 0 (truncate (float-time (time-subtract (current-time) atime))))))))))

(defun ssh-session-manager--file-uid (path)
  "Return numeric owner UID of PATH, or nil."
  (when (and path (string-prefix-p "/" path))
    (let ((attrs (ignore-errors
                   (file-attributes (ssh-session-manager--host-path path) 'integer))))
      (when attrs
        (file-attribute-user-id attrs)))))

(defun ssh-session-manager--collect-procs ()
  "Collect running processes via `ps' and enrich SSH procs from `/proc'."
  (pcase-let ((`(,rc ,stdout _)
               (ssh-session-manager--run-command
                ssh-session-manager-ps-program
                '("-ewwo" "pid=,ppid=,user:32=,stat=,etime=,tty=,args="))))
    (when (eql rc 0)
      (let ((procs (ssh-session-manager--parse-ps-output stdout)))
        (dolist (p procs)
          (let ((cmd (ssh-session-manager--proc-command p)))
            (when (string-match-p (rx (or "ssh" "sshd")) cmd)
              (when-let* ((argv (ssh-session-manager--read-proc-cmdline
                                 (ssh-session-manager--proc-pid p))))
                (setf (ssh-session-manager--proc-argv p) argv)))))
        procs))))

(defun ssh-session-manager--collect-sockets (&optional sudo)
  "Collect TCP and Unix sockets via `ss' (or `lsof' fallback).
When SUDO is non-nil, run with elevated privileges."
  (if (executable-find ssh-session-manager-ss-program t)
      (let* ((tcp-res (ssh-session-manager--run-command
                       ssh-session-manager-ss-program
                       '("-tnapHeoO") sudo))
             (unix-res (ssh-session-manager--run-command
                        ssh-session-manager-ss-program
                        '("-xapHO") sudo))
             (tcp-socks (if (eql (car tcp-res) 0)
                            (ssh-session-manager--parse-ss-tcp (cadr tcp-res))
                          nil))
             (unix-socks (if (eql (car unix-res) 0)
                             (ssh-session-manager--parse-ss-unix (cadr unix-res))
                           nil)))
        (append tcp-socks unix-socks))
    (when (executable-find ssh-session-manager-lsof-program t)
      (pcase-let ((`(,rc ,stdout _)
                   (ssh-session-manager--run-command
                    ssh-session-manager-lsof-program
                    '("-nP" "-iTCP" "-U" "-F" "pntPT") sudo)))
        (when (eql rc 0)
          (ssh-session-manager--parse-lsof-output stdout))))))

(defun ssh-session-manager--query-ssh-config (host &optional port user)
  "Query `ssh -G' for HOST (with optional PORT and USER), using cache."
  (when (and ssh-session-manager-query-ssh-config
             host
             (not (member host '("?" ""))))
    (let* ((key (list host port user (ssh-session-manager--remote-prefix)))
           (cached (gethash key ssh-session-manager--ssh-config-cache 'miss)))
      (if (not (eq cached 'miss))
          cached
        (let* ((args (append (list "-G")
                             (when port (list "-p" (number-to-string port)))
                             (when user (list "-l" user))
                             (list "--" host)))
               (res (ssh-session-manager--run-command
                     ssh-session-manager-ssh-program args))
               (parsed (when (eql (car res) 0)
                         (ssh-session-manager--parse-ssh-G-output (cadr res)))))
          (puthash key parsed ssh-session-manager--ssh-config-cache)
          parsed)))))

(defun ssh-session-manager--read-ssh-config-hosts ()
  "Return a list of host aliases from `ssh-session-manager-config-file'."
  (let ((path (ssh-session-manager--host-path
               (expand-file-name ssh-session-manager-config-file))))
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (ssh-session-manager--parse-ssh-config-hosts (buffer-string))))))


;;; Correlation, Forward Merging & Health Diagnostics

(defun ssh-session-manager--current-emacs-ancestors (proc-by-pid)
  "Return ancestor PIDs of current Emacs using hash table PROC-BY-PID."
  (unless (ssh-session-manager--remote-prefix)
    (let ((pid (emacs-pid))
          ancestors
          (seen (make-hash-table :test 'eql)))
      (while (and pid (> pid 1) (not (gethash pid seen)))
        (puthash pid t seen)
        (push pid ancestors)
        (let ((p (gethash pid proc-by-pid)))
          (setq pid (and p (ssh-session-manager--proc-ppid p)))))
      ancestors)))

(defun ssh-session-manager--active-emacs-ttys ()
  "Return list of short TTY names (e.g. `\"pts/0\"') used by Emacs."
  (unless (ssh-session-manager--remote-prefix)
    (let (ttys)
      (when-let* ((env-tty (getenv "SSH_TTY")))
        (push (string-remove-prefix "/dev/" env-tty) ttys))
      (dolist (term (terminal-list))
        (let ((name (terminal-name term)))
          (when (and (stringp name) (string-prefix-p "/dev/" name))
            (cl-pushnew (string-remove-prefix "/dev/" name) ttys :test #'equal))))
      ttys)))

(defun ssh-session-manager--ignored-unix-socket-p (path)
  "Return non-nil if Unix socket PATH is a known non-forward daemon socket."
  (or (null path)
      (not (string-prefix-p "/" path))
      (string-match-p
       (rx (or "/openssh_agent"
               "/S.gpg-agent"
               "/ssh-unix-local/"
               "/gnupg/"
               "/systemd/"
               "/pulse/"
               "/pipewire"
               "/wayland-"
               "/bus"
               "/.X11-unix/"
               "/X11-unix/"
               "/dbus-"
               "crd_ssh_auth_sock"))
       path)))

(defun ssh-session-manager--sshd-agent-socket-pid (path)
  "If PATH is an OpenSSH `sshd.<PID>' agent socket, return integer PID."
  (when (and path (string-match (rx ".sshd." (group (+ digit)) ".") path))
    (string-to-number (match-string 1 path))))

(defun ssh-session-manager--sock-matches-forward-p (sock fwd)
  "Return non-nil if listening SOCK corresponds to declared forward FWD."
  (and (eq (ssh-session-manager--sock-proto sock)
           (ssh-session-manager-forward-proto fwd))
       (string= (ssh-session-manager--sock-state sock) "LISTEN")
       (if (eq (ssh-session-manager--sock-proto sock) 'unix)
           (equal (ssh-session-manager--sock-local-addr sock)
                  (ssh-session-manager-forward-bind-addr fwd))
         (and (eql (ssh-session-manager--sock-local-port sock)
                   (ssh-session-manager-forward-bind-port fwd))
              (let ((saddr (ssh-session-manager--sock-local-addr sock))
                    (faddr (ssh-session-manager-forward-bind-addr fwd)))
                (or (and (ssh-session-manager--loopback-p faddr)
                         (ssh-session-manager--loopback-p saddr))
                    (and (ssh-session-manager--wildcard-p faddr)
                         (ssh-session-manager--wildcard-p saddr))
                    (equal (ssh-session-manager--normalize-ip saddr)
                           (ssh-session-manager--normalize-ip faddr))))))))

(defun ssh-session-manager--collapse-loopback-listens (socks)
  "Collapse paired IPv4 `127.0.0.1:P' and IPv6 `[::1]:P' LISTEN TCP SOCKS.
Also collapses paired `0.0.0.0:P' and `[::]:P' wildcard listens.
Return a list of representative `ssh-session-manager--sock' structs."
  (let ((by-key (make-hash-table :test 'equal))
        order)
    (dolist (s socks)
      (if (and (eq (ssh-session-manager--sock-proto s) 'tcp)
               (string= (ssh-session-manager--sock-state s) "LISTEN"))
          (let* ((addr (ssh-session-manager--sock-local-addr s))
                 (port (ssh-session-manager--sock-local-port s))
                 (bucket (cond
                          ((ssh-session-manager--loopback-p addr)
                           (cons 'loopback port))
                          ((ssh-session-manager--wildcard-p addr)
                           (cons 'wildcard port))
                          (t (cons addr port))))
                 (existing (gethash bucket by-key)))
            (if existing
                ;; Paired IPv4+IPv6 loopback -> normalize display to "localhost" / "*".
                (when (and (not (equal (ssh-session-manager--sock-local-addr existing) addr))
                           (memq (car bucket) '(loopback wildcard)))
                  (let ((copy (copy-ssh-session-manager--sock existing)))
                    (setf (ssh-session-manager--sock-local-addr copy)
                          (if (eq (car bucket) 'loopback) "localhost" "*"))
                    (puthash bucket copy by-key)))
              (puthash bucket s by-key)
              (push bucket order)))
        (let ((uniq (list 'other s)))
          (puthash uniq s by-key)
          (push uniq order))))
    (mapcar (lambda (k) (gethash k by-key)) (nreverse order))))

(defun ssh-session-manager--reconcile-outgoing-forwards
    (session declared-forwards pid-socks &optional have-sock-scan)
  "Merge DECLARED-FORWARDS and live PID-SOCKS for outgoing SESSION.
HAVE-SOCK-SCAN non-nil means socket inspection ran for this session."
  (let* ((ctl (ssh-session-manager-session-control-path session))
         (ledger (and ctl (gethash ctl ssh-session-manager--runtime-forwards)))
         (runtime-added (plist-get ledger :added))
         (runtime-canceled (plist-get ledger :canceled))
         (listen-socks (seq-filter
                        (lambda (s)
                          (and (string= (ssh-session-manager--sock-state s) "LISTEN")
                               (not (and (eq (ssh-session-manager--sock-proto s) 'unix)
                                         ctl
                                         (or (equal (ssh-session-manager--sock-local-addr s) ctl)
                                             (string-prefix-p (concat ctl ".")
                                                              (or (ssh-session-manager--sock-local-addr s) "")))))))
                        pid-socks))
         (collapsed-listens (ssh-session-manager--collapse-loopback-listens listen-socks))
         (all-declared nil)
         (seen-keys (make-hash-table :test 'equal))
         result)
    ;; Combine CLI/config + runtime-added, filtering out runtime-canceled.
    (dolist (f (append declared-forwards runtime-added))
      (let* ((cancel-pair (cons (ssh-session-manager-forward-type f)
                                (ssh-session-manager-forward-spec f)))
             (key (ssh-session-manager--forward-key f)))
        (unless (or (member cancel-pair runtime-canceled)
                    (gethash key seen-keys))
          (puthash key t seen-keys)
          (push (ssh-session-manager-forward-copy f) all-declared))))
    (setq all-declared (nreverse all-declared))
    ;; Match declared forwards against live listening sockets.
    (let ((matched-socks (make-hash-table :test 'eq)))
      (dolist (f all-declared)
        (if (memq (ssh-session-manager-forward-type f) '(local dynamic))
            (let ((matched nil))
              (dolist (s listen-socks)
                (when (ssh-session-manager--sock-matches-forward-p s f)
                  (setq matched t)
                  (puthash s t matched-socks)))
              (when (or matched (not have-sock-scan))
                (setf (ssh-session-manager-forward-active-p f) matched)
                (push f result)))
          ;; Remote (-R) forwards listen remotely; keep declared ones.
          (push f result)))
      ;; Any remaining live LISTEN sockets not matched to declared forwards
      ;; (e.g., added via external `ssh -O forward' or ControlPersist mux).
      (dolist (s collapsed-listens)
        (let ((already (cl-some
                        (lambda (raw)
                          (and (gethash raw matched-socks)
                               (eq (ssh-session-manager--sock-proto raw)
                                   (ssh-session-manager--sock-proto s))
                               (if (eq (ssh-session-manager--sock-proto s) 'unix)
                                   (equal (ssh-session-manager--sock-local-addr raw)
                                          (ssh-session-manager--sock-local-addr s))
                                 (and (eql (ssh-session-manager--sock-local-port raw)
                                           (ssh-session-manager--sock-local-port s))
                                      (let ((ra (ssh-session-manager--sock-local-addr raw))
                                            (sa (ssh-session-manager--sock-local-addr s)))
                                        (or (equal ra sa)
                                            (and (ssh-session-manager--loopback-p ra)
                                                 (ssh-session-manager--loopback-p sa))
                                            (and (ssh-session-manager--wildcard-p ra)
                                                 (ssh-session-manager--wildcard-p sa))))))))
                        listen-socks)))
          (unless already
            (push (ssh-session-manager-forward-create
                   :type 'local
                   :proto (ssh-session-manager--sock-proto s)
                   :bind-addr (ssh-session-manager--sock-local-addr s)
                   :bind-port (ssh-session-manager--sock-local-port s)
                   :source 'ss
                   :active-p t)
                  result)))))
    (nreverse result)))

(defun ssh-session-manager--diagnose-health (session)
  "Populate `hung-reasons' and `stale-reasons' on SESSION."
  (let (hung stale)
    ;; Process state checks.
    (let ((st (or (ssh-session-manager-session-state session) "")))
      (cond
       ((string-prefix-p "D" st) (push "process state D (uninterruptible)" hung))
       ((string-prefix-p "Z" st) (push "process state Z (zombie)" hung))
       ((string-prefix-p "T" st) (push "process state T (stopped)" hung))))
    ;; TCP socket checks.
    (let ((tcp-st (ssh-session-manager-session-tcp-state session))
          (send-q (ssh-session-manager-session-send-q session))
          (timer (ssh-session-manager-session-tcp-timer session)))
      (when (and tcp-st
                 (member tcp-st '("CLOSE-WAIT" "FIN-WAIT-1" "FIN-WAIT-2"
                                  "CLOSING" "LAST-ACK")))
        (push (format "TCP %s" tcp-st) hung))
      (when (and send-q (> send-q 0))
        (push (format "TCP Send-Q=%dB" send-q) hung))
      (when timer
        (let ((kind (plist-get timer :kind))
              (retries (or (plist-get timer :retries) 0)))
          (cond
           ((and (string= kind "on") (> retries 0))
            (push (format "TCP retransmitting (%d)" retries) hung))
           ((and (string= kind "keepalive") (> retries 0))
            (push (format "TCP keepalive probes unanswered (%d)" retries) hung))
           ((and (string= kind "persist") (> retries 0))
            (push (format "TCP zero-window persist (%d)" retries) hung))))))
    ;; Soft idle warning for incoming PTY sessions.
    (let ((idle (ssh-session-manager-session-idle session)))
      (when (and (eq (ssh-session-manager-session-direction session) 'in)
                 idle
                 ssh-session-manager-idle-warning-threshold
                 (>= idle ssh-session-manager-idle-warning-threshold))
        (push (format "idle %s" (ssh-session-manager--format-duration idle)) stale)))
    (setf (ssh-session-manager-session-hung-reasons session) (nreverse hung))
    (setf (ssh-session-manager-session-stale-reasons session) (nreverse stale))
    session))

(defun ssh-session-manager--correlate-sessions
    (procs sockets &optional opts)
  "Correlate PROCS and SOCKETS into SSH sessions and unattributed forwards.
OPTS is an optional plist allowing dependency injection for testing:
  `:current-user'   username string (default function `user-login-name')
  `:current-uid'    integer UID (default function `user-uid')
  `:all-users'      non-nil to include other users
  `:env-fn'         (lambda (pid) -> env-alist)
  `:idle-fn'        (lambda (tty) -> seconds-or-nil)
  `:file-exists-fn' (lambda (path) -> bool)
  `:file-uid-fn'    (lambda (path) -> uid-or-nil)
  `:config-fn'      (lambda (host port user) -> parsed-ssh-G-plist)
  `:ancestors'      list of current Emacs ancestor PIDs
  `:emacs-ttys'     list of current Emacs TTY names
  `:emacs-ssh-conn' `SSH_CONNECTION' value for current Emacs.

Return `(SESSIONS . UNATTRIBUTED-FORWARDS)'."
  (let* ((cur-user (or (plist-get opts :current-user) (user-login-name)))
         (cur-uid (or (plist-get opts :current-uid) (user-uid)))
         (all-users (plist-get opts :all-users))
         (env-fn (or (plist-get opts :env-fn) #'ssh-session-manager--read-proc-environ))
         (idle-fn (or (plist-get opts :idle-fn) #'ssh-session-manager--read-tty-idle))
         (exists-fn (or (plist-get opts :file-exists-fn) #'file-exists-p))
         (fuid-fn (or (plist-get opts :file-uid-fn) #'ssh-session-manager--file-uid))
         (config-fn (or (plist-get opts :config-fn) #'ssh-session-manager--query-ssh-config))
         (proc-by-pid (make-hash-table :test 'eql))
         (children-by-ppid (make-hash-table :test 'eql))
         (socks-by-pid (make-hash-table :test 'eql))
         (have-sock-scan (not (null sockets)))
         (tcp-estab-by-4tuple (make-hash-table :test 'equal))
         (all-control-paths (make-hash-table :test 'equal))
         sessions
         unattributed)
    ;; Index processes and child lists.
    (dolist (p procs)
      (puthash (ssh-session-manager--proc-pid p) p proc-by-pid)
      (push (cons (ssh-session-manager--proc-pid p)
                  (ssh-session-manager--proc-command p))
            (gethash (ssh-session-manager--proc-ppid p) children-by-ppid)))
    ;; Index sockets by PID and 4-tuple.
    (dolist (s sockets)
      (dolist (pid (ssh-session-manager--sock-pids s))
        (push s (gethash pid socks-by-pid)))
      (when (and (eq (ssh-session-manager--sock-proto s) 'tcp)
                 (not (string= (ssh-session-manager--sock-state s) "LISTEN"))
                 (ssh-session-manager--sock-local-port s)
                 (ssh-session-manager--sock-peer-port s))
        (let ((key (list (ssh-session-manager--normalize-ip
                          (ssh-session-manager--sock-local-addr s))
                         (ssh-session-manager--sock-local-port s)
                         (ssh-session-manager--normalize-ip
                          (ssh-session-manager--sock-peer-addr s))
                         (ssh-session-manager--sock-peer-port s))))
          (puthash key s tcp-estab-by-4tuple))))

    (let* ((ancestors (if (plist-member opts :ancestors)
                          (plist-get opts :ancestors)
                        (ssh-session-manager--current-emacs-ancestors proc-by-pid)))
           (emacs-ttys (if (plist-member opts :emacs-ttys)
                           (plist-get opts :emacs-ttys)
                         (ssh-session-manager--active-emacs-ttys)))
           (emacs-ssh-conn (if (plist-member opts :emacs-ssh-conn)
                               (plist-get opts :emacs-ssh-conn)
                             (unless (ssh-session-manager--remote-prefix)
                               (getenv "SSH_CONNECTION")))))
      ;; Classify SSH session processes.
      (dolist (p procs)
        (when-let* ((sess (ssh-session-manager--classify-proc p)))
          (when (or all-users
                    (string= (ssh-session-manager-session-user sess) cur-user))
            (let* ((pid (ssh-session-manager-session-pid sess))
                   (children (nreverse (copy-sequence (gethash pid children-by-ppid))))
                   (pid-socks (gethash pid socks-by-pid)))
              (setf (ssh-session-manager-session-children sess) children)
              (if (eq (ssh-session-manager-session-direction sess) 'out)
                  ;; --- Outgoing session enrichment ---
                  (let* ((cfg (unless (eq (ssh-session-manager-session-role sess) 'persist)
                                (funcall config-fn
                                         (ssh-session-manager-session-host sess)
                                         (ssh-session-manager-session-port sess)
                                         (ssh-session-manager-session-remote-user sess))))
                         (cfg-forwards (plist-get cfg :forwards))
                         (ctl (or (ssh-session-manager-session-control-path sess)
                                  (plist-get cfg :control-path)))
                         (tcp-estab nil)
                         (has-listen-ctl nil))
                    ;; Inspect Unix sockets for ControlMaster sockets.
                    (dolist (s pid-socks)
                      (when (and (eq (ssh-session-manager--sock-proto s) 'unix)
                                 (string= (ssh-session-manager--sock-state s) "LISTEN")
                                 (ssh-session-manager--sock-local-addr s))
                        (let* ((raw-path (ssh-session-manager--sock-local-addr s))
                               (norm-pair (ssh-session-manager--normalize-mux-socket-path
                                           raw-path exists-fn))
                               (norm-path (car norm-pair))
                               (renamed-p (cdr norm-pair)))
                          (when (or renamed-p
                                    (and ctl (equal norm-path ctl))
                                    (eq (ssh-session-manager-session-role sess) 'persist))
                            (setq ctl norm-path
                                  has-listen-ctl t)))))
                    (if (and ctl
                             (or has-listen-ctl
                                 (eq (ssh-session-manager-session-role sess) 'persist)
                                 (funcall exists-fn ctl)))
                        (progn
                          (setf (ssh-session-manager-session-control-path sess) ctl)
                          (when has-listen-ctl
                            (puthash ctl t all-control-paths)
                            (unless (eq (ssh-session-manager-session-role sess) 'persist)
                              (setf (ssh-session-manager-session-role sess) 'master))))
                      (setf (ssh-session-manager-session-control-path sess) nil))
                    ;; Find main SSH TCP connection (first ESTAB on lowest port or port 22/configured port).
                    (let ((estabs (seq-filter
                                   (lambda (s)
                                     (and (eq (ssh-session-manager--sock-proto s) 'tcp)
                                          (not (string= (ssh-session-manager--sock-state s) "LISTEN"))))
                                   pid-socks))
                          (cfg-port (or (ssh-session-manager-session-port sess)
                                        (plist-get cfg :port)
                                        22)))
                      (setq tcp-estab
                            (or (seq-find (lambda (s)
                                            (eql (ssh-session-manager--sock-peer-port s) cfg-port))
                                          estabs)
                                (car estabs)))
                      (when tcp-estab
                        (setf (ssh-session-manager-session-local-addr sess)
                              (ssh-session-manager--format-host-port
                               (ssh-session-manager--sock-local-addr tcp-estab)
                               (ssh-session-manager--sock-local-port tcp-estab))
                              (ssh-session-manager-session-peer-addr sess)
                              (ssh-session-manager--format-host-port
                               (ssh-session-manager--sock-peer-addr tcp-estab)
                               (ssh-session-manager--sock-peer-port tcp-estab))
                              (ssh-session-manager-session-tcp-state sess)
                              (ssh-session-manager--sock-state tcp-estab)
                              (ssh-session-manager-session-send-q sess)
                              (ssh-session-manager--sock-send-q tcp-estab)
                              (ssh-session-manager-session-recv-q sess)
                              (ssh-session-manager--sock-recv-q tcp-estab)
                              (ssh-session-manager-session-tcp-timer sess)
                              (ssh-session-manager--sock-timer tcp-estab))
                        ;; For ControlPersist [mux] masters, refine host with peer IP if helpful.
                        (when (eq (ssh-session-manager-session-role sess) 'persist)
                          (setf (ssh-session-manager-session-host sess)
                                (format "%s (%s)"
                                        (ssh-session-manager--sock-peer-addr tcp-estab)
                                        (file-name-nondirectory (or ctl "mux"))))))
                      ;; Detect multiplexed slave clients (no TCP socket, but connected to a ctl socket).
                      (when (and (null tcp-estab)
                                 (not has-listen-ctl)
                                 (eq (ssh-session-manager-session-role sess) 'client)
                                 (cl-some (lambda (s)
                                            (and (eq (ssh-session-manager--sock-proto s) 'unix)
                                                 (string= (ssh-session-manager--sock-state s) "ESTAB")))
                                          pid-socks))
                        (setf (ssh-session-manager-session-role sess) 'mux-client)))
                    ;; Reconcile forwards (skip mux-client slaves; the master owns them).
                    (if (eq (ssh-session-manager-session-role sess) 'mux-client)
                        (setf (ssh-session-manager-session-forwards sess) nil)
                      (setf (ssh-session-manager-session-forwards sess)
                            (ssh-session-manager--reconcile-outgoing-forwards
                             sess
                             (append (ssh-session-manager-session-forwards sess)
                                     cfg-forwards)
                             pid-socks
                             (and have-sock-scan
                                  (not (null pid-socks)))))))

                ;; --- Incoming session enrichment ---
                (let ((env nil)
                      (in-forwards nil)
                      (x11-port nil)
                      (transport-sock nil)
                      (ssh-conn-str nil))
                  ;; Inspect child process environ (readable even when sshd-session is non-dumpable).
                  (cl-dolist (child children)
                    (let ((e (funcall env-fn (car child))))
                      (when (or (assoc "SSH_CONNECTION" e)
                                (assoc "SSH_CLIENT" e)
                                (assoc "SSH_TTY" e))
                        (setq env e)
                        (cl-return))))
                  ;; Parse SSH_CONNECTION / SSH_CLIENT from child environ.
                  (when-let* ((conn (cdr (assoc "SSH_CONNECTION" env))))
                    (setq ssh-conn-str conn)
                    (pcase (split-string conn)
                      (`(,cip ,cport ,sip ,sport)
                       (setf (ssh-session-manager-session-peer-addr sess)
                             (ssh-session-manager--format-host-port
                              cip (string-to-number cport))
                             (ssh-session-manager-session-local-addr sess)
                             (ssh-session-manager--format-host-port
                              sip (string-to-number sport))
                             (ssh-session-manager-session-host sess) cip)
                       ;; Correlate 4-tuple against `ss' TCP table (works without sudo!).
                       (setq transport-sock
                             (gethash (list (ssh-session-manager--normalize-ip sip)
                                            (string-to-number sport)
                                            (ssh-session-manager--normalize-ip cip)
                                            (string-to-number cport))
                                      tcp-estab-by-4tuple)))))
                  (when (and (equal (ssh-session-manager-session-host sess) "?")
                             (cdr (assoc "SSH_CLIENT" env)))
                    (pcase (split-string (cdr (assoc "SSH_CLIENT" env)))
                      (`(,cip ,cport ,_sport)
                       (setf (ssh-session-manager-session-host sess) cip
                             (ssh-session-manager-session-peer-addr sess)
                             (ssh-session-manager--format-host-port
                              cip (string-to-number cport))))))
                  ;; TTY from environ if missing, plus idle time.
                  (when (and (null (ssh-session-manager-session-tty sess))
                             (cdr (assoc "SSH_TTY" env)))
                    (setf (ssh-session-manager-session-tty sess)
                          (string-remove-prefix "/dev/" (cdr (assoc "SSH_TTY" env)))))
                  (setf (ssh-session-manager-session-idle sess)
                        (funcall idle-fn (ssh-session-manager-session-tty sess)))
                  ;; X11 & Agent forwarding from child environ.
                  (when-let* ((disp (cdr (assoc "DISPLAY" env))))
                    (when (string-match (rx ":" (group (+ digit))) disp)
                      (setq x11-port (+ 6000 (string-to-number (match-string 1 disp))))
                      (push (ssh-session-manager-forward-create
                             :type 'x11 :proto 'tcp
                             :bind-addr disp :bind-port x11-port
                             :source 'environ :active-p t)
                            in-forwards)))
                  (when-let* ((auth (cdr (assoc "SSH_AUTH_SOCK" env))))
                    (when (or (eql (ssh-session-manager--sshd-agent-socket-pid auth) pid)
                              (string-match-p (rx (or "/ssh-" "/.ssh/agent/")) auth))
                      (push (ssh-session-manager-forward-create
                             :type 'agent :proto 'unix
                             :bind-addr auth :source 'environ :active-p t)
                            in-forwards)))
                  ;; Also detect OpenSSH 10.x `sshd.<PID>' agent sockets directly from `ss -x'.
                  (unless (cl-some (lambda (f) (eq (ssh-session-manager-forward-type f) 'agent))
                                   in-forwards)
                    (dolist (s sockets)
                      (when (and (eq (ssh-session-manager--sock-proto s) 'unix)
                                 (string= (ssh-session-manager--sock-state s) "LISTEN")
                                 (eql (ssh-session-manager--sshd-agent-socket-pid
                                       (ssh-session-manager--sock-local-addr s))
                                      pid))
                        (push (ssh-session-manager-forward-create
                               :type 'agent :proto 'unix
                               :bind-addr (ssh-session-manager--sock-local-addr s)
                               :source 'ss :active-p t)
                              in-forwards))))
                  ;; Direct PID socket attribution (available with sudo or dumpable sshd).
                  (when pid-socks
                    (let* ((tcp-estabs (seq-filter
                                        (lambda (s)
                                          (and (eq (ssh-session-manager--sock-proto s) 'tcp)
                                               (not (string= (ssh-session-manager--sock-state s) "LISTEN"))))
                                        pid-socks))
                           (listens (ssh-session-manager--collapse-loopback-listens
                                     (seq-filter
                                      (lambda (s)
                                        (string= (ssh-session-manager--sock-state s) "LISTEN"))
                                      pid-socks))))
                      (unless transport-sock
                        (setq transport-sock
                              (or (seq-find (lambda (s)
                                              (eql (ssh-session-manager--sock-local-port s) 22))
                                            tcp-estabs)
                                  (car tcp-estabs))))
                      ;; Listening sockets on sshd-session are incoming `-R' forwards
                      ;; (or X11 / agent sockets already accounted for).
                      (dolist (s listens)
                        (cond
                         ((eq (ssh-session-manager--sock-proto s) 'tcp)
                          (unless (and x11-port
                                       (eql (ssh-session-manager--sock-local-port s) x11-port))
                            (push (ssh-session-manager-forward-create
                                   :type 'remote :proto 'tcp
                                   :bind-addr (ssh-session-manager--sock-local-addr s)
                                   :bind-port (ssh-session-manager--sock-local-port s)
                                   :source 'ss :active-p t)
                                  in-forwards)))
                         ((eq (ssh-session-manager--sock-proto s) 'unix)
                          (let ((upath (ssh-session-manager--sock-local-addr s)))
                            (unless (or (ssh-session-manager--ignored-unix-socket-p upath)
                                        (ssh-session-manager--sshd-agent-socket-pid upath)
                                        (cl-some (lambda (f)
                                                   (equal (ssh-session-manager-forward-bind-addr f)
                                                          upath))
                                                 in-forwards))
                              (push (ssh-session-manager-forward-create
                                     :type 'remote :proto 'unix
                                     :bind-addr upath
                                     :source 'ss :active-p t)
                                    in-forwards))))))
                      ;; Non-transport ESTAB TCP sockets on sshd-session are active
                      ;; client `-L'/`-D' connections exiting on this host!
                      (dolist (s tcp-estabs)
                        (unless (eq s transport-sock)
                          (push (ssh-session-manager-forward-create
                                 :type 'active :proto 'tcp
                                 :bind-addr (ssh-session-manager--sock-local-addr s)
                                 :bind-port (ssh-session-manager--sock-local-port s)
                                 :target-host (ssh-session-manager--sock-peer-addr s)
                                 :target-port (ssh-session-manager--sock-peer-port s)
                                 :source 'ss :active-p t)
                                in-forwards)))))
                  ;; Apply transport socket telemetry (works both with and without sudo!).
                  (when transport-sock
                    (setf (ssh-session-manager-session-local-addr sess)
                          (ssh-session-manager--format-host-port
                           (ssh-session-manager--sock-local-addr transport-sock)
                           (ssh-session-manager--sock-local-port transport-sock))
                          (ssh-session-manager-session-peer-addr sess)
                          (ssh-session-manager--format-host-port
                           (ssh-session-manager--sock-peer-addr transport-sock)
                           (ssh-session-manager--sock-peer-port transport-sock))
                          (ssh-session-manager-session-tcp-state sess)
                          (ssh-session-manager--sock-state transport-sock)
                          (ssh-session-manager-session-send-q sess)
                          (ssh-session-manager--sock-send-q transport-sock)
                          (ssh-session-manager-session-recv-q sess)
                          (ssh-session-manager--sock-recv-q transport-sock)
                          (ssh-session-manager-session-tcp-timer sess)
                          (ssh-session-manager--sock-timer transport-sock))
                    (when (equal (ssh-session-manager-session-host sess) "?")
                      (setf (ssh-session-manager-session-host sess)
                            (ssh-session-manager--sock-peer-addr transport-sock))))
                  ;; Detect whether this incoming session hosts the current Emacs.
                  (when (or (memq pid ancestors)
                            (and (ssh-session-manager-session-tty sess)
                                 (member (ssh-session-manager-session-tty sess) emacs-ttys))
                            (and emacs-ssh-conn ssh-conn-str
                                 (string= emacs-ssh-conn ssh-conn-str)))
                    (setf (ssh-session-manager-session-current-p sess) t))
                  (setf (ssh-session-manager-session-forwards sess) (nreverse in-forwards))))
              (ssh-session-manager--diagnose-health sess)
              (push sess sessions))))))

    (setq sessions (nreverse sessions))

    ;; Handle non-dumpable incoming sshd-session sockets when running unprivileged:
    ;; Any LISTEN TCP socket owned by `cur-uid' with no `users:' PID, or LISTEN
    ;; Unix socket owned by `cur-uid' with no `users:' PID (excluding known daemons),
    ;; comes from a non-dumpable user process (typically sshd-session -R).
    (let* ((unattr-incoming
            (seq-filter
             (lambda (s)
               (and (eq (ssh-session-manager-session-direction s) 'in)
                    (string= (ssh-session-manager-session-user s) cur-user)
                    (null (gethash (ssh-session-manager-session-pid s) socks-by-pid))))
             sessions))
           (known-x11-ports
            (delq nil
                  (mapcar (lambda (f)
                            (when (eq (ssh-session-manager-forward-type f) 'x11)
                              (ssh-session-manager-forward-bind-port f)))
                          (mapcan (lambda (s) (copy-sequence (ssh-session-manager-session-forwards s)))
                                  unattr-incoming)))))
      (when unattr-incoming
        (let* ((raw-unattr-tcp
                (seq-filter
                 (lambda (sock)
                   (and (eq (ssh-session-manager--sock-proto sock) 'tcp)
                        (string= (ssh-session-manager--sock-state sock) "LISTEN")
                        (null (ssh-session-manager--sock-pids sock))
                        (eql (ssh-session-manager--sock-uid sock) cur-uid)
                        (not (memq (ssh-session-manager--sock-local-port sock)
                                   known-x11-ports))))
                 sockets))
               (collapsed-tcp (ssh-session-manager--collapse-loopback-listens raw-unattr-tcp))
               (raw-unattr-unix
                (seq-filter
                 (lambda (sock)
                   (let ((p (ssh-session-manager--sock-local-addr sock)))
                     (and (eq (ssh-session-manager--sock-proto sock) 'unix)
                          (string= (ssh-session-manager--sock-state sock) "LISTEN")
                          (null (ssh-session-manager--sock-pids sock))
                          (not (ssh-session-manager--ignored-unix-socket-p p))
                          (not (ssh-session-manager--sshd-agent-socket-pid p))
                          (not (gethash p all-control-paths))
                          (eql (funcall fuid-fn p) cur-uid))))
                 sockets)))
          (dolist (sock (append collapsed-tcp raw-unattr-unix))
            (push (ssh-session-manager-forward-create
                   :type 'remote
                   :proto (ssh-session-manager--sock-proto sock)
                   :bind-addr (ssh-session-manager--sock-local-addr sock)
                   :bind-port (ssh-session-manager--sock-local-port sock)
                   :source 'inferred
                   :active-p t)
                  unattributed))
          (setq unattributed (nreverse unattributed))
          ;; If there is exactly one unattributed incoming session for the current user,
          ;; attach these inferred forwards to it (marked with `source = inferred').
          (when (and (= (length unattr-incoming) 1) unattributed)
            (let ((only (car unattr-incoming)))
              (setf (ssh-session-manager-session-forwards only)
                    (append (ssh-session-manager-session-forwards only) unattributed)
                    (ssh-session-manager-session-inferred-p only) t)
              (setq unattributed nil))))))

    ;; Prune stale runtime forward entries for dead control paths.
    (when have-sock-scan
      (let (stale-keys)
        (maphash (lambda (ctl _val)
                   (unless (or (gethash ctl all-control-paths)
                               (cl-some (lambda (s)
                                          (equal (ssh-session-manager-session-control-path s)
                                                 ctl))
                                        sessions))
                     (push ctl stale-keys)))
                 ssh-session-manager--runtime-forwards)
        (dolist (k stale-keys)
          (remhash k ssh-session-manager--runtime-forwards))))

    (cons sessions unattributed)))

(defun ssh-session-manager--collect-all (&optional all-users sudo)
  "Collect and correlate all SSH sessions on the current host.
When ALL-USERS is non-nil, include sessions owned by other users.
When SUDO is non-nil, run socket inspection with elevated privileges.
Return `(SESSIONS . UNATTRIBUTED-FORWARDS)'."
  (let* ((procs (ssh-session-manager--collect-procs))
         (socks (ssh-session-manager--collect-sockets sudo)))
    (ssh-session-manager--correlate-sessions
     procs socks (list :all-users all-users))))


;;; ControlMaster (`ssh -O') Operations

(defun ssh-session-manager--run-ctl (control-path ctl-cmd &optional extra-args)
  "Run `ssh -F none -S CONTROL-PATH -O CTL-CMD [EXTRA-ARGS...] _'.
Return `(EXIT-CODE STDOUT STDERR)'."
  (unless control-path
    (user-error "Session has no ControlMaster socket"))
  (ssh-session-manager--run-command
   ssh-session-manager-ssh-program
   (append (list "-F" "none" "-S" control-path "-O" ctl-cmd)
           extra-args
           (list "_"))))

(defun ssh-session-manager--record-runtime-add (control-path fwd)
  "Record dynamically added FWD for CONTROL-PATH."
  (let* ((entry (or (gethash control-path ssh-session-manager--runtime-forwards)
                    (list :added nil :canceled nil)))
         (added (plist-get entry :added))
         (canceled (plist-get entry :canceled))
         (pair (cons (ssh-session-manager-forward-type fwd)
                     (ssh-session-manager-forward-spec fwd))))
    (setq canceled (delete pair canceled))
    (setq added (cl-remove-if (lambda (x)
                                (equal (ssh-session-manager--forward-key x)
                                       (ssh-session-manager--forward-key fwd)))
                              added))
    (push (ssh-session-manager-forward-copy fwd) added)
    (puthash control-path (list :added added :canceled canceled)
             ssh-session-manager--runtime-forwards)))

(defun ssh-session-manager--record-runtime-cancel (control-path type spec)
  "Record dynamically canceled TYPE and SPEC for CONTROL-PATH."
  (let* ((entry (or (gethash control-path ssh-session-manager--runtime-forwards)
                    (list :added nil :canceled nil)))
         (added (plist-get entry :added))
         (canceled (plist-get entry :canceled))
         (pair (cons type spec)))
    (setq added (cl-remove-if (lambda (x)
                                (and (eq (ssh-session-manager-forward-type x) type)
                                     (equal (ssh-session-manager-forward-spec x) spec)))
                              added))
    (cl-pushnew pair canceled :test #'equal)
    (puthash control-path (list :added added :canceled canceled)
             ssh-session-manager--runtime-forwards)))

(defun ssh-session-manager--ctl-add-forward (control-path type spec)
  "Add forward of TYPE and SPEC to CONTROL-PATH via `ssh -O forward'."
  (let ((parsed (ssh-session-manager--parse-forward-spec type spec 'runtime)))
    (unless parsed
      (user-error "Invalid %s forward specification: %s"
                  (ssh-session-manager--forward-cli-flag type) spec))
    (pcase-let ((`(,rc ,_out ,err)
                 (ssh-session-manager--run-ctl
                  control-path "forward"
                  (list (ssh-session-manager--forward-cli-flag type)
                        (ssh-session-manager-forward-spec parsed)))))
      (unless (eql rc 0)
        (user-error "Failed to add forward %s %s: %s"
                    (ssh-session-manager--forward-cli-flag type)
                    spec
                    (string-trim err)))
      (ssh-session-manager--record-runtime-add control-path parsed)
      parsed)))

(defun ssh-session-manager--ctl-cancel-forward (control-path type spec)
  "Cancel forward of TYPE and SPEC on CONTROL-PATH via `ssh -O cancel'."
  (let ((parsed (ssh-session-manager--parse-forward-spec type spec 'runtime)))
    (unless parsed
      (user-error "Invalid %s forward specification: %s"
                  (ssh-session-manager--forward-cli-flag type) spec))
    (pcase-let ((`(,rc ,_out ,err)
                 (ssh-session-manager--run-ctl
                  control-path "cancel"
                  (list (ssh-session-manager--forward-cli-flag type)
                        (ssh-session-manager-forward-spec parsed)))))
      (unless (eql rc 0)
        (user-error "Failed to cancel forward %s %s: %s"
                    (ssh-session-manager--forward-cli-flag type)
                    spec
                    (string-trim err)))
      (ssh-session-manager--record-runtime-cancel
       control-path type (ssh-session-manager-forward-spec parsed))
      parsed)))

(defun ssh-session-manager--ctl-edit-forward
    (control-path old-type old-spec new-type new-spec)
  "Replace OLD-TYPE/OLD-SPEC with NEW-TYPE/NEW-SPEC on CONTROL-PATH.
If adding the new forward fails, attempts to restore the old forward."
  (let ((new-parsed (ssh-session-manager--parse-forward-spec
                     new-type new-spec 'runtime)))
    (unless new-parsed
      (user-error "Invalid %s forward specification: %s"
                  (ssh-session-manager--forward-cli-flag new-type) new-spec))
    ;; Cancel the old forward first (needed when reusing the same port).
    (ssh-session-manager--ctl-cancel-forward control-path old-type old-spec)
    (condition-case err
        (ssh-session-manager--ctl-add-forward control-path new-type new-spec)
      (error
       ;; Attempt rollback so the user does not lose their original tunnel.
       (let ((rolled-back
              (condition-case nil
                  (progn
                    (ssh-session-manager--ctl-add-forward
                     control-path old-type old-spec)
                    t)
                (error nil))))
         (user-error "%s (%s)"
                     (error-message-string err)
                     (if rolled-back
                         "rolled back to original forward"
                       "WARNING: rollback also failed")))))))


;;; Signaling / Process Termination

(defun ssh-session-manager--signal-pid (pid signal &optional sudo)
  "Send SIGNAL (string like `\"TERM\"' or integer) to PID."
  (let ((sig-str (if (integerp signal) (number-to-string signal) signal)))
    (if (and (not sudo) (not (ssh-session-manager--remote-prefix)))
        (signal-process pid (if (integerp signal) signal (intern (concat "SIG" sig-str))))
      (pcase-let ((`(,rc ,_out ,err)
                   (ssh-session-manager--run-command
                    "kill" (list (concat "-" sig-str) (number-to-string pid)) sudo)))
        (unless (eql rc 0)
          (user-error "Failed to signal PID %d with %s: %s"
                      pid sig-str (string-trim err)))))))


;;; Filtering & Table Rendering (`*SSH Sessions*')

(defun ssh-session-manager--session-matches-filter-p (sess filter)
  "Return non-nil if SESS matches FILTER plist."
  (and (or (null (plist-get filter :direction))
           (eq (ssh-session-manager-session-direction sess)
               (plist-get filter :direction)))
       (or (null (plist-get filter :forwards-only))
           (not (null (ssh-session-manager-session-forwards sess))))
       (or (null (plist-get filter :hung-only))
           (not (null (or (ssh-session-manager-session-hung-reasons sess)
                          (ssh-session-manager-session-stale-reasons sess)))))
       (or (null (plist-get filter :masters-only))
           (memq (ssh-session-manager-session-role sess) '(master persist)))
       (or (null (plist-get filter :user))
           (string= (ssh-session-manager-session-user sess)
                    (plist-get filter :user)))
       (or (null (plist-get filter :regexp))
           (let ((re (plist-get filter :regexp)))
             (or (string-match-p re (or (ssh-session-manager-session-host sess) ""))
                 (string-match-p re (or (ssh-session-manager-session-peer-addr sess) ""))
                 (string-match-p re (or (ssh-session-manager-session-command sess) ""))
                 (cl-some (lambda (f)
                            (string-match-p re (ssh-session-manager--forward-summary f)))
                          (ssh-session-manager-session-forwards sess)))))))

(defun ssh-session-manager--format-dir-cell (sess)
  "Format the `Dir' column cell for SESS."
  (pcase (ssh-session-manager-session-direction sess)
    ('in
     (if (ssh-session-manager-session-current-p sess)
         (propertize "IN*" 'face 'ssh-session-manager-current
                     'help-echo "Current Emacs SSH session")
       (propertize "IN" 'face 'ssh-session-manager-incoming)))
    ('out
     (pcase (ssh-session-manager-session-role sess)
       ((or 'master 'persist)
        (propertize "MUX" 'face 'ssh-session-manager-master
                    'help-echo (format "ControlMaster (%s)"
                                       (ssh-session-manager-session-control-path sess))))
       ('mux-client
        (propertize "SLV" 'face 'ssh-session-manager-outgoing
                    'help-echo "Multiplexed client"))
       ('proxy
        (propertize "JMP" 'face 'ssh-session-manager-outgoing
                    'help-echo "ProxyJump (-W) helper"))
       (_
        (propertize "OUT" 'face 'ssh-session-manager-outgoing))))))

(defun ssh-session-manager--format-host-cell (sess)
  "Format the `Host/Peer' column cell for SESS."
  (if (eq (ssh-session-manager-session-direction sess) 'out)
      (let ((ruser (ssh-session-manager-session-remote-user sess))
            (host (or (ssh-session-manager-session-host sess) "?"))
            (port (ssh-session-manager-session-port sess)))
        (concat (if ruser (concat ruser "@") "")
                host
                (if (and port (/= port 22)) (format ":%d" port) "")))
    (or (ssh-session-manager-session-peer-addr sess)
        (ssh-session-manager-session-host sess)
        "?")))

(defun ssh-session-manager--format-state-cell (sess)
  "Format the `State' column cell for SESS."
  (let ((hung (ssh-session-manager-session-hung-reasons sess))
        (stale (ssh-session-manager-session-stale-reasons sess))
        (tcp-st (ssh-session-manager-session-tcp-state sess))
        (proc-st (or (ssh-session-manager-session-state sess) "?")))
    (cond
     (hung
      (let* ((reason (car hung))
             (label (cond
                     ((string-match-p "retransmitting" reason) "HUNG:RETX")
                     ((string-match-p "keepalive" reason) "HUNG:KA")
                     ((string-match-p "Send-Q" reason) "HUNG:SNDQ")
                     ((string-match-p "TCP " reason)
                      (concat "HUNG:" (substring reason 4)))
                     (t "HUNG"))))
        (propertize label
                    'face 'ssh-session-manager-hung
                    'help-echo (string-join hung "; "))))
     (stale
      (propertize "STALE"
                  'face 'ssh-session-manager-stale
                  'help-echo (string-join stale "; ")))
     (tcp-st tcp-st)
     ((eq (ssh-session-manager-session-role sess) 'mux-client) "MUX-SLV")
     (t proc-st))))

(defun ssh-session-manager--format-forwards-cell (sess)
  "Format the `Forwards' column cell for SESS."
  (let ((fwds (ssh-session-manager-session-forwards sess)))
    (if fwds
        (mapconcat #'ssh-session-manager--forward-summary fwds ", ")
      "")))

(defun ssh-session-manager--session-to-entry (sess)
  "Convert SESS into a `tabulated-list-entries' element."
  (let ((pid (ssh-session-manager-session-pid sess))
        (elapsed (ssh-session-manager-session-elapsed sess))
        (idle (ssh-session-manager-session-idle sess)))
    (list pid
          (vector
           (ssh-session-manager--format-dir-cell sess)
           (propertize (number-to-string pid) 'ssh-session-manager-num pid)
           (or (ssh-session-manager-session-user sess) "")
           (ssh-session-manager--format-host-cell sess)
           (or (ssh-session-manager-session-tty sess) "-")
           (ssh-session-manager--format-state-cell sess)
           (propertize (ssh-session-manager--format-duration elapsed)
                       'ssh-session-manager-num (or elapsed -1))
           (propertize (ssh-session-manager--format-duration idle)
                       'ssh-session-manager-num (or idle -1))
           (ssh-session-manager--format-forwards-cell sess)))))

(defun ssh-session-manager--make-num-sorter (col-index)
  "Return a `tabulated-list-format' numeric sort predicate for COL-INDEX."
  (lambda (a b)
    (let ((na (or (get-text-property 0 'ssh-session-manager-num (aref (cadr a) col-index)) -1))
          (nb (or (get-text-property 0 'ssh-session-manager-num (aref (cadr b) col-index)) -1)))
      (< na nb))))

(defun ssh-session-manager--filter-description ()
  "Return a concise string describing active buffer filters, or nil."
  (let (parts)
    (when-let* ((d (plist-get ssh-session-manager--filter :direction)))
      (push (if (eq d 'in) "in" "out") parts))
    (when (plist-get ssh-session-manager--filter :forwards-only)
      (push "fwds" parts))
    (when (plist-get ssh-session-manager--filter :hung-only)
      (push "hung" parts))
    (when (plist-get ssh-session-manager--filter :masters-only)
      (push "mux" parts))
    (when-let* ((u (plist-get ssh-session-manager--filter :user)))
      (push (concat "user=" u) parts))
    (when-let* ((re (plist-get ssh-session-manager--filter :regexp)))
      (push (concat "/" re "/") parts))
    (when parts
      (string-join (nreverse parts) ","))))

(defun ssh-session-manager--update-mode-name ()
  "Update `mode-name' in `*SSH Sessions*' with active flags and filters."
  (let (tags)
    (when ssh-session-manager--sudo
      (push "sudo" tags))
    (when ssh-session-manager--all-users
      (push "all" tags))
    (when-let* ((fdesc (ssh-session-manager--filter-description)))
      (push fdesc tags))
    (when ssh-session-manager--unattributed-forwards
      (push (format "%d unattr fwd%s (!)"
                    (length ssh-session-manager--unattributed-forwards)
                    (if (= (length ssh-session-manager--unattributed-forwards) 1) "" "s"))
            tags))
    (setq mode-name
          (if tags
              (format "SSH-Sessions[%s]" (string-join (nreverse tags) " "))
            "SSH-Sessions"))
    (force-mode-line-update)))

(defun ssh-session-manager--apply-marks ()
  "Re-apply saved mark tags from `ssh-session-manager--marks' to buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((pid (tabulated-list-get-id))
             (mark (cdr (assq pid ssh-session-manager--marks))))
        (when mark
          (tabulated-list-put-tag (char-to-string mark))))
      (forward-line 1))))

(defun ssh-session-manager--refresh-data ()
  "Refresh session data and populate `tabulated-list-entries'."
  (pcase-let ((`(,sessions . ,unattr)
               (ssh-session-manager--collect-all
                ssh-session-manager--all-users
                ssh-session-manager--sudo)))
    (setq ssh-session-manager--sessions sessions
          ssh-session-manager--unattributed-forwards unattr)
    ;; Drop marks for PIDs that no longer exist.
    (let ((live-pids (mapcar #'ssh-session-manager-session-pid sessions)))
      (setq ssh-session-manager--marks
            (seq-filter (lambda (cell) (memq (car cell) live-pids))
                        ssh-session-manager--marks)))
    (setq tabulated-list-entries
          (mapcar #'ssh-session-manager--session-to-entry
                  (seq-filter (lambda (s)
                                (ssh-session-manager--session-matches-filter-p
                                 s ssh-session-manager--filter))
                              sessions)))
    (ssh-session-manager--update-mode-name)))

(defun ssh-session-manager--reprint-filtered ()
  "Re-filter existing sessions and re-render `*SSH Sessions*'."
  (setq tabulated-list-entries
        (mapcar #'ssh-session-manager--session-to-entry
                (seq-filter (lambda (s)
                              (ssh-session-manager--session-matches-filter-p
                               s ssh-session-manager--filter))
                            ssh-session-manager--sessions)))
  (ssh-session-manager--update-mode-name)
  (tabulated-list-print t)
  (ssh-session-manager--apply-marks))

(defun ssh-session-manager--refresh-all-views ()
  "Refresh all open `ssh-session-manager' buffers."
  (let (session-bufs forward-bufs inspect-bufs)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (cond
           ((derived-mode-p 'ssh-session-manager-mode)
            (push buf session-bufs))
           ((derived-mode-p 'ssh-session-manager-forwards-mode)
            (push buf forward-bufs))
           ((derived-mode-p 'ssh-session-manager-inspect-mode)
            (push buf inspect-bufs))))))
    (dolist (buf session-bufs)
      (with-current-buffer buf (revert-buffer nil t)))
    (dolist (buf forward-bufs)
      (with-current-buffer buf (revert-buffer nil t)))
    (dolist (buf inspect-bufs)
      (with-current-buffer buf (ssh-session-manager-inspect-refresh t)))))

(defun ssh-session-manager--session-by-pid (pid)
  "Look up `ssh-session-manager-session' with PID in current buffer."
  (seq-find (lambda (s) (eql (ssh-session-manager-session-pid s) pid))
            ssh-session-manager--sessions))

(defun ssh-session-manager--session-at-point ()
  "Return the `ssh-session-manager-session' at point, or signal an error."
  (let* ((pid (tabulated-list-get-id))
         (sess (and pid (ssh-session-manager--session-by-pid pid))))
    (unless sess
      (user-error "No SSH session at point"))
    sess))

(defun ssh-session-manager--target-sessions (&optional mark-char)
  "Return list of target sessions: marked with MARK-CHAR (or `*'), or at point."
  (let* ((wanted (or mark-char ?*))
         (marked-pids (mapcar #'car
                              (seq-filter (lambda (c) (eql (cdr c) wanted))
                                          ssh-session-manager--marks))))
    (if marked-pids
        (delq nil (mapcar #'ssh-session-manager--session-by-pid marked-pids))
      (list (ssh-session-manager--session-at-point)))))


;;; Interactive Commands: Marking & Filtering

(defun ssh-session-manager--set-mark (char)
  "Set mark CHAR (or nil to unmark) on the session at point and advance."
  (let ((pid (tabulated-list-get-id)))
    (unless pid
      (user-error "No SSH session at point"))
    (setq ssh-session-manager--marks
          (assq-delete-all pid ssh-session-manager--marks))
    (when char
      (push (cons pid char) ssh-session-manager--marks))
    (tabulated-list-put-tag (if char (char-to-string char) "") t)))

(defun ssh-session-manager-mark ()
  "Mark the SSH session at point with `*'."
  (interactive)
  (ssh-session-manager--set-mark ?*))

(defun ssh-session-manager-mark-delete ()
  "Flag the SSH session at point for termination (`D')."
  (interactive)
  (ssh-session-manager--set-mark ?D))

(defun ssh-session-manager-unmark ()
  "Remove any mark on the SSH session at point."
  (interactive)
  (ssh-session-manager--set-mark nil))

(defun ssh-session-manager-unmark-all ()
  "Remove every mark tag in `*SSH Sessions*'."
  (interactive)
  (setq ssh-session-manager--marks nil)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag "")
      (forward-line 1))))

(defun ssh-session-manager-toggle-marks ()
  "Toggle `*' mark tag across all visible sessions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((pid (tabulated-list-get-id)))
        (if (eql (cdr (assq pid ssh-session-manager--marks)) ?*)
            (progn
              (setq ssh-session-manager--marks
                    (assq-delete-all pid ssh-session-manager--marks))
              (tabulated-list-put-tag ""))
          (setq ssh-session-manager--marks
                (cons (cons pid ?*)
                      (assq-delete-all pid ssh-session-manager--marks)))
          (tabulated-list-put-tag "*")))
      (forward-line 1))))

(defun ssh-session-manager-mark-hung ()
  "Mark all visible sessions with hung or stale indicators (`*').
Excludes the session hosting the current Emacs."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((pid (tabulated-list-get-id))
                    (sess (ssh-session-manager--session-by-pid pid)))
          (when (and (or (ssh-session-manager-session-hung-reasons sess)
                         (ssh-session-manager-session-stale-reasons sess))
                     (not (ssh-session-manager-session-current-p sess)))
            (setq ssh-session-manager--marks
                  (cons (cons pid ?*)
                        (assq-delete-all pid ssh-session-manager--marks)))
            (tabulated-list-put-tag "*")
            (setq count (1+ count))))
        (forward-line 1)))
    (message "Marked %d hung/stale session%s" count (if (= count 1) "" "s"))))

(defun ssh-session-manager-filter-incoming ()
  "Toggle filtering to incoming SSH sessions only."
  (interactive)
  (setq ssh-session-manager--filter
        (plist-put (copy-sequence ssh-session-manager--filter)
                   :direction
                   (unless (eq (plist-get ssh-session-manager--filter :direction) 'in)
                     'in)))
  (ssh-session-manager--reprint-filtered))

(defun ssh-session-manager-filter-outgoing ()
  "Toggle filtering to outgoing SSH sessions only."
  (interactive)
  (setq ssh-session-manager--filter
        (plist-put (copy-sequence ssh-session-manager--filter)
                   :direction
                   (unless (eq (plist-get ssh-session-manager--filter :direction) 'out)
                     'out)))
  (ssh-session-manager--reprint-filtered))

(defun ssh-session-manager-filter-forwards ()
  "Toggle filtering to sessions that have port forwards."
  (interactive)
  (setq ssh-session-manager--filter
        (plist-put (copy-sequence ssh-session-manager--filter)
                   :forwards-only
                   (not (plist-get ssh-session-manager--filter :forwards-only))))
  (ssh-session-manager--reprint-filtered))

(defun ssh-session-manager-filter-hung ()
  "Toggle filtering to hung or stale sessions only."
  (interactive)
  (setq ssh-session-manager--filter
        (plist-put (copy-sequence ssh-session-manager--filter)
                   :hung-only
                   (not (plist-get ssh-session-manager--filter :hung-only))))
  (ssh-session-manager--reprint-filtered))

(defun ssh-session-manager-filter-masters ()
  "Toggle filtering to ControlMaster sessions only."
  (interactive)
  (setq ssh-session-manager--filter
        (plist-put (copy-sequence ssh-session-manager--filter)
                   :masters-only
                   (not (plist-get ssh-session-manager--filter :masters-only))))
  (ssh-session-manager--reprint-filtered))

(defun ssh-session-manager-filter-user (user)
  "Filter sessions to those owned by USER (empty clears user filter)."
  (interactive
   (list (completing-read
          "Filter by user (empty to clear): "
          (delete-dups (mapcar #'ssh-session-manager-session-user
                               ssh-session-manager--sessions)))))
  (setq ssh-session-manager--filter
        (plist-put (copy-sequence ssh-session-manager--filter)
                   :user (unless (string-empty-p user) user)))
  (ssh-session-manager--reprint-filtered))

(defun ssh-session-manager-filter-regexp (regexp)
  "Filter sessions matching REGEXP in host, peer, command, or forwards."
  (interactive "sFilter regexp (empty to clear): ")
  (setq ssh-session-manager--filter
        (plist-put (copy-sequence ssh-session-manager--filter)
                   :regexp (unless (string-empty-p regexp) regexp)))
  (ssh-session-manager--reprint-filtered))

(defun ssh-session-manager-filter-clear ()
  "Clear all active filters in `*SSH Sessions*'."
  (interactive)
  (setq ssh-session-manager--filter nil)
  (ssh-session-manager--reprint-filtered))


;;; Interactive Commands: Termination & ControlMaster Lifecycle

(defun ssh-session-manager--confirm-and-kill (sessions signal)
  "Prompt (when appropriate) and send SIGNAL to SESSIONS."
  (unless sessions
    (user-error "No SSH sessions selected"))
  (let* ((includes-current (cl-some #'ssh-session-manager-session-current-p sessions))
         (summary (if (= (length sessions) 1)
                      (let ((s (car sessions)))
                        (format "%s PID %d (%s)"
                                (if (eq (ssh-session-manager-session-direction s) 'in)
                                    "incoming" "outgoing")
                                (ssh-session-manager-session-pid s)
                                (ssh-session-manager--format-host-cell s)))
                    (format "%d SSH sessions (PIDs: %s)"
                            (length sessions)
                            (mapconcat (lambda (s)
                                         (number-to-string
                                          (ssh-session-manager-session-pid s)))
                                       sessions ", "))))
         (prompt (if includes-current
                     (format "WARNING: Includes CURRENT Emacs SSH session! Send SIG%s to %s? "
                             signal summary)
                   (format "Send SIG%s to %s? " signal summary)))
         (ok (cond
              (includes-current (yes-or-no-p prompt))
              (ssh-session-manager-confirm-kill (y-or-n-p prompt))
              (t t))))
    (when ok
      (let ((killed 0)
            (use-sudo (and ssh-session-manager--sudo
                           (cl-some (lambda (s)
                                      (not (string= (ssh-session-manager-session-user s)
                                                    (user-login-name))))
                                    sessions))))
        (dolist (s sessions)
          (ssh-session-manager--signal-pid
           (ssh-session-manager-session-pid s) signal use-sudo)
          (setq killed (1+ killed)))
        (sit-for 0.15)
        (ssh-session-manager--refresh-all-views)
        (message "Sent SIG%s to %d session%s"
                 signal killed (if (= killed 1) "" "s"))))))

(defun ssh-session-manager-kill (&optional arg)
  "Send SIGTERM to marked SSH sessions (or session at point).
With prefix ARG, prompt for the signal to send."
  (interactive "P")
  (let* ((sessions (ssh-session-manager--target-sessions ?*))
         (sig (if arg
                  (completing-read
                   "Signal: "
                   '("TERM" "KILL" "HUP" "INT" "QUIT" "STOP" "CONT")
                   nil nil nil nil "TERM")
                "TERM")))
    (ssh-session-manager--confirm-and-kill sessions sig)))

(defun ssh-session-manager-force-kill ()
  "Send SIGKILL to marked SSH sessions (or session at point)."
  (interactive)
  (ssh-session-manager--confirm-and-kill
   (ssh-session-manager--target-sessions ?*) "KILL"))

(defun ssh-session-manager-execute-deletes (&optional arg)
  "Terminate all SSH sessions flagged with `D'.
Sends SIGTERM by default, or SIGKILL with prefix ARG."
  (interactive "P")
  (let* ((pids (mapcar #'car
                       (seq-filter (lambda (c) (eql (cdr c) ?D))
                                   ssh-session-manager--marks)))
         (sessions (delq nil (mapcar #'ssh-session-manager--session-by-pid pids))))
    (unless sessions
      (user-error "No sessions flagged for deletion (`d')"))
    (ssh-session-manager--confirm-and-kill sessions (if arg "KILL" "TERM"))))

(defun ssh-session-manager--require-master-session (&optional sess)
  "Return SESS (or session at point) if it has a ControlMaster socket."
  (let ((s (or sess (ssh-session-manager--session-at-point))))
    (unless (eq (ssh-session-manager-session-direction s) 'out)
      (user-error "ControlMaster operations apply only to outgoing SSH sessions"))
    (unless (ssh-session-manager-session-control-path s)
      (user-error
       "Session PID %d (%s) has no ControlMaster socket; dynamic control requires ControlMaster"
       (ssh-session-manager-session-pid s)
       (ssh-session-manager-session-host s)))
    s))

(defun ssh-session-manager-check-master ()
  "Run `ssh -O check' on the outgoing ControlMaster session at point."
  (interactive)
  (let* ((sess (ssh-session-manager--require-master-session))
         (ctl (ssh-session-manager-session-control-path sess)))
    (pcase-let ((`(,rc ,out ,err) (ssh-session-manager--run-ctl ctl "check")))
      (let ((msg (string-trim (if (string-empty-p err) out err))))
        (if (eql rc 0)
            (message "ControlMaster OK: %s" msg)
          (user-error "ControlMaster check failed: %s" msg))))))

(defun ssh-session-manager-master-exit ()
  "Request the outgoing ControlMaster session at point to exit (`ssh -O exit')."
  (interactive)
  (let* ((sess (ssh-session-manager--require-master-session))
         (ctl (ssh-session-manager-session-control-path sess)))
    (when (or (not ssh-session-manager-confirm-kill)
              (y-or-n-p (format "Ask ControlMaster for %s (%s) to exit? "
                                (ssh-session-manager--format-host-cell sess)
                                ctl)))
      (pcase-let ((`(,rc ,_out ,err) (ssh-session-manager--run-ctl ctl "exit")))
        (unless (eql rc 0)
          (user-error "ControlMaster exit failed: %s" (string-trim err)))
        (sit-for 0.15)
        (ssh-session-manager--refresh-all-views)
        (message "ControlMaster %s exited" (ssh-session-manager--format-host-cell sess))))))

(defun ssh-session-manager-master-stop ()
  "Tell the ControlMaster at point to stop accepting requests (`ssh -O stop')."
  (interactive)
  (let* ((sess (ssh-session-manager--require-master-session))
         (ctl (ssh-session-manager-session-control-path sess)))
    (pcase-let ((`(,rc ,_out ,err) (ssh-session-manager--run-ctl ctl "stop")))
      (unless (eql rc 0)
        (user-error "ControlMaster stop failed: %s" (string-trim err)))
      (ssh-session-manager--refresh-all-views)
      (message "ControlMaster %s stopped accepting new multiplexing requests"
               (ssh-session-manager--format-host-cell sess)))))


;;; Interactive Commands: Dynamic Port Forward Management

(defun ssh-session-manager--read-forward-type (&optional default)
  "Prompt for a forward type (`local', `remote', or `dynamic').
Optional DEFAULT pre-selects that forward type."
  (let* ((choices '(("local (-L)" . local)
                    ("remote (-R)" . remote)
                    ("dynamic (-D SOCKS)" . dynamic)))
         (def-str (car (rassq default choices)))
         (sel (completing-read "Forward type: " choices nil t nil nil def-str)))
    (cdr (assoc sel choices))))

(defun ssh-session-manager--read-forward-spec (type &optional initial)
  "Prompt the user for an OpenSSH forward spec of TYPE, prefilling INITIAL."
  (let ((prompt
         (pcase type
           ('local "Local forward (-L [bind:]port:host:hostport or local_sock:remote_sock): ")
           ('remote "Remote forward (-R [bind:]port:host:hostport or remote_sock:local_sock): ")
           ('dynamic "Dynamic SOCKS forward (-D [bind:]port): "))))
    (string-trim
     (read-from-minibuffer prompt initial nil nil
                           'ssh-session-manager-forward-history))))

(defun ssh-session-manager--pick-master-session ()
  "Return the master session at point, or prompt to select one."
  (cond
   ((derived-mode-p 'ssh-session-manager-mode)
    (ssh-session-manager--require-master-session))
   ((derived-mode-p 'ssh-session-manager-inspect-mode)
    (ssh-session-manager--require-master-session
     (ssh-session-manager--inspect-current-session)))
   ((derived-mode-p 'ssh-session-manager-forwards-mode)
    (let* ((row (tabulated-list-get-id))
           (sess (car-safe row)))
      (if (and sess (ssh-session-manager-session-control-path sess))
          sess
        (let* ((masters (seq-filter
                         (lambda (s)
                           (and (eq (ssh-session-manager-session-direction s) 'out)
                                (memq (ssh-session-manager-session-role s) '(master persist))
                                (ssh-session-manager-session-control-path s)))
                         ssh-session-manager--sessions))
               (alist (mapcar (lambda (s)
                                (cons (format "%s (PID %d)"
                                              (ssh-session-manager--format-host-cell s)
                                              (ssh-session-manager-session-pid s))
                                      s))
                              masters)))
          (unless alist
            (user-error "No active outgoing ControlMaster sessions found"))
          (cdr (assoc (completing-read "ControlMaster session: " alist nil t)
                      alist))))))
   (t (user-error "Not in an SSH session manager buffer"))))

(defun ssh-session-manager--pick-session-forward (sess prompt)
  "Select a controllable forward on SESS using PROMPT.
Returns `(TYPE . SPEC-OR-PREFIX)'."
  (let* ((controllable
          (seq-filter (lambda (f)
                        (memq (ssh-session-manager-forward-type f)
                              '(local remote dynamic)))
                      (ssh-session-manager-session-forwards sess)))
         (alist
          (mapcar (lambda (f)
                    (let* ((type (ssh-session-manager-forward-type f))
                           (flag (ssh-session-manager--forward-cli-flag type))
                           (spec (ssh-session-manager-forward-spec f))
                           (label (if spec
                                      (format "%s %s" flag spec)
                                    (format "%s %s -> ? (from ss; enter full spec)"
                                            flag
                                            (ssh-session-manager--forward-bind-display f)))))
                      (cons label f)))
                  controllable)))
    (if (null alist)
        (let* ((type (ssh-session-manager--read-forward-type 'local))
               (spec (ssh-session-manager--read-forward-spec type)))
          (cons type spec))
      (let* ((choice (completing-read prompt alist nil nil))
             (fwd (cdr (assoc choice alist))))
        (if fwd
            (let* ((type (ssh-session-manager-forward-type fwd))
                   (spec (or (ssh-session-manager-forward-spec fwd)
                             ;; Forward discovered from `ss' only; let user complete target.
                             (ssh-session-manager--read-forward-spec
                              type
                              (concat (ssh-session-manager--forward-bind-display fwd) ":")))))
              (cons type spec))
          ;; Allow free-form entry like "-L 8080:localhost:80".
          (cond
           ((string-match (rx bos "-L" (+ space) (group (+ anything)) eos) choice)
            (cons 'local (match-string 1 choice)))
           ((string-match (rx bos "-R" (+ space) (group (+ anything)) eos) choice)
            (cons 'remote (match-string 1 choice)))
           ((string-match (rx bos "-D" (+ space) (group (+ anything)) eos) choice)
            (cons 'dynamic (match-string 1 choice)))
           (t
            (cons (ssh-session-manager--read-forward-type 'local) choice))))))))

(defun ssh-session-manager-add-forward ()
  "Dynamically add a port forward (`-L', `-R', `-D') to a ControlMaster session."
  (interactive)
  (let* ((sess (ssh-session-manager--pick-master-session))
         (ctl (ssh-session-manager-session-control-path sess))
         (type (ssh-session-manager--read-forward-type 'local))
         (spec (ssh-session-manager--read-forward-spec type)))
    (ssh-session-manager--ctl-add-forward ctl type spec)
    (ssh-session-manager--refresh-all-views)
    (message "Added %s %s on %s"
             (ssh-session-manager--forward-cli-flag type)
             spec
             (ssh-session-manager--format-host-cell sess))))

(defun ssh-session-manager-remove-forward ()
  "Cancel a port forward on an outgoing ControlMaster session."
  (interactive)
  (let* ((sess (ssh-session-manager--pick-master-session))
         (ctl (ssh-session-manager-session-control-path sess))
         (pair (if (derived-mode-p 'ssh-session-manager-forwards-mode)
                   (let* ((row (tabulated-list-get-id))
                          (fwd (cdr-safe row)))
                     (unless (and fwd (memq (ssh-session-manager-forward-type fwd)
                                            '(local remote dynamic)))
                       (user-error "Forward at point cannot be canceled via ssh -O"))
                     (cons (ssh-session-manager-forward-type fwd)
                           (or (ssh-session-manager-forward-spec fwd)
                               (ssh-session-manager--read-forward-spec
                                (ssh-session-manager-forward-type fwd)
                                (concat (ssh-session-manager--forward-bind-display fwd) ":")))))
                 (ssh-session-manager--pick-session-forward
                  sess "Cancel forward: "))))
    (ssh-session-manager--ctl-cancel-forward ctl (car pair) (cdr pair))
    (ssh-session-manager--refresh-all-views)
    (message "Canceled %s %s on %s"
             (ssh-session-manager--forward-cli-flag (car pair))
             (cdr pair)
             (ssh-session-manager--format-host-cell sess))))

(defun ssh-session-manager-edit-forward (&optional change-type)
  "Modify an existing port forward on an outgoing ControlMaster session.
Cancels the old forward and adds the updated one, rolling back if
adding the replacement fails.  With prefix arg CHANGE-TYPE, also
prompt for a new forward type (`-L', `-R', `-D')."
  (interactive "P")
  (let* ((sess (ssh-session-manager--pick-master-session))
         (ctl (ssh-session-manager-session-control-path sess))
         (old (if (derived-mode-p 'ssh-session-manager-forwards-mode)
                  (let* ((row (tabulated-list-get-id))
                         (fwd (cdr-safe row)))
                    (unless (and fwd (memq (ssh-session-manager-forward-type fwd)
                                           '(local remote dynamic)))
                      (user-error "Forward at point cannot be edited via ssh -O"))
                    (cons (ssh-session-manager-forward-type fwd)
                          (or (ssh-session-manager-forward-spec fwd)
                              (ssh-session-manager--read-forward-spec
                               (ssh-session-manager-forward-type fwd)
                               (concat (ssh-session-manager--forward-bind-display fwd) ":")))))
                (ssh-session-manager--pick-session-forward
                 sess "Edit forward: ")))
         (old-type (car old))
         (old-spec (cdr old))
         (new-type (if change-type
                       (ssh-session-manager--read-forward-type old-type)
                     old-type))
         (new-spec (ssh-session-manager--read-forward-spec new-type old-spec)))
    (when (and (eq old-type new-type) (string= old-spec new-spec))
      (user-error "Forward specification unchanged"))
    (ssh-session-manager--ctl-edit-forward ctl old-type old-spec new-type new-spec)
    (ssh-session-manager--refresh-all-views)
    (message "Updated forward on %s: %s %s -> %s %s"
             (ssh-session-manager--format-host-cell sess)
             (ssh-session-manager--forward-cli-flag old-type) old-spec
             (ssh-session-manager--forward-cli-flag new-type) new-spec)))


;;; Interactive Commands: Connect, TRAMP, Copy, Toggles

;;;###autoload
(defun ssh-session-manager-connect (destination &optional forward-args)
  "Start a new background ControlMaster SSH session to DESTINATION.
Optional FORWARD-ARGS is a list of extra CLI arguments such as
`(\"-L\" \"8080:localhost:80\")'.  When called interactively, prompts
for DESTINATION (completing from `~/.ssh/config') and optional initial
port forwards."
  (interactive
   (let* ((hosts (ssh-session-manager--read-ssh-config-hosts))
          (dest (completing-read "SSH destination: " hosts nil nil nil
                                 'ssh-session-manager-host-history))
          (fwd-str (string-trim
                    (read-from-minibuffer
                     "Initial forwards (optional, e.g. -L 8080:localhost:80 -D 1080): "
                     nil nil nil 'ssh-session-manager-forward-history))))
     (when (string-empty-p dest)
       (user-error "Destination cannot be empty"))
     (list dest (unless (string-empty-p fwd-str)
                  (split-string-and-unquote fwd-str)))))
  (let* ((cfg (ssh-session-manager--query-ssh-config destination))
         (ctl (plist-get cfg :control-path))
         (ctl-args
          (if ctl
              (list "-o" "ControlMaster=auto" "-o" "ControlPersist=yes")
            (let ((dir (expand-file-name ssh-session-manager-control-directory)))
              (unless (file-directory-p (ssh-session-manager--host-path dir))
                (make-directory (ssh-session-manager--host-path dir) t)
                (set-file-modes (ssh-session-manager--host-path dir) #o700))
              (list "-o" "ControlMaster=yes"
                    "-o" "ControlPersist=yes"
                    "-S" (expand-file-name "%C" dir)))))
         (args (append (list "-f" "-N")
                       ctl-args
                       forward-args
                       (list "--" destination)))
         (out-buf (generate-new-buffer (format " *ssh-connect-%s*" destination)))
         (proc (apply #'start-file-process
                      (format "ssh-connect-%s" destination)
                      out-buf
                      ssh-session-manager-ssh-program
                      args)))
    (set-process-filter
     proc
     (lambda (p chunk)
       (when (buffer-live-p (process-buffer p))
         (with-current-buffer (process-buffer p)
           (goto-char (point-max))
           (insert chunk)))
       (let ((trimmed (string-trim chunk)))
         (unless (string-empty-p trimmed)
           (message "ssh (%s): %s" destination trimmed)))))
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (let ((rc (process-exit-status p))
               (out (when (buffer-live-p (process-buffer p))
                      (with-current-buffer (process-buffer p)
                        (string-trim (buffer-string))))))
           (when (buffer-live-p (process-buffer p))
             (kill-buffer (process-buffer p)))
           (if (eql rc 0)
               (progn
                 (ssh-session-manager--refresh-all-views)
                 (message "Connected SSH ControlMaster to %s" destination))
             (message "SSH connect to %s failed (exit %d): %s"
                      destination rc (or out "")))))))
    (message "Connecting SSH ControlMaster to %s..." destination)))

(defun ssh-session-manager-open-tramp ()
  "Open a TRAMP Dired buffer for the outgoing SSH session at point."
  (interactive)
  (let ((sess (ssh-session-manager--session-at-point)))
    (unless (eq (ssh-session-manager-session-direction sess) 'out)
      (user-error "TRAMP open applies only to outgoing SSH sessions"))
    (let* ((host (ssh-session-manager-session-host sess))
           (ruser (ssh-session-manager-session-remote-user sess))
           (port (ssh-session-manager-session-port sess))
           (target (format "/ssh:%s%s%s:"
                           (if ruser (concat ruser "@") "")
                           host
                           (if (and port (/= port 22))
                               (format "#%d" port)
                             ""))))
      (find-file target))))

(defun ssh-session-manager-copy-info ()
  "Copy a selected field of the SSH session at point to the kill ring."
  (interactive)
  (let* ((sess (ssh-session-manager--session-at-point))
         (candidates
          (delq nil
                (append
                 (list (cons (format "PID: %d" (ssh-session-manager-session-pid sess))
                             (number-to-string (ssh-session-manager-session-pid sess)))
                       (cons (format "Host/Peer: %s" (ssh-session-manager--format-host-cell sess))
                             (ssh-session-manager--format-host-cell sess)))
                 (when-let* ((peer (ssh-session-manager-session-peer-addr sess)))
                   (list (cons (format "Peer addr: %s" peer) peer)))
                 (when-let* ((ctl (ssh-session-manager-session-control-path sess)))
                   (list (cons (format "ControlPath: %s" ctl) ctl)))
                 (list (cons (format "Command: %s" (ssh-session-manager-session-command sess))
                             (ssh-session-manager-session-command sess)))
                 (mapcar (lambda (f)
                           (let ((val (or (ssh-session-manager-forward-spec f)
                                          (ssh-session-manager--forward-bind-display f))))
                             (cons (format "Forward: %s" (ssh-session-manager--forward-summary f))
                                   val)))
                         (ssh-session-manager-session-forwards sess)))))
         (choice (completing-read "Copy field: " candidates nil t))
         (value (cdr (assoc choice candidates))))
    (kill-new value)
    (message "Copied: %s" value)))

(defun ssh-session-manager-toggle-sudo ()
  "Toggle privileged socket inspection in the current buffer."
  (interactive)
  (setq ssh-session-manager--sudo (not ssh-session-manager--sudo))
  (revert-buffer nil t)
  (message "Privileged socket inspection %s"
           (if ssh-session-manager--sudo "enabled" "disabled")))

(defun ssh-session-manager-toggle-all-users ()
  "Toggle showing SSH sessions for all users vs only current user."
  (interactive)
  (setq ssh-session-manager--all-users (not ssh-session-manager--all-users))
  (revert-buffer nil t)
  (message "Showing SSH sessions for %s"
           (if ssh-session-manager--all-users "all users" (user-login-name))))


;;; Session Inspect Buffer (`*SSH Session: <pid>*')

(defun ssh-session-manager--insert-kv (label value &optional face)
  "Insert a formatted `LABEL: VALUE' line into the inspect buffer.
When FACE is non-nil, apply it to VALUE."
  (when value
    (insert (propertize (format "  %-16s " (concat label ":"))
                        'face 'ssh-session-manager-label)
            (if face (propertize value 'face face) value)
            "\n")))

(defun ssh-session-manager--render-inspect-buffer (sess &optional ctl-details)
  "Render detailed information about SESS into the current buffer.
Optional CTL-DETAILS is a plist `(:check STR :conninfo STR :channels STR)'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize
             (format "SSH Session PID %d (%s — %s)\n"
                     (ssh-session-manager-session-pid sess)
                     (if (eq (ssh-session-manager-session-direction sess) 'in)
                         "Incoming" "Outgoing")
                     (ssh-session-manager--format-host-cell sess))
             'face 'ssh-session-manager-section-heading))
    (insert (make-string 68 ?─) "\n\n")

    (when (ssh-session-manager-session-current-p sess)
      (insert (propertize
               "  * This session hosts the current Emacs instance.\n\n"
               'face 'ssh-session-manager-current)))

    (when-let* ((hung (ssh-session-manager-session-hung-reasons sess)))
      (insert (propertize
               (format "  ! Hung indicators: %s\n\n" (string-join hung "; "))
               'face 'ssh-session-manager-hung)))

    (when-let* ((stale (ssh-session-manager-session-stale-reasons sess)))
      (insert (propertize
               (format "  ! Stale indicators: %s\n\n" (string-join stale "; "))
               'face 'ssh-session-manager-stale)))

    (insert (propertize "Overview\n" 'face 'ssh-session-manager-section-heading))
    (ssh-session-manager--insert-kv
     "Direction" (format "%s (%s)"
                         (ssh-session-manager-session-direction sess)
                         (ssh-session-manager-session-role sess)))
    (ssh-session-manager--insert-kv
     "PID / PPID" (format "%d / %d"
                          (ssh-session-manager-session-pid sess)
                          (ssh-session-manager-session-ppid sess)))
    (ssh-session-manager--insert-kv "User" (ssh-session-manager-session-user sess))
    (when (ssh-session-manager-session-remote-user sess)
      (ssh-session-manager--insert-kv
       "Remote User" (ssh-session-manager-session-remote-user sess)))
    (ssh-session-manager--insert-kv "Host / Peer" (ssh-session-manager--format-host-cell sess))
    (ssh-session-manager--insert-kv "TTY" (or (ssh-session-manager-session-tty sess) "-"))
    (ssh-session-manager--insert-kv "Process State" (ssh-session-manager-session-state sess))
    (ssh-session-manager--insert-kv
     "Uptime" (ssh-session-manager--format-duration
               (ssh-session-manager-session-elapsed sess)))
    (when (ssh-session-manager-session-idle sess)
      (ssh-session-manager--insert-kv
       "TTY Idle" (ssh-session-manager--format-duration
                   (ssh-session-manager-session-idle sess))))
    (ssh-session-manager--insert-kv "Command" (ssh-session-manager-session-command sess))

    (when (or (ssh-session-manager-session-local-addr sess)
              (ssh-session-manager-session-peer-addr sess)
              (ssh-session-manager-session-tcp-state sess))
      (insert "\n" (propertize "Transport Socket\n"
                               'face 'ssh-session-manager-section-heading))
      (ssh-session-manager--insert-kv "Local Address" (ssh-session-manager-session-local-addr sess))
      (ssh-session-manager--insert-kv "Peer Address" (ssh-session-manager-session-peer-addr sess))
      (ssh-session-manager--insert-kv "TCP State" (ssh-session-manager-session-tcp-state sess))
      (when (or (ssh-session-manager-session-recv-q sess)
                (ssh-session-manager-session-send-q sess))
        (ssh-session-manager--insert-kv
         "Queues" (format "Recv-Q=%d  Send-Q=%d"
                          (or (ssh-session-manager-session-recv-q sess) 0)
                          (or (ssh-session-manager-session-send-q sess) 0))))
      (when-let* ((timer (ssh-session-manager-session-tcp-timer sess)))
        (ssh-session-manager--insert-kv
         "TCP Timer" (format "%s (time=%s, retries=%d)"
                             (plist-get timer :kind)
                             (plist-get timer :time)
                             (plist-get timer :retries)))))

    (insert "\n" (propertize "Port Forwards\n" 'face 'ssh-session-manager-section-heading))
    (let ((fwds (ssh-session-manager-session-forwards sess)))
      (if (null fwds)
          (insert "  (none detected)\n")
        (dolist (f fwds)
          (insert "  "
                  (ssh-session-manager--forward-summary f)
                  (format "  [source=%s%s%s]\n"
                          (ssh-session-manager-forward-source f)
                          (if (ssh-session-manager-forward-active-p f) ", live" "")
                          (if (ssh-session-manager-forward-spec f)
                              (concat ", spec=" (ssh-session-manager-forward-spec f))
                            "")))))
      (when (ssh-session-manager-session-inferred-p sess)
        (insert (propertize
                 "  Note: `?' forwards were inferred from unattributed user-owned sockets.\n  Enable sudo inspection (`!') in *SSH Sessions* for exact PID attribution.\n"
                 'face 'shadow))))

    (when-let* ((ctl (ssh-session-manager-session-control-path sess)))
      (insert "\n" (propertize "ControlMaster\n" 'face 'ssh-session-manager-section-heading))
      (ssh-session-manager--insert-kv "ControlPath" ctl)
      (when-let* ((chk (plist-get ctl-details :check)))
        (ssh-session-manager--insert-kv "Master Check" chk))
      (when-let* ((ci (plist-get ctl-details :conninfo)))
        (insert "\n" (propertize "  Connection Info (ssh -O conninfo):\n"
                                 'face 'ssh-session-manager-label))
        (dolist (l (split-string ci "\n" t))
          (insert "    " l "\n")))
      (when-let* ((ch (plist-get ctl-details :channels)))
        (insert "\n" (propertize "  Open Channels (ssh -O channels):\n"
                                 'face 'ssh-session-manager-label))
        (dolist (l (split-string ch "\n" t))
          (insert "    " l "\n"))))

    (when-let* ((children (ssh-session-manager-session-children sess)))
      (insert "\n" (propertize "Child Processes\n" 'face 'ssh-session-manager-section-heading))
      (dolist (c children)
        (insert (format "  %7d  %s\n" (car c) (cdr c)))))

    (insert "\n" (propertize
                  "Keys: [g] refresh  [a] add fwd  [e] edit fwd  [r] cancel fwd  [k] kill  [q] quit\n"
                  'face 'shadow))
    (goto-char (point-min))))

(defun ssh-session-manager--fetch-ctl-details (control-path)
  "Query `ssh -O check', `conninfo', and `channels' for CONTROL-PATH."
  (when control-path
    (let* ((chk (ssh-session-manager--run-ctl control-path "check"))
           (ci (ssh-session-manager--run-ctl control-path "conninfo"))
           (ch (ssh-session-manager--run-ctl control-path "channels")))
      (list :check (string-trim (if (string-empty-p (nth 2 chk))
                                    (nth 1 chk)
                                  (nth 2 chk)))
            :conninfo (when (eql (car ci) 0) (string-trim (nth 1 ci)))
            :channels (when (eql (car ch) 0) (string-trim (nth 1 ch)))))))

(defvar ssh-session-manager-inspect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?g] #'ssh-session-manager-inspect-refresh)
    (define-key map [?a] #'ssh-session-manager-add-forward)
    (define-key map [?e] #'ssh-session-manager-edit-forward)
    (define-key map [?r] #'ssh-session-manager-remove-forward)
    (define-key map [?k] #'ssh-session-manager-inspect-kill)
    (define-key map [?K] #'ssh-session-manager-inspect-force-kill)
    map)
  "Keymap for `ssh-session-manager-inspect-mode'.")

(define-derived-mode ssh-session-manager-inspect-mode special-mode "SSH-Inspect"
  "Major mode for inspecting a single SSH session."
  (setq-local truncate-lines nil))

(defun ssh-session-manager--inspect-current-session (&optional refresh)
  "Return the session currently displayed in the inspect buffer.
When REFRESH is non-nil, refresh the source buffer (or re-collect
directly if the source buffer is gone)."
  (when ssh-session-manager--inspect-pid
    (if (and ssh-session-manager--source-buffer
             (buffer-live-p ssh-session-manager--source-buffer))
        (with-current-buffer ssh-session-manager--source-buffer
          (when refresh (revert-buffer nil t))
          (ssh-session-manager--session-by-pid
           ssh-session-manager--inspect-pid))
      (seq-find (lambda (s)
                  (eql (ssh-session-manager-session-pid s)
                       ssh-session-manager--inspect-pid))
                (car (ssh-session-manager--collect-all
                      ssh-session-manager-show-all-users
                      ssh-session-manager-use-sudo))))))

(defun ssh-session-manager-inspect-refresh (&optional no-source-revert)
  "Refresh the current `*SSH Session: ...*' buffer.
When NO-SOURCE-REVERT is non-nil, read cached session data from the
source buffer without reverting it again."
  (interactive)
  (when ssh-session-manager--inspect-pid
    (let ((sess (ssh-session-manager--inspect-current-session
                 (not no-source-revert))))
      (if sess
          (ssh-session-manager--render-inspect-buffer
           sess
           (ssh-session-manager--fetch-ctl-details
            (ssh-session-manager-session-control-path sess)))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "SSH session PID %d is no longer running.\n"
                          ssh-session-manager--inspect-pid)))))))

(defun ssh-session-manager-inspect-kill (&optional arg)
  "Kill the inspected SSH session (SIGTERM, or prompt with prefix ARG)."
  (interactive "P")
  (let ((sess (ssh-session-manager--inspect-current-session)))
    (unless sess
      (user-error "Session PID %d is no longer running" ssh-session-manager--inspect-pid))
    (let ((sig (if arg
                   (completing-read "Signal: " '("TERM" "KILL" "HUP" "INT") nil nil nil nil "TERM")
                 "TERM")))
      (ssh-session-manager--confirm-and-kill (list sess) sig))))

(defun ssh-session-manager-inspect-force-kill ()
  "Send SIGKILL to the inspected SSH session."
  (interactive)
  (let ((sess (ssh-session-manager--inspect-current-session)))
    (unless sess
      (user-error "Session PID %d is no longer running" ssh-session-manager--inspect-pid))
    (ssh-session-manager--confirm-and-kill (list sess) "KILL")))

(defun ssh-session-manager-inspect ()
  "Inspect the SSH session at point in a detail buffer."
  (interactive)
  (let* ((src (cond
               ((derived-mode-p 'ssh-session-manager-mode) (current-buffer))
               ((derived-mode-p 'ssh-session-manager-forwards-mode)
                (or ssh-session-manager--source-buffer (current-buffer)))
               (t (current-buffer))))
         (sess (cond
                ((derived-mode-p 'ssh-session-manager-mode)
                 (ssh-session-manager--session-at-point))
                ((derived-mode-p 'ssh-session-manager-forwards-mode)
                 (or (car-safe (tabulated-list-get-id))
                     (user-error "Forward is unattributed; press `!' to enable sudo attribution")))
                (t (user-error "No SSH session at point"))))
         (pid (ssh-session-manager-session-pid sess))
         (dir default-directory)
         (ctl-details (ssh-session-manager--fetch-ctl-details
                       (ssh-session-manager-session-control-path sess)))
         (buf (get-buffer-create (format "*SSH Session: %d*" pid))))
    (with-current-buffer buf
      (setq default-directory dir)
      (ssh-session-manager-inspect-mode)
      (setq ssh-session-manager--inspect-pid pid
            ssh-session-manager--source-buffer src)
      (ssh-session-manager--render-inspect-buffer sess ctl-details))
    (display-buffer buf)))


;;; Dedicated Port Forwards Buffer (`*SSH Port Forwards*')

(defun ssh-session-manager--forward-status-string (fwd)
  "Return status label for FWD in `*SSH Port Forwards*'."
  (pcase (ssh-session-manager-forward-source fwd)
    ('inferred (propertize "INFERRED" 'face 'ssh-session-manager-stale))
    (_
     (cond
      ((eq (ssh-session-manager-forward-type fwd) 'active) "ACTIVE")
      ((ssh-session-manager-forward-active-p fwd)
       (propertize "LISTEN" 'face 'success))
      ((eq (ssh-session-manager-forward-source fwd) 'runtime) "RUNTIME")
      (t "DECLARED")))))

(defun ssh-session-manager--forward-row-to-entry (row)
  "Convert ROW `(SESSION-OR-NIL . FORWARD)' to a `tabulated-list-entries' item."
  (let* ((sess (car row))
         (fwd (cdr row))
         (type (ssh-session-manager-forward-type fwd))
         (pid (and sess (ssh-session-manager-session-pid sess))))
    (list row
          (vector
           (propertize (ssh-session-manager--forward-type-tag type)
                       'face (ssh-session-manager--forward-face type))
           (if sess
               (ssh-session-manager--format-dir-cell sess)
             (propertize "IN?" 'face 'ssh-session-manager-stale))
           (propertize (if pid (number-to-string pid) "-")
                       'ssh-session-manager-num (or pid -1))
           (if sess
               (ssh-session-manager--format-host-cell sess)
             "(unattributed; press ! for sudo)")
           (ssh-session-manager--forward-bind-display fwd)
           (or (ssh-session-manager--forward-target-display fwd)
               (if (and sess (eq (ssh-session-manager-session-direction sess) 'in))
                   "(via client)"
                 "?"))
           (ssh-session-manager--forward-status-string fwd)))))

(defun ssh-session-manager--forwards-refresh-data ()
  "Refresh data for `*SSH Port Forwards*'."
  (pcase-let ((`(,sessions . ,unattr)
               (ssh-session-manager--collect-all
                ssh-session-manager--all-users
                ssh-session-manager--sudo)))
    (setq ssh-session-manager--sessions sessions
          ssh-session-manager--unattributed-forwards unattr)
    (let (rows)
      (dolist (s sessions)
        (when (or (null ssh-session-manager--forwards-filter-pid)
                  (eql (ssh-session-manager-session-pid s)
                       ssh-session-manager--forwards-filter-pid))
          (dolist (f (ssh-session-manager-session-forwards s))
            (push (cons s f) rows))))
      (unless ssh-session-manager--forwards-filter-pid
        (dolist (f unattr)
          (push (cons nil f) rows)))
      (setq ssh-session-manager--forward-rows (nreverse rows)
            tabulated-list-entries
            (mapcar #'ssh-session-manager--forward-row-to-entry
                    ssh-session-manager--forward-rows))
      (setq mode-name
            (concat "SSH-Forwards"
                    (cond
                     (ssh-session-manager--forwards-filter-pid
                      (format "[PID %d]" ssh-session-manager--forwards-filter-pid))
                     (ssh-session-manager--sudo "[sudo]")
                     (t ""))))
      (force-mode-line-update))))

(defun ssh-session-manager-forwards-kill-owner (&optional arg)
  "Kill the SSH session owning the port forward at point.
Sends SIGTERM by default, or prompts for signal with prefix ARG."
  (interactive "P")
  (let ((sess (car-safe (tabulated-list-get-id))))
    (unless sess
      (user-error "Forward is unattributed; press `!' to enable sudo attribution"))
    (let ((sig (if arg
                   (completing-read "Signal: " '("TERM" "KILL" "HUP" "INT") nil nil nil nil "TERM")
                 "TERM")))
      (ssh-session-manager--confirm-and-kill (list sess) sig))))

(defun ssh-session-manager-forwards-copy ()
  "Copy the forward specification or bind address at point."
  (interactive)
  (let* ((row (tabulated-list-get-id))
         (fwd (cdr-safe row)))
    (unless fwd
      (user-error "No forward at point"))
    (let ((val (or (ssh-session-manager-forward-spec fwd)
                   (ssh-session-manager--forward-bind-display fwd))))
      (kill-new val)
      (message "Copied: %s" val))))

(defun ssh-session-manager-forwards-switch-to-sessions ()
  "Switch from `*SSH Port Forwards*' back to `*SSH Sessions*'."
  (interactive)
  (let ((pid (when-let* ((sess (car-safe (tabulated-list-get-id))))
               (ssh-session-manager-session-pid sess))))
    (ssh-session-manager)
    (when pid
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (eql (tabulated-list-get-id) pid)))
        (forward-line 1)))))

(defvar ssh-session-manager-forwards-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-m] #'ssh-session-manager-inspect)
    (define-key map [?i] #'ssh-session-manager-inspect)
    (define-key map [?a] #'ssh-session-manager-add-forward)
    (define-key map [?e] #'ssh-session-manager-edit-forward)
    (define-key map [?r] #'ssh-session-manager-remove-forward)
    (define-key map [?d] #'ssh-session-manager-remove-forward)
    (define-key map [?K] #'ssh-session-manager-forwards-kill-owner)
    (define-key map [?w] #'ssh-session-manager-forwards-copy)
    (define-key map [?s] #'ssh-session-manager-forwards-switch-to-sessions)
    (define-key map [?!] #'ssh-session-manager-toggle-sudo)
    (define-key map [?A] #'ssh-session-manager-toggle-all-users)
    map)
  "Keymap for `ssh-session-manager-forwards-mode'.")

(define-derived-mode ssh-session-manager-forwards-mode tabulated-list-mode "SSH-Forwards"
  "Major mode for viewing and managing SSH port forwards.

\\{ssh-session-manager-forwards-mode-map}"
  (setq tabulated-list-format
        (vector
         '("Type" 6 t)
         '("Dir" 5 t)
         (list "PID" 8 (ssh-session-manager--make-num-sorter 2) :right-align t)
         '("Host/Peer" 24 t)
         '("Bind" 28 t)
         '("Target" 24 t)
         '("Status" 0 t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Bind" . nil))
  (setq ssh-session-manager--all-users ssh-session-manager-show-all-users
        ssh-session-manager--sudo ssh-session-manager-use-sudo)
  (add-hook 'tabulated-list-revert-hook
            #'ssh-session-manager--forwards-refresh-data nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun ssh-session-manager-list-forwards (&optional pid)
  "Display all SSH port forwards in `*SSH Port Forwards*'.
If PID is non-nil, show only forwards belonging to that session."
  (interactive)
  (let* ((dir default-directory)
         (src (when (derived-mode-p 'ssh-session-manager-mode) (current-buffer)))
         (all-u (if src ssh-session-manager--all-users
                  ssh-session-manager-show-all-users))
         (sudo (if src ssh-session-manager--sudo
                 ssh-session-manager-use-sudo))
         (buf (get-buffer-create ssh-session-manager--forwards-buffer-name)))
    (with-current-buffer buf
      (setq default-directory dir)
      (unless (derived-mode-p 'ssh-session-manager-forwards-mode)
        (ssh-session-manager-forwards-mode))
      (setq ssh-session-manager--source-buffer src
            ssh-session-manager--forwards-filter-pid pid
            ssh-session-manager--all-users all-u
            ssh-session-manager--sudo sudo)
      (revert-buffer nil t))
    (pop-to-buffer buf)))

(defun ssh-session-manager-list-session-forwards ()
  "Open `*SSH Port Forwards*' filtered to the SSH session at point."
  (interactive)
  (let ((sess (ssh-session-manager--session-at-point)))
    (ssh-session-manager-list-forwards
     (ssh-session-manager-session-pid sess))))


;;; Main Mode (`ssh-session-manager-mode') & Entry Point

(defvar ssh-session-manager-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?i] #'ssh-session-manager-filter-incoming)
    (define-key map [?o] #'ssh-session-manager-filter-outgoing)
    (define-key map [?f] #'ssh-session-manager-filter-forwards)
    (define-key map [?h] #'ssh-session-manager-filter-hung)
    (define-key map [?m] #'ssh-session-manager-filter-masters)
    (define-key map [?u] #'ssh-session-manager-filter-user)
    (define-key map [?r] #'ssh-session-manager-filter-regexp)
    (define-key map [?/] #'ssh-session-manager-filter-clear)
    map)
  "Prefix keymap for filtering in `ssh-session-manager-mode'.")

(defvar ssh-session-manager-forward-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?a] #'ssh-session-manager-add-forward)
    (define-key map [?r] #'ssh-session-manager-remove-forward)
    (define-key map [?e] #'ssh-session-manager-edit-forward)
    (define-key map [?l] #'ssh-session-manager-list-session-forwards)
    map)
  "Prefix keymap for port-forward commands in `ssh-session-manager-mode'.")

(defvar ssh-session-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-m] #'ssh-session-manager-inspect)
    (define-key map [?i] #'ssh-session-manager-inspect)
    (define-key map [?F] #'ssh-session-manager-list-forwards)
    (define-key map [?c] #'ssh-session-manager-check-master)
    (define-key map [?m] #'ssh-session-manager-mark)
    (define-key map [?d] #'ssh-session-manager-mark-delete)
    (define-key map [?u] #'ssh-session-manager-unmark)
    (define-key map [?U] #'ssh-session-manager-unmark-all)
    (define-key map [?t] #'ssh-session-manager-toggle-marks)
    (define-key map [?H] #'ssh-session-manager-mark-hung)
    (define-key map [?k] #'ssh-session-manager-kill)
    (define-key map [?K] #'ssh-session-manager-force-kill)
    (define-key map [?x] #'ssh-session-manager-execute-deletes)
    (define-key map [?E] #'ssh-session-manager-master-exit)
    (define-key map [?P] #'ssh-session-manager-master-stop)
    (define-key map [?a] #'ssh-session-manager-add-forward)
    (define-key map [?r] #'ssh-session-manager-remove-forward)
    (define-key map [?e] #'ssh-session-manager-edit-forward)
    (define-key map [?f] ssh-session-manager-forward-map)
    (define-key map [?/] ssh-session-manager-filter-map)
    (define-key map [?+] #'ssh-session-manager-connect)
    (define-key map [?T] #'ssh-session-manager-open-tramp)
    (define-key map [?w] #'ssh-session-manager-copy-info)
    (define-key map [?!] #'ssh-session-manager-toggle-sudo)
    (define-key map [?A] #'ssh-session-manager-toggle-all-users)
    map)
  "Keymap for `ssh-session-manager-mode'.")

(defun ssh-session-manager--stop-auto-refresh ()
  "Cancel the buffer-local auto-refresh timer if active."
  (when (timerp ssh-session-manager--refresh-timer)
    (cancel-timer ssh-session-manager--refresh-timer)
    (setq ssh-session-manager--refresh-timer nil)))

(defun ssh-session-manager--maybe-start-auto-refresh ()
  "Start buffer-local auto-refresh timer if configured."
  (ssh-session-manager--stop-auto-refresh)
  (when (and (numberp ssh-session-manager-auto-refresh-interval)
             (> ssh-session-manager-auto-refresh-interval 0))
    (let ((buf (current-buffer)))
      (setq ssh-session-manager--refresh-timer
            (run-with-timer
             ssh-session-manager-auto-refresh-interval
             ssh-session-manager-auto-refresh-interval
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (get-buffer-window buf t)
                     (revert-buffer nil t))))))))))

(defun ssh-session-manager--revert (&optional _ignore-auto _noconfirm)
  "Revert function for `ssh-session-manager-mode'."
  (ssh-session-manager--refresh-data)
  (tabulated-list-print t)
  (ssh-session-manager--apply-marks))

(define-derived-mode ssh-session-manager-mode tabulated-list-mode "SSH-Sessions"
  "Major mode for managing incoming and outgoing SSH sessions.

\\{ssh-session-manager-mode-map}"
  (setq tabulated-list-format
        (vector
         '("Dir" 5 t)
         (list "PID" 8 (ssh-session-manager--make-num-sorter 1) :right-align t)
         '("User" 10 t)
         '("Host/Peer" 24 t)
         '("TTY" 8 t)
         '("State" 10 t)
         (list "Uptime" 8 (ssh-session-manager--make-num-sorter 6) :right-align t)
         (list "Idle" 7 (ssh-session-manager--make-num-sorter 7) :right-align t)
         '("Forwards" 0 t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Dir" . nil))
  (setq ssh-session-manager--all-users ssh-session-manager-show-all-users
        ssh-session-manager--sudo ssh-session-manager-use-sudo)
  (setq-local revert-buffer-function #'ssh-session-manager--revert)
  (add-hook 'kill-buffer-hook #'ssh-session-manager--stop-auto-refresh nil t)
  (tabulated-list-init-header)
  (ssh-session-manager--maybe-start-auto-refresh))

;;;###autoload
(defun ssh-session-manager (&optional directory)
  "Open the SSH session manager buffer (`*SSH Sessions*').
With prefix argument DIRECTORY (or when called from a remote TRAMP
buffer), inspect SSH sessions on that host."
  (interactive
   (list (if current-prefix-arg
             (read-directory-name "Manage SSH sessions on host/dir: ")
           default-directory)))
  (let* ((dir (or directory default-directory))
         (remote (file-remote-p dir))
         (buf-name (if remote
                       (format "*SSH Sessions (%s)*"
                               (string-remove-suffix ":" remote))
                     ssh-session-manager--buffer-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (setq default-directory dir)
      (unless (derived-mode-p 'ssh-session-manager-mode)
        (ssh-session-manager-mode))
      (revert-buffer nil t))
    (pop-to-buffer buf)))

(provide 'ssh-session-manager)
;;; ssh-session-manager.el ends here
