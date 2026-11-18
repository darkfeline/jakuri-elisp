;;; ssh-session-manager-test.el --- ssh-session-manager.el tests -*- lexical-binding: t; -*-

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

;; Unit and integration tests for ssh-session-manager.el.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ssh-session-manager)


;;; Time & Duration Tests

(ert-deftest ssh-session-manager-test-parse-etime ()
  (should (= (ssh-session-manager--parse-etime "00:42") 42))
  (should (= (ssh-session-manager--parse-etime "12:34") (+ (* 12 60) 34)))
  (should (= (ssh-session-manager--parse-etime "01:02:03")
             (+ 3600 120 3)))
  (should (= (ssh-session-manager--parse-etime "2-03:04:05")
             (+ (* 2 86400) (* 3 3600) (* 4 60) 5)))
  (should (= (ssh-session-manager--parse-etime "17135") 17135))
  (should (= (ssh-session-manager--parse-etime "") 0)))

(ert-deftest ssh-session-manager-test-format-duration ()
  (should (equal (ssh-session-manager--format-duration nil) "-"))
  (should (equal (ssh-session-manager--format-duration 42) "42s"))
  (should (equal (ssh-session-manager--format-duration 125) "2m05s"))
  (should (equal (ssh-session-manager--format-duration 3720) "1h02m"))
  (should (equal (ssh-session-manager--format-duration 93600) "1d02h")))


;;; Forward Specification Parsing Tests

(ert-deftest ssh-session-manager-test-parse-forward-local ()
  ;; port:host:hostport
  (let ((f (ssh-session-manager--parse-forward-spec 'local "8080:localhost:80" 'cli)))
    (should f)
    (should (eq (ssh-session-manager-forward-type f) 'local))
    (should (eq (ssh-session-manager-forward-proto f) 'tcp))
    (should (null (ssh-session-manager-forward-bind-addr f)))
    (should (= (ssh-session-manager-forward-bind-port f) 8080))
    (should (equal (ssh-session-manager-forward-target-host f) "localhost"))
    (should (= (ssh-session-manager-forward-target-port f) 80))
    (should (equal (ssh-session-manager-forward-spec f) "8080:localhost:80")))
  ;; bind:port:host:hostport with wildcard and IPv6
  (let ((f (ssh-session-manager--parse-forward-spec 'local "[::1]:8443:[::1]:443" 'cli)))
    (should f)
    (should (equal (ssh-session-manager-forward-bind-addr f) "::1"))
    (should (= (ssh-session-manager-forward-bind-port f) 8443))
    (should (equal (ssh-session-manager-forward-target-host f) "::1"))
    (should (= (ssh-session-manager-forward-target-port f) 443)))
  (let ((f (ssh-session-manager--parse-forward-spec 'local ":8080:10.0.0.2:80" 'cli)))
    (should f)
    (should (equal (ssh-session-manager-forward-bind-addr f) "*"))
    (should (= (ssh-session-manager-forward-bind-port f) 8080)))
  ;; Space-separated ssh_config format
  (let ((f (ssh-session-manager--parse-forward-spec
            'local "127.0.0.1:5432 db.internal:5432" 'config)))
    (should f)
    (should (equal (ssh-session-manager-forward-bind-addr f) "127.0.0.1"))
    (should (= (ssh-session-manager-forward-bind-port f) 5432))
    (should (equal (ssh-session-manager-forward-target-host f) "db.internal"))
    (should (= (ssh-session-manager-forward-target-port f) 5432))
    (should (equal (ssh-session-manager-forward-spec f)
                   "127.0.0.1:5432:db.internal:5432")))
  ;; Unix socket variants
  (let ((f (ssh-session-manager--parse-forward-spec
            'local "/tmp/local.sock:/var/run/docker.sock" 'cli)))
    (should f)
    (should (eq (ssh-session-manager-forward-proto f) 'unix))
    (should (equal (ssh-session-manager-forward-bind-addr f) "/tmp/local.sock"))
    (should (equal (ssh-session-manager-forward-target-host f) "/var/run/docker.sock")))
  (let ((f (ssh-session-manager--parse-forward-spec
            'local "127.0.0.1:9000:/var/run/app.sock" 'cli)))
    (should f)
    (should (eq (ssh-session-manager-forward-proto f) 'tcp))
    (should (= (ssh-session-manager-forward-bind-port f) 9000))
    (should (equal (ssh-session-manager-forward-target-host f) "/var/run/app.sock")))
  (let ((f (ssh-session-manager--parse-forward-spec
            'local "/tmp/local.sock:localhost:8080" 'cli)))
    (should f)
    (should (eq (ssh-session-manager-forward-proto f) 'unix))
    (should (equal (ssh-session-manager-forward-bind-addr f) "/tmp/local.sock"))
    (should (= (ssh-session-manager-forward-target-port f) 8080))))

(ert-deftest ssh-session-manager-test-parse-forward-remote-and-dynamic ()
  ;; Remote TCP and remote SOCKS
  (let ((f (ssh-session-manager--parse-forward-spec 'remote "9999:127.0.0.1:9999" 'cli)))
    (should f)
    (should (eq (ssh-session-manager-forward-type f) 'remote))
    (should (= (ssh-session-manager-forward-bind-port f) 9999))
    (should (= (ssh-session-manager-forward-target-port f) 9999)))
  (let ((f (ssh-session-manager--parse-forward-spec 'remote "127.0.0.1:1080" 'cli)))
    (should f)
    (should (equal (ssh-session-manager-forward-target-host f) "SOCKS"))
    (should (= (ssh-session-manager-forward-bind-port f) 1080)))
  (let ((f (ssh-session-manager--parse-forward-spec 'remote "1080" 'cli)))
    (should f)
    (should (equal (ssh-session-manager-forward-target-host f) "SOCKS"))
    (should (= (ssh-session-manager-forward-bind-port f) 1080)))
  ;; Dynamic SOCKS
  (let ((f (ssh-session-manager--parse-forward-spec 'dynamic "1080" 'cli)))
    (should f)
    (should (eq (ssh-session-manager-forward-type f) 'dynamic))
    (should (= (ssh-session-manager-forward-bind-port f) 1080)))
  (let ((f (ssh-session-manager--parse-forward-spec 'dynamic "[::1]:1081" 'cli)))
    (should f)
    (should (equal (ssh-session-manager-forward-bind-addr f) "::1"))
    (should (= (ssh-session-manager-forward-bind-port f) 1081)))
  ;; Invalid specs
  (should-not (ssh-session-manager--parse-forward-spec 'local "notaport:host:80"))
  (should-not (ssh-session-manager--parse-forward-spec 'dynamic "abc"))
  (should-not (ssh-session-manager--parse-forward-spec 'local "99999:host:80")))


;;; Process & Argv Parsing Tests

(ert-deftest ssh-session-manager-test-parse-ps-and-classify ()
  (let* ((ps-out
          (string-join
           '("  100     1 root     Ss   5-12:00:00 ?        sshd: /usr/sbin/sshd -D [listener] 0 of 25-25 startups"
             "  200   100 root     Ss     04:45:38 ?        sshd-session: alice [postauth]"
             "  201   200 alice    S      04:45:35 ?        sshd-session: alice@pts/0"
             "  202   200 alice    S      01:10:00 ?        sshd-session: alice@notty"
             "  203   200 alice    S      00:05:00 ?        sshd: alice@pts/3"
             "  300     1 alice    Ss     00:30:00 ?        ssh: /home/alice/.ssh/cm/prod [mux]"
             "  301   201 alice    S+     00:10:00 pts/0    ssh -fNML8080:localhost:80 -R 9000:127.0.0.1:3000 -D 1080 -S /tmp/ctl.sock -p 2222 bob@prod.example.com"
             "  302   301 alice    S+     00:00:01 pts/0    ssh -O check -S /tmp/ctl.sock prod.example.com"
             "  303   201 alice    S+     00:02:00 pts/0    ssh -W [10.0.0.5]:22 bastion"
             "  304   201 alice    Z+     00:00:00 pts/0    [ssh] <defunct>")
           "\n"))
         (procs (ssh-session-manager--parse-ps-output ps-out))
         (classified (delq nil (mapcar #'ssh-session-manager--classify-proc procs))))
    (should (= (length procs) 10))
    ;; Listener (100), postauth monitor (200), ssh -O (302), defunct (304) skipped.
    (should (equal (mapcar #'ssh-session-manager-session-pid classified)
                   '(201 202 203 300 301 303)))
    ;; Incoming OpenSSH 9.8+ @pts/0
    (let ((s201 (nth 0 classified)))
      (should (eq (ssh-session-manager-session-direction s201) 'in))
      (should (eq (ssh-session-manager-session-role s201) 'session))
      (should (equal (ssh-session-manager-session-tty s201) "pts/0")))
    ;; Incoming @notty
    (let ((s202 (nth 1 classified)))
      (should (eq (ssh-session-manager-session-direction s202) 'in))
      (should (eq (ssh-session-manager-session-role s202) 'notty))
      (should (equal (ssh-session-manager-session-tty s202) "notty")))
    ;; Incoming legacy OpenSSH < 9.8
    (let ((s203 (nth 2 classified)))
      (should (eq (ssh-session-manager-session-direction s203) 'in))
      (should (equal (ssh-session-manager-session-tty s203) "pts/3")))
    ;; ControlPersist [mux]
    (let ((s300 (nth 3 classified)))
      (should (eq (ssh-session-manager-session-direction s300) 'out))
      (should (eq (ssh-session-manager-session-role s300) 'persist))
      (should (equal (ssh-session-manager-session-control-path s300)
                     "/home/alice/.ssh/cm/prod")))
    ;; Outgoing ssh with bundled flags & forwards
    (let ((s301 (nth 4 classified)))
      (should (eq (ssh-session-manager-session-direction s301) 'out))
      (should (eq (ssh-session-manager-session-role s301) 'master))
      (should (equal (ssh-session-manager-session-remote-user s301) "bob"))
      (should (equal (ssh-session-manager-session-host s301) "prod.example.com"))
      (should (= (ssh-session-manager-session-port s301) 2222))
      (should (equal (ssh-session-manager-session-control-path s301) "/tmp/ctl.sock"))
      (should (= (length (ssh-session-manager-session-forwards s301)) 3))
      (should (equal (mapcar #'ssh-session-manager-forward-type
                             (ssh-session-manager-session-forwards s301))
                     '(local remote dynamic))))
    ;; ProxyJump -W
    (let ((s303 (nth 5 classified)))
      (should (eq (ssh-session-manager-session-role s303) 'proxy))
      (should (equal (ssh-session-manager-session-host s303) "bastion")))))

(ert-deftest ssh-session-manager-test-parse-ssh-argv-options ()
  (let ((parsed (ssh-session-manager--parse-ssh-argv
                 '("/usr/bin/ssh"
                   "-o" "LocalForward=127.0.0.1:8080 localhost:80"
                   "-oRemoteForward=9090 localhost:3000"
                   "-o" "ControlPath=/tmp/my.ctl"
                   "-o" "ControlMaster=auto"
                   "ssh://carol@[::1]:2200"
                   "echo" "-L" "not-a-forward"))))
    (should parsed)
    (should (equal (plist-get parsed :host) "::1"))
    (should (equal (plist-get parsed :remote-user) "carol"))
    (should (= (plist-get parsed :port) 2200))
    (should (equal (plist-get parsed :control-path) "/tmp/my.ctl"))
    (should (plist-get parsed :master-flag))
    ;; Remote command `-L` after destination must not be parsed as forward.
    (should (= (length (plist-get parsed :forwards)) 2))))


;;; Socket, `ssh -G`, and Config Parsing Tests

(ert-deftest ssh-session-manager-test-parse-ss-tcp-and-unix ()
  (let* ((tcp-out
          (string-join
           '("LISTEN 0 128 127.0.0.1:8080 0.0.0.0:* users:((\"ssh\",pid=301,fd=4)) uid:1000 ino:111"
             "ESTAB 0 128 [::ffff:172.28.67.62]:22 [::ffff:10.0.0.9]:55000 timer:(on,15sec,4) uid:0 ino:222"
             "ESTAB 0 0 127.0.0.1:49215 127.0.0.1:55590 users:((\"sshd-session\",pid=201,fd=8),(\"sshd-session\",pid=200,fd=8)) uid:0 ino:333")
           "\n"))
         (tcp (ssh-session-manager--parse-ss-tcp tcp-out)))
    (should (= (length tcp) 3))
    (should (equal (ssh-session-manager--sock-pids (nth 0 tcp)) '(301)))
    (should (= (ssh-session-manager--sock-uid (nth 0 tcp)) 1000))
    (let ((s1 (nth 1 tcp)))
      (should (equal (ssh-session-manager--sock-local-addr s1) "172.28.67.62"))
      (should (= (ssh-session-manager--sock-local-port s1) 22))
      (should (equal (ssh-session-manager--sock-peer-addr s1) "10.0.0.9"))
      (should (= (ssh-session-manager--sock-send-q s1) 128))
      (should (equal (ssh-session-manager--sock-timer s1)
                     '(:kind "on" :time "15sec" :retries 4))))
    (should (equal (ssh-session-manager--sock-pids (nth 2 tcp)) '(201 200))))

  (let* ((unix-out
          (string-join
           '("u_str LISTEN 0 64 /tmp/ctl.sock.XBPeuS0u4DZSGEaY 178869350 * 0 users:((\"ssh\",pid=301,fd=6))"
             "u_str LISTEN 0 128 /home/alice/.ssh/agent/s.abc.sshd.201.xyz 169607089 * 0"
             "u_str LISTEN 0 128 /tmp/ssh_luonnotar_dbus.sock 169607088 * 0")
           "\n"))
         (unix (ssh-session-manager--parse-ss-unix unix-out)))
    (should (= (length unix) 3))
    (should (equal (ssh-session-manager--sock-pids (nth 0 unix)) '(301)))
    (should (equal (ssh-session-manager--sshd-agent-socket-pid
                    (ssh-session-manager--sock-local-addr (nth 1 unix)))
                   201))
    ;; Test OpenSSH atomic-bind `.16chars` rename normalization.
    (let ((norm (ssh-session-manager--normalize-mux-socket-path
                 "/tmp/ctl.sock.XBPeuS0u4DZSGEaY"
                 (lambda (p) (equal p "/tmp/ctl.sock")))))
      (should (equal norm '("/tmp/ctl.sock" . t))))))

(ert-deftest ssh-session-manager-test-parse-lsof ()
  (let* ((lsof-out
          (string-join
           '("p301"
             "f3" "tIPv4" "TST=ESTABLISHED" "n10.0.0.1:44444->10.0.0.2:22"
             "f4" "tIPv4" "TST=LISTEN" "n127.0.0.1:8080"
             "f5" "tunix" "n/tmp/ctl.sock type=STREAM")
           "\n"))
         (socks (ssh-session-manager--parse-lsof-output lsof-out)))
    (should (= (length socks) 3))
    (should (equal (ssh-session-manager--sock-state (nth 0 socks)) "ESTAB"))
    (should (= (ssh-session-manager--sock-peer-port (nth 0 socks)) 22))
    (should (equal (ssh-session-manager--sock-state (nth 1 socks)) "LISTEN"))
    (should (= (ssh-session-manager--sock-local-port (nth 1 socks)) 8080))
    (should (equal (ssh-session-manager--sock-local-addr (nth 2 socks))
                   "/tmp/ctl.sock"))))

(ert-deftest ssh-session-manager-test-parse-ssh-G-and-config-hosts ()
  (let ((parsed (ssh-session-manager--parse-ssh-G-output
                 (string-join
                  '("hostname 10.20.30.40"
                    "user deploy"
                    "port 2222"
                    "controlpath /tmp/cm-prod"
                    "localforward 127.0.0.1:5432 localhost:5432"
                    "remoteforward 9999 localhost:9999"
                    "dynamicforward 1080")
                  "\n"))))
    (should (equal (plist-get parsed :hostname) "10.20.30.40"))
    (should (equal (plist-get parsed :user) "deploy"))
    (should (= (plist-get parsed :port) 2222))
    (should (equal (plist-get parsed :control-path) "/tmp/cm-prod"))
    (should (= (length (plist-get parsed :forwards)) 3)))
  (let ((hosts (ssh-session-manager--parse-ssh-config-hosts
                "Host prod staging *.internal\n  User deploy\nHost !bad bastion\n")))
    (should (equal hosts '("prod" "staging" "bastion")))))


;;; End-to-End Correlation Tests

(ert-deftest ssh-session-manager-test-correlate-incoming-non-dumpable ()
  "Verify incoming non-dumpable sshd-session correlation without sudo."
  (let* ((procs
          (ssh-session-manager--parse-ps-output
           (string-join
            '("  100     1 root  Ss   5-00:00:00 ?     sshd: /usr/sbin/sshd -D [listener]"
              "  200   100 root  Ss     02:00:00 ?     sshd-session: alice [postauth]"
              "  201   200 alice S      02:00:00 ?     sshd-session: alice@pts/0"
              "  202   201 alice Ss     02:00:00 pts/0 -bash")
            "\n")))
         (socks
          (append
           (ssh-session-manager--parse-ss-tcp
            (string-join
             '("ESTAB 0 256 172.28.67.62:22 172.253.31.237:58577 timer:(on,12sec,3) uid:0 ino:10"
               "LISTEN 0 128 127.0.0.1:9999 0.0.0.0:* uid:1000 ino:11"
               "LISTEN 0 128 [::1]:9999 [::]:* uid:1000 ino:12"
               "LISTEN 0 128 127.0.0.1:631 0.0.0.0:* uid:0 ino:13")
             "\n"))
           (ssh-session-manager--parse-ss-unix
            (string-join
             '("u_str LISTEN 0 128 /home/alice/.ssh/agent/s.abc.sshd.201.xyz 20 * 0"
               "u_str LISTEN 0 128 /tmp/ssh_luonnotar_dbus.sock 21 * 0"
               "u_str LISTEN 0 128 /run/user/1000/openssh_agent 22 * 0")
             "\n"))))
         (res (ssh-session-manager--correlate-sessions
               procs socks
               (list :current-user "alice"
                     :current-uid 1000
                     :env-fn (lambda (pid)
                               (when (= pid 202)
                                 '(("SSH_CONNECTION" . "172.253.31.237 58577 172.28.67.62 22")
                                   ("SSH_TTY" . "/dev/pts/0")
                                   ("DISPLAY" . "localhost:10.0"))))
                     :idle-fn (lambda (tty) (when (equal tty "pts/0") 120))
                     :file-uid-fn (lambda (p) (if (equal p "/tmp/ssh_luonnotar_dbus.sock") 1000 0))
                     :ancestors '(9999)
                     :emacs-ttys '("pts/0")
                     :emacs-ssh-conn nil)))
         (sessions (car res))
         (unattr (cdr res)))
    (should (= (length sessions) 1))
    (should (null unattr))
    (let ((s (car sessions)))
      (should (= (ssh-session-manager-session-pid s) 201))
      (should (ssh-session-manager-session-current-p s))
      (should (equal (ssh-session-manager-session-peer-addr s) "172.253.31.237:58577"))
      (should (= (ssh-session-manager-session-send-q s) 256))
      ;; Hung detection picked up both Send-Q and TCP retransmit timer!
      (should (>= (length (ssh-session-manager-session-hung-reasons s)) 2))
      (should (ssh-session-manager-session-inferred-p s))
      ;; Forwards: X11, Agent, inferred collapsed R:localhost:9999, inferred R:/tmp/ssh_luonnotar_dbus.sock
      ;; (port 631 owned by uid 0 and openssh_agent are excluded).
      (let ((fwds (ssh-session-manager-session-forwards s)))
        (should (cl-some (lambda (f) (eq (ssh-session-manager-forward-type f) 'x11)) fwds))
        (should (cl-some (lambda (f) (eq (ssh-session-manager-forward-type f) 'agent)) fwds))
        (should (= (length (seq-filter (lambda (f)
                                         (and (eq (ssh-session-manager-forward-type f) 'remote)
                                              (eql (ssh-session-manager-forward-bind-port f) 9999)))
                                       fwds))
                   1))
        (should (cl-some (lambda (f)
                           (equal (ssh-session-manager-forward-bind-addr f)
                                  "/tmp/ssh_luonnotar_dbus.sock"))
                         fwds))))))

(ert-deftest ssh-session-manager-test-correlate-multiple-incoming-unattributed ()
  "With multiple non-dumpable incoming sessions, keep unattributed forwards separate."
  (let* ((procs
          (ssh-session-manager--parse-ps-output
           (string-join
            '("  201   200 alice S  02:00:00 ? sshd-session: alice@pts/0"
              "  205   204 alice S  00:10:00 ? sshd-session: alice@notty")
            "\n")))
         (socks
          (ssh-session-manager--parse-ss-tcp
           "LISTEN 0 128 127.0.0.1:9999 0.0.0.0:* uid:1000 ino:11\n"))
         (res (ssh-session-manager--correlate-sessions
               procs socks
               (list :current-user "alice" :current-uid 1000
                     :env-fn (lambda (_) nil)
                     :idle-fn (lambda (_) nil)
                     :ancestors nil :emacs-ttys nil :emacs-ssh-conn nil))))
    (should (= (length (car res)) 2))
    (should (= (length (cdr res)) 1))
    (should (= (ssh-session-manager-forward-bind-port (car (cdr res))) 9999))))

(ert-deftest ssh-session-manager-test-correlate-outgoing-reconcile ()
  "Verify outgoing ControlMaster forward reconciliation against live `ss'."
  (let ((ssh-session-manager--runtime-forwards (make-hash-table :test 'equal)))
    ;; Simulate an externally canceled CLI forward (8080 missing from ss),
    ;; a live CLI forward (8081 present in ss), a live dynamic external forward (8082 in ss only),
    ;; and a CLI remote forward (9090).
    (let* ((procs
            (ssh-session-manager--parse-ps-output
             "  301 1 alice Ss 00:30:00 ? ssh -N -M -S /tmp/ctl.sock -L 8080:localhost:80 -L 8081:localhost:81 -R 9090:localhost:3000 prod\n"))
           (socks
            (append
             (ssh-session-manager--parse-ss-tcp
              (string-join
               '("ESTAB 0 0 10.0.0.1:45000 10.0.0.2:22 users:((\"ssh\",pid=301,fd=3)) uid:1000"
                 "LISTEN 0 128 127.0.0.1:8081 0.0.0.0:* users:((\"ssh\",pid=301,fd=4)) uid:1000"
                 "LISTEN 0 128 [::1]:8081 [::]:* users:((\"ssh\",pid=301,fd=5)) uid:1000"
                 "LISTEN 0 128 127.0.0.1:8082 0.0.0.0:* users:((\"ssh\",pid=301,fd=7)) uid:1000")
               "\n"))
             (ssh-session-manager--parse-ss-unix
              "u_str LISTEN 0 64 /tmp/ctl.sock.ABCDEFGHIJKLMNOP 999 * 0 users:((\"ssh\",pid=301,fd=6))\n")))
           (res (ssh-session-manager--correlate-sessions
                 procs socks
                 (list :current-user "alice" :current-uid 1000
                       :file-exists-fn (lambda (p) (equal p "/tmp/ctl.sock"))
                       :config-fn (lambda (&rest _) nil)
                       :ancestors nil :emacs-ttys nil :emacs-ssh-conn nil)))
           (sess (car (car res)))
           (fwds (ssh-session-manager-session-forwards sess)))
      (should (eq (ssh-session-manager-session-role sess) 'master))
      (should (equal (ssh-session-manager-session-control-path sess) "/tmp/ctl.sock"))
      ;; 8080 was dropped because not listening in ss; 8081, 9090, 8082 remain.
      (should-not (cl-some (lambda (f) (eql (ssh-session-manager-forward-bind-port f) 8080)) fwds))
      (should (= (length (seq-filter (lambda (f) (eql (ssh-session-manager-forward-bind-port f) 8081)) fwds)) 1))
      (should (cl-some (lambda (f) (and (eq (ssh-session-manager-forward-type f) 'remote)
                                        (eql (ssh-session-manager-forward-bind-port f) 9090)))
                       fwds))
      (should (cl-some (lambda (f) (and (eq (ssh-session-manager-forward-source f) 'ss)
                                        (eql (ssh-session-manager-forward-bind-port f) 8082)))
                       fwds)))))


;;; ControlMaster Forward Edit & Rollback Tests

(ert-deftest ssh-session-manager-test-ctl-edit-and-rollback ()
  (let ((ssh-session-manager--runtime-forwards (make-hash-table :test 'equal))
        (calls nil))
    ;; Successful edit: cancels old, adds new.
    (cl-letf (((symbol-function 'ssh-session-manager--run-command)
               (lambda (prog args &optional _sudo)
                 (push (cons prog args) calls)
                 '(0 "" ""))))
      (ssh-session-manager--ctl-edit-forward
       "/tmp/ctl.sock" 'local "8080:localhost:80" 'local "8080:localhost:8081")
      (setq calls (nreverse calls))
      (should (= (length calls) 2))
      (should (equal (cdr (nth 0 calls))
                     '("-F" "none" "-S" "/tmp/ctl.sock" "-O" "cancel" "-L" "8080:localhost:80" "_")))
      (should (equal (cdr (nth 1 calls))
                     '("-F" "none" "-S" "/tmp/ctl.sock" "-O" "forward" "-L" "8080:localhost:8081" "_"))))
    ;; Failed add triggers automatic rollback to original forward.
    (setq calls nil)
    (let ((attempt 0))
      (cl-letf (((symbol-function 'ssh-session-manager--run-command)
                 (lambda (prog args &optional _sudo)
                   (setq attempt (1+ attempt))
                   (push (cons prog args) calls)
                   (if (= attempt 2)
                       '(255 "" "Port forwarding failed")
                     '(0 "" "")))))
        (let ((err (should-error
                    (ssh-session-manager--ctl-edit-forward
                     "/tmp/ctl.sock" 'local "8080:localhost:8081"
                     'local "80:localhost:80")
                    :type 'user-error)))
          (should (string-match-p "rolled back" (error-message-string err)))
          (setq calls (nreverse calls))
          (should (= (length calls) 3))
          ;; Third call restored the original forward.
          (should (equal (cdr (nth 2 calls))
                         '("-F" "none" "-S" "/tmp/ctl.sock" "-O" "forward"
                           "-L" "8080:localhost:8081" "_"))))))))


;;; UI Buffer, Filtering, Marking, Kill Safety & Inspect Tests

(defun ssh-session-manager-test--sample-sessions ()
  "Return sample incoming (current + hung) and outgoing ControlMaster sessions."
  (list
   (ssh-session-manager-session-create
    :pid 1001 :ppid 1000 :direction 'in :role 'session
    :user "alice" :host "10.0.0.1" :peer-addr "10.0.0.1:50001"
    :local-addr "10.0.0.2:22" :tty "pts/0" :state "S"
    :tcp-state "ESTAB" :elapsed 3600 :idle 5 :current-p t
    :command "sshd-session: alice@pts/0"
    :forwards (list (ssh-session-manager-forward-create
                     :type 'agent :proto 'unix :bind-addr "/tmp/agent"
                     :source 'environ :active-p t)))
   (ssh-session-manager-session-create
    :pid 1002 :ppid 1000 :direction 'in :role 'notty
    :user "alice" :host "10.0.0.9" :peer-addr "10.0.0.9:50002"
    :local-addr "10.0.0.2:22" :tty "notty" :state "S"
    :tcp-state "ESTAB" :send-q 120
    :tcp-timer '(:kind "on" :time "10sec" :retries 5)
    :elapsed 72000 :hung-reasons '("TCP Send-Q=120B" "TCP retransmitting (5)")
    :command "sshd-session: alice@notty"
    :forwards (list (ssh-session-manager-forward-create
                     :type 'remote :proto 'tcp :bind-addr "127.0.0.1"
                     :bind-port 9999 :source 'ss :active-p t)))
   (ssh-session-manager-session-create
    :pid 2001 :ppid 1 :direction 'out :role 'master
    :user "alice" :remote-user "deploy" :host "prod"
    :peer-addr "198.51.100.10:22" :local-addr "10.0.0.2:41234"
    :state "Ss" :tcp-state "ESTAB" :elapsed 600
    :control-path "/tmp/cm-prod"
    :command "ssh -fNM -S /tmp/cm-prod -L 8080:localhost:80 deploy@prod"
    :forwards (list (ssh-session-manager-forward-create
                     :type 'local :proto 'tcp :bind-port 8080
                     :target-host "localhost" :target-port 80
                     :spec "8080:localhost:80" :source 'cli :active-p t)))))

(ert-deftest ssh-session-manager-test-ui-sessions-marks-filters-and-kill ()
  (let ((sample (ssh-session-manager-test--sample-sessions))
        (signaled nil)
        (prompted-yes-or-no nil))
    (cl-letf (((symbol-function 'ssh-session-manager--collect-all)
               (lambda (&rest _) (cons sample nil)))
              ((symbol-function 'ssh-session-manager--signal-pid)
               (lambda (pid sig &optional _sudo) (push (cons pid sig) signaled)))
              ((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompted-yes-or-no p) t))
              ((symbol-function 'sit-for) (lambda (&rest _) nil)))
      (with-temp-buffer
        (ssh-session-manager-mode)
        (revert-buffer nil t)
        (should (= (length tabulated-list-entries) 3))
        ;; Mark hung sessions: marks PID 1002 only (never current PID 1001).
        (ssh-session-manager-mark-hung)
        (should (equal ssh-session-manager--marks '((1002 . ?*))))
        ;; Kill marked hung session (uses y-or-n-p, NOT yes-or-no-p).
        (ssh-session-manager-kill)
        (should (equal signaled '((1002 . "TERM"))))
        (should-not prompted-yes-or-no)
        ;; Killing current session PID 1001 MUST escalate to yes-or-no-p warning!
        (ssh-session-manager-unmark-all)
        (goto-char (point-min))
        (while (and (not (eobp)) (not (eql (tabulated-list-get-id) 1001)))
          (forward-line 1))
        (should (eql (tabulated-list-get-id) 1001))
        (ssh-session-manager-kill)
        (should prompted-yes-or-no)
        (should (string-match-p "CURRENT Emacs" prompted-yes-or-no))
        ;; Filters work cleanly.
        (ssh-session-manager-filter-outgoing)
        (should (= (length tabulated-list-entries) 1))
        (should (eql (car (car tabulated-list-entries)) 2001))
        (ssh-session-manager-filter-clear)
        (should (= (length tabulated-list-entries) 3))
        (ssh-session-manager-filter-hung)
        (should (= (length tabulated-list-entries) 1))
        (should (eql (car (car tabulated-list-entries)) 1002))))))

(ert-deftest ssh-session-manager-test-ui-forwards-and-inspect ()
  (let ((sample (ssh-session-manager-test--sample-sessions)))
    (cl-letf (((symbol-function 'ssh-session-manager--collect-all)
               (lambda (&rest _) (cons sample nil)))
              ((symbol-function 'ssh-session-manager--fetch-ctl-details)
               (lambda (_)
                 '(:check "Master running (pid=2001)"
                   :conninfo "Connection information for prod pid 2001\n  cipher aes256-gcm@openssh.com"
                   :channels "The following connections are open:\n  #3 mux-control"))))
      ;; Test *SSH Port Forwards* table.
      (with-temp-buffer
        (ssh-session-manager-forwards-mode)
        (revert-buffer nil t)
        (should (= (length tabulated-list-entries) 3))
        (should (string-match-p "9999" (buffer-string)))
        (should (string-match-p "8080" (buffer-string))))
      ;; Test inspect buffer rendering.
      (with-temp-buffer
        (ssh-session-manager-inspect-mode)
        (ssh-session-manager--render-inspect-buffer
         (nth 2 sample)
         (ssh-session-manager--fetch-ctl-details "/tmp/cm-prod"))
        (let ((text (buffer-string)))
          (should (string-match-p "PID 2001" text))
          (should (string-match-p "/tmp/cm-prod" text))
          (should (string-match-p "aes256-gcm@openssh.com" text))
          (should (string-match-p "L:8080->localhost:80" text)))))))

(ert-deftest ssh-session-manager-test-correlate-incoming-direct-pid ()
  "Verify incoming sshd-session with direct PID socket attribution (sudo mode)."
  (let* ((procs
          (ssh-session-manager--parse-ps-output
           "  201 200 alice S 01:00:00 ? sshd-session: alice@notty\n"))
         (socks
          (append
           (ssh-session-manager--parse-ss-tcp
            (string-join
             '("ESTAB 0 0 10.0.0.2:22 10.0.0.50:54321 users:((\"sshd-session\",pid=201,fd=8)) uid:0"
               "LISTEN 0 128 127.0.0.1:9999 0.0.0.0:* users:((\"sshd-session\",pid=201,fd=9)) uid:1000"
               "ESTAB 0 0 127.0.0.1:41000 127.0.0.1:5432 users:((\"sshd-session\",pid=201,fd=10)) uid:1000")
             "\n"))
           (ssh-session-manager--parse-ss-unix
            "u_str LISTEN 0 128 /tmp/forwarded.sock 555 * 0 users:((\"sshd-session\",pid=201,fd=11))\n")))
         (res (ssh-session-manager--correlate-sessions
               procs socks
               (list :current-user "alice" :current-uid 1000
                     :env-fn (lambda (_) nil)
                     :idle-fn (lambda (_) nil)
                     :ancestors nil :emacs-ttys nil :emacs-ssh-conn nil)))
         (sess (car (car res)))
         (fwds (ssh-session-manager-session-forwards sess)))
    (should-not (ssh-session-manager-session-inferred-p sess))
    (should (equal (ssh-session-manager-session-peer-addr sess) "10.0.0.50:54321"))
    ;; Remote TCP listen (9999), remote Unix listen (/tmp/forwarded.sock),
    ;; and active outbound client -L connection (to 127.0.0.1:5432).
    (should (cl-some (lambda (f)
                       (and (eq (ssh-session-manager-forward-type f) 'remote)
                            (eql (ssh-session-manager-forward-bind-port f) 9999)
                            (eq (ssh-session-manager-forward-source f) 'ss)))
                     fwds))
    (should (cl-some (lambda (f)
                       (and (eq (ssh-session-manager-forward-type f) 'remote)
                            (eq (ssh-session-manager-forward-proto f) 'unix)
                            (equal (ssh-session-manager-forward-bind-addr f) "/tmp/forwarded.sock")))
                     fwds))
    (should (cl-some (lambda (f)
                       (and (eq (ssh-session-manager-forward-type f) 'active)
                            (eql (ssh-session-manager-forward-target-port f) 5432)))
                     fwds))))

(ert-deftest ssh-session-manager-test-correlate-mux-client-and-non-master ()
  "Verify mux-client slave detection and inactive config ControlPath filtering."
  (let* ((procs
          (ssh-session-manager--parse-ps-output
           (string-join
            '("  301 1 alice Ss 00:30:00 ? ssh prod"
              "  302 1 alice S+ 00:05:00 pts/1 ssh prod"
              "  303 1 alice S+ 00:01:00 pts/2 ssh other")
            "\n")))
         (socks
          (append
           (ssh-session-manager--parse-ss-tcp
            (string-join
             '("ESTAB 0 0 10.0.0.1:45000 10.0.0.2:22 users:((\"ssh\",pid=301,fd=3)) uid:1000"
               "LISTEN 0 128 127.0.0.1:8080 0.0.0.0:* users:((\"ssh\",pid=301,fd=4)) uid:1000"
               "ESTAB 0 0 10.0.0.1:45001 10.0.0.3:22 users:((\"ssh\",pid=303,fd=3)) uid:1000")
             "\n"))
           (ssh-session-manager--parse-ss-unix
            (string-join
             '("u_str LISTEN 0 64 /tmp/cm-prod.ABCDEFGHIJKLMNOP 100 * 0 users:((\"ssh\",pid=301,fd=5))"
               "u_str ESTAB 0 0 * 101 /tmp/cm-prod 100 users:((\"ssh\",pid=302,fd=4))")
             "\n"))))
         (cfg-fwd (ssh-session-manager--parse-forward-spec 'local "8080:localhost:80" 'config))
         (res (ssh-session-manager--correlate-sessions
               procs socks
               (list :current-user "alice" :current-uid 1000
                     :file-exists-fn (lambda (p) (equal p "/tmp/cm-prod"))
                     :config-fn (lambda (host _port _user)
                                  (if (equal host "prod")
                                      (list :control-path "/tmp/cm-prod"
                                            :forwards (list cfg-fwd))
                                    ;; Non-existent ControlPath in Host * config.
                                    (list :control-path "/tmp/cm-other-missing")))
                     :ancestors nil :emacs-ttys nil :emacs-ssh-conn nil)))
         (sessions (car res))
         (s301 (seq-find (lambda (s) (= (ssh-session-manager-session-pid s) 301)) sessions))
         (s302 (seq-find (lambda (s) (= (ssh-session-manager-session-pid s) 302)) sessions))
         (s303 (seq-find (lambda (s) (= (ssh-session-manager-session-pid s) 303)) sessions)))
    ;; 301 is master owning the forward.
    (should (eq (ssh-session-manager-session-role s301) 'master))
    (should (= (length (ssh-session-manager-session-forwards s301)) 1))
    ;; 302 is mux-client slave and does NOT duplicate master's forwards.
    (should (eq (ssh-session-manager-session-role s302) 'mux-client))
    (should (null (ssh-session-manager-session-forwards s302)))
    ;; 303 is standalone client; missing config ControlPath is cleared.
    (should (eq (ssh-session-manager-session-role s303) 'client))
    (should (null (ssh-session-manager-session-control-path s303)))))

(ert-deftest ssh-session-manager-test-interactive-commands ()
  "Verify interactive forward add/remove/edit, master ops, and delete execution."
  (let ((sample (ssh-session-manager-test--sample-sessions))
        (ssh-session-manager--runtime-forwards (make-hash-table :test 'equal))
        (ctl-calls nil)
        (signaled nil))
    (cl-letf (((symbol-function 'ssh-session-manager--collect-all)
               (lambda (&rest _) (cons sample nil)))
              ((symbol-function 'ssh-session-manager--run-command)
               (lambda (_prog args &optional _sudo)
                 (push args ctl-calls)
                 '(0 "" "Master running (pid=2001)")))
              ((symbol-function 'ssh-session-manager--signal-pid)
               (lambda (pid sig &optional _sudo) (push (cons pid sig) signaled)))
              ((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'sit-for) (lambda (&rest _) nil)))
      (with-temp-buffer
        (ssh-session-manager-mode)
        (revert-buffer nil t)
        ;; Move to hung session 1002, flag for delete (`d`), execute (`x`).
        (goto-char (point-min))
        (while (and (not (eobp)) (not (eql (tabulated-list-get-id) 1002)))
          (forward-line 1))
        (ssh-session-manager-mark-delete)
        (ssh-session-manager-execute-deletes)
        (should (equal signaled '((1002 . "TERM"))))
        ;; Move to ControlMaster 2001, test check/stop/exit and add/remove/edit forward.
        (goto-char (point-min))
        (while (and (not (eobp)) (not (eql (tabulated-list-get-id) 2001)))
          (forward-line 1))
        (ssh-session-manager-check-master)
        (ssh-session-manager-master-stop)
        (ssh-session-manager-master-exit)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt _table &rest _)
                     (if (string-match-p "Forward type" prompt)
                         "local (-L)"
                       "-L 8080:localhost:80")))
                  ((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) "9090:localhost:90")))
          (ssh-session-manager-add-forward)
          (ssh-session-manager-edit-forward)
          (ssh-session-manager-remove-forward))
        (setq ctl-calls (nreverse ctl-calls))
        (should (cl-some (lambda (a) (member "check" a)) ctl-calls))
        (should (cl-some (lambda (a) (member "stop" a)) ctl-calls))
        (should (cl-some (lambda (a) (member "exit" a)) ctl-calls))
        (should (cl-some (lambda (a) (and (member "forward" a) (member "9090:localhost:90" a)))
                         ctl-calls))
        (should (cl-some (lambda (a) (and (member "cancel" a) (member "8080:localhost:80" a)))
                         ctl-calls))))))

(provide 'ssh-session-manager-test)
;;; ssh-session-manager-test.el ends here
