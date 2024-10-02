;;; ipcalc6.el --- IP subnet calculator

;; Filename: ipcalc6.el
;; Description: IP calculator
;; Author: "Aleksandar Simic" <asimic@gmail.com>
;; License: BSD
;; Created: 2012-03-11 17:10
;; Version: 0.2.5
;; Package-Version: 20170926.805
;; URL: http://github.com/dotemacs/ipcalc6.el
;; Keywords: networking tools
;; Package-Requires: ((cl-lib "0.5"))

;; This file is NOT part of GNU Emacs
;;
;; Copyright (c) 2012-2016, Aleksandar Simic
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Usage: evaluate (ipcalc6 "192.168.0.23/21")

;;; Code:

(require 'cl-lib)

(defconst ipcalc6--cidr-default 128 "CIDR value.")

(defun ipcalc6-int-to-bin-string (n &optional length)
  ;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
  "Convert integer N to bit string (LENGTH, default 8)."
  (let* ((i  0)
	 (len (or length 8))
	 (s (make-string len ?0)))
    (while (< i len)
      (if (not (zerop (logand n (ash 1 i))))
          (aset s (- len (1+ i)) ?1))
      (setq i (1+ i)))
    s))

(defun ipcalc6-hextets-as-binary (list-of-hextets)
  "Return LIST-OF-HEXTETS as a single binary string."
  (let ((binary ""))
  (while list-of-hextets
    (setq binary
          (concat binary
                  (ipcalc6-int-to-bin-string
                   (string-to-number (car list-of-hextets) 16) 16)))
      (setq list-of-hextets (cdr list-of-hextets)))
  binary))

(defun ipcalc6-ip-to-hextets (ip)
  "Split IP address and return the hextets."
  (let ((hextets (split-string ip ":")))
    (let ((less (- 8 (length hextets))))
      (if (= less 0)
          hextets
        (let ((i (cl-position "" hextets)))
          (if (not i)
              (error "Not correct IP format")
            (append (cl-subseq hextets 0 i)
                    (make-list less "0000")
                    (cl-subseq hextets (1+ i)))))))))

(defun ipcalc6-ones-and-pad (num)
  "Return 1's equal to NUM and pad the rest up to 32."
  (unless (and (<= num ipcalc6--cidr-default) (> num 0))
    (error "Wrong value provided"))
  (concat (make-string num ?1)
          (make-string (- ipcalc6--cidr-default num) ?0)))

(defun ipcalc6-invert-binary (num)
  "Invert NUM's 1s to 0s and vice versa.

  (credit: the simpler version below re-writen by pjb from #emacs
          on freenode)"
  (cl-map 'string
       (lambda (ch) (aref "10" (cl-position ch "01")))
       (cl-remove-if-not (lambda (ch) (cl-position ch "01")) num)))

(defun ipcalc6-network (ip cidr)
  "Takes IP & CIDR and produces network."
  (let ((cidr-as-int (string-to-number cidr)))
    (concat
     (substring (ipcalc6-hextets-as-binary
                 (ipcalc6-ip-to-hextets ip)) 0 cidr-as-int)
     (make-string (- ipcalc6--cidr-default cidr-as-int) ?0))))

(defun ipcalc6-host+1 (binary-ip)
  "Increment the given BINARY-IP by 1."
  (let ((tmp binary-ip))
    (aset tmp (- ipcalc6--cidr-default 1) ?1)
    tmp))

(defun ipcalc6-host-max (ip cidr)
  "Given IP and CIDR, return maximum host as a binary value."
  (let ((count (string-to-number cidr))
        (max (- ipcalc6--cidr-default 1)))
    (while (< count max)
      (aset ip count ?1)
      (setq count (cl-incf count)))
    ip))

(defun ipcalc6-hosts/net (num)
  "Calculate Hosts/Net for the NUM given.
The value is a string as it may exceed int size."
  (calc-eval (format "2**(%d-%d) - 2" ipcalc6--cidr-default num)))

(defun ipcalc6-bin-to-int (bin)
  "Convert binary value BIN to integer."
  (format "%x" (read (concat "#b" bin))))

(defun ipcalc6-binary-to-ip (binary)
  "Convert BINARY to IP address."
  (mapconcat #'ipcalc6-bin-to-int
             (let (hextets)
               (nreverse (dotimes (x 8 hextets)
                           (push (substring binary (* x 16) (* (1+ x) 16)) hextets))))
             ":"))

;;;###autoload
(defun ipcalc6 (ip/cidr)
  "IP calculator for given IP/CIDR."
  (interactive "sIP/CIDR: ")
  (let* ((ip (car (split-string ip/cidr "/")))
         (ip-in-binary (ipcalc6-hextets-as-binary (ipcalc6-ip-to-hextets ip)))
         (cidr (car (cdr (split-string ip/cidr "/"))))
         (cidr-int (string-to-number cidr))
         (cidr-binary (ipcalc6-ones-and-pad cidr-int))
         (wildcard-binary (ipcalc6-invert-binary (ipcalc6-ones-and-pad cidr-int)))
         (wildcard-ip (ipcalc6-binary-to-ip wildcard-binary))
         (netmask (ipcalc6-binary-to-ip (ipcalc6-invert-binary wildcard-binary)))
         (net-binary (ipcalc6-network ip cidr))
         (net-ip (ipcalc6-binary-to-ip net-binary))
         (host-max-binary (ipcalc6-host-max (ipcalc6-network ip cidr) cidr))
         (host-max-ip (ipcalc6-binary-to-ip host-max-binary))
         (host-min-binary (ipcalc6-host+1 (ipcalc6-network ip cidr)))
         (host-min-ip (ipcalc6-binary-to-ip host-min-binary))
         (broadcast-binary (ipcalc6-host+1 (ipcalc6-host-max net-binary cidr)))
         (broadcast-ip (ipcalc6-binary-to-ip broadcast-binary))
         (buffer "*ipcalc6*"))
    (if (get-buffer buffer)
        (kill-buffer buffer))
    (pop-to-buffer buffer)
    (setq truncate-lines t)
    (insert
     (format "Address:  %-40s       %s\n" ip ip-in-binary)
     (format "Netmask:  %-40s = %-3s %s\n" netmask cidr cidr-binary)
     (format "Wildcard: %-40s       %s\n" wildcard-ip wildcard-binary)
     "=>\n"
     (format "Network:   %-40s %s\n" net-ip (ipcalc6-network ip cidr))
     (format "HostMin:   %-40s %s\n" host-min-ip host-min-binary)
     (format "HostMax:   %-40s %s\n" host-max-ip host-max-binary)
     (format "Broadcast: %-40s %s\n" broadcast-ip broadcast-binary)
     (format "Hosts/Net: %s\n" (ipcalc6-hosts/net cidr-int)))))

(provide 'ipcalc6)

;;; ipcalc6.el ends here
