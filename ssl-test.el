;;; ssl-test.el --- SSL tests                    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Allen Li

;; Author: Allen Li <darkfeline@felesatra.moe>
;; Keywords: local

;;; Commentary:

;;

;;; Code:

(require 'ert)

(ert-deftest ssl-test/ok ()
  (should (condition-case nil
              (url-retrieve-synchronously "https://badssl.com/")
            (error nil))))

(defmacro ssl-test--define-bad (test)
  "Define an ERT test case for the bad SSL test TEST."
  `(ert-deftest ,(intern (format "ssl-test/bad-%s" (symbol-name test))) ()
     (should (not (condition-case nil
                      (url-retrieve-synchronously ,(format "https://%s.badssl.com/" test))
                    (error nil))))))

(ssl-test--define-bad 3des)
(ssl-test--define-bad captive-portal)
(ssl-test--define-bad dh-composite)
(ssl-test--define-bad dh-small-subgroup)
(ssl-test--define-bad dh480)
(ssl-test--define-bad dh512)
(ssl-test--define-bad dsdtestprovider)
(ssl-test--define-bad edellroot)
(ssl-test--define-bad expired)
(ssl-test--define-bad invalid-expected-sct)
(ssl-test--define-bad mitm-software)
(ssl-test--define-bad mozilla-old)
(ssl-test--define-bad null)
(ssl-test--define-bad pinning-test)
(ssl-test--define-bad preact-cli)
(ssl-test--define-bad rc4-md5)
(ssl-test--define-bad rc4)
(ssl-test--define-bad revoked)
(ssl-test--define-bad self-signed)
(ssl-test--define-bad sha1-2017)
(ssl-test--define-bad sha1-intermediate)
(ssl-test--define-bad subdomain)
(ssl-test--define-bad superfish)
(ssl-test--define-bad untrusted-root)
(ssl-test--define-bad webpack-dev-server)
(ssl-test--define-bad wrong.host)

(provide 'ssl-test)
;;; ssl-test.el ends here
