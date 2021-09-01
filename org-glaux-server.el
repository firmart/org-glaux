;;; org-glaux-server.el --- Org-glaux web server -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.3
;; Keywords: outlines, files, convenience
;; URL: https://www.github.com/firmart/org-glaux
;; Package-Requires: ((emacs "25.1") (org "9.3") (cl-lib "0.5"))


;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a way to view wiki pages on a web server using `simple-httpd'.

(require 'simple-httpd)
(require 'org-glaux-core)

;;;; Webserver settings
(defcustom org-glaux-server-port "8000"
  "Default port to server org-glaux static files server."
  :type  'string
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

(defcustom org-glaux-server-host "0.0.0.0"
  "Default address that the server listens to."
  :type  'string
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;;;; Server

;; See the following upstream issue:
;; https://github.com/skeeto/emacs-web-server/issues/23
;; It's easy to reproduce: serve your wiki and click e.g. on a id link, then
;; a new process whose the name is "httpd <127.0.0.1:60216>" would appear.
(defun org-glaux-server-running-p ()
  "Return all httpd process if there is any, otherwise `nil'."
  (cl-remove-if-not (lambda (proc)
		      (let ((name (process-name proc)))
			(and name
			     (string-prefix-p "httpd" name))))
		    (process-list)))

(defun org-glaux-server-stop ()
  "Stop org-glaux http server."
  (interactive)
  (let ((procs (org-glaux-server-running-p)))
    (when procs
      (mapc #'delete-process procs)
      (httpd-log `(stop ,(current-time-string)))
      (run-hooks 'httpd-stop-hook))))

;;;###autoload
(defun org-glaux-server-toggle ()
  "Start/stop org-glaux http server."
  (interactive)
  (let ((httpd-host org-glaux-server-host)
	(httpd-port org-glaux-server-port)
	(httpd-root org-glaux-location))

    (if (org-glaux-server-running-p)
	(progn (org-glaux-server-stop)
	       (message "Stop simple-httpd on %s:%s, serving: %s" httpd-host httpd-port httpd-root))
      (httpd-start)
      (message "Started simple-httpd on %s:%s, serving: %s" httpd-host httpd-port httpd-root)
      (browse-url (format "%s:%s" httpd-host org-glaux-server-port)))))

;;; org-glaux-server.el ends here
(provide 'org-glaux-server)
