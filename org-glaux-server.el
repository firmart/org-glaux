;;; org-glaux-server.el --- Org glaux -*- lexical-binding: t; -*-

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

;; This file provides the core functions used by every Org glaux component.

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

;;;###autoload
(defun org-glaux-server-toggle ()
  "Start/stop org-glaux http server."
  (interactive)
  (let ((httpd-host org-glaux-server-host)
	(httpd-port org-glaux-server-port)
	(httpd-root org-glaux-location))

    (if (httpd-running-p)
	(progn (httpd-stop)
	       (message "Stop simple-httpd on %s:%s, serving: %s" httpd-host httpd-port httpd-root))
      (httpd-start)
      (message "Started simple-httpd on %s:%s, serving: %s" httpd-host httpd-port httpd-root)
      (browse-url (format "http://localhost:%s" org-glaux-server-port)))))

(provide 'org-glaux-server)
