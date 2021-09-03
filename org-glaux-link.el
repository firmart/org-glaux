;;; org-glaux-link.el --- Org glaux links support -*- lexical-binding: t; -*-

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

;; This file provides additional Org link support:
;; - Add "wiki:" link for Org glaux personal wiki
;; - Dead link highlighting for file links
;; - Dead/timeout link highlighting for http & https links
;;
;; TODO: It may be a better idea to use overlays or text properties for http links.
;;       1. It will not overwrite people http link :face
;;       2. It offers more complex (but bug-prone) display (e.g. a superscript to hint [broken link])
;;       3. Link archive + status archive.

;;; Code:

(require 'org)
(require 'org-glaux-core)
(require 'org-glaux-auto-answer)

;;;; Customization
;;;;; HTTP Link Settings
(defcustom org-glaux-link-http-timeout-max 4
  "Maximum timeout beyond which link is considered as inaccessible, in seconds."
  :type 'integer
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

(defcustom org-glaux-link-http-status-expiration 90
  "Time after which url status in `org-glaux-link-http-htbl' expires, in days."
  :type 'integer
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

(defcustom org-glaux-link-http-htbl-fpath
  (expand-file-name ".org-glaux-http-htbl.el" user-emacs-directory)
  "Filepath where the hash table of url `org-glaux-link-http-htbl' is saved."
  :type 'file
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

;;;;; Faces
(defface org-glaux-link-found-face
  '((t (:inherit org-link)))
  "Face for valid link."
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

(defface org-glaux-link-not-found-face
  '((t (:inherit org-warning)))
  "Face for inaccessible/dead link."
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

(defface org-glaux-link-http-timeout-face
  '((t (:inherit org-warning :foreground "yellow")))
  "Face for access timeout of http link."
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

;;;; Links
;;;;; Wiki link
;; Hyperlinks to other wiki pages.
;; wiki:<wiki-path> or [[wiki:<wiki-path>][<description>]]
(org-link-set-parameters "wiki"
			 :follow #'org-glaux--wiki-follow
			 :export #'org-glaux--wiki-export
			 :face #'org-glaux-link-wiki-face)

;;;;; File link
(org-link-set-parameters "file" :face #'org-glaux-link-generic-face)

;;;;; Http(s) link
(org-link-set-parameters "http" :face #'org-glaux-link-http-face)
(org-link-set-parameters "https" :face #'org-glaux-link-http-face)

;;;; HTTP(s) Links Hash Table

(defun org-glaux-link-http-read-htbl ()
  "Read `org-glaux-link-http-htbl-fpath'."
  (when (file-exists-p org-glaux-link-http-htbl-fpath)
    (with-temp-buffer
      (insert-file-contents org-glaux-link-http-htbl-fpath)
      (read (buffer-string)))))

(defun org-glaux-link-http-save-htbl ()
  "Save `org-glaux-link-http-htbl' into `org-glaux-link-http-htbl-fpath'."
  (interactive)
  (with-temp-buffer
    (insert (prin1-to-string org-glaux-link-http-htbl))
    (write-file org-glaux-link-http-htbl-fpath)))

(defun org-glaux-link-http-init-htbl ()
  "Init `org-glaux-link-http-htbl' if it's not done and return it."
  (or org-glaux-link-http-htbl
      (setq org-glaux-link-http-htbl
	    (or (org-glaux-link-http-read-htbl)
		(make-hash-table :test 'equal)))))

(defvar org-glaux-link-http-htbl nil)

(add-hook 'kill-emacs-hook #'org-glaux-link-http-save-htbl)
;;;; Org Link Dynamic Face
(defun org-glaux-link-generic-face (fpath)
  "Use different faces for available and broken filepath."
  ;; Do not fontify remote filepath and filepath outside Org-glaux pages.
  (when (and (org-glaux--wiki-buffer-p (current-buffer))
	     (not (file-remote-p fpath)))
    (if (file-exists-p fpath)
	'org-glaux-link-found-face
      'org-glaux-link-not-found-face)))

(defun org-glaux-link-wiki-face (wiki-path)
  "Dynamic face for WIKI-PATH link."
  (let ((fpath (org-glaux--wiki-path-fpath wiki-path)))
    (org-glaux-link-generic-face fpath)))

;; TODO test on URL needing authentication
(defun org-glaux-link-http-face (link)
  "Dynamic face for HTTP(S) LINK."
  (when (org-glaux--wiki-buffer-p (current-buffer))
    (let* ((context (org-element-context))
	   (url (org-element-property :raw-link context))
	   (htbl (org-glaux-link-http-init-htbl))
	   (value (gethash url htbl))
	   (status (and value (plist-get value :status)))
	   (timeout (and value (plist-get value :timeout)))
	   (last-access (and value (plist-get value :last-access)))
	   (expired? (and value (org-glaux-link--http-status-expired-p last-access))))

      (if (and value
	       (not expired?))
	  ;; not expired nor cache url
	  (if (eq status 'timeout)
	      ;; last status was timeout, try again with a higher timeout
	      (org-glaux-link--http-status2face
	       (org-glaux-link--http-puthash
		url
		(when timeout
		  (min (* timeout 2) org-glaux-link-http-timeout-max))))
	    ;; otherwise, we use the last status
	      (org-glaux-link--http-status2face status))
	;; new url
	(org-glaux-link--http-status2face
	 (org-glaux-link--http-puthash url))))))

;;;; Internal
;;;;; HTTP(s) Links Utilities
(defun org-glaux-link--http-status (url &optional timeout)
  "Return the relevant status resulting from the access of the URL."
  ;; url-file-exists-p: Certificate are always [s]ession-only
  (let ((org-glaux-auto-answer '(("Continue connecting?" . ?s)))
	(timeout (or timeout 1)))
    (with-timeout (timeout 'timeout)
      (condition-case _
	  (if (url-file-exists-p url)
	      'found
	    'not-found)
	(error 'not-found)))))

(defun org-glaux-link--http-status2face (status)
  "Given a HTTP STATUS, return the link font corresponding."
  (pcase status
    ('not-found 'org-glaux-link-not-found-face)
    ('found 'org-glaux-link-found-face)
    ('timeout 'org-glaux-link-http-timeout-face)))

(defun org-glaux-link--http-status-expired-p (last-access)
  "Return `t' if LAST-ACCESS refers to an expired date."
  (let ((expiration-secs (* org-glaux-link-http-status-expiration 24 60 60)))
    (< (+ last-access expiration-secs)
       (org-glaux-link--current-uts))))

(defun org-glaux-link--http-puthash (url &optional timeout)
  "Get the URL status based on TIMEOUT and put the result to `org-glaux-link-http-htbl'."
  (let ((htbl (org-glaux-link-http-init-htbl))
	(status (org-glaux-link--http-status url timeout))
	(timeout (or timeout 1)))

    (when (and (eq status 'timeout)
	       (>= timeout org-glaux-link-http-timeout-max))
      (setq status 'not-found))

    (prog1 status
      (puthash url `(:status ,status
		     :last-access ,(org-glaux-link--current-uts)
		     :timeout ,timeout)
	       htbl))))

;;; org-glaux-link.el ends here
(provide 'org-glaux-link)
