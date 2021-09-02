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

;;; Code:

(require 'org)
(require 'org-glaux-core)
(require 'org-glaux-auto-answer)

;;; Links
;;;; Customization
;;;;; Hash table
(defcustom org-glaux-url-htbl-fpath
  (expand-file-name ".org-glaux-url-htbl.el" user-emacs-directory)
  "Filepath where the hash table of url `org-glaux--url-htbl' is saved."
  :type 'file
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

;;;;; Faces
(defface org-glaux-link-valid-face
  '((t (:inherit org-link)))
  "Face for valid link."
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

(defface org-glaux-link-not-found-face
  '((t (:inherit org-warning)))
  "Face for unaccessible/dead link."
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

(defface org-glaux-link-timeout-face
  '((t (:inherit org-warning :foreground "yellow")))
  "Face for access timeout of http link."
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

;;;; Wiki link
;; Hyperlinks to other wiki pages.
;; wiki:<wiki-path> or [[wiki:<wiki-path>][<description>]]
(org-link-set-parameters "wiki"
			 :follow #'org-glaux--wiki-follow
			 :export #'org-glaux--wiki-export
			 :face #'org-glaux--wiki-face)

;;;; File link
(org-link-set-parameters "file" :face #'org-glaux--generic-face)

;;;; Http link
(org-link-set-parameters "http" :face #'org-glaux--url-dynamic-face)
(org-link-set-parameters "https" :face #'org-glaux--url-dynamic-face)

;;;; Internal: Hash Table
(defvar org-glaux--url-htbl (org-glaux--read-url-htbl))

(defun org-glaux--read-url-htbl ()
  "Read `org-glaux-url-htbl-fpath'."
  (when (file-exists-p org-glaux-url-htbl-fpath)
    (with-temp-buffer
      (insert-file-contents org-glaux-url-htbl-fpath)
      (read (buffer-string)))))

(defun org-glaux--save-url-htbl ()
  "Save `org-glaux--url-htbl' into `org-glaux-url-htbl-fpath'."
  (with-temp-buffer
    (insert (prin1-to-string org-glaux--url-htbl))
    (write-file org-glaux-url-htbl-fpath)))

(add-hook 'kill-emacs-hook #'org-glaux--save-url-htbl)
;;;; Internal: Dynamic Face

(defun org-glaux--generic-face (fpath)
  "Use different faces for available and broken filepath."
  ;; Do not fontify remote filepath and filepath outside Org-glaux pages.
  (when (and (org-glaux--wiki-buffer-p current-buffer)
	     (not (file-remote-p fpath)))
    (if (file-exists-p fpath)
	'org-glaux-link-valid-face
      'org-glaux-link-not-found-face)))

(defun org-glaux--wiki-face (wiki-path)
  "Dynamic face for WIKI-PATH link."
  (let ((fpath (org-glaux--wiki-path-fpath wiki-path)))
    (org-glaux--generic-face fpath)))

(defun org-glaux--url-face (url &optional timeout)
  "Return the relevant face resulting from the access of the URL."
  ;; url-file-exists-p: Certificate are always [s]ession-only
  (let ((org-glaux-auto-answer '(("Continue connecting?" . ?s))))
    (with-timeout ((or timeout 1) 'org-glaux-link-timeout-face)
      (condition-case err
	  (if (url-file-exists-p url)
	      'org-glaux-link-valid-face
	    'org-glaux-link-not-found-face)
	(error 'org-glaux-link-not-found-face)))))

;; TODO
;;  :last-access (e.g. invalid result after 1 month)
;;  :timeout (e.g. double the timeout till a threshold => 'org-warning)
;;  :status (instead of using :face)
(defun org-glaux--url-dynamic-face (link)
  "Dynamic face for url LINK."
  (when (org-glaux--wiki-buffer-p (current-buffer))
    (let* ((context (org-element-context))
	   (url (org-element-property :raw-link context))
	   (htbl (or org-glaux--url-htbl
		     (setq org-glaux--url-htbl (make-hash-table :test 'equal))))
	   (value (gethash url htbl))
	   (face (and value (plist-get value :face))))

      (if value
	  face
	(setq face (org-glaux--url-face url))
	(prog1 face
	  (puthash url `(:face ,face :last-access ,(format-time-string "%s" (current-time)))
		   htbl))))))

;;; org-glaux-link.el ends here
(provide 'org-glaux-link)
