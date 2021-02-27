;;; org-glaux-export.el --- Org glaux export facilities -*- lexical-binding: t; -*-

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

;; This file provides Org-glaux export facilities.

;;; Code:

(require 'org)
(require 'org-glaux-core)

;;;; Customization
;;;;; Async export settings
(defcustom org-glaux-emacs-path "emacs"
  "Path to Emacs executable.  Default value 'Emacs'."
  :type 'file
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;; Additional publishing options
(defcustom org-glaux-publish-plist '()
  "Additional options passed to `org-publish'."
  :type 'plist
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;;;; Export

;;;###autoload
(defun org-glaux-export-as (org-exporter)
  "Export all pages to a given format.
ORG-EXPORTER is a function that exports an `org-mode' page to a specific format
like html.  It can be for instance:

`org-html-publish-to-html',`org-latex-publish-to-pdf',
`org-latex-publish-to-latex', etc.

WARN: This is a synchronous function and can freeze Emacs.  Emacs will freeze
while the exporting doesn't finish.  Type \\<keyboard-quit> to abort the
execution."
  (interactive)
  (let ((org-html-htmlize-output-type "css")
        (org-html-htmlize-font-prefix "org-")
        (pub-plist (org-glaux--make-org-publish-plist org-exporter)))
    (org-publish pub-plist t)))

;;;###autoload
(defun org-glaux-export-html-async ()
  "Export all pages to html in asynchronous mode."
  (interactive)
  (let ((org-html-htmlize-output-type "css")
	(org-html-htmlize-font-prefix "org-"))
    (org-publish (org-glaux--make-org-publish-plist 'org-html-publish-to-html) t t)))

;;;###autoload
(defun org-glaux-export-html-page ()
  "Export the current wiki page to html and opens it in the browser."
  (interactive)
  (org-html-export-to-html)
  (browse-url (org-glaux--replace-extension buffer-file-name "html")))

;;;###autoload
(defun org-glaux-export-html-sync ()
  "Export all pages to html in synchronous mode."
  (interactive)
  (let ((org-html-htmlize-output-type "css")
        (org-html-htmlize-font-prefix "org-"))
    (org-publish (org-glaux--make-org-publish-plist 'org-html-publish-to-html)
		 t)))

;;;; Internal: Publish

;;;###autoload
(defun org-glaux--pages-to-publish ()
  (let ((pages (org-glaux--pages-list)))
    (cl-loop for p in pages
	     as h = (concat (file-name-sans-extension p) ".html")
	     if (or (not (file-exists-p h))
		    (string< (format-time-string "%s" (file-attribute-modification-time (file-attributes h)))
			     (format-time-string "%s" (file-attribute-modification-time (file-attributes p)))))
	     collect p)))

;;;###autoload
(defun org-glaux--make-org-publish-plist (org-exporter)
  "Prepare plist for use with `org-publish'.
Argument ORG-EXPORTER an org-exporter."
  (let ((plist-base
	 `("html"
	   :base-directory        ,org-glaux-location
	   :base-extension        "org"
	   :include               ,(org-glaux--pages-to-publish)
	   :with-broken-links     t
	   :publishing-directory  ,org-glaux-location
	   :publishing-function   ,org-exporter)))
    (setcdr plist-base
	    ;; combine with custom publish settings
	    (org-combine-plists (cdr plist-base) org-glaux-publish-plist))
    plist-base))

;;; org-glaux-export.el ends here
(provide 'org-glaux-export)
