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
;; - Dead link highlighting for file links (WIP: http & https)

;;; Code:

(require 'org)
(require 'org-glaux-core)

;;; Links
;;;; Wiki link
;; Hyperlinks to other wiki pages.
;; wiki:<wiki-path> or [[wiki:<wiki-path>][<description>]]
(org-link-set-parameters "wiki" :follow #'org-glaux--wiki-follow
			 :export #'org-glaux--wiki-export
			 :face #'org-glaux--wiki-face) 

;;;; File link
(org-link-set-parameters "file" :face #'org-glaux--generic-face)

;;;; Http link
;; TODO Still has some work todo to be smooth enough
;; (org-link-set-parameters "http" :face #'org-glaux--url-face)
;; (org-link-set-parameters "https" :face #'org-glaux--url-face)

;;;; Internal: Dynamic Face

(defun org-glaux--generic-face (fpath)
  ;; Do not connect to remote files
  (unless (file-remote-p fpath)
    (if (file-exists-p fpath)
	'org-link
      ;; file link broken
      'org-warning)))

(defun org-glaux--wiki-face (wiki-path)
  "Dynamic face for WIKI-PATH link."
  (let ((fpath (org-glaux--wiki-path-fpath wiki-path)))
    (org-glaux--generic-face fpath)))

(defvar org-glaux--link-url-hash-table nil)

;; TODO caching link availability with timeout
;; this functionality may worth to be isolated as a package of its own
(defun org-glaux--url-face (link)
  "Dynamic face for url LINK."
  (let* ((url (concat "https:" link))
	 (context (org-element-context))
         (type (org-element-type context))
         (beg (org-element-property :begin context))
	 (hash-key (format "%s::%s" (buffer-name) beg))
	 (prev-url))

    (when (eq type 'link)
      ;; init hash table
      (unless org-glaux--link-url-hash-table
	(setq org-glaux--link-url-hash-table (make-hash-table :test 'equal)))

      (setq prev-url (gethash hash-key org-glaux--link-url-hash-table))
      ;; Trigger link check whenever
      ;; - it's the first time we see this link
      ;; - url is the same than the previous one.
      (if (or (not prev-url)
	      (and prev-url (string= url prev-url)))
	  (condition-case err 
	      (if (url-file-exists-p url)
		  'org-link
		'org-warning)
	    (error 'org-warning)) 
	(puthash hash-key url org-glaux--link-url-hash-table)
	'org-default))))

;;; org-glaux-link.el ends here
(provide 'org-glaux-link)
