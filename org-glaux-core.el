;;; org-glaux-core.el --- Core library of Org glaux -*- lexical-binding: t; -*-

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

;;; Code:
;;;; Customization
(defcustom org-glaux-index-file-basename "index"
  "Default index page (index.org) accessed with \\<org-glaux-index>."
  :type 'string
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

(defcustom org-glaux--assets-dir-suffix "_assets"
  "Assets' directory name suffix. 

If it is an empty string, assets and sub-wiki pages will be 
under the same directory."
  :type  'string
  :group 'org-glaux
  :package-version '(org-glaux . "0.3"))

;;;; Variables
(defvar org-glaux-location nil
  "Current wiki directory.  If nil, set it to the CAR of `org-glaux-location-list' in runtime.")

;;;; Internal: Path computing
(defun org-glaux--file-wiki-path (fpath)
  "Return the wiki-path of FPATH (filepath)."
  (file-relative-name (file-name-sans-extension fpath) org-glaux-location))

(defun org-glaux--replace-extension (fpath extension)
  "Replace FPATH's extension by a new EXTENSION."
  (concat (file-name-directory fpath)
	  (file-name-base fpath)
	  "."
	  extension))

(defun org-glaux--wiki-path-fpath (wiki-path &optional buffer-fpath)
  "Return filepath of given WIKI-PATH relative to BUFFER-FPATH.

BUFFER-FPATH defaults to current buffer filepath.

Page:
  - Relative wiki-path:
    - Children page: \"/test\" -> \"<current-file-assets-dir>/test.org\"
    - Sibling page: \"../test\" -> \"<current-dir>/test.org\"
  - Absolute wiki-path: \"test\" -> \"<org-glaux-location>/test.org\"
Directory (ends with a slash \"/\"):
  - Relative wiki-path:
    - Children directory: \"/test/\" -> \"<current-file-assets-dir>/test/\"
    - Sibling directory: \"../test/\" -> \"<current-dir>/test/\"
  - Absolute wiki-path:
    - \"/\" -> \"<org-glaux-location>/\"
    - \"test/\" -> \"<org-glaux-location>/test/\""
  (let ((buffer-fpath (or buffer-fpath buffer-file-name)))
    (if (string-empty-p wiki-path)
	(file-name-as-directory (expand-file-name org-glaux-location))
      (let ((prefix-path
	     (expand-file-name
	      (concat
	       ;; if wiki-path starts with (../)+ or / then it's a relative wiki-path
	       (if (string-match "^\\(\\(\\.\\.\\/\\)+\\|\\/\\)" wiki-path)
		   (file-name-as-directory (file-name-sans-extension buffer-fpath))
		 (file-name-as-directory org-glaux-location))
	       wiki-path))))
	(if (directory-name-p wiki-path)
	    (file-name-as-directory prefix-path)
	  (concat prefix-path ".org"))))))


(defun org-glaux--page-assets-dir (filename)
  "Return asset directory path of org-glaux page with FILENAME."
  (concat (file-name-sans-extension filename) org-glaux--assets-dir-suffix))

(defun org-glaux--cur-page-assets-dir ()
  "Return current org-glaux page's asset directory path."
  (org-glaux--page-assets-dir buffer-file-name))

;; TODO rename this function
(defun org-glaux--cur-page-assets-file (filename)
  "Return current page's asset path given its FILENAME."
  (expand-file-name filename (org-glaux--cur-page-assets-dir)))

(defun org-glaux--page->html-file (wiki-path)
  "Convert a page's WIKI-PATH to corresponding html filepath."
  (org-glaux--replace-extension (org-glaux--wiki-path-fpath wiki-path) "html"))

;;;; Internal: Predicate
(defun org-glaux--wiki-file-p (fpath)
  "Return non-nil if FPATH is an org-glaux file under `org-glaux-location'."
  (when fpath
    (file-in-directory-p fpath (expand-file-name org-glaux-location))))

(defun org-glaux--wiki-buffer-p (buffer)
  "Return non-nil if BUFFER is an org-glaux buffer under `org-glaux-location'."
  (org-glaux--wiki-file-p (buffer-file-name buffer)))

;;;; Time

(defun org-glaux-link--current-uts ()
  "Return current Unix timestamp."
  (string-to-number (format-time-string "%s" (current-time))))

;;; org-glaux-core.el ends here
(provide 'org-glaux-core)
