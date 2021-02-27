;;; org-glaux-init.el --- Org glaux init utilities -*- lexical-binding: t; -*-

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

;; This file provides utilities to initialize a wiki or a new page (through a template).

;;; Code:

(require 'org-glaux-core)

(defcustom org-glaux-location-list '("~/org/wiki")
  "List of org-glaux root directories."
  :type  '(repeat directory)
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;; TODO change to a function type defaults to org-glaux--insert-header with org-glaux-template
;; TODO use mustache.el instead this dirty trick
(defcustom org-glaux-template
  "#+OPTIONS: d:nil tags:nil todo:nil toc:t ^:nil
#+TITLE: %n
#+DESCRIPTION:
#+TAGS:
#+STARTUP:  overview
#+DATE: %d"
  "Default template used to create org-glaux pages/files.
- %n - is replaced by the page name.
- %d - is replaced by current date in the format year-month-day."
  :type 'string
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;;;; Internal -- Initialization
(defun org-glaux--insert-header (&optional title date)
  "Insert a header at the top of the buffer.

Optional arguments TITLE and DATE."
  ;; Save current cursor location and restore it
  ;; after completion of block insider save-excursion.
  (let*
      ;; replace '%n' by page title
      ((text1 (replace-regexp-in-string
	       "%n"
	       (or title
		   (when buffer-file-name (file-name-base buffer-file-name)))
	       org-glaux-template))
       ;; Replace %d by current date in the format %Y-%m-%d
       (text2 (replace-regexp-in-string
	       "%d"
	       (or date (format-time-string "%Y-%m-%d"))
	       text1)))
    ;; Got to top of file
    (goto-char (point-min))
    (insert text2)))

(defun org-glaux--init-location ()
  "Initialize `org-glaux-location' variable if not set yet."
  (if (not org-glaux-location)
      (let ((fst-loc (car org-glaux-location-list)))
	(unless (or org-glaux-location-list (file-exists-p fst-loc))
	  (error (format "`org-glaux-location-list' is nil or it contains non-existent directory '%s'" fst-loc)))
	(setq org-glaux-location fst-loc))))

;;; org-glaux-init.el ends here
(provide 'org-glaux-init)
