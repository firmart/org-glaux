;;; org-glaux-menu.el --- Menu for Org-glaux -*- lexical-binding: t; -*-

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

;; This file provides menu for Org-glaux. Enable it through `org-glaux-menu-enable' and in case you haven't done it, activate `menu-bar-mode'.
;; The menu entries are generated automatically through `org-glaux--easy-men-entry' which takes a category name and a common prefix of interactive functions.

;;; Code:

;; TODO: disable.
;;;###autoload
(defun org-glaux-menu-enable ()
  "Build a menu for org-glaux."
  (interactive)
  (require 'easymenu)
  (easy-menu-define org-glaux-menu global-map "Org-glaux"
    (list "Org-glaux"
	  (org-glaux--easy-menu-entry "Backup" "org-glaux-backup")
	  (org-glaux--easy-menu-entry "Close" "org-glaux-close")
	  (org-glaux--easy-menu-entry "Dired" "org-glaux-dired")
	  (org-glaux--easy-menu-entry "Export" "org-glaux-export")
	  (org-glaux--easy-menu-entry "Index" "org-glaux-index")
	  (org-glaux--easy-menu-entry "Insert" "org-glaux-insert")
	  (org-glaux--easy-menu-entry "Navigation" "org-glaux-navi")
	  (org-glaux--easy-menu-entry "Search" "org-glaux-search")
	  (org-glaux--easy-menu-entry "Select" "org-glaux-select")
	  (org-glaux--easy-menu-entry "Server" "org-glaux-server")
	  (org-glaux--easy-menu-entry "Version control" "org-glaux-vc")
	  ["---" nil]
	  (list "Miscellaneous"
		(vector (documentation 'org-glaux-new-page) 'org-glaux-new-page)
		(vector (documentation 'org-glaux-website) 'org-glaux-website)))))

;;;; Internal: Menu
(defun org-glaux--easy-menu-entry (entry-name prefix)
  "Create a menu entry with ENTRY-NAME including functions prefixed by PREFIX."
  ;; (entry-name [doc_f1 f1] [doc_f2 f2] ...)
  (cons entry-name
	(mapcar
	 (lambda (f)
	   ;; [<func-doc_first_line> <func>]
	   (vector (car (split-string (documentation (intern f)) "\n"))
		   (intern f)))
	 ;; list of functions prefixed by PREFIX
	 (let (glaux-list)
	   (mapatoms (lambda (x)
		       (when (and (fboundp x)
				  (string-prefix-p prefix (symbol-name x)))
			 (push (symbol-name x) glaux-list ))))
	   glaux-list))))

;;; org-glaux-menu.el ends here
(provide 'org-glaux-menu)
