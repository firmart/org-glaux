;;; org-glaux.el --- Desktop wiki extension for org-mode -*- lexical-binding: t; -*-

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

;; Org-glaux is an org-mode extension that provides tools to manage and
;; build a desktop wiki where each wiki page is an org-mode file.

;;; Code:

;; built-in Emacs lib
(require 'org)
(require 'ox-html)
(require 'cl-lib)     ;; Common-lisp emulation library
(require 'subr-x)     ;; Provides string trim functions. FIXME (still in use ?)

;; Org-glaux libraries
(require 'org-glaux-core)
(require 'org-glaux-init)
(require 'org-glaux-vc)
(require 'org-glaux-menu)
(require 'org-glaux-stats)
(require 'org-glaux-server)
(require 'org-glaux-export)
(require 'org-glaux-link)

;;; Custom group
;;;; General settings

(defgroup org-glaux nil
  "Org-glaux Settings"
  :group 'org
  :package-version '(org-glaux . "0.1"))

(defvar org-glaux--page-history nil "Stack of wiki pages visited.")

(defcustom org-glaux-default-read-only nil
  "If this variable is non-nil all org-glaux pages will be read-only by default.
You can toggle read-only mode with \\<read-only-mode>."
  :type  'boolean
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

(defcustom org-glaux-close-root-switch t
  "If set, all org-glaux pages are closed when root directory is switched.
Default value: \\[t]"
  :type  'boolean
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

(defcustom org-glaux-save-on-follow nil
  "If \\[t], save current buffer before following a page link."
  :type 'boolean
  :group 'org-glaux
  :package-version '(org-glaux . "0.2"))

;;;; Backup settings
(defcustom org-glaux-backup-location nil
  "Path to backup directory."
  :type 'directory
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;;;; Hooks
;; (defcustom org-glaux-pre-close-hook nil
;;   "List of hook functions run by `org-glaux-close-root-switch', `org-glaux-close-pages' and `org-glaux-close-images'. (see `run-hooks')."
;;   :type 'hook
;;   :group 'org-glaux
;;   :package-version '(org-glaux . "0.2"))

;;; Interactive functions
;;;; Backup
;;;###autoload
(defun org-glaux-backup ()
  "Make a org-glaux backup."
  (interactive)
  (let* ((zipfile  (concat "org-glaux-" (format-time-string "%Y-%m-%d") ".zip"))
         (destfile (concat (file-name-directory org-glaux-backup-location) zipfile))
         (default-directory  org-glaux-location))
    (switch-to-buffer "*org-glaux-backup*")

    ;; Create org-glaux backup location directory if doesn't exist.
    (if (not (file-exists-p org-glaux-backup-location))
        (make-directory org-glaux-backup-location t))

    (when (file-exists-p destfile)
      (delete-file destfile))

    (when (file-exists-p zipfile)
      (delete-file zipfile))

    ;; Clear buffer removing all lines
    (delete-region (point-min) (point-max))
    (set-process-sentinel
     (start-process
      "org-glaux-backup" ;; Process name
      "*org-glaux-backup*" ;; Buffer where output is displayed.
      "zip" "-r" "-9" zipfile ".") ;; Shell command
     (lambda (process desc)
       (if (equal (process-exit-status process) 0)
	   (progn
	     (switch-to-buffer "*org-glaux-backup*")
	     (rename-file zipfile org-glaux-backup-location t)
	     (message "org-glaux: backup done")
	     (insert  "\nBackup done."))
	 (message "%s" desc))))))

;;;; Close
;;;###autoload
(defun org-glaux-close-pages ()
  "Close all opened wiki page buffers and save them."
  (interactive)
  ;; register all opened files to version control
  (let ((wiki-buffers
	 (cl-remove-if-not
	  (lambda (b) (and (org-glaux--wiki-buffer-p b)
		           (string-suffix-p ".org" (buffer-file-name b))))
	  (buffer-list))))
    (mapc (lambda (b) (with-current-buffer b (save-buffer))) wiki-buffers)
    ;; register and commit
    (org-glaux--vc-git-commit-files
     (mapcar 'buffer-file-name wiki-buffers)
     'close
     "org-glaux: automatic commit on close")
    (mapc #'kill-buffer wiki-buffers)
    (message "org-glaux: all wiki files closed")))

;;;; Dired
;;;###autoload
(defun org-glaux-dired-assets ()
  "Open the asset directory of current wiki page."
  (interactive)
  (let ((pagename (file-name-base buffer-file-name)))
    ;; TODO check if it is a wiki-page
    (org-glaux--assets-make-dir pagename)
    (dired (org-glaux--cur-page-assets-dir))))

;;;###autoload
(defun org-glaux-dired-backup ()
  "Open org-glaux backup directory in dired mode."
  (interactive)
  ;; Create org-glaux backup location directory if doesn't exist.
  (if (not (file-exists-p org-glaux-backup-location))
      (make-directory org-glaux-backup-location t))
  ;; Open backup location
  (dired org-glaux-backup-location)
  ;; Update buffer
  (revert-buffer))

;;;###autoload
(defun org-glaux-dired-files ()
  "Show all wiki files in all sub-directories of `org-glaux-location'."
  (interactive)
  (find-dired org-glaux-location "-name '*.org'"))

;;;; Files

;; TODO move dir function (difficulty: update all file under old dir)
;; 1. separate move and rename
;; rename: file -> file
;; rename: dir -> dir
;; move: file -> dir
;; move: dir -> dir
;; TODO Update back-links
;; TODO prefix C-u for inter-wiki move
(defun org-glaux-rename (src-wiki-path new-page-base &optional non-verb)
  "Rename SRC-WIKI-PATH with NEW-PAGE-BASE.

If SRC-WIKI-PATH designates a page, its associated assets
directory is renamed as well.  Conversely, if it designates an
assets directory, rename its page as well.

If NON-VERB is non-nil, suppress all message."
  (interactive
   (let ((src-wiki-path (completing-read
			 "[Rename] wiki-path of source (dir or page): "
			 (mapcar (lambda (file)
				   (org-glaux--file-wiki-path file))
				 (append
				  (org-glaux--pages-list)
				  (org-glaux--dirs-list)))
			 nil t))
	 (new-page-base (read-from-minibuffer "[Rename] new base name: ")))
     (list src-wiki-path new-page-base nil)))

  (when (or (string-empty-p src-wiki-path)
	    (string-empty-p new-page-base))
    (signal 'org-glaux--file-error (list "source or target is empty")))

  (let* ((src-fpath (org-glaux--wiki-path-fpath src-wiki-path))
	 (dest-fpath-assets-dir (file-name-as-directory (expand-file-name (concat "../" new-page-base) src-fpath)))
	 (dest-fpath-page (concat (directory-file-name dest-fpath-assets-dir) ".org"))
	 (src-fpath-page)
	 (src-fpath-assets-dir))

    (if (directory-name-p src-fpath)
	(setq src-fpath-page (concat (directory-file-name src-fpath) ".org"))
      (setq src-fpath-page src-fpath))

    (if (directory-name-p src-fpath)
	(setq src-fpath-assets-dir src-fpath)
      (setq src-fpath-assets-dir (file-name-as-directory (file-name-sans-extension src-fpath))))

    ;; Interrupt immediately if there is any error.
    (condition-case nil
	(progn
	  (when (file-exists-p src-fpath-page)
	    (org-glaux--move-file-fpath src-fpath-page dest-fpath-page non-verb)
	    (unless non-verb
	      (message "org-glaux: rename %s -> %s" src-fpath-page dest-fpath-page)))

	  (when (file-exists-p src-fpath-assets-dir)
	    (org-glaux--move-file-fpath src-fpath-assets-dir dest-fpath-assets-dir non-verb)
	    (unless non-verb
	      (message "org-glaux: rename %s -> %s" src-fpath-assets-dir dest-fpath-assets-dir)))

	  (org-glaux--move-hook src-fpath-page src-fpath-assets-dir dest-fpath-page dest-fpath-assets-dir))
      (error nil))))

(defun org-glaux-move-page (src-wiki-path dest-wiki-path &optional non-verb)
  "Move/rename wiki page and its assets directory.

- SRC-WIKI-PATH is the wiki-path of the page to move/rename.
- DEST-WIKI-PATH is
  - either the wiki-path of a page to which SRC-WIKI-PATH will be
    renamed ;
  - or the wiki-path of a directory under which SRC-WIKI-PATH will be moved.

The assets directory associated to the wiki page is moved/renamed accordingly.
If NON-VERB is non-nil, suppress all message."

  (interactive
   (let ((src-wiki-path (org-glaux--select 'identity "Wiki-path of source (page): "))
	 (dest-wiki-path (org-glaux--select 'identity "Wiki-path of target (dir, page): ")))
     (list src-wiki-path dest-wiki-path nil)))

  (let* ((src-fpath-page (org-glaux--wiki-path-fpath src-wiki-path))
	 (src-fpath-assets-dir (org-glaux--wiki-path-fpath (file-name-as-directory src-wiki-path)))

	 (dest-fpath-page (org-glaux--wiki-path-fpath dest-wiki-path))
	 (dest-fpath-assets-dir (org-glaux--wiki-path-fpath (file-name-as-directory dest-wiki-path)))
	 (org-glaux-location-list-norm (mapcar #'expand-file-name org-glaux-location-list)))

    ;; Check that we operate inside the wikis
    (when (cl-notany
	   (lambda (wiki-dir) (file-in-directory-p src-fpath-page (file-name-as-directory wiki-dir)))
	   org-glaux-location-list-norm)
      (signal 'org-glaux--file-error (list "attempt to move external file" src-fpath-page)))

    (when (cl-notany
	   (lambda (wiki-dir) (file-in-directory-p dest-fpath-page (file-name-as-directory wiki-dir) ))
	   org-glaux-location-list-norm)
      (signal 'org-glaux--file-error (list "attempt to move file outside wikis" dest-fpath-page)))

    ;; Interrupt immediately if there is any error.
    (condition-case nil
	(progn
	  ;; Perform the Move/rename operation on files
	  (org-glaux--move-file-fpath src-fpath-page dest-fpath-page non-verb)
	  (when (file-exists-p src-fpath-assets-dir)
	    (org-glaux--move-file-fpath src-fpath-assets-dir dest-fpath-assets-dir non-verb))

	  (org-glaux--move-hook src-fpath-page src-fpath-assets-dir dest-fpath-page dest-fpath-assets-dir))
      (error nil))))

;;;; Index

;;;###autoload
(defun org-glaux-index ()
  "Open the index page: <org-glaux-location>/index.org.

The page is created if it doesn't exist."
  (interactive)
  (org-glaux--init-location)
  (org-glaux--wiki-follow org-glaux-index-file-basename))

;;;###autoload
(defun org-glaux-index-frame ()
  "Open the index page in a new frame."
  (interactive)
  (with-selected-frame (make-frame)
    (org-glaux-index)))

;;;###autoload
(defun org-glaux-index-html ()
  "Open the Wiki (Index) in the default web browser."
  (interactive)
  (browse-url (concat "file://"
		      (org-glaux--page->html-file
		       org-glaux-index-file-basename))))

;;;; Insert
;;;###autoload
(defun org-glaux-insert-asset ()
  "Insert at point a file link to a current page's asset file.
The link type file is opened with Emacs."
  (interactive)
  (let* ((file (org-glaux--assets-select #'identity))
         (asset-link (format "file:%s/%s"
			     (org-glaux--cur-page-assets-dir)
	                     (file-name-nondirectory file)))
         (desc (read-string "Description: "
                            (when (use-region-p)
                              (buffer-substring-no-properties
                               (region-beginning)
                               (region-end))))))

    (save-excursion
      (when (use-region-p)
	(delete-region (region-beginning) (region-end)))
      (insert
       (org-link-make-string asset-link
			     (if (string-empty-p desc)
                                 (file-name-nondirectory file)
                               desc))))))

;;;###autoload
(defun org-glaux-insert-download ()
  "Download a file from a URL in the clibpoard and insert a link file link.
Note: This function is synchronous and blocks Emacs."
  (interactive)
  (org-glaux--assets-download-hof
   (lambda (output-file)
     (let ((desc (read-string "Description: "
                              (when (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end))))))
       (save-excursion
         (when (use-region-p) (delete-region (region-beginning) (region-end)))
         (insert (format "[[file:%s/%s][%s]]"
			 (org-glaux--cur-page-assets-dir) output-file desc)))))))

;;;###autoload
(defun org-glaux-insert-new-link ()
  "Insert a new link at point."
  (interactive)
  (let ((page-name (read-string  "Page wiki-link: "))
        (desc (read-string "Description: "
                           (when (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning)
                              (region-end))))))
    (save-excursion
      (when (use-region-p) (delete-region (region-beginning) (region-end)))
      (insert (org-link-make-string (concat "wiki:" page-name)
                                    (if (string-empty-p desc) page-name desc))))))

;;;###autoload
(defun org-glaux-insert-select-link ()
  "Insert a Wiki link at point for a existing page."
  (interactive)
  (let ((page-wpath (org-glaux--select 'identity))
        (desc (read-string
               "Optional description: "
               (when (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))))))
    (save-excursion
      (when (use-region-p) (delete-region (region-beginning) (region-end)))
      (insert (org-glaux--make-link page-wpath desc)))))

;;;; Miscellaneous

;;;###autoload
(defun org-glaux-help ()
  "Show org-glaux commands."
  (interactive)
  (command-apropos "org-glaux-"))

;;;###autoload
(defun org-glaux-new-page ()
  "Create a new wiki page and open it without inserting a link."
  (interactive)
  (org-glaux--wiki-follow (read-string "New page path: ")))

;;;###autoload
(defun org-glaux-website ()
  "Open org-glaux webpage."
  (interactive)
  (browse-url "https://firmart.github.io/org-glaux"))

;;;; Navigation

;;;###autoload
(defun org-glaux-navi-back ()
  "Navigate back to the previous wiki page in the history.

- Skip previous page which doesn't exist until found an existing one.
- If there is no wiki page in the history, come back to the index page."
  (interactive)
  (let ((prev-page (pop org-glaux--page-history)))
    (while (not (and prev-page (file-exists-p prev-page)))
      (setq prev-page (pop org-glaux--page-history))
      (unless prev-page
	(org-glaux--init-location)
	(setq prev-page (org-glaux--wiki-path-fpath org-glaux-index-file-basename))))
    (org-glaux--wiki-follow (org-glaux--file-wiki-path prev-page) t)))

;;;; Search

;;;###autoload
(defun org-glaux-search-regex (pattern)
  "Search all wiki pages that contain a PATTERN (regexp or name)."
  (interactive "sorg-glaux - Search for: ")
  (grep-compute-defaults) ;; Set up grep-find-command
  (rgrep pattern "*.org" org-glaux-location))
;;;; Select

;;;###autoload

;; (defun org-glaux-select-asset ()
;;   "Open in Emacs a selected asset file of the current page from a menu."
;;   (interactive)
;;   (org-glaux--assets-select #'org-open-file))

;;;###autoload
(defun org-glaux-select-assets-dired ()
  "Select and open with dired the assets directory of a wiki page."
  (interactive)
  (org-glaux--select
   (lambda (page)
     (let ((page-path (org-glaux--wiki-path-fpath page)))
       (org-glaux--assets-make-dir page-path)
       (dired (org-glaux--page-assets-dir page-path))))))

;;;###autoload
  (defun org-glaux-select-buffer ()
    "Switch between org-glaux page buffers."
    (interactive)
    (let ((target (completing-read "Wiki pages:"
				   (mapcar (lambda (b) (org-glaux--file-wiki-path (buffer-file-name b)))
					   (org-glaux--get-buffers-filename))))
	  (action 'org-glaux--wiki-follow))
      (funcall action target)))

;;;###autoload
(defun org-glaux-select-frame ()
  "Select a wiki page and open it in a new frame."
  (interactive)
  (org-glaux--select  (lambda (act)
			(with-selected-frame (make-frame)
			  (set-frame-name (concat "Org-glaux: " act))
			  (org-glaux--wiki-follow act)))))

;;;###autoload
(defun org-glaux-select-html ()
  "Select a wiki page and open it in html.

The html page is created if it doesn't exist yet."
  (interactive)
  (let ((target (completing-read "Wiki pages:" (org-glaux--pages-list)))
	(action
	 (lambda (fpath)
	   (let ((html-file   (org-glaux--replace-extension fpath "html")))
	     (if (not (file-exists-p html-file))
		 (with-current-buffer (find-file fpath)
		   (org-html-export-to-html)))
	     (browse-url html-file)))))
    (funcall action target)))

;;;###autoload
(defun org-glaux-select-page ()
  "Select and open a wiki page."
  (interactive)
  (org-glaux--select #'org-glaux--wiki-follow))

;;;###autoload
(defun org-glaux-select-asset ()
  "Select and open a wiki page's asset."
  (interactive)
  (let ((target (completing-read  "Wiki assets: " (org-glaux--assets-list))))
    (org-open-file target)))

;;;###autoload
(defun org-glaux-select-root ()
  "Switch org-glaux root directory."
  (interactive)
  (let ((target (completing-read "Org-glaux root directories:" org-glaux-location-list))
	(action
	 (lambda (p)
	   ;; If the selected wiki is the same, no need to erase navigation history
	   (unless (string= p org-glaux-location)
	     ;; Close all current org-glaux pages if custom value set
	     (when org-glaux-close-root-switch
	       (org-glaux-close-pages)
	       (message (format "Org-glaux pages under directory %s are saved" org-glaux-location)))
	     (setq org-glaux--page-history nil) ;; clean history of the previous wiki
	     (setq org-glaux-location p)
	     (when (not (file-exists-p org-glaux-location))
	       (make-directory org-glaux-location t))
	     (org-glaux-index)
	     (message (format "Org-glaux root directory set to: %s" p))))))
    (funcall action target)))
;;; Internal functions
;;;; Internal: Download

(defun org-glaux--assets-download-hof (callback)
  "Higher order function to download a file.

CALLBACK is a function with this signature: (callback <filename>)

This function works as follows:
1. Ask the user for the URL (suggested URL comes from the clipboard).
2. Ask the user for the filename to be downloaded (suggested filename comes from
the URL).
3. Call the callback function passing the current page name and the file name."
  (let* ;; Get the URL suggestion from clibpoard
      ((text (with-temp-buffer
	       (clipboard-yank)
	       (buffer-substring-no-properties (point-min)
					       (point-max))))
       (url (read-string "Url: " text))
       (default-directory (org-glaux--cur-page-assets-dir))
       (output-file  (read-string "File name: "
				  (car (last (split-string url "/"))))))

    (org-glaux--assets-make-dir buffer-file-name)
    (url-copy-file url output-file)
    (funcall callback output-file)))

;;;; Internal: Links
;;;;; Wiki Links

(defun org-glaux--make-link (wiki-path &optional desc)
  "Return a string containing a wiki link [[wiki:WIKI-PATH][TITLE]].
Argument WIKI-PATH: the link which is a wiki-path.
Argument DESC: the link description."
  (org-link-make-string
   (concat "wiki:" wiki-path)
   (if (or (string-empty-p desc) (not desc)) ;; if desc is an empty string or nil
       (org-glaux--global-prop-value (org-glaux--wiki-path-fpath wiki-path) "TITLE")
     desc)))

(defun org-glaux--wiki-follow (wiki-path &optional no-history?)
  "Open or create if it doesn't exist an org-glaux page given its WIKI-PATH.

- It pushes current wiki buffer into history so that `org-glaux-navi-back' can
come back to it.
- It returns the opened buffer.

If NO-HISTORY? is non-nil, do not push the `current-buffer' to
navigation history stack."
  (let ((page-fpath (org-glaux--wiki-path-fpath wiki-path))
	(dest-buffer)
	(cur-buf-fpath buffer-file-name))
    ;; push current buffer in page history stack
    (when (and (org-glaux--wiki-buffer-p (current-buffer))
	       (not no-history?))
      (push cur-buf-fpath org-glaux--page-history))
    ;; register & commit into vcs (if in follow mode)
    (when (and (org-glaux--wiki-buffer-p (current-buffer))
               (buffer-modified-p (current-buffer)))
      (org-glaux--vc-git-commit-files (list buffer-file-name) 'follow "org-glaux: automatic commit on page follow"))
    ;; save current buffer if it's customized so
    (when (and org-glaux-save-on-follow
	       (org-glaux--wiki-buffer-p (current-buffer)))
      (save-buffer))
    (if (file-exists-p page-fpath)
	;; if the page exists, open it
	(progn (setq dest-buffer (find-file page-fpath))
	       (when org-glaux-default-read-only (read-only-mode t)))
      ;; if the page doesn't exist, create it
      (make-directory (file-name-directory page-fpath) t)
      (setq dest-buffer (find-file page-fpath))
      ;; remove possible legacy buffer content
      (delete-region (point-min) (point-max))
      ;; TODO obtain link description
      (org-glaux--insert-header)
      (save-buffer)
      ;; refontify previous buffer as the wiki link exist now
      (with-current-buffer (find-file-noselect cur-buf-fpath)
	(font-lock-flush))
      (org-glaux--assets-make-dir page-fpath))
    dest-buffer))

;; TODO latex export: if description is a wikipath, sanitize it (e.g. escape "_")
(defun org-glaux--wiki-export (wiki-path desc format)
  "Export a wiki page WIKI-PATH from Org files.
Argument DESC wiki link description.
Argument FORMAT format to export."
  (cl-case format
    (html (format "<a href='%s'>%s</a>" (file-relative-name (org-glaux--page->html-file wiki-path) ".") (or desc wiki-path)))
    (ascii (format "%s (%s)" (or desc wiki-path) wiki-path))
    (latex (format "\\href{%s}{%s}"
		   (file-relative-name
		    (expand-file-name wiki-path org-glaux-location) ".") 
		   (or desc wiki-path)))))

;;;; Internal: Files

(define-error 'org-glaux--file-error "File error")

;; (defun org-glaux--move-file-fpath (src-fpath dest-fpath &optional non-verb)
;;    "Behave as Linux's command mv: move/rename SRC-FPATH to DEST-FPATH.
;; 
;; 	- SRC-FPATH and DEST-FPATH are respectively the full path of source and
;;           destination.
;; 	- Set NON-VERB to \\[t] to supress message emitted by this function.
;; 	Note that parents directories are created if needed."
;;    ;; pre-handle directory to directory/file cases
;;    (if (file-exists-p src-fpath)
;;        ;; source file is directory
;;        (when (file-directory-p src-fpath)
;; 	 ;; check if src-fpath = `org-glaux-location'
;; 	 (when (string-equal src-fpath (expand-file-name org-glaux-location))
;; 	   (signal 'org-glaux--file-error (list "attempt to move wiki location")))
;; 	 ;; dest file is directory
;; 	 (if (file-exists-p dest-fpath)
;; 	     ;; dest file is directory
;; 	     (if (file-directory-p dest-fpath)
;; 		 (when (file-in-directory-p dest-fpath src-fpath)
;; 		   (signal 'org-glaux--file-error (list "cannot move" src-fpath "to a subdirectory of itself" dest-fpath)))
;; 	       (signal 'org-glaux--file-error (list "cannot overwrite non-directory" dest-fpath "with directory" src-fpath)))
;; 	   (when (directory-name-p dest-fpath)
;; 	     ;; To rename a directory with `rename-file', dest-fpath should not end with slash
;; 	     (setq dest-fpath (directory-file-name dest-fpath)))))
;;      (signal 'org-glaux--file-error (list "source file doesn't exist" src-fpath)))
;; 
;;    ;; could fail if 2nd arg is nil (relative path)
;;    (make-directory (file-name-directory dest-fpath) t)
;;    (rename-file src-fpath dest-fpath 0))

;; TODO update backlinks
;; TODO update navigation history
;; TODO define a move-hook and append subroutine in this function
;; (defun org-glaux--move-hook  (src-fpath-page src-fpath-assets-dir dest-fpath-page dest-fpath-assets-dir)
;;    "Update buffers and VC after file renaming.
;; 
;; The following links are absolute.
;; - SRC-FPATH-PAGE: source wiki page filepath
;; - SRC-FPATH-ASSETS-DIR: source assets directory filepath
;; - DEST-FPATH-PAGE: target wiki page filepath
;; - DEST-FPATH-ASSETS-DIR: target assets directory filepath"
;;    (let ((src-buffer (get-file-buffer src-fpath-page)))
;; 
;;      ;; Update renamed/moved buffer associated filepath
;;      (when src-buffer
;;        (with-current-buffer src-buffer
;; 	 (set-visited-file-name dest-fpath-page)))
;; 
;;      ;; Update buffers' associated filepath under moved/renamed assets directory
;;      (mapc
;;       (lambda (b)
;; 	(with-current-buffer b
;; 	  (set-visited-file-name
;; 	   (concat
;; 	    dest-fpath-assets-dir
;; 	    (file-name-as-directory (file-name-base src-fpath-page))
;; 	    (file-relative-name (buffer-file-name b) src-fpath-assets-dir)))))
;;       (cl-remove-if-not
;;        (lambda (b) (and (org-glaux--wiki-buffer-p b)
;; 			(file-in-directory-p (buffer-file-name b) src-fpath-assets-dir)))
;;        (buffer-list)))
;; 
;;      ;; Refontify wiki page buffers
;;      (mapc (lambda (b)
;; 	     (with-current-buffer b
;; 	       (font-lock-flush)))
;; 	   (org-glaux--get-buffers-filename))
;; 
;;      ;; In-sync with version control, removed files are committed unconditionally
;;      (org-glaux--vc-git-commit-files
;;       (list dest-fpath-page dest-fpath-assets-dir)
;;       'manual "org-glaux: automatic commit due to file renaming")))

;;;; Internal: List
(defun org-glaux--assets-page-files (fpath)
  "Return all assets from a given wiki page's FPATH."
  (let ((dirpath (org-glaux--page-assets-dir fpath)))
    (when (file-directory-p dirpath)
      (directory-files-recursively dirpath "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))) 

(defun org-glaux--get-buffers-filename ()
  "Return all org-glaux page buffers filename under `org-glaux-location`."
  (org-glaux--init-location)
  (cl-remove-if-not
   (lambda (buffer)
     (let* ((fp (buffer-file-name buffer))
	    (fpath (if fp (expand-file-name fp))))
       ;; test if file exists
       (and fpath
	    ;; test if buffer file has extension .org
	    (string-suffix-p ".org" fpath)
	    ;; test if buffer file is under current wiki location
	    (org-glaux--wiki-buffer-p buffer))))
   (buffer-list)))

(defun org-glaux--pages-list ()
  "Return a list containing all wiki-pages under `org-glaux-location`."
  (org-glaux--init-location)
  (let ((ignored-regexs (mapcar
			 #'org-glaux--glob2regex
			 (append
			  org-glaux-vc-ignored-files-glob
			  org-glaux-vc-ignored-dirs-glob))))
    (org-glaux--vc-ignore-files
     (directory-files-recursively org-glaux-location "\\.org$")
     ignored-regexs)))

(defun org-glaux--assets-list ()
    "Return a list containing all assets under `org-glaux-location`."
    (org-glaux--init-location)
    (let ((pages (org-glaux--pages-list))
	  (ignored-regexs (mapcar
			   #'org-glaux--glob2regex
			    org-glaux-vc-ignored-files-glob)))
	  (org-glaux--vc-ignore-files
	   (apply #'append
		  (remove nil
			  (mapcar #'org-glaux--assets-page-files 
				  pages)))
	   ignored-regexs)))

(defun org-glaux--dirs-list ()
  "Return all (sub)-directories under `org-glaux-location'."
  (org-glaux--init-location)
  (let ((ignored-regexs (mapcar
			 #'org-glaux--glob2regex
			 org-glaux-vc-ignored-dirs-glob)))
    (mapcar
     #'file-name-as-directory
     (org-glaux--vc-ignore-files
      (cl-remove-if-not
       #'file-directory-p
       (directory-files-recursively org-glaux-location "" t))
      ignored-regexs))))

;;;; Internal: Make dir

(defun org-glaux--assets-buffer-make-dir ()
  "Create asset directory of current buffer page if it doesn't exit."
  (if (not (org-glaux--wiki-buffer-p (current-buffer)))
      (error "Not in a wiki page")
    (org-glaux--assets-make-dir buffer-file-name)))

(defun org-glaux--assets-make-dir (filepath)
  "Create the asset directory from a page's FILEPATH if it doesn't exist."
  (let ((assets-dir (org-glaux--page-assets-dir filepath)))
    (if (not (file-exists-p assets-dir))
	(make-directory assets-dir t))))

;;;; Internal: Org properties

(defun org-glaux--global-props (fpath &optional property)
  "Return the plists of global org properties of a FPATH.

Argument FPATH: filepath.
Optional argument PROPERTY: the property seeking."
  (let* ((bp (get-file-buffer fpath))
	 (buffer (find-file-noselect fpath)))
    (unless property (setq property "PROPERTY"))
    (with-current-buffer (or buffer (current-buffer))
      (let ((value (org-element-map (org-element-parse-buffer)
		       'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))
	;; close the file if it was opened by this function
	(unless bp
	  (with-current-buffer buffer
	    (save-buffer)
	    (kill-this-buffer)))
	value))))

(defun org-glaux--global-prop-value (fpath key)
  "Return global org property KEY of current buffer.
Argument FPATH: filepath."
  (org-element-property :value (car (org-glaux--global-props fpath key))))

;;;; Internal: Selection

(defun org-glaux--select (callback &optional prompt)
  "Select a wiki page and invokes the CALLBACK function on it.

Use PROMPT if it is non-nil."
  (let ((target (completing-read (or prompt "Wiki pages: ")
				 (mapcar (lambda (file) (org-glaux--file-wiki-path file))
					 (org-glaux--pages-list)))))
    (funcall callback target)))

(defun org-glaux--assets-select (callback &optional prompt)
  "Select an asset of the current page and invokes the CALLBACK function on it.

Use PROMPT if it is non-nil."
  (let ((target (completing-read (or prompt "Asset: ")
				 (org-glaux--assets-page-files buffer-file-name))))
    (funcall callback (org-glaux--cur-page-assets-file target))))

;;; org-glaux.el ends here
(provide 'org-glaux)
