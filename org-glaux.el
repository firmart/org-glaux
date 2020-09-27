;;; org-glaux.el --- Desktop wiki extension for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.2
;; Keywords: outlines, files, convenience
;; URL: https://www.github.com/firmart/org-glaux
;; Package-Requires: ((emacs "25.1") (org "9.3") (cl-lib "0.5") (dash "2.17"))


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

;; external libraries
(require 'org)
(require 'ox-html)

;; built-in Emacs lib
(require 'cl-lib)     ;; Common-lisp emulation library
(require 'subr-x)     ;; Provides string trim functions.
(require 'vc)
(require 'vc-git)
(require 'vc-hooks) ;; `vc-directory-exclusion-list'

;;; Code:
;;; Custom group
;;;; General settings

(defgroup org-glaux nil
  "Org-glaux Settings"
  :group 'org
  :package-version '(org-glaux . "0.1"))

(defcustom org-glaux-location-list '("~/org/wiki")
  "List of org-glaux root directories."
  :type  '(repeat directory)
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

(defvar org-glaux-location nil
  "Current wiki directory.  If nil, set it to the CAR of `org-glaux-location-list' in runtime.")

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

;; TODO change to a function type defaults to org-glaux--insert-header with org-glaux-template
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

(defcustom org-glaux-index-file-basename "index"
  "Default index page (index.org) accessed with \\<org-glaux-index>."
  :type 'string
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

(defcustom org-glaux-save-on-follow nil
  "If \\[t], save current buffer before following a page link."
  :type 'boolean
  :group 'org-glaux
  :package-version '(org-glaux . "0.2"))

;;;; Version control settings
;; TODO add other vcs on request
(defcustom org-glaux-vc-backend 'git
  "Activate edit history using version control software."
  :type '(radio (const :tag "git" :value 'git)
		            (const :tag "no version control" :value nil))
  :group 'org-glaux
  :package-version '(org-glaux . "0.2"))

(defcustom org-glaux-vc-ignore-files-exceed-size (* 150 1024 1024)
  "Do not register file having size exceeding an amount of bytes."
  :type 'integer
  :group 'org-glaux
  :package-version '(org-glaux . "0.2"))

(defcustom org-glaux-vc-ignored-dirs-glob
  (append '("*ltximg*" "*_minted*")
	        (mapcar (lambda (d) (format "*%s*" d)) vc-directory-exclusion-list))
  "List of glob patterns to exclude directories from version control.

To specify a per-wiki or per-directory files pattern to ignore,
include them in a local `.gitignore' instead or as local variable
in index page."
  :type '(repeat string)
  :group 'org-glaux
  :package-version '(org-glaux . "0.2"))

(defcustom org-glaux-vc-ignored-files-glob '("*.html" "*.bbl" "*.tex" "*~" "*#*?#" "*.#*")
  "List of glob patterns to exclude file-path from version control.

  To specify a per-wiki or per-directory files pattern to ignore,
  include them in a local `.gitignore' instead or as local variable
  in index page."
  :type '(repeat string)
  :group 'org-glaux
  :package-version '(org-glaux . "0.2"))

(defcustom org-glaux-vc-commit-when 'close
  "Indicate when files are registered in version control and committed."
  :type '(radio (const :tag "commit opened wiki buffers when `org-glaux-close-pages' is called" :value 'close)
		            (const :tag "commit when a page is saved (not yet available)" :value 'save)
		            (const :tag "commit current wiki buffer when a wiki link is followed" :value 'follow)
		            (const :tag "same as 'close or 'follow" :value 'close+follow)
		            (const :tag "same as 'save or 'follow" :value 'follow+save)
		            (const :tag "same as 'close or 'save" :value 'close+save)
		            (const :tag "same as 'close or 'follow or 'save" :value 'close+follow+save)
		            (const :tag "always commit manually" :value 'manual))
  :group 'org-glaux
  :package-version '(org-glaux . "0.2"))
;;;; Python webserver settings
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

;;;; Async export settings
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
    (dired (file-name-sans-extension buffer-file-name))))

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
  (org-glaux--assets-select
   (lambda (file)
     (insert
      (org-link-make-string
       (format "file:%s/%s"
	             (file-name-base buffer-file-name)
	             (file-name-nondirectory file))
       (read-string "Description: "))))))

;;;###autoload
(defun org-glaux-insert-download ()
  "Download a file from a URL in the clibpoard and insert a link file link.
Note: This function is synchronous and blocks Emacs."
  (interactive)
  (org-glaux--assets-download-hof
   (lambda (output-file)
     (save-excursion (insert (format "[[file:%s/%s][%s]]"
				                             (file-name-base buffer-file-name) output-file output-file))))))

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
(defun org-glaux-menu-enable ()
  "Build a menu for org-glaux."
  (interactive)
  (require 'easymenu)
  (easy-menu-define org-glaux-menu global-map "Org-glaux"
    (list "org-glaux"
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
(defun org-glaux-select-asset ()
  "Open in Emacs a selected asset file of the current page from a menu."
  (interactive)
  (org-glaux--assets-select #'org-open-file))

(defun org-glaux-select-assets-dired ()
  "Select and open with dired the assets directory of a wiki page."
  (interactive)
  (org-glaux--select
   (lambda (page)
     (org-glaux--assets-make-dir page)
     (dired (file-name-sans-extension page)))))

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
;;;; Server

;; Despite this function was implemented as a interface to
;; Python3 simple http server, it can be refactored to work
;; with another more powerful http server such as Nginx.

;;;###autoload
(defun org-glaux-server-toggle ()
  "Start/stop org-glaux http server.  It requires Python3.
Note: This command requires Python3 installed."
  (interactive)
  (let (;; Process name
	      (pname  "org-glaux-server")
	      ;; Buffer name - Display process output (stdout)
	      (bname   "*org-glaux-server*")
	      ;; Set current directory to org-glaux repository.
	      (default-directory org-glaux-location))
    (if (not (get-buffer bname))
	      (progn
	        (sit-for 0.1)
	        (switch-to-buffer bname)
	        (save-excursion ;; Save cursor position
	          (insert "Server started ...\n\n")
	          (message "org-glaux: server started ..."))
	        (start-process pname
			                   bname
			                   "python3"
			                   "-m"
			                   "http.server"
			                   "--bind"
			                   org-glaux-server-host
			                   org-glaux-server-port)
	        (when (y-or-n-p "Open server in browser ? ")
	          (browse-url (format "http://localhost:%s" org-glaux-server-port))))
      (progn  (switch-to-buffer bname)
	            (kill-process (get-process pname))
	            (message "org-glaux: server stopped")))))

;;;; Version control

;;;###autoload
(defun org-glaux-vc-git-init-root ()
  "Init the current wiki root as a git repository if it's not the case."
  (interactive)
  (org-glaux--vc-git-install-check)
  (when (and (not (org-glaux--vc-git-find-root org-glaux-location))
	         (equal org-glaux-vc-backend 'git))
    (let ((index (org-glaux--wiki-path-fpath org-glaux-index-file-basename)))
      (with-current-buffer (find-file-noselect index)
	      (vc-git-create-repo)))))

;;;###autoload
(defun org-glaux-vc-git-full-commit ()
  "Register and commit all relevant files of the full wiki."
  (interactive)
  ;; move to index to obtain wiki-based configuration on ignored glob
  (org-glaux--vc-git-commit-files
   (directory-files-recursively org-glaux-location "^.*$")
   'manual
   "org-glaux: manually commit relevant files of the full wiki.")
  (save-excursion
    (switch-to-buffer "*vc*")))

;;; Internal functions
;;;; Internal -- Download
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
       (default-directory (file-name-sans-extension buffer-file-name))

       (output-file  (read-string "File name: "
				                          (car  (last (split-string url "/"))))))

    (org-glaux--assets-make-dir buffer-file-name)
    (url-copy-file url output-file)
    (funcall callback output-file)))

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

;;;; Internal -- Links
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
      (org-glaux--insert-header)
      (save-buffer)
      ;; refontify previous buffer as the wiki link exist now
      (with-current-buffer (find-file-noselect cur-buf-fpath)
	      (font-lock-flush))
      (org-glaux--assets-make-dir page-fpath))
    dest-buffer))

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
;;;;; Links Utilities
(defun org-glaux--get-file-links (fpath &rest link-type)
  "Return all links of type LINK-TYPE appearing in file FPATH."
  (with-temp-buffer (insert-file-contents fpath)
                    (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
	                      (when (member (org-element-property :type link) link-type)
	                        (org-element-property :path link))))))

(defun org-glaux--get-all-links-by-page (&rest link-type)
  "Return an alist (PAGE-FPATH . LINK-TYPE list)."
  (mapcar (lambda (f)
	          (cons f (apply #'org-glaux--get-file-links f link-type)))
	        (org-glaux--pages-list)))

;;;###autoload
(defun org-glaux--get-all-links-count-by-page (&rest link-type)
  "Return a list of links count of type LINK-TYPE."
  (mapcar (lambda (f)
	          (length (apply #'org-glaux--get-file-links f link-type)))
	        (org-glaux--pages-list)))


(defun org-glaux--get-page-back-links (fpath &optional wlbp)
  "Return a list of page's file-path which has a wiki-link to FPATH.

WLBP is the returned value of (`org-glaux--get-all-links-by-page')."
  (let ((wlbp (or wlbp (org-glaux--get-all-links-by-page)))
	      (back-links))
    (dolist (entry wlbp back-links)
      (let ((page-path (car entry))
	          (wpath-list (cdr entry)))
	      (when (some (lambda (wpath)
		                  (string= (org-glaux--wiki-path-fpath wpath page-path)
			                         (expand-file-name fpath)))
		                wpath-list)
	        (push page-path back-links))))))


(defun org-glaux--positive-broken-links-by-page ()
  "Return an alist of (fpath . broken-count) by wiki page.

Only wiki pages having broken-links > 0 are in the alist."
  (cl-remove-if
   (lambda (entry)
     (equal 0 (cdr entry)))
   (mapcar
    (lambda (entry)
      (let ((f (car entry))
	          (l (cdr entry)))
	      (cons f
	            (length (cl-remove-if
		                   #'file-exists-p
		                   (mapcar
			                  (lambda (w) (org-glaux--wiki-path-fpath w f) )
			                  l))))))
    (org-glaux--get-all-links-by-page "wiki"))))

;;;; Internal -- Stats

;;;###autoload
;; TODO show broken file: link
(defun org-glaux--show-wiki-stats ()
  "Show current wiki statistics."
  (interactive)
  (let ((bname "*org-glaux stats*")
	      (progress-reporter (make-progress-reporter "Computing wiki statistics..." 0 20)))
    (save-excursion
      ;; clean old stats
      (when (get-buffer bname)
	      (kill-buffer bname))
      (switch-to-buffer bname)
      (org-mode)
      (org-glaux--insert-header "Wiki statistics")
      (let* ((wlcbp (progn (progress-reporter-update progress-reporter 1)
			                     (org-glaux--get-all-links-count-by-page "wiki")))
	           (pblbp (progn (progress-reporter-update progress-reporter 1) (org-glaux--positive-broken-links-by-page))))
	      (org-insert-heading)
	      ;; Pages
	      (insert "Pages Stats\n")
	      (insert (format "  - Pages count: %d" (length wlcbp)))
	      (org-insert-heading)
	      (insert "Links Stats\n")
	      ;; Wiki links
	      (org-insert-subheading nil)
	      (insert "Internal Links Stats\n")
	      (insert (format "  - Total wiki links: %d\n" (progn (progress-reporter-update progress-reporter 2)
							                                              (apply '+ wlcbp))))
	      (insert (format "  - Wiki links by page: min.: %d, median: %.1f, avg.: %.2f, max.: %d\n"
			                  (progn (progress-reporter-update progress-reporter 3)
			                         (seq-min wlcbp))
			                  (progn (progress-reporter-update progress-reporter 4)
			                         (org-glaux--get-median-links-by-page nil wlcbp))
			                  (progn (progress-reporter-update progress-reporter 5)
			                         (org-glaux--get-avg-links-by-page nil wlcbp))
			                  (progn (progress-reporter-update progress-reporter 6)
			                         (seq-max wlcbp))))
	      (insert (format "  - Top %d page with the most broken links\n" 10))
	      (mapc (lambda (entry)
		            (insert (format "    - %4d broken link(s): [[wiki:%s]]\n"
				                        (cdr entry)
				                        (car entry))))
	            (org-glaux--stats-top-broken-links-count-files 10 pblbp)))
      (let* ((url-lcbp  (progn (progress-reporter-update progress-reporter 7)
			                         (org-glaux--get-all-links-count-by-page "https" "http"))))
	      ;; External links
	      (org-insert-heading)
	      (insert "External Links Stats\n")
	      (insert (format "  - Total url: %d\n" (progn (progress-reporter-update progress-reporter 8)
						                                         (apply '+ url-lcbp))))
	      (insert (format "  - Url(s) by page: min.: %d, median: %.1f, avg.: %.2f, max.: %d\n"
			                  (progn (progress-reporter-update progress-reporter 9)
			                         (seq-min url-lcbp))
			                  (progn (progress-reporter-update progress-reporter 10)
			                         (org-glaux--get-median-links-by-page nil url-lcbp))
			                  (progn (progress-reporter-update progress-reporter 11)
			                         (org-glaux--get-avg-links-by-page nil url-lcbp))
			                  (progn (progress-reporter-update progress-reporter 12)
			                         (seq-max url-lcbp)))))
      ;; VCS
      (condition-case nil
	        (progn
	          (org-glaux--vc-git-install-check)
	          (progress-reporter-update progress-reporter 13)
	          (org-insert-heading)
	          (org-do-promote) ;; promote to level 1
	          (insert "VCS Stats\n")
	          (let* ((ecbf (progn (progress-reporter-update progress-reporter 14) (org-glaux--stats-git-edit-count-by-file)))
		               (ec (progn (progress-reporter-update progress-reporter 15) (org-glaux--stats-git-edits-count ecbf))))
	            (insert (format "  - Edits by file: min.: %d, median: %.1f, avg: %.2f, max.: %d\n"
			                        (progn (progress-reporter-update progress-reporter 16) (seq-min ec))
			                        (progn (progress-reporter-update progress-reporter 17) (org-glaux--stats-git-avg-edit-count-by-file ec))
			                        (progn (progress-reporter-update progress-reporter 18) (org-glaux--stats-git-median-edit-count-by-file ec))
			                        (progn (progress-reporter-update progress-reporter 19) (seq-max ec))))
	            (insert (format "  - Top %d most edited pages\n" 10))
	            (mapc (lambda (entry)
		                  (insert (format "    - %4d edit(s): [[%s]]\n"
				                              (cdr entry)
                                      (if (string-suffix-p ".org" (car entry))
                                          (concat "wiki:" (org-glaux--file-wiki-path (car entry)))
				                                (concat "file:" (car entry))))))
		                (org-glaux--stats-git-top-edit-count-files 10 ecbf))))
	      (org-glaux--vc-git-not-installed nil))
      (progress-reporter-done progress-reporter))))

;;;;; Internal -- Stats -- Edits Count

(defun org-glaux--stats-git-edit-count-by-file ()
  "Return (file . edit-count) alist."
  (let ((rm-files (org-glaux--vc-git-get-removed-files)))
    ;; Commit removed files
    (when rm-files
      (org-glaux--vc-git-register-removed-files)
      (org-glaux--vc-git-commit "org-glaux: automatic commit removed file(s) before stats."))
    (mapcar (lambda (f) (cons f (org-glaux--stats-git-file-edit-count f))) (org-glaux--vc-git-get-vc-files))))

(defun org-glaux--stats-git-file-edit-count (fpath)
  "Return git commit count of FPATH *in the working tree*.

Known bug: filename containing special character fails this function.
FIXME: replace them with unicode."
  (let ((default-directory org-glaux-location))
    (length (process-lines vc-git-program "log" "--oneline" fpath))))

(defun org-glaux--stats-git-edits-count (ecbf)
  "Given ECBF (edits-count by file), compute a list of edits count."
  (mapcar #'cdr (org-glaux--stats-git-edit-count-by-file)))

(defun org-glaux--stats-git-avg-edit-count-by-file (&optional edits-count ecbf)
  "Compute the average of edits by file.

EDITS-COUNT and ECBF (edits-count by file) are computed when needed."
  (let ((edits-count (or edits-count
			                  (org-glaux--stats-git-edits-count
			                   (or ecbf (org-glaux--stats-git-edit-count-by-file))))))
    (/ (float (apply #'+ edits-count)) (length edits-count))))

(defun org-glaux--stats-git-median-edit-count-by-file (&optional edits-count ecbf)
  "Compute the median of edits by file.

EDITS-COUNT and ECBF (edits-count by file) are computed when needed."
  (let ((edits-count (or edits-count
			                  (org-glaux--stats-git-edits-count
			                   (or ecbf (org-glaux--stats-git-edit-count-by-file))))))
    (org-glaux--calc-median edits-count)))

(defun org-glaux--stats-git-top-edit-count-files (n &optional ecbf)
  "Return a sorted alist (file-path . edits count) of the top N edits files.

If N is nil, return all files sorted in descending edits count order.
ECBF (edits-count by file) is computed when needed."
  (org-glaux--stats-top-count-files n #'org-glaux--stats-git-edit-count-by-file ecbf))

(defun org-glaux--stats-top-broken-links-count-files (n &optional pblbp)
  "Return the top N wiki page having the most broken links.

The result consists of a sorted alist (wiki-path . broken-links-count).
If N is nil, return all files in descending broken links count order.
PBLBP (positive broken links by page) is computed when needed."
  (mapcar (lambda (entry) (cons (org-glaux--file-wiki-path (car entry)) (cdr entry)))
          (org-glaux--stats-top-count-files n #'org-glaux--positive-broken-links-by-page pblbp)))

(defun org-glaux--stats-top-count-files (n f &optional alist-by-page)
  "Return the top N entry (file-path . count) having the highest count.

If N is nil, return all files in descending count order.
If precomputed ALIST-BY-PAGE is not provided, compute it by funcall F
with no argument."
  (let* ((alist-by-page (or alist-by-page (funcall f)))
	       (len (length alist-by-page))
	       (sorted (sort alist-by-page (lambda (a b) (> (cdr a) (cdr b))))))
    (if n
	      (butlast sorted (- len n))
      sorted)))

;;;;; Internal -- Stats -- Links by page

(defun org-glaux--get-avg-links-by-page (lbp &optional links-count pages-count)
  "Given LBP (links by page), compute the average number of links by page.

LINKS-COUNT and PAGES-COUNT are computed when needed."
  (let* ((links-count (or links-count (org-glaux--get-links-count-by-page lbp)))
	       (pages-count (or pages-count (length links-count))))
    (/ (float (apply #'+ links-count)) pages-count)))

(defun org-glaux--get-median-links-by-page (lbp &optional links-count)
  "Given LBP (links by page), compute the median of links by page.

LINKS-COUNT is computed when needed."
  (let ((links-count (or links-count
			                  (org-glaux--get-links-count-by-page lbp))))
    (org-glaux--calc-median links-count)))

(defun org-glaux--get-links-count-by-page (lbp &optional rmdup)
  "Given LBP (page . links) alist, compute the number of links by page.

Duplicated links are removed if RMDUP is non-nil."
  (mapcar (lambda (entry) (length (if rmdup
				                         (remove-duplicates (cdr entry))
			                         (cdr entry))))
	        lbp))

;;;; Internal -- Computation
(defun org-glaux--calc-median (nlist)
  "Return the mean of the number list NLIST."
  (let* ((sortl (sort nlist '<))
	       (len (length sortl)))
    (if (oddp len)
	      (nth (/ len 2) sortl)
      ;; mean of the two middle numbers
      (/ (+ (nth ;; (len + 1)/2 - 1
	           (- (/ (+ len 1) 2) 1)
	           sortl)
	          (nth ;; (len + 1)/2
	           (/ (+ len 1) 2)
	           sortl))
	       (float 2)))))

;;;; Internal -- Files


(define-error 'org-glaux--file-error "File error")

(defun org-glaux--move-file-fpath (src-fpath dest-fpath &optional non-verb)
  "Behave as Linux's command mv: move/rename SRC-FPATH to DEST-FPATH.

	- SRC-FPATH and DEST-FPATH are respectively the full path of source and
          destination.
	- Set NON-VERB to \\[t] to supress message emitted by this function.
	Note that parents directories are created if needed."
  ;; pre-handle directory to directory/file cases
  (if (file-exists-p src-fpath)
      ;; source file is directory
      (when (file-directory-p src-fpath)
	      ;; check if src-fpath = `org-glaux-location'
	      (when (string-equal src-fpath (expand-file-name org-glaux-location))
	        (signal 'org-glaux--file-error (list "attempt to move wiki location")))
	      ;; dest file is directory
	      (if (file-exists-p dest-fpath)
	          ;; dest file is directory
	          (if (file-directory-p dest-fpath)
		            (when (file-in-directory-p dest-fpath src-fpath)
		              (signal 'org-glaux--file-error (list "cannot move" src-fpath "to a subdirectory of itself" dest-fpath)))
	            (signal 'org-glaux--file-error (list "cannot overwrite non-directory" dest-fpath "with directory" src-fpath)))
	        (when (directory-name-p dest-fpath)
	          ;; To rename a directory with `rename-file', dest-fpath should not end with slash
	          (setq dest-fpath (directory-file-name dest-fpath)))))
    (signal 'org-glaux--file-error (list "source file doesn't exist" src-fpath)))

  ;; could fail if 2nd arg is nil (relative path)
  (make-directory (file-name-directory dest-fpath) t)
  (rename-file src-fpath dest-fpath 0))

;; TODO update backlinks
;; TODO update navigation history
;; TODO define a move-hook and append subroutine in this function
(defun org-glaux--move-hook  (src-fpath-page src-fpath-assets-dir dest-fpath-page dest-fpath-assets-dir)
  "Update buffers and VC after file renaming.

The following links are absolute.
- SRC-FPATH-PAGE: source wiki page filepath
- SRC-FPATH-ASSETS-DIR: source assets directory filepath
- DEST-FPATH-PAGE: target wiki page filepath
- DEST-FPATH-ASSETS-DIR: target assets directory filepath"
  (let ((src-buffer (get-file-buffer src-fpath-page)))

    ;; Update renamed/moved buffer associated filepath
    (when src-buffer
      (with-current-buffer src-buffer
	      (set-visited-file-name dest-fpath-page)))

    ;; Update buffers' associated filepath under moved/renamed assets directory
    (mapc
     (lambda (b)
       (with-current-buffer b
	       (set-visited-file-name
	        (concat
	         dest-fpath-assets-dir
	         (file-name-as-directory (file-name-base src-fpath-page))
	         (file-relative-name (buffer-file-name b) src-fpath-assets-dir)))))
     (cl-remove-if-not
      (lambda (b) (and (org-glaux--wiki-buffer-p b)
		            (file-in-directory-p (buffer-file-name b) src-fpath-assets-dir)))
      (buffer-list)))

    ;; Refontify wiki page buffers
    (mapc (lambda (b)
	          (with-current-buffer b
	            (font-lock-flush)))
	        (org-glaux--get-buffers-filename))

    ;; In-sync with version control, removed files are committed unconditionally
    (org-glaux--vc-git-commit-files
     (list dest-fpath-page dest-fpath-assets-dir)
     'manual "org-glaux: automatic commit due to file renaming")))

;;;; Internal -- List
(defun org-glaux--assets-page-files (dirpath)
  "Return all asset files from a given page's asset DIRPATH."
  (org-glaux--assets-make-dir dirpath)
  (let* ((dir-files (directory-files dirpath nil
				                             ;; exclude ".", "..", "ltximg"
				                             "^\\([^.]\\|\\.[^.]\\|\\.\\..\\|ltximg\\)"))
	       (org-files (directory-files dirpath nil "\\.org$")))
    (cl-remove-if
     (lambda (f) (or (file-directory-p ;; remove directories
		            (expand-file-name f dirpath))
	             (some
		            (lambda (org-file)
		              ;; remove suffix or prefix of children org-file name
		              (or (string-prefix-p (file-name-base org-file) f)
		                 ;; exclude auto-generated latex dirs such as _minted-<page-name>
		                 (string-suffix-p (file-name-base org-file) f)))
		            org-files)))
     dir-files)))

(defun org-glaux--get-buffers-filename ()
  "Return all org-glaux page buffers filename under `org-glaux-location`."
  (org-glaux--init-location)
  (cl-remove-if-not
   (lambda (buffer)
     (let* ((fp (buffer-file-name buffer))
	          (fpath (if fp (expand-file-name fp))))
       ;; test if file exists
       (and  fpath
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

(defun org-glaux--wiki-face (wiki-path)
  "Dynamic face for WIKI-PATH link."
  (let ((fpath (org-glaux--wiki-path-fpath wiki-path)))
    (unless (file-remote-p fpath) ;; Do not connect to remote files
      (if (file-exists-p fpath)
	        'org-link
	      ;; file link broken
	      'org-warning))))

(defun org-glaux--url-face (url)
  "Dynamic face for URL links."
  (let ((https+url (if (string-match "https?://" url)
		                   url
		                 (concat "https://" url))))
    (condition-case nil
	      (when (url-file-exists-p https+url)
	        'org-link)
      ;; url broken or FIXME: connection error...
      (error 'org-warning))))

;;;; Internal -- Make dir
(defun org-glaux--assets-buffer-make-dir ()
  "Create asset directory of current buffer page if it doesn't exit."
  (if (not (org-glaux--wiki-buffer-p (current-buffer)))
      (error "Not in a wiki page")
    (org-glaux--assets-make-dir (file-name-base buffer-file-name))))

(defun org-glaux--assets-make-dir (filepath)
  "Create the asset directory from a page's FILEPATH if it doesn't exist."
  (let ((assets-dir (file-name-sans-extension filepath)))
    (if (not (file-exists-p assets-dir))
	      (make-directory assets-dir t))))

;;;; Internal -- Menu
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


;;;; Internal -- Org properties
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

;;;; Internal -- Path computing
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

(defun org-glaux--cur-page-assets-dir ()
  "Return current org-glaux page's asset directory path."
  (expand-file-name (file-name-base buffer-file-name)
		                (file-name-directory buffer-file-name)))

;; TODO rename this function
(defun org-glaux--cur-page-assets-file (filename)
  "Return current page's asset path given its FILENAME."
  (expand-file-name filename (org-glaux--cur-page-assets-dir)))

(defun org-glaux--page->html-file (wiki-path)
  "Convert a page's WIKI-PATH to corresponding html filepath."
  (org-glaux--replace-extension (org-glaux--wiki-path-fpath wiki-path) "html"))
;;;; Internal -- Predicate
(defun org-glaux--wiki-file-p (fpath)
  "Return non-nil if FPATH is an org-glaux file under `org-glaux-location'."
  (when fpath
    (file-in-directory-p fpath (expand-file-name org-glaux-location))))

(defun org-glaux--wiki-buffer-p (buffer)
  "Return non-nil if BUFFER is an org-glaux buffer under `org-glaux-location'."
  (org-glaux--wiki-file-p (buffer-file-name buffer)))

;;;; Internal -- Publish
(defun org-glaux--make-org-publish-plist (org-exporter)
  "Prepare plist for use with `org-publish'.
Argument ORG-EXPORTER an org-exporter."
  (let ((plist-base
	       `("html"
	         :base-directory        ,org-glaux-location
	         :base-extension        "org"
	         :recursive             t
	         :publishing-directory  ,org-glaux-location
	         :publishing-function   ,org-exporter)))
    (setcdr plist-base
	          ;; combine with custom publish settings
	          (org-combine-plists (cdr plist-base) org-glaux-publish-plist))
    plist-base))

;;;; Internal -- Selection
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
  (let ((target (completing-read (or prompt "Wiki pages: ")
				                         (org-glaux--assets-page-files
				                          (file-name-sans-extension buffer-file-name)))))
    (funcall callback (org-glaux--cur-page-assets-file target))))

;;;; Internal -- Version control

(define-error 'org-glaux--vc-git-not-installed "Git is not installed")

(defun org-glaux--vc-git-install-p ()
  "Return non-nil if git is installed."
  (executable-find vc-git-program))

(defun org-glaux--vc-git-install-check ()
  "Emit an error if git is not installed."
  (unless (org-glaux--vc-git-install-p)
    (signal 'org-glaux--vc-git-not-installed nil)))

(defun org-glaux--vc-git-find-root (fpath)
  "Find the root of a project under VC from a FPATH.

  The function walks up the directory tree from FPATH looking for \".git\".
  If \".git\" is not found, return nil, otherwise return the root."
  (vc-find-root fpath ".git"))

(defun org-glaux--vc-ignore-files (files ignored-regexs)
  "Return FILES (filepath list) without those matching IGNORED-REGEXS."
  (cl-remove-if
   (lambda (fpath)
     (let ((remove? nil))
       (dolist (regex ignored-regexs remove?)
	       ;; TODO short-circuit evaluation
	       (setq remove? (or remove? (string-match-p regex fpath))))))
   files))

(defun org-glaux--vc-filter-files (files)
  "Filter FILES according to `org-glaux-vc-*' settings.

See  `org-glaux-vc-ignored-files-glob'."
  (let ((wiki-files (cl-remove-if-not 'org-glaux--wiki-file-p
				                              (-flatten
				                               (mapcar
				                                (lambda (f)
					                                (if (file-directory-p f)
					                                    (directory-files-recursively f "^.*$")
					                                  f))
				                                files))))
        (ignored-regexs (mapcar
		                     #'org-glaux--glob2regex
		                     (append
			                    org-glaux-vc-ignored-files-glob
			                    org-glaux-vc-ignored-dirs-glob))))
    ;; remove ignored files
    (org-glaux--vc-ignore-files wiki-files ignored-regexs)))


(defun org-glaux--vc-git-register-files (files)
  "Register FILES to commit according to `.gitignore' and filtering.

Return number of files registered.
See `org-glaux--vc-filter-files'."
  (let* ((default-directory org-glaux-location)
	       (potential-candidates (org-glaux--vc-filter-files files))
	       (candidates (cl-remove-if
		                  (lambda (fpath)
			                  ;; ignore file according to `.gitignore'.
			                  (or (equal (vc-git-state fpath) 'ignored)
			                     ;; removed file should be register by *-register-removed-files
			                     (member (vc-git-state fpath) '(up-to-date removed))
			                     (> (file-attribute-size
			                         (file-attributes fpath))
			                        org-glaux-vc-ignore-files-exceed-size)))
		                  potential-candidates)))
    (org-glaux--batch-execute-list 'vc-git-register 50 candidates)
    ;; TODO compute successful candidates
    (length candidates)))

(defun org-glaux--vc-git-register-removed-files ()
  "Register files in removed stage under git VCS."
  (interactive)
  (let ((default-directory org-glaux-location)
	      (removed-candidates (org-glaux--vc-git-get-removed-files)))
    (vc-git-command nil 0 removed-candidates "update-index" "--remove" "--")
    (length removed-candidates)))

(defun org-glaux--vc-git-commit (&optional message)
  "Commit files into git with optional MESSAGE.

Should be called after `org-glaux--vc-git-register-files'"
  (let ((default-directory org-glaux-location))
    (vc-git-command nil 0 nil
		                "commit"
		                "-m"
		                (or message "org-glaux: automatic commit."))))

(defun org-glaux--vc-git-commit-files (files context &optional message)
  "Register and commit FILES with optional MESSAGE depending the CONTEXT.

- The CONTEXT corresponds to the variable `org-glaux-vc-commit-when'.
- This function checks additionally possible errors."

  (let ((index (org-glaux--wiki-path-fpath org-glaux-index-file-basename)))
    (with-current-buffer (find-file-noselect index)
      (when (and (equal org-glaux-vc-backend 'git)
	             ;; manually commit is always accepted
	             (or (member context (list 'manual org-glaux-vc-commit-when))
	                (when (equal org-glaux-vc-commit-when 'close+follow)
		                (member context '(close follow)))
	                (when (equal org-glaux-vc-commit-when 'follow+save)
		                (member context '(follow save)))
	                (when (equal org-glaux-vc-commit-when 'close+save)
		                (member context '(close save)))
	                (when (equal org-glaux-vc-commit-when 'close+follow+save)
		                (member context '(close follow save)))))
        (condition-case err
	          (progn
	            (org-glaux--vc-git-install-check)
	            (org-glaux-vc-git-init-root)
	            (let ((register-count (+ (org-glaux--vc-git-register-files files)
				                               ;; unconditionally remove removed files
				                               (org-glaux--vc-git-register-removed-files))))
	              (when (> register-count 0)
	                (org-glaux--vc-git-commit message)
	                (message "%s" message))))
          (org-glaux--vc-git-not-installed (display-warning 'org-glaux (error-message-string err)))
          (error (display-warning 'org-glaux
			                            (format "org-glaux: unable to register & commit files in the context %s : %s"
				                                  (symbol-name context)
				                                  (error-message-string err)))))))))

(defun org-glaux--vc-git-commit-on-save ()
  "Commit change into git on save."
  (when (and (member org-glaux-vc-commit-when '(save close+save follow+save close+follow+save))
	         (org-glaux--wiki-buffer-p (current-buffer)))
    (org-glaux--vc-git-commit-files
     (list buffer-file-name)
     'save
     "org-glaux: automatic commit on page save")))

(add-hook 'after-save-hook 'org-glaux--vc-git-commit-on-save)

(defun org-glaux--vc-git-get-removed-files ()
  "Return files in removed state under git VCS."
  ;; vc-git-state have to be in the version-controlled tree
  (let ((default-directory org-glaux-location))
    (cl-remove-if
     #'null
     (mapcar (lambda (f) (when (equal (vc-git-state f) 'removed) f))
	           (org-glaux--vc-git-get-vc-files)))))

(defun org-glaux--vc-git-get-vc-files ()
  "Return all files in the working tree under git VCS."
  (let ((default-directory org-glaux-location))
    ;; If there is no yet any commit, commands below will fail
    ;; First check if it's the case or not with git branch -a
    (when (process-lines vc-git-program "branch" "-a")
      (mapcar (lambda (rel-fpath)
                (expand-file-name
                 ;; The command "git ls-tree -r <branch> --name-only" returns
                 ;; escaped string surrounded by double-quote if the string contains
                 ;; Unicode. We convert it to `'utf-8'.
                 (concat org-glaux-location
                         "/"
                         (org-glaux--decode-escaped-to-utf8
                          (replace-regexp-in-string "^\"\\(.*?\\)\"$" "\\1"
                                                    rel-fpath)))))
	            (let ((git-cur-branch (car (process-lines vc-git-program "rev-parse" "--abbrev-ref" "HEAD"))))
	              (process-lines vc-git-program "ls-tree" "-r" git-cur-branch "--name-only"))))))

;;;; Internal -- Miscellaneous
;; from https://emacs.stackexchange.com/a/5730/23697
(defun org-glaux--chop-string (string &optional separators omit-nulls keep-sep)
  "Split STRING into substrings bounded by match for SEPARATORS.
OMIT-NULLS behaves the same as in `split-string'. If KEEP-SEP is non-nil, keep
the separators."
  (let* ((keep-nulls (not (if separators omit-nulls t)))
         (rexp (or separators split-string-default-separators))
         (start 0)
         this-start this-end
         notfirst
         (list nil)
         (push-one
          (lambda ()
            (when (or keep-nulls (< this-start this-end))
              (let ((this (substring string this-start this-end)))
                (when (or keep-nulls (> (length this) 0))
                  (push this list)))))))
    (while (and (string-match
               rexp string
               (if (and notfirst
                      (= start (match-beginning 0))
                      (< start (length string)))
                   (1+ start) start))
              (< start (length string)))
      (setq notfirst t)
      (setq this-start start this-end (match-beginning 0)
            start (match-end 0))
      (funcall push-one)
      (when keep-sep
        (push (match-string 0 string) list)))
    (setq this-start start this-end (length string))
    (funcall push-one)
    (nreverse list)))

(defun org-glaux--decode-escaped-to-utf8 (str)
  "Decode an escaped string STR to utf-8 (e.g. \"\\303\\273\" => \"û\")."
  (decode-coding-string
   (apply
    #'concat
    (mapcar
     (lambda (ns)
       (if (number-or-marker-p ns)
           (unibyte-string ns)
         ns))
     (mapcar
      (lambda (s)
        (if (string-prefix-p "\\" s)
            (string-to-number (substring s 1) 8)
          s))
      (org-glaux--chop-string str "\\\\[0-7]\\{3\\}" t t))))
   'utf-8))

;; TODO add progress bar
(defun org-glaux--batch-execute-list (f size data-list)
  "Execute function F per batch of DATA-LIST with size SIZE."
  (let* ((sublist (butlast data-list (- (length data-list) size)))
	       (offset size))
    (while sublist
      (funcall f sublist)
      (setq sublist (butlast (nthcdr offset data-list) (- (length data-list) size offset)))
      (setq offset (+ offset size)))))


(defun org-glaux--glob2regex (glob)
  "Convert GLOB expression to regex.

- <glob-expr> -> ^<glob-expr>$
- . -> \\.
- * -> .*"
  (replace-regexp-in-string "\\*" ".*"
			                      (replace-regexp-in-string "\\." "\\\\."
						                                          (format "^%s$" glob))))
;;; Links
;;;; Wiki link
;; Hyperlinks to other wiki pages.
;; wiki:<wiki-path> or [[wiki:<wiki-path>][<description>]]
(org-link-set-parameters "wiki"
			                   :follow #'org-glaux--wiki-follow
			                   :export #'org-glaux--wiki-export
			                   :face #'org-glaux--wiki-face)

;;;; Http link
;; (org-link-set-parameters "http"
;;   			 :face #'org-glaux--url-face)
;;
;; (org-link-set-parameters "https"
;;   			 :face #'org-glaux--url-face)

(provide 'org-glaux)
;;; org-glaux.el ends here
