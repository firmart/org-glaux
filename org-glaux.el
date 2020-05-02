;;; org-glaux.el --- Desktop wiki extension for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 5.1
;; Keywords: outlines, files, convenience
;; URL: https://www.github.com/firmart/org-glaux'
;; Package-Requires: ((helm-core "2.0") (cl-lib "0.5") (emacs "25.1") (org "7.0"))


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

;; Org-glaux is a org-mode extension that provides tools to manage and
;; build a desktop wiki where each wiki page is an org-mode file.
;;

;; external libraries
(require 'org)
(require 'ox-html)
(require 'helm)

;; built-in Emacs lib
(require 'cl-lib)     ;; Common-lisp emulation library
(require 'easymenu)
(require 'subr-x)     ;; Provides string trim functions.

;; ****************** USER-SETTINGS ************************ ;;

;;; Code:

(defgroup org-glaux nil
  "Org-wiki Settings"
  :group 'tools)

(defcustom org-glaux-location-list '("~/org/wiki")
  "List of org-glaux root directories."
  :type  '(repeat directory)
  :group 'org-glaux)

(defvar org-glaux-location nil)

(defcustom org-glaux-default-read-only nil
  "If this variable is non-nil all org-glaux pages will be read-only by default.
You can toggle read-only mode with \\<read-only-mode>."
  :type  'boolean
  :group 'org-glaux)

(defcustom org-glaux-close-root-switch t
  "If set, all org-glaux pages are closed when root directory is switched.
\(Default value: true)"
  :type  'boolean
  :group 'org-glaux)

;;; =======  Python Webserver Settings =========== ;;

(defcustom org-glaux-server-port "8000"
  "Default port to server org-glaux static files server."
  :type  'string
  :group 'org-glaux)

(defcustom org-glaux-server-host "0.0.0.0"
  "Default address that the server listens to."
  :type  'string
  :group 'org-glaux)

;; ======== Async export settings ================ ;;

(defcustom org-glaux-emacs-path "emacs"
  "Path to Emacs executable.  Default value 'Emacs'."
  :type 'file
  :group 'org-glaux)

;;; Path to init file like init.el used by function org-glaux-html-export
;; The user can set for a more lightweight file in order to speed up the
;; exporting speed.
;;
(defcustom org-glaux-user-init-file (concat (file-name-as-directory user-emacs-directory) "init.el")
  "Path to init.el file used for asynchronous export."
  :type 'file
  :group 'org-glaux)

(defcustom org-glaux-backup-location nil
  "Path to backup directory."
  :type 'directory
  :group 'org-glaux)


;;; Default index page (index.org) accessed with M-x org-glaux-index
;;
(defvar org-glaux-index-file-basename "index")

;;; Additional publishing options
(defcustom org-glaux-publish-plist '()
  "Additional options passed to `org-publish'."
  :type 'plist
  :group 'org-glaux)

(defcustom org-glaux-template
  (string-trim
   "
#+OPTIONS: d:nil tags:nil todo:nil toc:t ^:nil
#+TITLE: %n
#+DESCRIPTION:
#+KEYWORDS:
#+STARTUP:  overview
#+DATE: %d
")
  "Default template used to create org-glaux pages/files.
- %n - is replaced by the page name.
- %d - is replaced by current date in the format year-month-day."

  :type 'string
  :group 'org-glaux)

;; https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
(defun org-glaux--global-props (fpath &optional property)
  "Return the plists of global org properties of a FPATH.

Argument FPATH: filepath.
Optional argument PROPERTY: the property seeking."
  (let* ((bp (get-file-buffer fpath))
	 (buffer (find-file fpath)))
    (unless property (setq property "PROPERTY"))
    (with-current-buffer (or buffer (current-buffer))
      (let ((value (org-element-map (org-element-parse-buffer)
		       'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))
	;; close the file if it was open by this function
	(unless bp
	  (with-current-buffer buffer
	    (save-buffer)
	    (kill-this-buffer)))
	value))))

(defun org-glaux--global-prop-value (fpath key)
  "Return global org property KEY of current buffer.

Argument FPATH: filepath."
  (org-element-property :value (car (org-glaux--global-props fpath key))))

(defun org-glaux--file-wiki-path (fpath)
  "Return the wiki-path of FPATH (filepath)."
  (file-relative-name (file-name-sans-extension fpath) org-glaux-location))

;; TODO delete this function
(defun org-glaux--concat-path (base relpath)
  "Concat directory path (BASE) and a relative path (RELPATH)."
  (concat (file-name-as-directory base) relpath))

;; TODO Rename this function
(defun org-glaux--start-location ()
  "Initialize `org-glaux-location' variable if not set yet."
  (if (not org-glaux-location)
      (setq org-glaux-location (car org-glaux-location-list))))

(defun org-glaux--get-buffers ()
  "Return all org-glaux page buffers filename under `org-glaux-location`."
  (org-glaux--start-location)
  (cl-remove-if-not
   (lambda (buffer)
     (let* ((fp (buffer-file-name buffer))
	    (fpath (if fp (expand-file-name fp))))
       ;; test if file exists
       (and  fpath
	   ;; test if buffer file is under current wiki location
	   (string-prefix-p (expand-file-name org-glaux-location) fpath)
	   ;; test if buffer file has extension .org
	   (string-suffix-p ".org" fpath))))
   (buffer-list)))

(defun org-glaux--replace-extension (filename extension)
  "Replace FILENAME's extension by a new EXTENSION."
  (concat (car (split-string filename "\\.")) "." extension))

;; TODO rename this function
(defun org-glaux--page->file (wiki-path)
  "Return filepath of given WIKI-PATH.
- Relative wiki-path:
    - Children page: \"/test\" -> \"<current-file-assets-dir>/test.org\"
    - Sibling page: \"../test\" -> \"<current-dir>/test.org\"
- Absolute wiki-path: \"test\" -> \"<org-glaux-location>/test.org\""
  (expand-file-name
   (concat
    (concat
     ;; if wiki-path starts with (../)+ or / then it's a relative wiki-path
     (if (string-match "^\\(\\(\\.\\.\\/\\)+\\|\\/\\)" wiki-path)
	 (file-name-as-directory (file-name-sans-extension buffer-file-name))
       (file-name-as-directory org-glaux-location))
     wiki-path) ".org")))

(defun org-glaux--current-page-name ()
  "Return current org-glaux page's name bound to current buffer."
  (file-name-base buffer-file-name))

;; TODO rename this function
(defun org-glaux--current-page-assets-dir ()
  "Return current org-glaux page's asset directory path."
  (expand-file-name (org-glaux--current-page-name)
		    (file-name-directory buffer-file-name)))

;; TODO rename this function
(defun org-glaux--current-page-assets-file (filename)
  "Return current page's asset path given its FILENAME."
  (expand-file-name filename (org-glaux--current-page-assets-dir)))

(defun org-glaux--page->html-file (wiki-path)
  "Convert a page's WIKI-PATH to corresponding html filepath."
  (org-glaux--replace-extension (org-glaux--page->file wiki-path) "html"))

(defun org-glaux--page-files ()
  "Return a list containing all wiki-pages under `org-glaux-location`."
  (org-glaux--start-location)
  (cl-remove-if
   (lambda (f)
     (let ((b (file-name-base f)))
       (or (string-prefix-p ".#" b)
	  (string-suffix-p "~"  b)
	  (string-prefix-p "#"  b)
	  (string-suffix-p "#"  b))))
   (directory-files-recursively org-glaux-location "\\.org$")))

(defun org-glaux--assets-get-dir (filepath)
  "Return the page's asset directory path given its FILEPATH."
  (file-name-sans-extension filepath))

(defun org-glaux--assets-make-dir (filepath)
  "Create the asset directory from a page's FILEPATH if it doesn't exist."
  (let ((assets-dir (org-glaux--assets-get-dir filepath)))
    (if (not (file-exists-p assets-dir))
        (make-directory assets-dir t))))

(defun org-glaux--assets-buffer-make-dir ()
  "Create asset directory of current buffer page if it doesn't exit."
  (if (not (org-glaux--is-buffer-in (current-buffer)))
      (error "Not in a wiki page")
    (org-glaux--assets-make-dir (org-glaux--current-page-name))))

(defun org-glaux--is-buffer-in (buffer)
  "Return non-nil if BUFFER is an org-glaux buffer under `org-glaux-location`."
  (string-prefix-p
   (expand-file-name org-glaux-location)
   (buffer-file-name buffer)))

(defun org-glaux--make-link (wiki-path)
  "Return a string containing a wiki link [[wiki:WIKI-PATH][TITLE]].
Argument WIKI-PATH: the link which is a wiki-path."
  (format "[[wiki:%s][%s]]"
	  wiki-path
	  (org-glaux--global-prop-value (org-glaux--page->file wiki-path) "TITLE")))

(defun org-glaux--xdg-open (filename)
  "Open a file FILENAME with default system application.

- Running in Linux or BSD invokes the script xdg-open
- Running in Windows invokes cmd.exe
- Running in Mac OSX invokes open"
  (cl-case system-type
    ;;; Linux
    (gnu/linux      (let ((process-connection-type  nil))
                      (start-process
		       "proc"
		       nil
		       ;; Command
		       "xdg-open" (expand-file-name filename))))
    ;;; Free BSD OS
    (gnu/kfreebsd    (let ((process-connection-type  nil))
                       (start-process
			"proc"
			nil
			;; Command
			"xdg-open" (expand-file-name filename))))
    ;; Mac OSX
    (darwin        (start-process
                    "proc"
                    nil
                    ;; Command
                    "open" (concat  (expand-file-name filename))))
    ;; Windows 7, 8, 10 - Kernel NT
    (windows-nt   (start-process
                   "proc"
                   nil
                   ;; Command
                   "cmd"  "/C"  "start" "" (expand-file-name filename))))) ;; End of org-glaux/xdg-open


;; Hyperlinks to other org-glaux pages.
;; wiki:<wiki-path> or [[wiki:<wiki-path>][<description>]]
(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "wiki"
			     :follow #'org-glaux--open-page
			     :export #'org-glaux--wiki-export)
  (add-hook 'org-mode-hook
	    ;; obsolete since org 9.0
	    (lambda () (org-add-link-type "wiki" 'org-glaux--open-page 'org-glaux--wiki-export))))

(defun org-glaux--open-page (wiki-path)
  "Open or create if it doesn't exist an org-glaux page given its WIKI-PATH."
  (let ((page-fpath (org-glaux--page->file wiki-path)))
    (if (file-exists-p page-fpath)
	;; if the page exists, open it
	(progn (find-file page-fpath)
	       (when org-glaux-default-read-only (read-only-mode t)))
      ;; if the page doesn't exist, create it
      (find-file page-fpath)
      (org-glaux--insert-header)
      (save-buffer)
      (org-glaux--assets-make-dir page-fpath))))

(defun org-glaux--wiki-export (wiki-path desc format)
  "Export a wiki page link from Org files."
  (cl-case format
    (html (format "<a href='%s'>%s</a>"
		  (file-relative-name (org-glaux--replace-extension
				       (org-glaux--page->file wiki-path) "html") ".")
		  (or desc wiki-path)))
    (ascii (format "%s (%s)" (or desc wiki-path) wiki-path))
    (latex (format "\\href{%s}{%s}"
		   (file-relative-name
		    (expand-file-name wiki-path org-glaux-location) ".")
		   (or desc wiki-path)))))

;; Hyperlinks to asset files that are opened with system applications.
;; wiki-assets-sys:<filename> or [[wiki-assets-sys:<filename>][<description>]
(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "wiki-assets-sys"
			     :follow #'org-glaux--wiki-assets-sys-open
			     :export #'org-glaux--wiki-assets-sys-export)
  (add-hook 'org-mode-hook
	    ;; obsolete since org 9.0
	    (lambda () (org-add-link-type "wiki-assets-sys" 'org-glaux--wiki-assets-sys-open 'org-glaux--wiki-assets-sys-export))))


(defun org-glaux--wiki-assets-sys-open (filename)
  "Open with default system app an org-glaux page's asset given its FILENAME."
  (org-glaux--xdg-open (org-glaux--current-page-assets-file filename)))

(defun org-glaux--wiki-assets-sys-export (path desc format)
  "Export a wiki page asset's link from Org files."
  (org-glaux--wiki-export (org-glaux--current-page-assets-file path) desc format))

(defun org-glaux--helm-selection (callback)
  "Open a helm menu to select the wiki page and invokes the CALLBACK function."
  (helm :sources
	(helm-build-sync-source "Wiki Pages"
	  :candidates  (cl-remove-if-not
			(lambda (p) (when (car p) t))
			(mapcar (lambda (file)
				  ;; FIXME performance issue
				  (cons (org-glaux--global-prop-value file "TITLE")
					(org-glaux--file-wiki-path file)))
				(org-glaux--page-files)))
	  :fuzzy-match t
	  :action callback)))

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

(defun org-glaux--assets-helm-selection (callback)
  "Call a CALLBACK function on the filepath of the current page's selected asset."
  (helm :sources
	(helm-build-sync-source "Wiki Pages"
	  :candidates (org-glaux--assets-page-files (org-glaux--assets-get-dir buffer-file-name))
	  :fuzzy-match t
	  :action (lambda (file) (funcall callback (org-glaux--current-page-assets-file file))))))

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
       (default-directory (org-glaux--assets-get-dir buffer-file-name))

       (output-file  (read-string "File name: "
				  (car  (last (split-string url "/"))))))

    (org-glaux--assets-make-dir buffer-file-name)
    (url-copy-file url output-file)
    (funcall callback output-file)))

(defun org-glaux-help ()
  "Show org-glaux commands."
  (interactive)
  (command-apropos "org-glaux-[^-]"))

(defun org-glaux-switch-root ()
  "Switch org-glaux root directory."
  (interactive)
  (helm :sources
	(helm-build-sync-source "Org-wiki root directories"
	  :candidates (mapcar (lambda (p) (cons (format "%s - %s" (file-name-nondirectory p) p) p))
			      (mapcar #'string-trim org-glaux-location-list))
	  :fuzzy-match t
	  :action (lambda (p)
		    ;; Close all current org-glaux pages if custom value set
		    (when org-glaux-close-root-switch
		      (org-glaux-close)
		      (message (format "Org-wiki pages under directory %s are saved" org-glaux-location)))
		    (setq org-glaux-location p)
		    (org-glaux-index)
		    (message (format "Org-wiki root directory set to: %s" p))))))

(defun org-glaux-index ()
  "Open the index page: <org-glaux-location>/index.org.
The page is created if it doesn't exist."
  (interactive)
  (org-glaux--start-location)
  (org-glaux--open-page org-glaux-index-file-basename))

(defun org-glaux-index-html ()
  "Open the Wiki (Index) in the default web browser."
  (interactive)
  (browse-url (concat "file://"
		      (org-glaux--page->html-file
		       org-glaux-index-file-basename))))

(defun org-glaux-index-frame ()
  "Open the index page in a new frame."
  (interactive)
  (with-selected-frame (make-frame)
    (org-glaux-index)))

(defun org-glaux-assets-dired ()
  "Open the asset directory of current wiki page."
  (interactive)
  (let ((pagename (org-glaux--current-page-name)))
    ;; TODO check if it is a wiki-page
    (org-glaux--assets-make-dir pagename)
    (dired (org-glaux--assets-get-dir buffer-file-name))))

(defun org-glaux-assets-insert ()
  "Insert at point a wiki-assets-sys link to a current page's asset file.
The link type wiki-assets-sys:<asset-file> is opened with default system's app."
  (interactive)
  (org-glaux--assets-helm-selection
   (lambda (file)
     (insert (format "[[wiki-assets-sys:%s][%s]]"
                     (file-name-nondirectory file)
                     (read-string "Description: " (file-name-nondirectory file)))))))

(defun org-glaux-assets-insert-file ()
  "Insert at point a file link to a current page's asset file.
The link type file is opened with Emacs."
  (interactive)
  (org-glaux--assets-helm-selection
   (lambda (file)
     (insert (if (fboundp 'org-link-make-string)
		 (org-link-make-string (format "file:%s/%s" (org-glaux--current-page-name) (file-name-nondirectory file)))
	       (org-make-link-string (format "file:%s/%s" (org-glaux--current-page-name) (file-name-nondirectory file))) ;; obsolete since org 9.3
	       (file-name-nondirectory file))))))

(defun org-glaux-assets-insert-image ()
  "Insert link file:<page>/<file> to images asset file at point.
This command is similar to `org-glaux-assets-insert-file' but it inserts a link
in this way: [[file:Linux/logo.png][file:Linux/logo.png/]]."
  (interactive)
  (org-glaux--assets-helm-selection
   (lambda (file)
     (save-excursion
       (insert (if (fboundp 'org-link-make-string)
		   (org-link-make-string (format "file:%s/%s"
						 (org-glaux--current-page-name)
						 (file-name-nondirectory file)))
		 ;; obsolete since org 9.3
		 (org-make-link-string
		  (format "file:%s/%s"
			  (org-glaux--current-page-name)
			  (file-name-nondirectory file)))))))))

(defun org-glaux-assets-find-file ()
  "Open in Emacs a selected asset file of the current page from a menu."
  (interactive)
  (org-glaux--assets-helm-selection #'find-file))

(defun org-glaux-assets-find-sys ()
  "Open with system's app a selected asset file of the current page from a menu."
  (interactive)
  (org-glaux--assets-helm-selection #'org-glaux--xdg-open))

(defun org-glaux-assets-download-insert-sys ()
  "Download a file from a URL and insert a wiki-assets-sys link.
Note: This function is synchronous and blocks Emacs."
  (interactive)
  (org-glaux--assets-download-hof
   (lambda (output-file)
     (save-excursion (insert (format "[[wiki-assets-sys:%s][%s]]"
                                     output-file output-file))))))

(defun org-glaux-assets-download-insert-file ()
  "Download a file from a URL in the clibpoard and insert a link file link.
Note: This function is synchronous and blocks Emacs."
  (interactive)
  (org-glaux--assets-download-hof
   (lambda (output-file)
     (save-excursion (insert (format "[[file:%s/%s][%s]]"
				     (org-glaux--current-page-name) output-file output-file))))))
(defun org-glaux-helm ()
  "Open a wiki page with helm."
  (interactive)
  (org-glaux--helm-selection #'org-glaux--open-page))

(defun org-glaux-helm-read-only ()
  "Open a wiki page with helm in read-only mode."
  (interactive)
  (org-glaux--helm-selection (lambda (pagename)
			      (find-file-read-only
			       (org-glaux--page->file pagename)))))

(defun org-glaux-helm-frame ()
  "Browser the wiki files using helm and opens it in a new frame."
  (interactive)
  (org-glaux--helm-selection  (lambda (act)
                               (with-selected-frame (make-frame)
                                 (set-frame-name (concat "Org-wiki: " act))
                                 (org-glaux--open-page act)))))

(defun org-glaux-helm-switch ()
  "Switch between org-glaux page buffers."
  (interactive)
  (helm :sources
	(helm-build-sync-source "Wiki Pages"
	  :candidates (mapcar (lambda (b)
				(cons (file-name-base (buffer-file-name b)) b))
			      (org-glaux--get-buffers))
	  :fuzzy-match t
	  :action 'switch-to-buffer)))

(defun org-glaux-helm-html ()
  "Browse a wiki page in html format using helm.  It is created if it doesn't exist yet."
  (interactive)
  (helm :sources
	(helm-build-sync-source "Wiki Pages"
	  :candidates (org-glaux--page-files)
	  :fuzzy-match t
	  :action (lambda (fpath)
		    (let ((html-file   (org-glaux--replace-extension fpath "html")))
		      (if (not (file-exists-p html-file))
			  (with-current-buffer (find-file fpath)
			    (org-html-export-to-html)))
		      (browse-url html-file))))))

(defun org-glaux-close ()
  "Close all opened wiki pages buffer and save them."
  (interactive)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (org-glaux--is-buffer-in b)
              ;; save the buffer if it is bound to a file
              ;; and it is not read-only
              (when (and (buffer-file-name b)
		       (not buffer-read-only))
                (save-buffer))
              (kill-this-buffer))))
        (buffer-list))
  (message "All wiki files closed."))

(defun org-glaux-close-images ()
  "Close all image/picture buffers which files are in org-glaux directory."
  (interactive)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (and (org-glaux--is-buffer-in b)
		     (equal major-mode 'image-mode))
              (kill-this-buffer))))
        (buffer-list))
  (message "All wiki images closed."))

(defun org-glaux-insert-link ()
  "Insert a Wiki link at point for a existing page."
  (interactive)
  (org-glaux--helm-selection
   (lambda (wiki-path)
     (insert (org-glaux--make-link wiki-path)))))

(defun org-glaux-insert-new ()
  "Create a new org-glaux and insert a link to it at point."
  (interactive)
  (let ((page-name (read-string  "Page: ")))
    (save-excursion (insert (if (fboundp 'org-link-make-string)
				(org-link-make-string (concat "wiki:" page-name))
			      (org-make-link-string (concat "wiki:" page-name)) ;; obsolete since org 9.3
			      page-name)))))

(defun org-glaux-new ()
  "Create a new wiki page and open it without inserting a link."
  (interactive)
  (org-glaux--open-page (read-string "Page Name: ")))

(defun org-glaux-html-page-open ()
  "Open the current wiki page in the browser.  It is created if it doesn't exist yet."
  (interactive)
  (let ((html-file   (org-glaux--replace-extension buffer-file-name "html")))
    (if (not (file-exists-p html-file))
        (org-html-export-to-html))
    (browse-url html-file)))

(defun org-glaux-html-page-export ()
  "Exports the current wiki page to html and opens it in the browser."
  (interactive)
  (org-html-export-to-html)
  (browse-url (org-glaux--replace-extension buffer-file-name "html")))

(defun org-glaux-search (pattern)
  "Search all wiki pages that contain a PATTERN (regexp or name)."
  (interactive "sorg-glaux - Search for: ")
  (grep-compute-defaults) ;; Set up grep-find-command
  (rgrep pattern "*\\.org" org-glaux-location))

;; TODO rename this function to something with "dired"
(defun org-glaux-assets-helm ()
  "Open the assets directory of a wiki page."
  (interactive)
  (org-glaux--helm-selection
   (lambda (page)
     (org-glaux--assets-make-dir page)
     (dired (org-glaux--assets-get-dir page)))))

(defun org-glaux-make-org-publish-plist (org-exporter)
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

(defun org-glaux-export-with (org-exporter)
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
        (pub-plist (org-glaux-make-org-publish-plist org-exporter)))
    (org-publish pub-plist t)))

(defun org-glaux-export-html-sync ()
  "Export all pages to html in synchronous mode."
  (interactive)
  (let ((org-html-htmlize-output-type "css")
        (org-html-htmlize-font-prefix "org-"))
    (org-publish (org-glaux-make-org-publish-plist 'org-html-publish-to-html)
		 t)))

(defun org-glaux-export-html-async ()
  "Export all pages to html in asynchronous mode."
  (interactive)
  (let ((org-html-htmlize-output-type "css")
	(org-html-htmlize-font-prefix "org-"))
    (org-publish (org-glaux-make-org-publish-plist 'org-html-publish-to-html)
		 t
		 t)))

(defun org-glaux-export-html ()
"Export all pages to html.
Note: This function doesn't freeze Emacs since it starts another Emacs process."
(interactive)
(compile (mapconcat 'identity
		    `(,org-glaux-emacs-path
		      "--batch"
		      "-l" ,org-glaux-user-init-file
		      "-f" "org-glaux-export-html-sync"
		      "--kill")
		    " ")))

;; TODO update this menu.
(defun org-glaux-menu ()
  "Optional command to build an utility menu."
  (interactive)
  (easy-menu-define org-wik-menu global-map "Org-wiki"

    `("org-glaux"
      ("Main"
       ["Go to Index page" (org-glaux-index)]

       ["---" nil]
       ["Browsing" nil]
       ["Browse page" (org-glaux-helm)]
       ["Browse page in other frame" (org-glaux-helm-frame)]
       ["Browse pages in read-only mode" (org-glaux-helm-read-only)]
       ["---" nil]
       ["Wiki Directory" nil]
       ["Close all pages" (org-glaux-close)]

       ["---" nil]
       ["Html export" nil]
       ["Open index page (html) in the browser" (org-glaux-index-html)]
       ["Export all pages to html" (org-glaux-export-html)]
       ["Help - Show all org-glaux commands" (org-glaux-help)])
      ["---"  nil]
      ("Page Commands"
       ["Browse current page asset directory."
        (org-glaux-assets-dired)]
       ["Browse current page asset directory with system's file manager."
        (org-glaux-assets-open)]

       ["Insert a link to a wiki page" (org-glaux-insert)]
       ["Insert a link of type wiki-assets-sys at point."
        (org-glaux-assets-insert)]
       ["Insert a link of type file:<page>/<asset> at point."
	(org-glaux-assets-insert-file)
	]
       ["Download an asset file and insert a wiki-assets-sys link at point."
	(org-glaux-assets-download-insert1)
	]

       ["Download an asset file and insert a link at point of type file:<page>/<file.pdf>."
	(org-glaux-assets-download-insert2)])
      ["---"  nil])))

;;
;; Despite this function was implemented as a interface to
;; Python3 simple http server, it can be refactored to work
;; with another more powerful http server such as Nginx.
;;
(defun org-glaux-server-toggle ()
  "Start/stop org-glaux http server.  It requires Python3.
Note: This command requires Python3 installed."
  (interactive)
  (let (
        ;; Process name
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
	    (message "\nServer started ...\n")

	    ;; Show machine network cards' IP addresses.
	    (cl-case system-type
                                        ;;; Linux
	      (gnu/linux       (insert (shell-command-to-string "ifconfig")))
                                        ;;; Free BSD OS
	      (gnu/kfreebsd    (insert (shell-command-to-string "ifconfig")))
	      ;; Mac OSX - (Not tested )
	      (darwin          (insert (shell-command-to-string "ifconfig")))
	      ;; Windows 7, 8, 10 - Kernel NT
	      (windows-nt      (insert (shell-command-to-string "ipconfig")))))
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
	      (message "Server stopped.")))))

(defun org-glaux-find-dired ()
  "Show all org-glaux files in all sub-directories of `org-glaux-location'."
  (interactive)
  (find-dired org-glaux-location "-name '*.org'"))

(defun org-glaux-website ()
  "Open org-glaux github repository."
  (interactive)
  (browse-url "http://www.github.com/firmart/org-glaux"))

(defun org-glaux--insert-header ()
  "Insert a header at the top of the file."
  ;; Save current cursor location and restore it
  ;; after completion of block insider save-excursion.
  (save-excursion
    (let*
	;; replace '%n' by page title
	((text1 (replace-regexp-in-string
		 "%n"
		 (org-glaux--current-page-name)
		 org-glaux-template))
	 ;; Replace %d by current date in the format %Y-%m-%d
	 (text2 (replace-regexp-in-string
		 "%d"
		 (format-time-string "%Y-%m-%d")
		 text1)))
      ;; Got to top of file
      (goto-char (point-min))
      (insert text2))))

;; ============ Backup Commands =============== ;;

(defun org-glaux-backup-make ()
  "Make a org-glaux backup."
  (interactive)
  (let* ((zipfile            (concat "org-glaux-" (format-time-string "%Y-%m-%d") ".zip"))
         (destfile           (concat (file-name-directory org-glaux-backup-location) zipfile))
         (default-directory  org-glaux-location))
    (switch-to-buffer "*org-glaux-backup*")

    ;; Crate org-glaux backup location directory if doesn't exist.
    (if (not (file-exists-p org-glaux-backup-location))
        (make-directory org-glaux-backup-location t))

    (if (file-exists-p destfile) (delete-file destfile))
    (if (file-exists-p zipfile)  (delete-file zipfile))

    ;; Clear buffer removing all lines
    (delete-region (point-min) (point-max))
    (set-process-sentinel
     (start-process
      "org-glaux-backup" ;; Process name
      "*org-glaux-backup*" ;; Buffer where output is displayed.
      ;; Shell command
      "zip" "-r" "-9" zipfile ".")
     (lexical-let ((zipfile  zipfile)
                   (destfile destfile))
       (lambda (process state)
         (when (equal (process-exit-status process) 0)
           (switch-to-buffer "*org-glaux-backup*")
           (rename-file zipfile org-glaux-backup-location t)
           (message "Backup done.")
           (insert  "\nBackup done.  Run M-x org-glaux-backup-dir to open backup directory.")))))))

(defun org-glaux-backup-dired ()
  "Open org-glaux backup directory in dired mode."
  (interactive)
  ;; Create org-glaux backup location directory if doesn't exist.
  (if (not (file-exists-p org-glaux-backup-location))
      (make-directory org-glaux-backup-location t))
  ;; Open backup location
  (dired org-glaux-backup-location)
  ;; Update buffer
  (revert-buffer))

(provide 'org-glaux)
;;; org-glaux.el ends here