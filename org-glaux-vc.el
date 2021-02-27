;;; org-glaux-vc.el --- Org glaux version control -*- lexical-binding: t; -*-

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

;; This file provides Org glaux version control supports such as
;; - manual commit of the full wiki; 
;; - automatic commit upon distinct events (page close, page follow, page save).

;;; Code:

(require 'vc)
(require 'vc-git)
(require 'vc-hooks) ;; `vc-directory-exclusion-list'

(require 'org-glaux-core)

;;; Custom group
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

;;;; Interactive functions

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

;;;; Internals
;;;;; VCS

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

;; TODO this affects publish performance
;; (add-hook 'after-save-hook 'org-glaux--vc-git-commit-on-save)

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
;;;;; Misc
(defun org-glaux--glob2regex (glob)
  "Convert GLOB expression to regex.

- <glob-expr> -> ^<glob-expr>$
- . -> \\.
- * -> .*"
  (replace-regexp-in-string "\\*" ".*"
			    (replace-regexp-in-string "\\." "\\\\."
						      (format "^%s$" glob)))) 

;; TODO add progress bar
(defun org-glaux--batch-execute-list (f size data-list)
  "Execute function F per batch of DATA-LIST with size SIZE."
  (let* ((sublist (butlast data-list (- (length data-list) size)))
	 (offset size))
    (while sublist
      (funcall f sublist)
      (setq sublist (butlast (nthcdr offset data-list) (- (length data-list) size offset)))
      (setq offset (+ offset size)))))

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

;; FIXME: there is probably a simpler way
(defun org-glaux--decode-escaped-to-utf8 (str)
  "Decode an escaped string STR to utf-8 (e.g. \"\\303\\273\" => \"û\")."
  (decode-coding-string
   (apply
    #'concat
    (mapcar
     (lambda (s)
       (let ((ns (if (and (not (string-equal "\\\\" s))
                          (string-prefix-p "\\" s))
                     (string-to-number (substring s 1) 8)
                   s)))
         (if (number-or-marker-p ns)
             (unibyte-string ns)
           ns)))
     (org-glaux--chop-string str "\\\\[0-7]\\{3\\}" t t)))
   'utf-8))

;;; org-glaux-vc.el ends here
(provide 'org-glaux-vc)
