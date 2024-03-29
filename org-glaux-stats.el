;;; org-glaux-stats.el --- Wiki statistics for Org-glaux -*- lexical-binding: t; -*-

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

;; (WIP) This file provides wiki statistics such as total pages, broken links count, orphelin pages (WIP) etc.

;;; Code:

;; built-in libraries
(require 'org)
(require 'seq) ;; TODO Drop this dependency: use cl-lib only
(require 'cl-lib)
(require 'vc)
(require 'vc-git)

(require 'org-glaux-init)
(require 'org-glaux-core)
(require 'org-glaux-vc)

;;;; Internal: Links Stats Utilities

;;;###autoload
(defun org-glaux--get-file-links (fpath &rest link-type)
  "Return all links of type LINK-TYPE appearing in file FPATH."
  (with-temp-buffer (insert-file-contents fpath)
                    (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
	                (when (member (org-element-property :type link) link-type)
	                  (org-element-property :path link)))))) 

;;;###autoload
(defun org-glaux--get-all-links-by-page (&rest link-type)
  "Return an alist (PAGE-FPATH . LINK-TYPE list)."
  (mapcar (lambda (f)
	    (cons f (apply #'org-glaux--get-file-links f link-type)))
	  (org-glaux--pages-list)))

;; TODO write better docstring
;; TODO incorporate to org-glaux--show-wiki-stats
;;;###autoload
(defun org-glaux--get-all-http-status (&optional http-by-page https-by-page no-connection)
  "Report http/https links status."
  (interactive "i\ni\nP")
  (let* ((https-by-page (or https-by-page (org-glaux--get-all-links-by-page "https")))
	 (https-urls (append (mapcar (lambda (form)
				       (mapcar
					(lambda (link) (concat "https:" link))
					(cdr form)))
				     https-by-page)))
	 (http-by-page (or http-by-page (org-glaux--get-all-links-by-page "http")))
	 (http-urls (append (mapcar (lambda (form)
				      (mapcar
				       (lambda (link) (concat "http:" link))
				       (cdr form)))
				    http-by-page)))
	 (all-urls (apply #'append (append http-urls https-urls)))
	 (len (length all-urls))
	 (progress-reporter (make-progress-reporter "Retrieve links status..." 1 len))
	 (counter 1)
	 (data (mapcar
		(lambda (url)
		  (progress-reporter-update progress-reporter
					    (setq counter (1+ counter)))
		  `(:url ,url
			 :status ,(org-glaux-link-http-htbl-status url no-connection)))
		all-urls)))
    (prog1 data
      (when (called-interactively-p 'any)
	  (cl-loop for d in data
		   for status = (plist-get d :status)
		   with found-total = 0
		   with timeout-total = 0
		   with not-found-total = 0
		   with unknown-total = 0
		   when (eq 'found status) do (setq found-total (1+ found-total))
		   when (eq 'timeout status) do (setq timeout-total (1+ timeout-total))
		   when (eq 'not-found status) do (setq not-found-total (1+ not-found-total))
		   when (and (not no-connection)
			     (eq 'unknown status))
		     do (setq unknown-total (1+ unknown-total))
		   finally do (message "links status: found=%s, timeout=%s, not found=%s%s"
				       found-total timeout-total not-found-total
				       (format ", unknown=%s" unknown-total)))))))

;;;###autoload
(defun org-glaux--get-all-links-count-by-page (&rest link-type)
  "Return a list of links count of type LINK-TYPE."
  (mapcar (lambda (f)
	    (length (apply #'org-glaux--get-file-links f link-type)))
	  (org-glaux--pages-list))) 

;;;###autoload
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

;;;###autoload
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

;;;; Internal: Wiki Stats

;; TODO Rename this function (drop wiki & make it public)
;; TODO Dispatch into multiple (interactive functions)
;; TODO Save Org page data in a file to greatly speed up the task
;;;###autoload
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
	;; Pages
	(org-insert-heading)
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

;;;###autoload
(defun org-glaux--stats-git-edit-count-by-file ()
  "Return (file . edit-count) alist."
  (let ((rm-files (org-glaux--vc-git-get-removed-files)))
    ;; Commit removed files
    (when rm-files
      (org-glaux--vc-git-register-removed-files)
      (org-glaux--vc-git-commit "org-glaux: automatic commit removed file(s) before stats."))
    (mapcar (lambda (f)
              (cons f (org-glaux--stats-git-file-edit-count f)))
            (org-glaux--vc-git-get-vc-files))))

;;;###autoload
(defun org-glaux--stats-git-file-edit-count (fpath)
  "Return git commit count of FPATH *in the working tree*.

Known bug: filename containing special character fails this function.
FIXME: replace them with unicode."
  (let ((default-directory org-glaux-location))
    (length (process-lines vc-git-program "log" "--oneline" (string-as-unibyte fpath)))))

;;;###autoload
(defun org-glaux--stats-git-edits-count (ecbf)
  "Given ECBF (edits-count by file), compute a list of edits count."
  (mapcar #'cdr (org-glaux--stats-git-edit-count-by-file)))

;;;###autoload
(defun org-glaux--stats-git-avg-edit-count-by-file (&optional edits-count ecbf)
  "Compute the average of edits by file.

EDITS-COUNT and ECBF (edits-count by file) are computed when needed."
  (let ((edits-count (or edits-count
			 (org-glaux--stats-git-edits-count
			  (or ecbf (org-glaux--stats-git-edit-count-by-file))))))
    (/ (float (apply #'+ edits-count)) (length edits-count))))

;;;###autoload
(defun org-glaux--stats-git-median-edit-count-by-file (&optional edits-count ecbf)
  "Compute the median of edits by file.

EDITS-COUNT and ECBF (edits-count by file) are computed when needed."
  (let ((edits-count (or edits-count
			 (org-glaux--stats-git-edits-count
			  (or ecbf (org-glaux--stats-git-edit-count-by-file))))))
    (org-glaux--calc-median edits-count)))

;;;###autoload
(defun org-glaux--stats-git-top-edit-count-files (n &optional ecbf)
  "Return a sorted alist (file-path . edits count) of the top N edits files.

If N is nil, return all files sorted in descending edits count order.
ECBF (edits-count by file) is computed when needed."
  (org-glaux--stats-top-count-files n #'org-glaux--stats-git-edit-count-by-file ecbf))

;;;###autoload
(defun org-glaux--stats-top-broken-links-count-files (n &optional pblbp)
  "Return the top N wiki page having the most broken links.

The result consists of a sorted alist (wiki-path . broken-links-count).
If N is nil, return all files in descending broken links count order.
PBLBP (positive broken links by page) is computed when needed."
  (mapcar (lambda (entry) (cons (org-glaux--file-wiki-path (car entry)) (cdr entry)))
          (org-glaux--stats-top-count-files n #'org-glaux--positive-broken-links-by-page pblbp)))

;;;###autoload
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

;;;###autoload
(defun org-glaux--get-avg-links-by-page (lbp &optional links-count pages-count)
  "Given LBP (links by page), compute the average number of links by page.

LINKS-COUNT and PAGES-COUNT are computed when needed."
  (let* ((links-count (or links-count (org-glaux--get-links-count-by-page lbp)))
	 (pages-count (or pages-count (length links-count))))
    (/ (float (apply #'+ links-count)) pages-count)))

;;;###autoload
(defun org-glaux--get-median-links-by-page (lbp &optional links-count)
  "Given LBP (links by page), compute the median of links by page.

LINKS-COUNT is computed when needed."
  (let ((links-count (or links-count
			 (org-glaux--get-links-count-by-page lbp))))
    (org-glaux--calc-median links-count)))

;;;###autoload
(defun org-glaux--get-links-count-by-page (lbp &optional rmdup)
  "Given LBP (page . links) alist, compute the number of links by page.

Duplicated links are removed if RMDUP is non-nil."
  (mapcar (lambda (entry) (length (if rmdup
				      (remove-duplicates (cdr entry))
			            (cdr entry))))
	  lbp))

;;;; Internal: Computation
;;;###autoload
(defun org-glaux--calc-median (nlist)
  "Return the mean of the number list NLIST."
  (let* ((sortl (sort nlist '<))
	 (len (length sortl)))
    (if (cl-oddp len)
	(nth (/ len 2) sortl)
      ;; mean of the two middle numbers
      (/ (+ (nth ;; (len + 1)/2 - 1
	     (- (/ (+ len 1) 2) 1)
	     sortl)
	    (nth ;; (len + 1)/2
	     (/ (+ len 1) 2)
	     sortl))
	 (float 2)))))

;;; org-glaux-stats.el ends here
(provide 'org-glaux-stats)
