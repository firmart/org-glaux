(require 'org-glaux-core)


(defcustom org-glaux-location-list '("~/org/wiki")
  "List of org-glaux root directories."
  :type  '(repeat directory)
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

(provide 'org-glaux-init)
