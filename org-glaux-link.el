(require 'org)
(require 'timer)
(require 'org-glaux-core)

;;; Links
;;;; Wiki link
;; Hyperlinks to other wiki pages.
;; wiki:<wiki-path> or [[wiki:<wiki-path>][<description>]]
(org-link-set-parameters "wiki"
			 :follow #'org-glaux--wiki-follow
			 :export #'org-glaux--wiki-export
			 :face #'org-glaux--wiki-face) 

;;;; File link
(org-link-set-parameters "file" :face #'org-glaux--generic-face)

;;;; Http link
(org-link-set-parameters "http"
  			 :face #'org-glaux--url-face)

(org-link-set-parameters "https"
  			 :face #'org-glaux--url-face)


;;;; Internal: Face

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

(defun org-glaux--url-face (link)
  "Dynamic face for url LINK."
  (let* ((url (concat "https:" https+url))
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
      ;; Trigger link check whenever url is the same than the previous one.
      (if (and prev-url (string= url prev-url))
	  (condition-case err 
	      (if (url-file-exists-p url)
		  'org-link
		'org-warning)
	    (error 'org-warning)) 
	(puthash hash-key url org-glaux--link-url-hash-table)
	'org-default))))

(provide 'org-glaux-link)
