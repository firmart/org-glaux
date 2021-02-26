

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

(provide 'org-glaux-menu)
