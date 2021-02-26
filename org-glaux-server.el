(require 'org-glaux-core)

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

(provide 'org-glaux-server)
