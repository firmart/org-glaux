
(require 'org)
(require 'org-glaux-core)

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

;;;; Internal: Publish

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

(provide 'org-glaux-export)
