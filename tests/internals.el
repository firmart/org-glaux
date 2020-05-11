;;; internals.el --- org-glaux internal functions tests -*- lexical-binding: t; -*-
;;;; Internal -- Download
(ert-deftest org-glaux-test-assets-download-hof ())
;;;; Internal -- Initialization
(ert-deftest org-glaux-test-insert-header ())
(ert-deftest org-glaux-test-init-location ())
;;;; Internal -- Wiki Link
(ert-deftest org-glaux-test-make-link ())
(ert-deftest org-glaux-test-wiki-follow ())
(ert-deftest org-glaux-test-wiki-export ())
;;;; Internal -- Computation
(ert-deftest org-glaux-test-calc-median ()
  ;; odd length list
  (should (equal (org-glaux--calc-median '(1 3 3 6 7 8 9)) 6))
  ;; even length list
  (should (equal (org-glaux--calc-median '(1 2 3 4 5 6 8 9)) 4.5)))
;;;; Internal -- List
(ert-deftest org-glaux-test-assets-page-files ())
(ert-deftest org-glaux-test-get-buffers ())
(ert-deftest org-glaux-test-page-files ())
(ert-deftest org-glaux-test-wiki-face ())
(ert-deftest org-glaux-test-url-face ())
;;;; Internal -- Make dir
(ert-deftest org-glaux-test-assets-buffer-make-dir ())
(ert-deftest org-glaux-test-assets-make-dir ())
;;;; Internal -- Menu
(ert-deftest org-glaux-test-easy-menu-entry ())
;;;; Internal -- Org properties
(ert-deftest org-glaux-test-global-props ())
(ert-deftest org-glaux-test-global-prop-value ())
;;;; Internal -- Path computing
(ert-deftest org-glaux-test-file-wiki-path ()
  (let ((org-glaux-location "~/org/wiki"))
    (should (string= (org-glaux--file-wiki-path "~/org/wiki/index.org") "index"))
    (should (string= (org-glaux--file-wiki-path "~/org/wiki/topic/Nephology.org") "topic/Nephology"))))

(ert-deftest org-glaux-test-replace-extension ()
  (let ((path-dot "~/.emacs.d/path/to/file.org")
	(ext-dot "tar.gz"))
    (should (string= (org-glaux--replace-extension path-dot ext-dot) "~/.emacs.d/path/to/file.tar.gz"))
    (should (string= (org-glaux--replace-extension path-dot "html") "~/.emacs.d/path/to/file.html"))))

(ert-deftest org-glaux--wiki-path-fpath ()
  (let ((org-glaux-location "/root/org/wiki")
	(buffer-fpath "/root/org/wiki/path/to/topic.org")
	(children "/test")
	(sibling "../test")
	(abs "test"))
    (should (string= (org-glaux--wiki-path-fpath children buffer-fpath) "/root/org/wiki/path/to/topic/test.org"))
    (should (string= (org-glaux--wiki-path-fpath sibling buffer-fpath) "/root/org/wiki/path/to/test.org"))
    (should (string= (org-glaux--wiki-path-fpath abs buffer-fpath) "/root/org/wiki/test.org"))))

(ert-deftest org-glaux-test-current-page-name ())
(ert-deftest org-glaux-test-current-page-assets-dir ())
(ert-deftest org-glaux-test-current-page-assets-file ())
(ert-deftest org-glaux-test-page->html-file ())
(ert-deftest org-glaux-test-assets-get-dir ())
;;;; Internal -- Predicate
(ert-deftest org-glaux-test-is-file-in ())
(ert-deftest org-glaux-test-is-buffer-in ())
;;;; Internal -- Publish
(ert-deftest org-glaux-test-make-org-publish-plist ())
;;;; Internal -- Selection
(ert-deftest org-glaux-test-select ())
(ert-deftest org-glaux-test-assets-select ())
;;;; Internal -- Version control
(ert-deftest org-glaux-test-vc-git-find-root ())
(ert-deftest org-glaux-test-vc-git-init-root ())
(ert-deftest org-glaux-test-vc-filter-files ()
  (let ((files '("/path/to/test.org"
		 "/path/to/test2.org"
		 "/path/to/test.tar.gz"
		 "/path/to/test.c"
		 "/path/to/test.h"
	         "/external/path/to/test.org"))
	(org-glaux-vc-ignored-files-glob '("*.[ch]"))
	(org-glaux-vc-ignored-dirs-glob
	 (append '("*ltximg*" "*_minted*")
		 (mapcar (lambda (d) (format "*%s*" d)) vc-directory-exclusion-list)))
	(org-glaux-vc-backend 'git)
	(org-glaux-vc-wiki-pages-only nil)
	(org-glaux-location "/path/to"))
    ;; `org-glaux-vc-wiki-pages-only' overwrites `org-glaux-vc-ignored-regex'
    (should (equal
	     (let ((org-glaux-vc-wiki-pages-only t))
	       (org-glaux--vc-filter-files files))
	     '("/path/to/test.org" "/path/to/test2.org")))
    ;; remove *.[ch]
    (should (equal
	     (let ((org-glaux-vc-wiki-pages-only nil))
	       (org-glaux--vc-filter-files files))
	     '("/path/to/test.org" "/path/to/test2.org" "/path/to/test.tar.gz")))))
(ert-deftest org-glaux--vc-git-register-files ())
