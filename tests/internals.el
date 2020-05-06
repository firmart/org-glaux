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
(ert-deftest org-glaux-test-file-wiki-path ())
(ert-deftest org-glaux-test-replace-extension ())
(ert-deftest org-glaux-test-page->file ())
(ert-deftest org-glaux-test-current-page-name ())
(ert-deftest org-glaux-test-current-page-assets-dir ())
(ert-deftest org-glaux-test-current-page-assets-file ())
(ert-deftest org-glaux-test-page->html-file ())
(ert-deftest org-glaux-test-assets-get-dir ())
;;;; Internal -- Predicate
(ert-deftest org-glaux-test-is-buffer-in ())
;;;; Internal -- Publish
(ert-deftest org-glaux-test-make-org-publish-plist ())
;;;; Internal -- Selection
(ert-deftest org-glaux-test-select ())
(ert-deftest org-glaux-test-assets-select ())
