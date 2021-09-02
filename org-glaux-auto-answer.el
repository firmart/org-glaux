;;; org-glaux-auto-answer.el --- Org glaux utility to answer prompt -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.3
;; Keywords: outlines, files, convenience
;; URL: https://www.github.com/firmart/org-glaux
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))


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

;; This package is a rewrite of https://github.com/YoungFrog/auto-answer
;; (written by Nicolas Richard <youngfrog@members.fsf.org>) using Emacs builtin
;; cl-lib instead of dash.el. As such, the description of auto-answer is kept
;; below.

;; Some functions prompt by calling y-or-n-p or yes-or-no-p. It is
;; sometimes desirable to give the answer to the prompt from lisp.

;; One way to do this is to push an event (corresponding to the
;; keypress) onto `unread-command-events`. Another way is to advice
;; the prompting function to bypass the prompt. This package does the
;; latter.

;; Example usage:
;; (let ((org-glaux-auto-answer
;;                     '(("\\`What's your name\\? \\'" "jack")
;;                      ("\\`What's your password\\? \\'" "secr3t"))))
;;   (list
;;    (read-string "What's your name? ")
;;    (read-passwd "What's your password? ")))
;; => ("jack" "secr3t")


;;; Code:

(require 'cl-lib)
(defvar org-glaux-auto-answer nil
  "When bound, prompting functions will not prompt.

This variable should be bound to a list of elements of the
form (REGEXP ANSWER).

If a prompt string, i.e. the first argument of one of the
function in `org-glaux-auto-answer-functions', matches REGEXP, then that
function will return ANSWER without prompting.")

(defconst org-glaux-auto-answer-functions
  '(y-or-n-p
    yes-or-no-p
    read-string
    read-from-minibuffer
    read-key-sequence
    read-key-sequence-vector
    read-event
    read-passwd)
  "List of functions to override.")

(defun org-glaux-auto-answer (oldfun &rest args)
  "Used as an around advice to auto-answer questions.

The questions that will be answered are those asked by functions
in the list `org-glaux-auto-answer-functions'.

See also function `org-glaux-auto-answer-rmc'."
  (let ((prompt (car args)))
    (let* ((matcher-answer (and (stringp prompt)
				(cl-find-if (lambda (form)
					      (string-match (car form) prompt))
					      org-glaux-auto-answer)))
           (dontask (and matcher-answer (cdr matcher-answer)))
           (answer (cadr matcher-answer)))
      (if dontask
          answer
        (apply oldfun args)))))

(defun org-glaux-auto-answer-rmc (oldfun &rest args)
  "Used as an around advice to auto-answer questions.

The questions that will be answered are those asked by the
function `read-multiple-choice'.

See also function `org-glaux-auto-answer'."
  (let ((prompt (car args)))
    (let* ((matcher-answer (and (stringp prompt)
				(cl-find-if
				 (lambda (form)
				   (string-match (car form) prompt))
				 org-glaux-auto-answer)))
           (dontask (and matcher-answer (cdr matcher-answer))))
      (if dontask
          (cl-find-if
	   (lambda (form)
	     (eq (car it) dontask))
	   (cadr args))
        (apply oldfun args)))))

(advice-add #'read-multiple-choice :around #'org-glaux-auto-answer-rmc)

(mapc (lambda (fun)
        (advice-add fun :around 'org-glaux-auto-answer))
      org-glaux-auto-answer-functions)

(provide 'org-glaux-auto-answer)
;;; org-glaux-auto-answer.el ends here
