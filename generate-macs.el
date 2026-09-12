;;; generate-macs.el --- Random testing utilities for Emacs Lisp -*- lexical-binding: t -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0.0
;; Keywords: tools, maint
;; Package-Requires: ((emacs "30.1") (dash "2.20.0") (s "1.13.1"))
;; Homepage: https://github.com/ApollonDeParnasse/generate

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Macros for generate.el

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'gv)
(require 'dash)
(require 's)

(defmacro generate--plural! (macro args)
  "Use ARGS to create a plural verson of MACRO."
  `(progn
     ,@(seq-map (lambda (p) `(,macro ,p))
		(symbol-value args))))

(defun generate--defun-partition-body (body)
  "Return ARGS as a list like (DOCSTRING DECLS BODY).
DOCSTRING is the first string in BODY if present and it succeeded by
more forms.  DECLS is a list of declarations in the DECLARE statement
if present after the docstring.   Everything else is BODY.
Shamelessly stolen from org-ml."
  (cl-flet
      ((is-declare
         (form)
         (eq 'declare (car form))))
    (let ((first (car body))
          (second (cadr body))
          (rest (cddr body)))
      (cond
       ((and (stringp first) (is-declare second))
        (list first (cdr second) rest))
       ((and (stringp first) second)
        (list first nil (cons second rest)))
       ((and (is-declare first) second)
        (list nil (cdr first) (cons second rest)))
       (t
        (list nil nil body))))))

(defconst generate--GEN-REPLACEMENTS-ALIST
  (list (cons "list-of-n" "random-list-of")))

(cl-defun generate--create-random-name-for-def (symbol &optional
						       (replacements-alist
							generate--GEN-REPLACEMENTS-ALIST))

  (-let* ((name (symbol-name symbol))
	  ((val-to-replace . new-val) (-first
				       (-compose
					(-rpartial #'s-contains-p name)
					#'car)
				       replacements-alist)))
    (->> name
	 (s-replace val-to-replace new-val)
	 (intern))))

(defun generate--split-def!-docstring (docstring)
  "Split DOCSTRING into two docstrings."
  (let* ((docstring-divider (s-index-of "\\" docstring))
	 (docstring-one (substring docstring 0 docstring-divider))
	 (docstring-two (substring docstring
				   (+ docstring-divider 1))))
    (list docstring-one docstring-two)))

(defun generate--convert-mv-name-into-sv-name (name)
  "Convert multiple value NAME into single value NAME."
  (->> name
       (symbol-name)
       (s-replace "-mv" "")
       (intern)))

(defmacro generate--defalias-mv! (symbol definition &optional docstring)
  "Create a single value and multiple values defalias for SYMBOL.
Set SYMBOL’s function definition to DEFINITION.  The optional third
argument DOCSTRING specifies the documentation string for SYMBOL."
  (declare (doc-string 3) (indent 2))
  (-let* (((mv-docstring single-docstring) (generate--split-def!-docstring docstring))
	  (single-symbol (generate--convert-mv-name-into-sv-name symbol)))
    `(progn
       (defalias ',symbol ,definition
	 ,mv-docstring)
       (defalias ',single-symbol
	 (-compose #'car #',symbol)
         ,single-docstring))))

(defmacro generate--defun-mv! (name arglist &rest args)
  "Return the multiple values version and the regular version of a generator function."
  (declare (doc-string 3) (indent 2))
  (-let* (((docstring decls body) (generate--defun-partition-body args))
	  ((mv-docstring regular-docstring) (generate--split-def!-docstring docstring))
	  (regular-name (generate--convert-mv-name-into-sv-name name)))
    `(progn
       (defun ,name ,arglist
         ,mv-docstring
	 ,decls
         ,@body)
       (defun ,regular-name ,arglist
         ,regular-docstring
	 ,decls
	 (car (,name ,@arglist))))))

(defmacro generate--defun-random! (name arglist &rest args)
  "Return a generator function and a randomized version of the function."
  (declare (doc-string 3) (indent 2))
  (-let* (((docstring decls body) (generate--defun-partition-body args))
	  ((regular-docstring random-docstring) (generate--split-def!-docstring docstring))
	  (random-name (generate--create-random-name-for-def name))
          (random-arglist (cdr arglist)))
    `(progn
       (defun ,name ,arglist
         ,regular-docstring
	 ,decls
         ,@body)
       (defun ,random-name ,random-arglist
         ,random-docstring
	 ,decls
	 (funcall (generate-default-convert-n-gen-to-random #',name))))))

(defmacro generate--defun-random-with-one-arg! (name arglist &rest args)
  "Return a generator function and a randomized version of the function."
  (declare (doc-string 3) (indent 2))
  (-let* (((docstring decls body) (generate--defun-partition-body args))
	  ((regular-docstring random-docstring) (generate--split-def!-docstring docstring))
	  (random-name (generate--create-random-name-for-def name))
          (random-arglist (cdr arglist)))
    `(progn
       (defun ,name ,arglist
         ,regular-docstring
         ,@body)
       (defun ,random-name ,random-arglist
         ,random-docstring
	 ,decls
	 (funcall (generate-default-convert-n-gen-to-random-with-arg #',name) ,@random-arglist)))))

(provide 'generate-macs)
;;; generate-macs.el ends here
