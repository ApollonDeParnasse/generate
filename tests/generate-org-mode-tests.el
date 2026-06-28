;;; generate-org-mode-tests.el --- Tests for org-mode  -*- lexical-binding: t; -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: testing
;; Package-Requires: ((emacs "30") (dash "2.20.0") (s "1.13.1"))
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

;;

;;; Code:

(require 'seq)
(require 'map)
(require 'org)
(require 'org-element)
(require 's)
(require 'dash)
(require 'generate)

(defconst generate-TEST-SRC-BLOCK-LANGS
  (list "elisp" "emacs-lisp" "org"))

(generate-ert-deftest-n-times generate--org-table-cell-values-helper ()
  :num-runs 100
  (-let* (((test-row-count test-column-count) (generate--two-random-nat-numbers-in-range-10))
	  ((expected-row expected-column) (mapcar (lambda (x) (generate--random-nat-number-between-0-and x)) (list test-row-count test-column-count)))
	  (test-val-generator (-lambda ((r c)) (format "%s,%s" (1- r) (1- c))))
	  (actual-values (generate--org-table-cell-values-helper test-val-generator test-row-count test-column-count)))
    (should (string-equal (nth expected-column (nth expected-row actual-values)) (format "%s,%s" expected-row expected-column)))))

(generate-ert-deftest-n-times generate--org-table-without-hlines ()
  :num-runs 100
  (-let* (((test-row-count test-column-count) (generate--two-random-nat-numbers-in-range-10))
	  ((test-val-generator test-cell-value) (generate-random-cl-constantly))
	  ((actual-table actual-table-values) (generate--org-table-without-hlines test-val-generator test-row-count test-column-count)))
    (should (s-starts-with-p "| " actual-table))
    (should (s-ends-with-p " |" actual-table))
    (should (length= (s-split "\n" actual-table) test-row-count))
    (should (length= actual-table-values test-row-count))
    (should (length= (generate-seq-take-random-value-from-seq actual-table-values) test-column-count))))

(generate-ert-deftest-n-times generate--org-table-with-hlines ()
  :num-runs 100
  (-let* (((test-row-count test-column-count) (generate--two-random-nat-numbers-in-range-10))
	  ((test-val-generator test-cell-value) (generate-random-cl-constantly))
	  ((actual-table actual-table-values) (generate--org-table-with-hlines test-val-generator test-row-count test-column-count)))
    (should (s-starts-with-p "| " actual-table))
    (should (s-ends-with-p " |" actual-table))
    (should (length= (generate-seq-take-random-value-from-seq (seq-filter (apply-partially #'generate--not-equal 'hline) actual-table-values)) test-column-count))))

(generate-ert-deftest-n-times generate--org-table ()
  :num-runs 100
  (-let* (((test-row-count test-column-count) (generate--two-random-nat-numbers-in-range-10))
	  ((test-val-generator test-cell-value) (generate-random-cl-constantly))
	  ((actual-table actual-table-values) (generate--org-table test-val-generator test-row-count test-column-count)))
    (should (s-starts-with-p "| " actual-table))
    (should (s-ends-with-p " |" actual-table))
    (should (length= (generate-seq-take-random-value-from-seq (seq-filter (apply-partially #'generate--not-equal 'hline) actual-table-values)) test-column-count))))

(generate-ert-deftest-n-times generate-with-buffer-with-org-table-without-hlines ()
  :num-runs 100
  (-let* (((test-list &as test-row-count test-column-count) (generate--two-random-nat-numbers-in-range-10))
	  ((test-row-number test-column-number) (seq-map (lambda (val) (generate-random-nat-number-in-range (list 1 val))) test-list))
	  ((test-val-generator test-cell-value) (generate-random-cl-constantly))
	  ((actual-cell-value actual-table) (generate-with-buffer-with-org-table-without-hlines (list test-val-generator test-row-count test-column-count)
					      (list (org-table-get test-row-number test-column-number) (org-table-to-lisp)))))
    (should (string-equal actual-cell-value test-cell-value))
    (should-not (member 'hline actual-table))))

(generate-ert-deftest-n-times generate-with-buffer-with-org-table-with-hlines ()
  :num-runs 100
  (-let* (((test-list &as test-row-count test-column-count) (generate--two-random-nat-numbers-in-range-10))
	  ((test-row-number test-column-number) (seq-map (lambda (val) (generate-random-nat-number-in-range (list 1 val))) test-list))
	  ((test-val-generator test-cell-value) (generate-random-cl-constantly))
	  (actual-cell-value (generate-with-buffer-with-org-table-with-hlines (list test-val-generator test-row-count test-column-count)
			       (org-table-get test-row-number test-column-number))))
    (should (string-equal actual-cell-value test-cell-value))))

(generate-ert-deftest-n-times generate-with-buffer-with-org-table ()
  :num-runs 100
  (-let* (((test-list &as test-row-count test-column-count) (generate--two-random-nat-numbers-in-range-10))
	  ((test-row-number test-column-number) (seq-map (lambda (val) (generate-random-nat-number-in-range (list 1 val))) test-list))
	  ((test-val-generator test-cell-value) (generate-random-cl-constantly))
	  ((actual-cell-value actual-table) (generate-with-buffer-with-org-table (list test-val-generator test-row-count test-column-count)
					      (list (org-table-get test-row-number test-column-number) (org-table-to-lisp)))))
    (should (string-equal actual-cell-value test-cell-value))))

(generate-ert-deftest-n-times generate-inactive-org-timestamp-string/with-time ()
  :num-runs 100
  (let* ((actual-timestamp (generate-inactive-org-timestamp-string t))
	 (actual-element (org-timestamp-from-string actual-timestamp)))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "[" actual-timestamp))
    (should (s-ends-with-p "]" actual-timestamp))))

(generate-ert-deftest-n-times generate-inactive-org-timestamp-string/without-time ()
  :num-runs 100
  (let* ((actual-timestamp (generate-inactive-org-timestamp-string))
	 (actual-element (org-timestamp-from-string actual-timestamp)))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "[" actual-timestamp))
    (should (s-ends-with-p "]" actual-timestamp))))

(generate-ert-deftest-n-times generate-random-inactive-org-timestamp-string ()
  :num-runs 100
  (let* ((actual-timestamp (generate-random-inactive-org-timestamp-string)))
    (should (string-match-p org-element--timestamp-regexp actual-timestamp))
    (should (s-starts-with-p "[" actual-timestamp))
    (should (s-ends-with-p "]" actual-timestamp))))

(generate-ert-deftest-n-times generate-active-org-timestamp-string/with-time ()
  :num-runs 100
  (let* ((actual-timestamp (generate-active-org-timestamp-string t))
	 (actual-element (org-timestamp-from-string actual-timestamp)))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "<" actual-timestamp))
    (should (s-ends-with-p ">" actual-timestamp))))

(generate-ert-deftest-n-times generate-active-org-timestamp-string/without-time ()
  :num-runs 100
  (let* ((actual-timestamp (generate-active-org-timestamp-string))
	 (actual-element (org-timestamp-from-string actual-timestamp)))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "<" actual-timestamp))
    (should (s-ends-with-p ">" actual-timestamp))))

(generate-ert-deftest-n-times generate-random-active-org-timestamp-string ()
  :num-runs 100
  (let* ((actual-timestamp (generate-random-active-org-timestamp-string)))
    (should (string-match-p org-element--timestamp-regexp actual-timestamp))
    (should (s-starts-with-p "<" actual-timestamp))
    (should (s-ends-with-p ">" actual-timestamp))))

(generate-ert-deftest-n-times generate-org-timestamp-string/with-time ()
  :num-runs 100
  (let* ((actual-timestamp (generate-org-timestamp-string t))
	 (actual-element (org-timestamp-from-string actual-timestamp)))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))
    (should (string-match-p org-element--timestamp-regexp actual-timestamp))))

(generate-ert-deftest-n-times generate-org-timestamp-string/without-time ()
  :num-runs 100
  (let* ((actual-timestamp (generate-org-timestamp-string))
	 (actual-element (org-timestamp-from-string actual-timestamp)))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))
    (should (string-match-p org-element--timestamp-regexp actual-timestamp))))

(generate-ert-deftest-n-times generate-random-org-timestamp-string ()
  :num-runs 100
  (let ((actual-timestamp (generate-random-org-timestamp-string)))
    (should (string-match-p org-element--timestamp-regexp actual-timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-inactive-org-timestamp-strings/with-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-timestamps (generate-list-of-n-inactive-org-timestamp-strings test-count t))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps))
	 (actual-element (org-timestamp-from-string actual-random-timestamp)))
    (length= actual-timestamps test-count)
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "[" actual-random-timestamp))
    (should (s-ends-with-p "]" actual-random-timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-inactive-org-timestamp-strings/without-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-timestamps (generate-list-of-n-inactive-org-timestamp-strings test-count))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps))
	 (actual-element (org-timestamp-from-string actual-random-timestamp)))
    (should (length= actual-timestamps test-count))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "[" actual-random-timestamp))
    (should (s-ends-with-p "]" actual-random-timestamp))))

(generate-ert-deftest-n-times generate-random-list-of-inactive-org-timestamp-strings ()
  :num-runs 100
  (let* ((actual-timestamps (generate-random-list-of-inactive-org-timestamp-strings))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps)))
    (should (proper-list-p actual-timestamps))
    (should (string-match-p org-element--timestamp-regexp actual-random-timestamp))
    (should (s-starts-with-p "[" actual-random-timestamp))
    (should (s-ends-with-p "]" actual-random-timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-active-org-timestamp-strings/with-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-timestamps (generate-list-of-n-active-org-timestamp-strings test-count t))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps))
	 (actual-element (org-timestamp-from-string actual-random-timestamp)))
    (should (length= actual-timestamps test-count))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "<" actual-random-timestamp))
    (should (s-ends-with-p ">" actual-random-timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-active-org-timestamp-strings/without-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-timestamps (generate-list-of-n-active-org-timestamp-strings test-count))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps))
	 (actual-element (org-timestamp-from-string actual-random-timestamp)))
    (should (length= actual-timestamps test-count))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))
    (should (s-starts-with-p "<" actual-random-timestamp))
    (should (s-ends-with-p ">" actual-random-timestamp))))

(generate-ert-deftest-n-times generate-random-list-of-active-org-timestamp-strings ()
  :num-runs 100
  (let* ((actual-timestamps (generate-random-list-of-active-org-timestamp-strings))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps)))
    (should (proper-list-p actual-timestamps))
    (should (string-match-p org-element--timestamp-regexp actual-random-timestamp))
    (should (s-starts-with-p "<" actual-random-timestamp))
    (should (s-ends-with-p ">" actual-random-timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-org-timestamp-strings/with-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-timestamps (generate-list-of-n-org-timestamp-strings test-count t))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps))
	 (actual-element (org-timestamp-from-string actual-random-timestamp)))
    (should (length= actual-timestamps test-count))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))
    (should (string-match-p org-element--timestamp-regexp actual-random-timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-org-timestamp-strings/without-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-timestamps (generate-list-of-n-org-timestamp-strings test-count))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-timestamps))
	 (actual-element (org-timestamp-from-string actual-random-timestamp)))
    (should (length= actual-timestamps test-count))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))
    (should (string-match-p org-element--timestamp-regexp actual-random-timestamp))))

(generate-ert-deftest-n-times generate-random-list-of-org-timestamp-strings ()
  :num-runs 100
  (let* ((actual-list-of-timestamps (generate-random-list-of-org-timestamp-strings))
	 (actual-random-timestamp (generate-seq-take-random-value-from-seq actual-list-of-timestamps)))
    (should (string-match-p org-element--timestamp-regexp actual-random-timestamp))
    (should (proper-list-p actual-list-of-timestamps))))

(generate-ert-deftest-n-times generate-inactive-org-timestamp-element-with-start-time ()
  :num-runs 100
  (let* ((actual-element (generate-inactive-org-timestamp-element t)))
    (should (equal (org-element-property :type actual-element) 'inactive))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-inactive-org-timestamp-element-without-start-time ()
  :num-runs 100
  (let* ((actual-element (generate-inactive-org-timestamp-element)))
    (should (equal (org-element-property :type actual-element) 'inactive))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-active-org-timestamp-element-with-start-time ()
  :num-runs 100
  (let* ((actual-element (generate-active-org-timestamp-element t)))
    (should (equal (org-element-property :type actual-element) 'active))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-active-org-timestamp-element-without-start-time ()
  :num-runs 100
  (let* ((actual-element (generate-active-org-timestamp-element nil)))
    (should (equal (org-element-property :type actual-element) 'active))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-random-inactive-org-timestamp-element ()
  :num-runs 100
  (let* ((actual-element (generate-random-inactive-org-timestamp-element)))
    (should (equal (org-element-property :type actual-element) 'inactive))))

(generate-ert-deftest-n-times generate-random-active-org-timestamp-element ()
  :num-runs 100
  (let* ((actual-element (generate-random-active-org-timestamp-element)))
    (should (equal (org-element-property :type actual-element) 'active))))

(generate-ert-deftest-n-times generate-org-timestamp-element/with-time ()
  :num-runs 100
  (let* ((actual-element (generate-org-timestamp-element t)))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-org-timestamp-element/without-time ()
  :num-runs 100
  (let* ((actual-element (generate-org-timestamp-element nil)))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-random-org-timestamp-element ()
  :num-runs 100
  (let* ((actual-element (generate-random-org-timestamp-element)))
    (should (org-element-type-p actual-element 'timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-inactive-org-timestamp-elements/with-start-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-elements (generate-list-of-n-inactive-org-timestamp-elements test-count t))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (length= actual-elements test-count))
    (should (equal (org-element-property :type actual-element) 'inactive))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-list-of-n-inactive-org-timestamp-elements-without-start-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-elements (generate-list-of-n-inactive-org-timestamp-elements test-count nil))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (length= actual-elements test-count))
    (should (equal (org-element-property :type actual-element) 'inactive))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-random-list-of-inactive-org-timestamp-elements ()
  :num-runs 100
  (let* ((actual-elements (generate-random-list-of-inactive-org-timestamp-elements))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (listp actual-elements))
    (should (equal (org-element-property :type actual-element) 'inactive))))

(generate-ert-deftest-n-times generate-list-of-n-active-org-timestamp-elements-with-start-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-elements (generate-list-of-n-active-org-timestamp-elements test-count t))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (length= actual-elements test-count))
    (should (equal (org-element-property :type actual-element) 'active))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-list-of-n-active-org-timestamp-elements/without-start-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-elements (generate-list-of-n-active-org-timestamp-elements test-count))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (length= actual-elements test-count))
    (should (equal (org-element-property :type actual-element) 'active))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-random-list-of-active-org-timestamp-elements ()
  :num-runs 100
  (let* ((actual-elements (generate-random-list-of-active-org-timestamp-elements))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (listp actual-element))
    (should (equal (org-element-property :type actual-element) 'active))))

(generate-ert-deftest-n-times generate-list-of-n-org-timestamp-elements/with-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-elements (generate-list-of-n-org-timestamp-elements test-count t))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (length= actual-elements test-count))
    (should (org-element-property :hour-start actual-element))
    (should (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-list-of-n-org-timestamp-elements/without-time ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-elements (generate-list-of-n-org-timestamp-elements test-count))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (length= actual-elements test-count))
    (should-not (org-element-property :hour-start actual-element))
    (should-not (org-element-property :minute-start actual-element))))

(generate-ert-deftest-n-times generate-random-list-of-org-timestamp-elements ()
  :num-runs 100
  (let* ((actual-elements (generate-random-list-of-org-timestamp-elements))
	 (actual-element (generate-seq-take-random-value-from-seq actual-elements)))
    (should (listp actual-elements))
    (should (org-element-type-p actual-element 'timestamp))))

(generate-ert-deftest-n-times generate-list-of-n-org-state-change-notes/default ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-notes (generate-list-of-n-org-state-change-notes test-count))
	 (actual-random-note (generate-seq-take-random-value-from-seq actual-notes)))
    (should (length= actual-notes test-count))
    (should (s-starts-with-p "-" actual-random-note))
    (should (s-contains-p "State \"DONE\"" actual-random-note))
    (should (s-contains-p "from \"TODO\"" actual-random-note))
    (should (string-match-p org-element--timestamp-regexp actual-random-note))))

(generate-ert-deftest-n-times generate-list-of-n-org-state-change-notes/with-random-states ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (test-from-state (generate-random-word))
	 (test-to-state (generate-random-word))
	 (actual-notes (generate-list-of-n-org-state-change-notes test-count :from test-from-state :to test-to-state))
	 (actual-random-note (generate-seq-take-random-value-from-seq actual-notes)))
    (should (length= actual-notes test-count))
    (should (s-starts-with-p "-" actual-random-note))
    (should (s-contains-p (format "State \"%s\"" test-to-state) actual-random-note))
    (should (s-contains-p (format "from \"%s\"" test-from-state) actual-random-note))
    (should (string-match-p org-element--timestamp-regexp actual-random-note))))

(generate-ert-deftest-n-times generate-random-list-of-org-state-change-notes ()
  :num-runs 100
  (let* ((actual-notes (generate-random-list-of-org-state-change-notes))
	 (actual-random-note (generate-seq-take-random-value-from-seq actual-notes)))
    (should (proper-list-p actual-notes))
    (should (s-starts-with-p "-" actual-random-note))
    (should (s-contains-p "State \"DONE\"" actual-random-note))
    (should (s-contains-p "from \"TODO\"" actual-random-note))
    (should (string-match-p org-element--timestamp-regexp actual-random-note))))

(generate-ert-deftest-n-times generate-block-of-n-org-state-change-notes ()
  :num-runs 100
  (let* ((test-count (generate--random-nat-number-in-range-10))
	 (actual-block (generate-block-of-n-org-state-change-notes test-count))
	 (actual-notes (s-split "\n" actual-block))
	 (actual-random-note (generate-seq-take-random-value-from-seq actual-notes)))
    (should (length= actual-notes test-count))
    (should (s-starts-with-p "-" actual-random-note))
    (should (s-contains-p "State \"DONE\"" actual-random-note))
    (should (s-contains-p "from \"TODO\"" actual-random-note))
    (should (string-match-p org-element--timestamp-regexp actual-random-note))))

(generate-ert-deftest-n-times generate-random-block-of-org-state-change-notes ()
  :num-runs 100
  (let* ((actual-block (generate-random-block-of-org-state-change-notes))
	 (actual-notes (s-split "\n" actual-block))
	 (actual-random-note (generate-seq-take-random-value-from-seq actual-notes)))
    (should (s-starts-with-p "-" actual-random-note))
    (should (s-contains-p "State \"DONE\"" actual-random-note))
    (should (s-contains-p "from \"TODO\"" actual-random-note))
    (should (string-match-p org-element--timestamp-regexp actual-random-note))))

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
