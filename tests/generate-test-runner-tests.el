;;; generate-test-runner-tests.el --- Tests for the generate test-runner  -*- lexical-binding: t; -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0.0
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

;; This file contains the code used to generate data for generate's test-runner
;; along with the tests themselves. The functions are stored here as generate.el
;; is already long enough as is.

;;; Code:

(require 'ert)
(require 'eieio)
(require 'cl-macs)
(require 'seq)
(require 'generate)
(require 's)

(defconst generate--OUTCOMES-FOR-STATS-TESTS
  (list (list :passed-expected #'generate--stats-passed-expected)
	(list :failed-expected #'generate--stats-failed-expected)
	(list :skipped #'generate--stats-skipped)
	(list :passed-unexpected #'generate--stats-passed-unexpected)
	(list :failed-unexpected #'generate--stats-failed-unexpected)))

(defconst generate--OUTCOME-PREDS-MAPPING
  (list (list :passed-expected #'ert-test-passed-p)
	(list :failed-expected #'ert-test-failed-p)
	(list :skipped #'ert-test-skipped-p)
	(list :passed-unexpected #'ert-test-passed-p)
	(list :failed-unexpected #'ert-test-failed-p)))

(defconst generate--TEST-SUMMARY-STRINGS
  (list "Passed as expected" "Failed as expected" "Skipped" "Failed unexpectedly" "Passed unexpectedly"))


(defalias 'generate--TEST-SUMMARY-STRING-PREDICATES  (apply #'-orfn (mapcar (lambda (str) (apply-partially #'s-starts-with-p str)) generate--TEST-SUMMARY-STRINGS)))

(defun generate--generate-test-base (test-identifier)
  (cl-function (lambda (test-name next-test-index-for-group &key documentation tags file-name expected-result-type)
		 (generate-ert-test (format "%s-%s-%s" test-name test-identifier next-test-index-for-group) :documentation documentation :tags tags :file-name file-name :expected-result-type expected-result-type))))

(defalias 'generate--generate-test (generate--generate-test-base generate--TEST-IDENTIFIER))

(cl-defun generate--test-name-unfolder-base (test-identifier (count . name))
  (generate--times count (lambda (index) (format "%s-%s-%s" name test-identifier index))))

(defalias 'generate--test-name-unfolder (apply-partially #'generate--test-name-unfolder-base generate--TEST-IDENTIFIER))

(cl-defun generate--generate-test-unfolder (test-group-name total-tests &optional expected-result-type)
  (generate--times total-tests (lambda (index) (generate--generate-test test-group-name index :expected-result-type expected-result-type))))

(defun generate--create-ert-tests-for-fake-test-group (test-group-name test-stats)
  (-let* (((&plist :total-tests :expected-result-type) test-stats)
	  (tests (generate--generate-test-unfolder test-group-name total-tests expected-result-type)))
    tests))

(defalias 'generate--create-list-of-tests-for-fake-tests-groups-alist (-compose (apply-partially #'-flatten-n 1) (apply-partially #'map-apply #'generate--create-ert-tests-for-fake-test-group)))

(defalias 'generate--create-vector-of-tests-for-fake-tests-groups-alist (-compose #'generate--applify-vector #'generate--create-list-of-tests-for-fake-tests-groups-alist))

(defun generate--fake-fresh-test-group-con-base (stats)
  (lambda (counts name index)
    (cons name (generate--plist-put :total-tests (nth index counts) stats))))
(defalias 'generate--fake-fresh-test-group-con (generate--fake-fresh-test-group-con-base generate--TEST-GROUPS-PLIST))

(defun generate--fake-fresh-tests-groups-alist-base (all-outcomes)
  (lambda ()
    (let* ((group-names (generate-random-list-of-unique-strings))
	   (total-relative-tests-count (length group-names))
	   (test-counts (generate--list-of-n-nat-numbers-in-range-5 :exact-length total-relative-tests-count))
	   (total-absolute-tests-count (-sum test-counts))
	   (tests-groups-alist (seq-map-indexed (apply-partially #'generate--fake-fresh-test-group-con test-counts) group-names)))
      (list tests-groups-alist total-relative-tests-count total-absolute-tests-count group-names))))

(defalias 'generate--fake-fresh-tests-groups-alist (generate--fake-fresh-tests-groups-alist-base generate--DEFAULT-OUTCOMES-PLIST))

(defun generate--create-fresh-ert-stats-for-tests-groups-alist (tests-groups-alist)
  (let ((tests (generate--create-vector-of-tests-for-fake-tests-groups-alist tests-groups-alist)))
    (ert--make-stats tests t)))

(defun generate--fake-fresh-tests-groups-alist-and-stats ()
  (-let* (((tests-groups-alist total-relative-tests-count total-absolute-tests-count group-names) (generate--fake-fresh-tests-groups-alist))
	  (stats (generate--create-fresh-ert-stats-for-tests-groups-alist tests-groups-alist)))
    (list tests-groups-alist stats total-relative-tests-count total-absolute-tests-count group-names)))

(defun generate--fake-mid-run-tests-groups-con-for-x-type (outcome finishedp)
  (-let* ((((ert-test-name . stats) _ completed-tests-count-for-group absolute-outcomes-counts-plist) (generate--random-fake-completed-test-group-con-for-outcome-x outcome))
	  (tests-to-add (if finishedp 1 (1+ (generate--random-nat-number-in-range-25))))
	  (next-test-index-for-group completed-tests-count-for-group)
	  (total-tests-count-for-group (+ completed-tests-count-for-group tests-to-add))
	  (new-stats (generate--plist-put :total-tests total-tests-count-for-group stats))
	  (test-group-con (cons ert-test-name new-stats)))
    (list test-group-con
	  ert-test-name
	  next-test-index-for-group
	  completed-tests-count-for-group
	  total-tests-count-for-group
	  absolute-outcomes-counts-plist)))

(defun generate--fake-mid-run-tests-groups-alist-for-x-type-base (all-outcomes)
  (lambda (outcome finishedp)
    (-let* (((fresh-tests-groups-alist _ fresh-total-absolute-tests-count) (generate--fake-fresh-tests-groups-alist))
	    ((currently-executing-test-group ert-test-name next-test-index-for-group completed-tests-count-for-group
					     total-tests-count-for-group absolute-outcomes-counts-plist)
	     (generate--fake-mid-run-tests-groups-con-for-x-type outcome finishedp))
	    (absolute-completed-tests-count completed-tests-count-for-group)
	    (mid-run-tests-groups-alist (list currently-executing-test-group))
	    (absolute-total-tests-count (+ fresh-total-absolute-tests-count total-tests-count-for-group))
	    (full-tests-groups-alist (map-merge 'alist mid-run-tests-groups-alist fresh-tests-groups-alist)))
      (list
       full-tests-groups-alist
       fresh-tests-groups-alist
       currently-executing-test-group
       ert-test-name
       next-test-index-for-group
       completed-tests-count-for-group
       total-tests-count-for-group
       absolute-total-tests-count
       absolute-outcomes-counts-plist))))

(defalias 'generate--fake-mid-run-tests-groups-alist-for-x-type (generate--fake-mid-run-tests-groups-alist-for-x-type-base 'generate--DEFAULT-OUTCOMES-PLIST))

(defun generate--fake-mid-run-ert-stats-for-tests-groups-alist-base (test-identifier)
  (lambda (full-tests-groups-alist
	   currently-executing-test-group
	   next-test-index-for-group
	   test-outcome
	   completed-tests-count-for-group
	   absolute-total-tests-count)
    (-let* (((currently-executing-test-group-name . (&plist :test-results :test-start-times :test-end-times)) currently-executing-test-group)
	    (all-tests (generate--create-list-of-tests-for-fake-tests-groups-alist full-tests-groups-alist))
	    (test-map (map-into (seq-map-indexed (lambda (test index) (cons (ert-test-name test) index)) all-tests) 'hash-table))
	    (next-ert-test (nth (map-elt test-map (intern (format "%s-%s-%s" currently-executing-test-group-name test-identifier next-test-index-for-group))) all-tests))
	    (random-duration (generate-random-float))
	    (next-ert-test-result (generate-ert-test-result-object test-outcome random-duration))
	    ((completed-results completed-start-times completed-end-times) (mapcar #'generate--applify-vector (list test-results test-start-times test-end-times)))
	    (total-nils (- absolute-total-tests-count completed-tests-count-for-group))
	    ((nil-results nil-start-times nil-end-times) (generate--times-no-args 3 (lambda () (make-vector total-nils nil))))
	    (stats
	     (make-ert--stats :selector 't
			      :start-time (generate--get-min-lisp-timestamp test-start-times)
			      :end-time (generate--get-max-lisp-timestamp test-end-times)
			      :tests (generate--applify-vector all-tests)
			      :test-map test-map
			      :test-results (vconcat completed-results nil-results)
			      :test-start-times (vconcat completed-start-times nil-start-times)
			      :test-end-times (vconcat completed-end-times nil-end-times))))
      (list stats next-ert-test next-ert-test-result))))

(defalias 'generate--fake-mid-run-ert-stats-for-tests-groups-alist (generate--fake-mid-run-ert-stats-for-tests-groups-alist-base generate--TEST-IDENTIFIER))

(defun generate--fake-mid-run-data-for-x-type (finishedp)
  (lambda (outcome)
    (-let* (((full-tests-groups-alist
	      fresh-tests-groups-alist
	      currently-executing-test-group
	      currently-executing-test-group-name
	      next-test-index-for-group
	      completed-tests-count-for-group
	      total-tests-count-for-group
	      absolute-total-tests-count
	      absolute-outcomes-counts-plist)
	     (generate--fake-mid-run-tests-groups-alist-for-x-type outcome finishedp))
	    ((ert-test-stats next-ert-test next-ert-test-result) (generate--fake-mid-run-ert-stats-for-tests-groups-alist
								  full-tests-groups-alist
								  currently-executing-test-group
								  next-test-index-for-group
								  outcome
								  completed-tests-count-for-group
								  absolute-total-tests-count)))
      (list full-tests-groups-alist ert-test-stats next-ert-test next-ert-test-result currently-executing-test-group-name next-test-index-for-group completed-tests-count-for-group total-tests-count-for-group absolute-outcomes-counts-plist))))

(defalias 'generate--fake-mid-run-data-for-group-with-one-more-test-left (generate--fake-mid-run-data-for-x-type t))

(defalias 'generate--fake-mid-run-data-for-group-with-more-than-one-test-left (generate--fake-mid-run-data-for-x-type nil))

(defun generate--fake-completed-test-group-con-for-outcome-x (base-outcomes-counts-plist)
  (-lambda ((group-name requested-outcome expected-result-type
			count-for-requested-outcome result-for-requested-outcome
			compatible-outcome count-for-compatible-outcome result-for-compatible-outcome
			test-start-time test-end-time test-duration))
    (-let* ((to-merge-absolute (list (copy-sequence base-outcomes-counts-plist) (list requested-outcome count-for-requested-outcome) (list compatible-outcome count-for-compatible-outcome)))
	    (absolute-outcomes-counts-plist (generate--map-merge-with-plus-plist to-merge-absolute))
	    (absolute-total-tests (+ count-for-requested-outcome count-for-compatible-outcome))
	    (duration (* absolute-total-tests test-duration))
	    (requested-results (make-list count-for-requested-outcome result-for-requested-outcome))
	    (test-results (if count-for-compatible-outcome
			      (generate-append-and-shuffle requested-results (make-list count-for-compatible-outcome result-for-compatible-outcome))
			    requested-results))
	    (total-duration-reason-result (list
					   :expected-result-type expected-result-type
					   :total-tests absolute-total-tests
					   :completed-tests absolute-total-tests
					   :duration duration
					   :test-start-times (make-list absolute-total-tests test-start-time)
					   :test-end-times (make-list absolute-total-tests test-end-time)
					   :test-results test-results))
	    (all-stats (append absolute-outcomes-counts-plist total-duration-reason-result)))
      (list (cons group-name all-stats) absolute-total-tests absolute-outcomes-counts-plist))))

(cl-defun generate--create-data-for-fake-completed-test-group (all-outcomes counts-for-requested-outcome counts-for-compatible-outcome (test-start-times test-end-times test-durations))
  (-lambda ((group-name . requested-outcome) index)
    (-let* ((requested-outcome-attributes (generate--plist-get requested-outcome all-outcomes))
	    ((requested-outcome-exclusivep compatible-outcome expected-result-type) (funcall (-juxt (apply-partially #'generate--plist-get :exclusive)
												    (apply-partially #'generate--plist-get :compatible)
												    (apply-partially #'generate--plist-get :expected-result-type))
											     requested-outcome-attributes))
	    ((count-for-requested-outcome test-start-time test-end-time test-duration) (mapcar (apply-partially #'generate-nth-mod index)
											       (list counts-for-requested-outcome test-start-times test-end-times test-durations)))
	    (result-for-requested-outcome (generate-ert-test-result-object requested-outcome test-duration))
	    ((count-for-compatible-outcome result-for-compatible-outcome) (if requested-outcome-exclusivep
									      (list 0 nil)
									    (list (generate-nth-mod index counts-for-compatible-outcome) (generate-ert-test-result-object compatible-outcome test-duration)))))
      (list group-name requested-outcome expected-result-type count-for-requested-outcome result-for-requested-outcome
	    compatible-outcome count-for-compatible-outcome result-for-compatible-outcome
	    test-start-time test-end-time test-duration))))

(defun generate--random-fake-completed-test-group-con-for-outcome-x-base (all-outcomes requested-outcome)
  (-let* ((base-outcomes-counts-plist (-interleave (map-keys all-outcomes) (make-list (length all-outcomes) 0)))
	  (other-outcomes (funcall (-compose (apply-partially #'-remove (apply-partially #'equal requested-outcome)) #'map-keys) all-outcomes))
	  (group-name (generate-random-string))
	  (counts-for-requested-outcome (list (generate--random-nat-number-in-range-25)))
	  (counts-for-compatible-outcome (list (generate--random-nat-number-in-range-25)))
	  (time-data (generate--list-of-n-unzipped-starts-ends-durations 1))
	  (fake-data-generator (generate--create-data-for-fake-completed-test-group
				all-outcomes
				counts-for-requested-outcome
				counts-for-compatible-outcome
				time-data))
	  (fake-data (funcall fake-data-generator (cons group-name requested-outcome) 0))
	  (con-generator (generate--fake-completed-test-group-con-for-outcome-x base-outcomes-counts-plist))
	  ((test-group-con total-test-count outcomes-count-plist) (funcall con-generator fake-data)))
    (list test-group-con group-name total-test-count outcomes-count-plist)))

(defalias 'generate--random-fake-completed-test-group-con-for-outcome-x (apply-partially #'generate--random-fake-completed-test-group-con-for-outcome-x-base generate--DEFAULT-OUTCOMES-PLIST))

(defun generate--fake-completed-tests-groups-alist-base (all-outcomes requested-outcome)
  (-let* ((base-outcomes-counts-plist (-interleave (map-keys all-outcomes) (make-list (length all-outcomes) 0)))
	  (other-outcomes (funcall (-compose (apply-partially #'-remove (apply-partially #'equal requested-outcome)) #'map-keys) all-outcomes))
	  (expected-group-names (generate-random-list-of-unique-strings))
	  (other-group-names (seq-map-indexed (lambda (name index) (concat (generate-seq-shuffle name) (number-to-string index))) expected-group-names))
	  (requested-groups (generate--zip-pair-longest expected-group-names (list requested-outcome)))
	  (other-groups (generate--zip-pair-first other-group-names other-outcomes))
	  (all-groups (generate-append-and-shuffle requested-groups other-groups))
	  (total-expected-groups (length expected-group-names))
	  (total-other-groups (length other-group-names))
	  (total-groups (+ total-expected-groups total-other-groups))
	  (counts-for-requested-outcome (generate--list-of-n-nat-numbers-in-range-10 :exact-length total-groups))
	  (counts-for-compatible-outcome (generate--list-of-n-nat-numbers-in-range-10 :exact-length total-groups))
	  (time-data (generate--list-of-n-unzipped-starts-ends-durations total-groups))
	  (fake-data-generator (generate--create-data-for-fake-completed-test-group
				all-outcomes
				counts-for-requested-outcome
				counts-for-compatible-outcome
				time-data))
	  (fake-data (seq-map-indexed fake-data-generator all-groups))
	  (alist-generator (generate--fake-completed-test-group-con-for-outcome-x base-outcomes-counts-plist))
	  ((tests-groups-alist list-of-total-tests-count list-of-absolute-outcomes-count-plists) (funcall (-compose #'-unzip-lists #'mapcar) alist-generator fake-data))
	  (absolute-outcomes-counts-plist (generate--map-merge-with-plus-plist list-of-absolute-outcomes-count-plists))
	  (list-of-relative-outcomes-counts-plist (append (list (list requested-outcome total-expected-groups)) (mapcar (-lambda ((_ . outcome)) (list outcome 1)) other-groups)))
	  (relative-outcomes-counts-plist (generate--map-merge-with-plus-plist list-of-relative-outcomes-counts-plist)))
    (list tests-groups-alist expected-group-names other-group-names (-sum list-of-total-tests-count) absolute-outcomes-counts-plist relative-outcomes-counts-plist)))

(defalias 'generate--fake-completed-tests-groups-alist (apply-partially #'generate--fake-completed-tests-groups-alist-base generate--DEFAULT-OUTCOMES-PLIST))

(defalias 'generate--random-fake-completed-tests-groups-alist (-compose #'generate--fake-completed-tests-groups-alist #'generate--random-ert-test-outcome))

(defun generate--create-completed-ert-stats-for-tests-groups-alist-mapper (tests-groups-alist)
  (lambda (test index)
    (-let* (((name . number) (generate--get-group-name-and-index-for-test test))
	    (test-group-stats (map-elt tests-groups-alist name))
	    (test-result (seq-elt (generate--plist-get :test-results test-group-stats) number))
	    (test-start-time (seq-elt (generate--plist-get :test-start-times test-group-stats) number))
	    (test-end-time (seq-elt (generate--plist-get :test-end-times test-group-stats) number)))
      (list (cons name index) test-result test-start-time test-end-time))))

(defun generate--create-completed-ert-stats-for-tests-groups-alist (total-tests tests-groups-alist)
  (-let* ((tests (generate--create-vector-of-tests-for-fake-tests-groups-alist tests-groups-alist))
	  ((test-map test-results test-start-times test-end-times) (funcall (-compose
									     (apply-partially #'-flatten-n 1)
									     (-juxt (-compose (-rpartial #'map-into 'hash-table) #'car) (-compose (apply-partially #'mapcar #'generate--applify-vector) #'cdr))
									     #'-unzip-lists
									     (apply-partially #'seq-map-indexed (generate--create-completed-ert-stats-for-tests-groups-alist-mapper tests-groups-alist)))
									    tests)))
    (make-ert--stats :selector 't
		     :start-time (generate--get-min-lisp-timestamp test-start-times)
		     :end-time (generate--get-max-lisp-timestamp test-end-times)
                     :tests tests
                     :test-map test-map
                     :test-results test-results
                     :test-start-times test-start-times
                     :test-end-times test-end-times)))

(defun generate--fake-completed-tests-groups-alist-and-stats (requested-outcome)
  (-let* (((tests-groups-alist group-names-for-requested-outcome other-group-names absolute-total-tests-count absolute-outcomes-counts-plist relative-outcomes-counts-plist) (generate--fake-completed-tests-groups-alist requested-outcome))
	  (stats (generate--create-completed-ert-stats-for-tests-groups-alist absolute-total-tests-count tests-groups-alist)))
    (list tests-groups-alist stats group-names-for-requested-outcome other-group-names absolute-total-tests-count absolute-outcomes-counts-plist relative-outcomes-counts-plist)))

(defalias 'generate--random-fake-completed-tests-groups-alist-and-stats (-compose #'generate--fake-completed-tests-groups-alist-and-stats #'generate--random-ert-test-outcome))

;; Above functions will be used in the following tests:

(generate-ert-deftest-n-times generate--parse-keys-and-body-body-only ()
  :num-runs 100
  (let* ((test-body (generate-random-should))
	 (test-docstring-keys-and-body (list test-body)))
    (cl-destructuring-bind
	(&key
	 (documentation nil documentation-supplied-p)
	 (expected-result nil expected-result-supplied-p)
	 (tags nil tags-supplied-p)
	 (num-runs 100)
	 (body nil))
	(generate--parse-keys-and-body test-docstring-keys-and-body)
      (should-not documentation)
      (should-not expected-result)
      (should-not tags)
      (should (equal num-runs 100))
      (should (equal body test-body)))))

(generate-ert-deftest-n-times generate--parse-keys-and-body-documentation-and-body ()
  :num-runs 100
  (let* ((test-body (generate-random-should))
	 (test-documentation (generate-random-string))
	 (test-docstring-keys-and-body (list test-documentation test-body)))
    (cl-destructuring-bind
	(&key
	 (documentation nil documentation-supplied-p)
	 (expected-result nil expected-result-supplied-p)
	 (tags nil tags-supplied-p)
	 (num-runs 100)
	 (body nil))
	(generate--parse-keys-and-body test-docstring-keys-and-body)
      (should (equal documentation test-documentation))
      (should-not expected-result)
      (should-not tags)
      (should (equal num-runs 100))
      (should (equal body test-body)))))

(generate-ert-deftest-n-times generate--parse-keys-and-body-num-runs-and-body ()
  :num-runs 100
  (let* ((test-body (generate-random-should))
	 (test-num-runs (generate-random-nat-number))
	 (test-docstring-keys-and-body (list :num-runs test-num-runs test-body)))
    (cl-destructuring-bind
	(&key
	 (documentation nil documentation-supplied-p)
	 (expected-result nil expected-result-supplied-p)
	 (tags nil tags-supplied-p)
	 (num-runs 100)
	 (body nil))
	(generate--parse-keys-and-body test-docstring-keys-and-body)
      (should-not documentation)
      (should-not expected-result)
      (should-not tags)
      (should (equal num-runs test-num-runs))
      (should (equal body test-body)))))

(generate-ert-deftest-n-times generate--parse-keys-and-body-keywords-mutiple-keyword-args-plus-body ()
  :num-runs 100
  (let* ((test-body (generate-random-should))
	 (test-num-runs (generate-random-nat-number))
	 (test-expected-result-type (generate--random-ert-expected-result-type))
	 (test-tags (generate-random-list-of-symbols))
	 (test-docstring-keys-and-body (list :num-runs test-num-runs :expected-result test-expected-result-type :tags test-tags test-body)))
    (cl-destructuring-bind
	(&key
	 (documentation nil documentation-supplied-p)
	 (expected-result nil expected-result-supplied-p)
	 (tags nil tags-supplied-p)
	 (num-runs 100)
	 (body nil))
	(generate--parse-keys-and-body test-docstring-keys-and-body)
      (should-not documentation)
      (should (equal expected-result test-expected-result-type))
      (should (equal tags test-tags))
      (should (equal num-runs test-num-runs))
      (should (equal body test-body)))))

(generate-ert-deftest-n-times generate--parse-keys-and-body-kitchen-sink ()
  :num-runs 100
  (let* ((test-body (generate-random-should))
	 (test-num-runs (generate-random-nat-number))
	 (test-expected-result-type (generate--random-ert-expected-result-type))
	 (test-tags (generate-random-list-of-symbols))
	 (test-documentation (generate-random-string))
	 (test-docstring-keys-and-body (list test-documentation :num-runs test-num-runs :expected-result test-expected-result-type :tags test-tags test-body)))
    (cl-destructuring-bind
	(&key
	 (documentation nil documentation-supplied-p)
	 (expected-result nil expected-result-supplied-p)
	 (tags nil tags-supplied-p)
	 (num-runs 100)
	 (body nil))
	(generate--parse-keys-and-body test-docstring-keys-and-body)
      (should (equal documentation test-documentation))
      (should (equal expected-result test-expected-result-type))
      (should (equal tags test-tags))
      (should (equal num-runs test-num-runs))
      (should (equal body test-body)))))

(cl-defun generate--helper-for-self-tests-with-ert-times-func-creator (test-name tags documentation &optional expected-result)
  (if expected-result
      (lambda (n)
	(let* ((expected-symbol (intern (format "%s-%s-%s" test-name generate--TEST-IDENTIFIER n)))
	       (actual-test (ert-get-test expected-symbol)))
	  (should (equal (ert-test-tags actual-test) tags))
	  (should (equal (ert-test-documentation actual-test) documentation))
	  (should (equal (ert-test-expected-result-type actual-test) expected-result))))
    (lambda (n)
      (let* ((expected-symbol (intern (format "%s-%s-%s" test-name generate--TEST-IDENTIFIER n)))
	     (actual-test (ert-get-test expected-symbol)))
	(should (equal (ert-test-tags actual-test) tags))
	(should (equal (ert-test-documentation actual-test) documentation))))))


(cl-defun generate--helper-for-self-tests-with-ert (test-name runs tags documentation &optional expected-result)
  (generate--times runs (generate--helper-for-self-tests-with-ert-times-func-creator test-name tags documentation expected-result)))

(ert-deftest generate-ert-deftest-n-times-simple ()
  (generate-ert-deftest-n-times generate-test-abc ()
    "foo"
    :tags '(bar)
    (should (equal 1 1)))
  (generate--helper-for-self-tests-with-ert "generate-test-abc" 100 '(bar) "foo"))

(ert-deftest generate-ert-deftest-n-times-with-numruns ()
  (generate-ert-deftest-n-times generate-test-123 ()
    "foo"
    :tags '(bar)
    :num-runs 20
    (should (equal 1 1)))
  (generate--helper-for-self-tests-with-ert "generate-test-123" 20 '(bar) "foo")
  (generate--times 80 (lambda (n)
			(let* ((expected-symbol (intern (format "%s-%s-%s" "generate-test-123" generate--TEST-IDENTIFIER (+ n 20)))))
			  (should-not (ert-test-boundp expected-symbol))))))

(ert-deftest generate-ert-deftest-n-times-with-numruns-and-expected-result ()
  (generate-ert-deftest-n-times generate-test-456 ()
    "foo"
    :tags '(bar)
    :num-runs 30
    :expected-result ':failed
    (should (equal 1 1)))
  (generate--helper-for-self-tests-with-ert "generate-test-456" 30 '(bar) "foo" ':failed)
  (generate--times 70 (lambda (n)
			(let* ((expected-symbol (intern (format "%s-%s-%s" "generate-test-456" generate--TEST-IDENTIFIER (+ n 30)))))
			  (should-not (ert-test-boundp expected-symbol))))))

(generate-ert-deftest-n-times generate--generate-test-simple ()
  :num-runs 100
  (let* ((test-group-name (generate-random-word))
	 (test-number (generate-random-nat-number))
	 (test-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFIER test-number))
	 (actual-test (generate--generate-test test-group-name test-number)))
    (should (ert-test-p actual-test))
    (should (s-contains-p generate--TEST-IDENTIFIER (symbol-name (ert-test-name actual-test))))))

(generate-ert-deftest-n-times generate--generate-test-with-expected-result-type ()
  :num-runs 100
  (let* ((test-group-name (generate-random-word))
	 (test-number (generate-random-nat-number))
	 (expected-result-type (generate--random-ert-expected-result-type))
	 (actual-test (generate--generate-test test-group-name test-number :expected-result-type expected-result-type)))
    (should (ert-test-p actual-test))
    (should (equal (ert-test-expected-result-type actual-test) expected-result-type))
    (should (s-contains-p generate--TEST-IDENTIFIER (symbol-name (ert-test-name actual-test))))))

(generate-ert-deftest-n-times generate--test-name-unfolder ()
  :num-runs 100
  (let* ((test-group-name (generate-random-word))
	 (test-count (generate--random-nat-number-in-range-10))
	 (actual-names (generate--test-name-unfolder (cons test-count test-group-name)))
	 (expected-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFIER (generate--random-nat-number-between-0-and test-count))))
    (should (member expected-name actual-names))))

(generate-ert-deftest-n-times generate--generate-test-unfolder ()
  :num-runs 100
  (let* ((test-group-name (generate-random-word))
	 (test-count (generate--random-nat-number-in-range-10))
	 (expected-result-type (generate--random-ert-expected-result-type))
	 (actual-tests (generate--generate-test-unfolder test-group-name test-count expected-result-type))
	 (actual-random-test (generate-seq-take-random-value-from-seq actual-tests))
	 (expected-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFIER (generate--random-nat-number-between-0-and test-count))))
    (should (length= actual-tests test-count))
    (should (equal (ert-test-expected-result-type actual-random-test) expected-result-type))
    (should (-any (lambda (actual-test) (equal (symbol-name (ert-test-name actual-test)) expected-name)) actual-tests))))

(generate-ert-deftest-n-times generate--create-ert-tests-for-fake-test-group-basic ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  (outcome-expected-result-type (generate--plist-get :expected-result-type (generate--plist-get test-outcome generate--DEFAULT-OUTCOMES-PLIST)))
	  (((test-group-name . test-stats)) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	  ((&plist :total-tests) test-stats)
	  (actual-tests (generate--create-ert-tests-for-fake-test-group test-group-name test-stats))
	  (actual-random-test (generate-seq-take-random-value-from-seq actual-tests)))
    (should (length= actual-tests total-tests))
    (should (equal (ert-test-expected-result-type actual-random-test) outcome-expected-result-type))
    (should (s-starts-with-p test-group-name (symbol-name (ert-test-name actual-random-test))))))

(generate-ert-deftest-n-times generate--create-list-of-tests-for-fake-tests-groups-alist ()
  :num-runs 100
  (-let* (((tests-groups-alist _ expected-total-absolute-tests-count test-group-names) (generate--fake-fresh-tests-groups-alist))
	  (expected-random-group-name (generate-seq-take-random-value-from-seq test-group-names))
	  (expected-random-group-name-test-count (generate--plist-get :total-tests (map-elt tests-groups-alist expected-random-group-name)))
	  (expected-random-index (generate--random-nat-number-between-0-and expected-random-group-name-test-count))
	  (actual-tests (generate--create-list-of-tests-for-fake-tests-groups-alist tests-groups-alist)))
    (should (length= actual-tests expected-total-absolute-tests-count))
    (should (-first (lambda (actual-test) (let ((actual-name (symbol-name (ert-test-name actual-test)))) (and (s-starts-with-p expected-random-group-name actual-name) (s-ends-with-p (number-to-string expected-random-index) actual-name)))) actual-tests))))

(generate-ert-deftest-n-times generate--fake-fresh-tests-groups-alist ()
  :num-runs 100
  (-let* (((actual-tests-groups-alist actual-total-relative-test-count actual-total-absolute-test-count actual-group-names) (generate--fake-fresh-tests-groups-alist))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist))
	  ((&plist
	    :total-tests
	    :passed-expected
	    :passed-unexpected
	    :failed-unexpected
	    :skipped
	    :failed-expected
	    :failed-unexpected
	    :duration
	    :test-results
	    :test-start-times
	    :test-end-times)
	   actual-group-stats))
    (should (seq-every-p #'zerop (list
				  passed-expected
				  passed-unexpected
				  failed-unexpected
				  skipped
				  duration)))
    (should (stringp actual-group-name))
    (should (g--gt0 total-tests))
    (should (length= test-results 0))
    (should (length= test-start-times 0))
    (should (length= test-end-times 0))
    (should (g--len-gt0 actual-group-names))
    (should (g--gt0 actual-total-relative-test-count))
    (should (g--gt0 actual-total-absolute-test-count))))

(generate-ert-deftest-n-times generate--create-fresh-ert-stats-for-tests-groups-alist ()
  :num-runs 100
  (-let* (((tests-groups-alist expected-total-test-count expected-relative-test-count expected-group-names) (generate--fake-fresh-tests-groups-alist))
	  (actual-ert-stats (generate--create-fresh-ert-stats-for-tests-groups-alist tests-groups-alist))
	  ((actual-random-ert-test
	    actual-passed-expected
	    actual-failed-expected
	    actual-passed-unexpected
	    actual-failed-unexpected
	    actual-skipped)
	   (funcall (-compose
		     (-juxt
		      (-compose #'generate-seq-take-random-value-from-seq #'ert--stats-tests)
		      #'ert--stats-passed-expected
		      #'ert--stats-passed-unexpected
		      #'ert--stats-failed-expected
		      #'ert--stats-failed-unexpected
		      #'ert--stats-skipped))
		    actual-ert-stats)))
    (should (ert--stats-p actual-ert-stats))
    (should (seq-every-p #'zerop (list
				  actual-passed-expected
				  actual-failed-expected
				  actual-passed-unexpected
				  actual-failed-unexpected
				  actual-skipped)))
    (should (ert-test-p actual-random-ert-test))))

(generate-ert-deftest-n-times generate--fake-fresh-tests-groups-alist-and-stats ()
  :num-runs 100
  (-let* (((actual-tests-groups-alist actual-ert-stats actual-total-relative-tests-count actual-total-absolute-tests-count actual-group-names) (generate--fake-fresh-tests-groups-alist-and-stats))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert--stats-p actual-ert-stats))
    (should (not (zerop actual-total-relative-tests-count)))
    (should (not (zerop actual-total-absolute-tests-count)))
    (should (g--len-gt0 actual-group-names))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-con-for-x-type-more-than-one-test-left ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  (((_ . actual-group-stats) actual-ert-test-name actual-next-test-index-for-group actual-completed-tests-count-for-group
	    actual-total-tests-count-for-group actual-absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-con-for-x-type test-outcome nil))
	  ((&plist
	    :total-tests
	    :completed-tests
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (< (1+ completed-tests) total-tests))
    (should (stringp actual-ert-test-name))
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-con-for-x-type-one-more-test-left ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  (((_ . actual-group-stats) actual-ert-test-name actual-next-test-index-for-group actual-completed-tests-count-for-group
	    actual-total-tests-count-for-group actual-absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-con-for-x-type test-outcome t))
	  ((&plist
	    :total-tests
	    :completed-tests
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (equal (1+ completed-tests) total-tests))
    (should (stringp actual-ert-test-name))
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-alist-for-x-type-more-than-one-test-left ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  ((actual-tests-groups-alist
	    actual-fresh-tests-groups-alist
	    actual-currently-executing-test-group
	    actual-ert-test-name
	    actual-next-test-index-for-group
	    actual-completed-tests-count-for-group
	    actual-total-tests-count-for-group
	    actual-absolute-total-tests-count
	    actual-absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome nil))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-ert-test-name))
	  ((&plist
	    :total-tests
	    :completed-tests
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (< (1+ completed-tests) total-tests))
    (should (stringp actual-ert-test-name))
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group actual-absolute-total-tests-count))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))
    (should (consp actual-currently-executing-test-group))
    (should (consp actual-fresh-tests-groups-alist))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-alist-for-x-type-one-more-test-left ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  ((actual-tests-groups-alist
	    actual-fresh-tests-groups-alist
	    actual-currently-executing-test-group
	    actual-ert-test-name
	    actual-next-test-index-for-group
	    actual-completed-tests-count-for-group
	    actual-total-tests-count-for-group
	    actual-absolute-total-tests-count
	    actual-absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome t))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-ert-test-name))
	  ((&plist
	    :total-tests
	    :completed-tests
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (equal (1+ completed-tests) total-tests))
    (should (stringp actual-ert-test-name))
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group actual-absolute-total-tests-count))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))
    (should (consp actual-currently-executing-test-group))
    (should (consp actual-fresh-tests-groups-alist))))

(generate-ert-deftest-n-times generate--fake-mid-run-ert-stats-for-tests-groups-alist-with-more-than-one-test-left ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  ((full-tests-groups-alist
	    fresh-tests-groups-alist
	    test-currently-executing-test-group
	    expected-test-group-name
	    test-next-test-index-for-group
	    expected-completed-tests-count-for-group
	    expected-total-tests-count-for-group
	    expected-absolute-total-tests-count
	    absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome nil))
	  ((actual-ert-stats actual-ert-test) (generate--fake-mid-run-ert-stats-for-tests-groups-alist
					       full-tests-groups-alist
					       test-currently-executing-test-group
					       test-next-test-index-for-group
					       test-outcome
					       expected-completed-tests-count-for-group
					       expected-absolute-total-tests-count)))
    (with-slots ((actual-selector selector)
		 (actual-tests tests)
		 (actual-test-map test-map)
		 (actual-test-results test-results)
		 (actual-test-start-times test-start-times)
		 (actual-test-end-times test-end-times)
		 (actual-start-time start-time)
		 (actual-end-time end-time))
	actual-ert-stats
      ;; selector
      (should (equal actual-selector t))
      ;; tests
      (should (vectorp actual-tests))
      (should (length= actual-tests expected-absolute-total-tests-count))
      ;; test-map
      (should (hash-table-p actual-test-map))
      (should (length= (map-keys actual-test-map) expected-absolute-total-tests-count))
      ;; test-results
      (should (vectorp actual-test-results))
      (should (length= actual-test-results expected-absolute-total-tests-count))
      (should (< (1+ (seq-count #'ert-test-result-p actual-test-results)) expected-total-tests-count-for-group))
      ;; test-start-times
      (should (vectorp actual-test-start-times))
      (should (length= actual-test-start-times expected-absolute-total-tests-count))
      (should (< (1+ (seq-count #'generate--lisp-timestampp actual-test-start-times)) expected-total-tests-count-for-group))
      ;; test-end-times
      (should (vectorp actual-test-end-times))
      (should (length= actual-test-end-times expected-absolute-total-tests-count))
      (should (< (1+ (seq-count #'generate--lisp-timestampp actual-test-end-times)) expected-total-tests-count-for-group))
      ;; start-time
      (should (generate--lisp-timestampp actual-start-time))
      ;; end-time
      (should (generate--lisp-timestampp actual-end-time)))
    (with-slots ((actual-name name))
	actual-ert-test
      (should (s-starts-with-p expected-test-group-name (symbol-name actual-name)))
      (should (s-ends-with-p (number-to-string test-next-test-index-for-group) (symbol-name actual-name))))))

(generate-ert-deftest-n-times generate--create-mid-run-ert-stats-for-tests-groups-alist-one-test-left ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  ((full-tests-groups-alist
	    fresh-tests-groups-alist
	    test-currently-executing-test-group
	    expected-test-group-name
	    test-next-test-index-for-group
	    expected-completed-tests-count-for-group
	    expected-total-tests-count-for-group
	    expected-absolute-total-tests-count
	    absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome t))
	  ((actual-ert-stats actual-ert-test) (generate--fake-mid-run-ert-stats-for-tests-groups-alist
					       full-tests-groups-alist
					       test-currently-executing-test-group
					       test-next-test-index-for-group
					       test-outcome
					       expected-completed-tests-count-for-group
					       expected-absolute-total-tests-count)))
    (with-slots ((actual-selector selector)
		 (actual-tests tests)
		 (actual-test-map test-map)
		 (actual-test-results test-results)
		 (actual-test-start-times test-start-times)
		 (actual-test-end-times test-end-times)
		 (actual-start-time start-time)
		 (actual-end-time end-time))
	actual-ert-stats
      ;; selector
      (should (equal actual-selector t))
      ;; tests
      (should (vectorp actual-tests))
      (should (length= actual-tests expected-absolute-total-tests-count))
      ;; test-map
      (should (hash-table-p actual-test-map))
      (should (length= (map-keys actual-test-map) expected-absolute-total-tests-count))
      ;; test-results
      (should (vectorp actual-test-results))
      (should (length= actual-test-results expected-absolute-total-tests-count))
      (should (equal (1+ (seq-count #'ert-test-result-p actual-test-results)) expected-total-tests-count-for-group))
      ;; test-start-times
      (should (vectorp actual-test-start-times))
      (should (length= actual-test-start-times expected-absolute-total-tests-count))
      (should (equal (1+ (seq-count #'generate--lisp-timestampp actual-test-start-times)) expected-total-tests-count-for-group))
      ;; test-end-times
      (should (vectorp actual-test-end-times))
      (should (length= actual-test-end-times expected-absolute-total-tests-count))
      (should (equal (1+ (seq-count #'generate--lisp-timestampp actual-test-end-times)) expected-total-tests-count-for-group))
      ;; start-time
      (should (generate--lisp-timestampp actual-start-time))
      ;; end-time
      (should (generate--lisp-timestampp actual-end-time)))
    (with-slots ((actual-name name))
	actual-ert-test
      (should (s-starts-with-p expected-test-group-name (symbol-name actual-name)))
      (should (s-ends-with-p (number-to-string test-next-test-index-for-group) (symbol-name actual-name))))))

(generate-ert-deftest-n-times generate--fake-mid-run-data-for-x-type ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  (test-finishedp (generate-random-boolean))
	  ((actual-tests-groups-alist actual-ert-stats
				      actual-next-ert-test actual-next-ert-test-result
				      actual-currently-executing-test-group-name actual-next-test-index
				      actual-completed-tests-count-for-group actual-total-tests-count-for-group
				      actual-absolute-outcomes-counts-plist)
	   (funcall (generate--fake-mid-run-data-for-x-type test-finishedp) test-outcome)))
    (should (map-elt actual-tests-groups-alist actual-currently-executing-test-group-name))
    (should (ert--stats-p actual-ert-stats))
    (should (ert-test-p actual-next-ert-test))
    (should (ert-test-result-p actual-next-ert-test-result))
    (should (natnump actual-next-test-index))
    (should (natnump actual-total-tests-count-for-group))
    (should (plistp actual-absolute-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--fake-completed-test-group-con-for-outcome-x-for-non-exclusive-outcomes ()
  :num-runs 100
  (-let*  ((test-group-name (generate-random-string))
	   (test-requested-outcome (generate--random-non-exclusive-ert-test-outcome))
	   (test-count-for-requested-outcome (generate--random-nat-number-in-range-10))
	   (test-compatible-outcome (generate--get-compatible-outcome test-requested-outcome))
	   (test-ert-expected-result-type (generate--random-ert-expected-result-type))
	   (test-count-for-compatible-outcome (generate--random-nat-number-in-range-10))
	   (expected-total-tests (+ test-count-for-requested-outcome test-count-for-compatible-outcome))
	   ((test-start-time test-end-time test-duration) (generate-random-lisp-timestamp-range-with-duration))
	   (test-result-for-requested-outcome (generate-ert-test-result-object test-requested-outcome test-duration))
	   (test-result-for-compatible-outcome (generate-ert-test-result-object test-compatible-outcome test-duration))
	   (test-base-outcomes-counts-plist (-interleave generate--DEFAULT-OUTCOMES (make-list (length generate--DEFAULT-OUTCOMES) 0)))
	   (test-func (generate--fake-completed-test-group-con-for-outcome-x test-base-outcomes-counts-plist))
	   (((actual-group-name . actual-stats) actual-absolute-total-tests actual-outcomes-counts-plist) (funcall test-func
														   (list test-group-name
															 test-requested-outcome
															 test-ert-expected-result-type
															 test-count-for-requested-outcome
															 test-result-for-requested-outcome
															 test-compatible-outcome
															 test-count-for-compatible-outcome
															 test-result-for-compatible-outcome
															 test-start-time
															 test-end-time
															 test-duration)))
    	   ((&plist
	     :total-tests actual-total-tests
	     :expected-result-type actual-expected-result-type
	     :duration actual-duration
	     :test-start-times actual-test-start-times
	     :test-end-times actual-test-end-times
	     :test-results actual-results)
	    actual-stats)
	   (actual-requested-outcome-count (generate--plist-get test-requested-outcome actual-stats)))
    (mapc (lambda (x) (should (proper-list-p x))) (list actual-test-start-times actual-test-end-times actual-results))
    (should-not (zerop actual-requested-outcome-count))
    (should (equal actual-total-tests expected-total-tests))
    (should (equal actual-expected-result-type test-ert-expected-result-type))
    (should (>= actual-duration test-duration))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-start-times) test-start-time))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-end-times) test-end-time))
    (should (seq-every-p #'ert-test-result-p actual-results))
    (should (plistp actual-outcomes-counts-plist))
    (should (numberp (generate-map-random-value actual-outcomes-counts-plist)))
    (should (g--gt0 actual-absolute-total-tests))))

(generate-ert-deftest-n-times generate--fake-completed-test-group-con-for-outcome-x-for-exclusive-outcomes ()
  :num-runs 100
  (-let*  ((test-group-name (generate-random-string))
	   (test-requested-outcome (generate--random-exclusive-ert-test-outcome))
	   (test-count-for-requested-outcome (generate--random-nat-number-in-range-10))
	   (test-compatible-outcome (generate--get-compatible-outcome test-requested-outcome))
	   (test-ert-expected-result-type (generate--random-ert-expected-result-type))
	   (test-count-for-compatible-outcome 0)
	   (expected-total-tests (+ test-count-for-requested-outcome test-count-for-compatible-outcome))
	   ((test-start-time test-end-time test-duration) (generate-random-lisp-timestamp-range-with-duration))
	   (test-result-for-requested-outcome (generate-ert-test-result-object test-requested-outcome test-duration))
	   (test-result-for-compatible-outcome (generate-ert-test-result-object test-compatible-outcome test-duration))
	   (test-base-outcomes-counts-plist (-interleave generate--DEFAULT-OUTCOMES (make-list (length generate--DEFAULT-OUTCOMES) 0)))
	   (test-func (generate--fake-completed-test-group-con-for-outcome-x test-base-outcomes-counts-plist))
	   (((actual-group-name . actual-stats) actual-absolute-total-tests actual-outcomes-counts-plist) (funcall test-func
														   (list test-group-name
															 test-requested-outcome
															 test-ert-expected-result-type
															 test-count-for-requested-outcome
															 test-result-for-requested-outcome
															 test-compatible-outcome
															 test-count-for-compatible-outcome
															 test-result-for-compatible-outcome
															 test-start-time
															 test-end-time
															 test-duration)))
    	   ((&plist
	     :expected-result-type actual-expected-result-type
	     :total-tests actual-total-tests
	     :duration actual-duration
	     :test-start-times actual-test-start-times
	     :test-end-times actual-test-end-times
	     :test-results actual-results)
	    actual-stats)
	   (actual-requested-outcome-count (generate--plist-get test-requested-outcome actual-stats)))
    (mapc (lambda (x) (should (proper-list-p x))) (list actual-test-start-times actual-test-end-times actual-results))
    (should-not (zerop actual-requested-outcome-count))
    (should (equal actual-total-tests expected-total-tests))
    (should (equal actual-expected-result-type test-ert-expected-result-type))
    (should (>= actual-duration test-duration))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-start-times) test-start-time))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-end-times) test-end-time))
    (should (seq-every-p #'ert-test-result-p actual-results))
    (should (plistp actual-outcomes-counts-plist))
    (should (numberp (generate-map-random-value actual-outcomes-counts-plist)))
    (should (g--gt0 actual-absolute-total-tests))))

(generate-ert-deftest-n-times generate--random-fake-completed-test-group-con-for-outcome-x ()
  :num-runs 100
  (-let* ((test-requested-outcome (generate--random-ert-test-outcome))
	  (((actual-group-name . actual-stats) actual-group-name actual-absolute-total-tests actual-outcomes-counts-plist) (generate--random-fake-completed-test-group-con-for-outcome-x test-requested-outcome))
    	  ((&plist
	    :expected-result-type actual-expected-result-type
	    :total-tests actual-total-tests
	    :duration actual-duration
	    :test-start-times actual-test-start-times
	    :test-end-times actual-test-end-times
	    :test-results actual-results)
	   actual-stats))
    (should (g--gt0 actual-total-tests))
    (should (member actual-expected-result-type generate--EXPECTED-RESULT-TYPES))
    (should (g--gt0 actual-duration))
    (should (consp (generate-seq-take-random-value-from-seq actual-test-start-times)))
    (should (consp (generate-seq-take-random-value-from-seq actual-test-end-times)))
    (should (ert-test-result-p (generate-seq-take-random-value-from-seq actual-results)))
    (should (g--gt0 actual-absolute-total-tests))
    (should (plistp actual-outcomes-counts-plist))
    (should (mapp actual-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--fake-completed-tests-groups-alist ()
  :num-runs 100
  (-let* ((expected-outcome (generate--random-ert-test-outcome))
	  ((actual-tests-groups-alist actual-group-names-for-requested-outcome actual-other-group-names actual-absolute-total-tests-count actual-absolute-outcomes-counts-plist actual-relative-outcomes-counts-plist) (generate--fake-completed-tests-groups-alist expected-outcome))
	  (actual-random-group-name (generate-seq-take-random-value-from-seq actual-group-names-for-requested-outcome))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-random-group-name))
	  ((&plist
	    :total-tests
	    :duration
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (g--gt0 (map-elt actual-group-stats expected-outcome)))
    (mapc (lambda (x) (should (g--gt0 x))) (list actual-absolute-total-tests-count total-tests duration))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (stringp (generate-seq-take-random-value-from-seq actual-other-group-names)))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))
    (should (natnump (generate-map-random-value actual-absolute-outcomes-counts-plist)))
    (should (plistp actual-relative-outcomes-counts-plist))
    (should (natnump (generate-map-random-value actual-relative-outcomes-counts-plist)))))

(generate-ert-deftest-n-times generate--random-fake-completed-tests-groups-alist ()
  :num-runs 100
  (-let* (((actual-tests-groups-alist actual-group-names-for-requested-outcome actual-other-group-names actual-absolute-total-tests-count actual-absolute-outcomes-counts-plist) (generate--random-fake-completed-tests-groups-alist))
	  ((actual-random-outcome actual-random-outcome-count) (generate-map-random-pair actual-absolute-outcomes-counts-plist))
	  (actual-random-group-name (generate-seq-take-random-value-from-seq actual-group-names-for-requested-outcome))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-random-group-name)))
    (should (plistp actual-group-stats))
    (should (listp actual-other-group-names))
    (should (natnump actual-random-outcome-count))
    (should (stringp actual-random-group-name))
    (should (g--gt0 actual-absolute-total-tests-count))
    (should (plistp actual-absolute-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--create-completed-ert-stats-for-tests-groups-alist ()
  :num-runs 100
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  ((tests-groups-alist test-group-names-for-requested-outcome test-other-group-names test-absolute-total-tests-count _)
	   (generate--fake-completed-tests-groups-alist test-outcome))
	  (actual-ert-stats (generate--create-completed-ert-stats-for-tests-groups-alist test-absolute-total-tests-count tests-groups-alist)))
    ;; selector
    (should (equal (ert--stats-selector actual-ert-stats) t))
    ;; tests
    (should (vectorp (ert--stats-tests actual-ert-stats)))
    (should (ert-test-p (generate-seq-take-random-value-from-seq (ert--stats-tests actual-ert-stats))))
    ;; test-map
    (should (hash-table-p (ert--stats-test-map actual-ert-stats)))
    (should-not (-difference (append test-group-names-for-requested-outcome test-other-group-names) (map-keys (ert--stats-test-map actual-ert-stats))))
    (should (natnump (generate-seq-take-random-value-from-seq (map-values (ert--stats-test-map actual-ert-stats)))))
    ;; test-results
    (should (vectorp (ert--stats-test-results actual-ert-stats)))
    (should (ert-test-result-p (generate-seq-take-random-value-from-seq (ert--stats-test-results actual-ert-stats))))
    ;; test-start-times
    (should (vectorp (ert--stats-test-start-times actual-ert-stats)))
    (should (decode-time (generate-seq-take-random-value-from-seq (ert--stats-test-start-times actual-ert-stats))))
    ;; test-end-times
    (should (vectorp (ert--stats-test-end-times actual-ert-stats)))
    (should (decode-time (generate-seq-take-random-value-from-seq (ert--stats-test-end-times actual-ert-stats))))
    ;; start-time
    (should (decode-time (ert--stats-start-time actual-ert-stats)))
    ;; end-time
    (should (decode-time (ert--stats-end-time actual-ert-stats)))))

(generate-ert-deftest-n-times generate--fake-completed-tests-groups-alist-and-stats ()
  :num-runs 100
  (-let* (((test-outcome random-other-outcome) (generate-seq-two-random-values generate--DEFAULT-OUTCOMES))
	  (test-outcome-slot-func (generate--get-ert-outcome-slot-func test-outcome))
	  ((actual-tests-groups-alist actual-ert-stats actual-group-names-for-requested-outcome actual-other-group-names actual-absolute-total-tests-count actual-absolute-outcomes-counts-plist actual-relative-outcomes-counts-plist)
	   (generate--fake-completed-tests-groups-alist-and-stats test-outcome))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (g--gt0 (generate--plist-get test-outcome actual-absolute-outcomes-counts-plist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert--stats-p actual-ert-stats))
    (should (consp (ert--stats-start-time actual-ert-stats)))
    (should (consp (ert--stats-end-time actual-ert-stats)))
    (should (natnump (funcall test-outcome-slot-func actual-ert-stats)))
    (should (not (zerop actual-absolute-total-tests-count)))
    (should (mapc (lambda (x) (g--len-gt0 x)) (list actual-group-names-for-requested-outcome actual-other-group-names actual-absolute-outcomes-counts-plist actual-relative-outcomes-counts-plist)))))

(generate-ert-deftest-n-times generate--random-fake-completed-tests-groups-alist-and-stats ()
  :num-runs 100
  (-let* (((actual-tests-groups-alist actual-ert-stats actual-group-names-for-requested-outcome actual-other-group-names actual-absolute-total-tests-count actual-absolute-outcomes-counts-plist actual-relative-outcomes-counts-plist)
	   (generate--random-fake-completed-tests-groups-alist-and-stats))
	  ((actual-random-outcome actual-random-outcome-count) (generate-map-random-pair actual-absolute-outcomes-counts-plist))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert--stats-p actual-ert-stats))
    (should (not (zerop actual-absolute-total-tests-count)))
    (should (natnump actual-random-outcome-count))
    (should (mapc (lambda (x) (g--len-gt0 x)) (list actual-group-names-for-requested-outcome actual-other-group-names actual-absolute-outcomes-counts-plist actual-relative-outcomes-counts-plist)))))

(generate-ert-deftest-n-times generate--get-group-name-and-index-for-test-simple ()
  :num-runs 100
  (-let* ((expected-name (generate-random-word))
	  (expected-index (generate-random-nat-number))
	  (test-name (format "%s-%s-%s" expected-name generate--TEST-IDENTIFIER expected-index))
	  (test (generate--generate-test test-name expected-index))
	  ((actual-name . actual-index) (generate--get-group-name-and-index-for-test test)))
    (should (equal actual-name expected-name))
    (should (equal actual-index expected-index))))

(generate-ert-deftest-n-times generate--get-group-name-and-index-for-test-name-with-dashes ()
  :num-runs 100
  (-let* ((expected-name (s-join "-" (generate-random-list-of-strings)))
	  (expected-index (generate-random-nat-number))
	  (test-name (format "%s-%s-%s" expected-name generate--TEST-IDENTIFIER expected-index))
	  (test (generate--generate-test test-name expected-index))
	  ((actual-name . actual-index) (generate--get-group-name-and-index-for-test test)))
    (should (equal actual-name expected-name))
    (should (equal actual-index expected-index))))

(generate-ert-deftest-n-times generate--get-group-name-and-index-for-each-test ()
  :num-runs 100
  (-let* ((test-group-name (s-join "-" (generate-random-list-of-strings)))
	  (test-total-tests (generate--random-nat-number-in-range-10))
	  (tests (generate--generate-test-unfolder test-group-name test-total-tests))
	  (expected-con (cons test-group-name (generate--random-nat-number-between-0-and test-total-tests)))
	  (actual-name-number-cons (generate--get-group-name-and-index-for-each-test tests)))
    (should (member expected-con actual-name-number-cons))))

(generate-ert-deftest-n-times generate--create-tests-groups-alist ()
  :num-runs 100
  (-let* ((test-groups (funcall (-compose #'generate-seq-shuffle (apply-partially #'seq-map-indexed (lambda (name count) (list name (1+ count)))) #'generate-random-list-of-unique-strings)))
	  ((expected-test-group expected-count) (generate-seq-take-random-value-from-seq test-groups))
	  (tests (-flatten-n 1 (seq-map (-applify #'generate--generate-test-unfolder) test-groups)))
	  (actual-tests-groups-alist (generate--create-tests-groups-alist tests))
	  ((&plist :total-tests :passed-expected :passed-unexpected :failed-unexpected :skipped :failed-expected :failed-unexpected :duration :reasons :test-results)
	   (map-elt actual-tests-groups-alist expected-test-group)))
    (mapc (lambda (actual-value) (zerop actual-value)) (list passed-expected passed-unexpected failed-unexpected skipped failed-expected failed-unexpected duration))
    (should (equal total-tests expected-count))
    (should (length= test-results 0))))

(generate-ert-deftest-n-times generate--stats ()
  :num-runs 100
  (-let* (((expected-outcome stats-func) (generate-seq-take-random-value-from-seq generate--OUTCOMES-FOR-STATS-TESTS))
	  ((tests-groups-alist group-names-for-requested-outcome other-group-names absolute-total-tests-count absolute-outcomes-counts-plist)
	   (generate--fake-completed-tests-groups-alist expected-outcome))
	  (expected-random-test-name (generate-seq-take-random-value-from-seq group-names-for-requested-outcome))
	  (expected-outcome-count (length group-names-for-requested-outcome))
	  ((actual-outcome . actual-outcome-count) (funcall stats-func tests-groups-alist)))
    (should (equal actual-outcome expected-outcome))
    (should (equal actual-outcome-count expected-outcome-count))))

(generate-ert-deftest-n-times generate--print-final-test-group-stats-for-expected-outcomes ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-expected-ert-test-outcome))
	      ((test-group-con group-name total-test-count outcomes-count-plist) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	      (test-outcome-attributes (generate--plist-get test-outcome generate--DEFAULT-OUTCOMES-PLIST))
	      (expected-outcome-string (funcall (generate--plist-get :summary-message (generate--plist-get test-outcome generate--DEFAULT-OUTCOMES-PLIST)) total-test-count))
	      ((_ . test-group-stats) test-group-con)
	      ((&plist :duration) test-group-stats)
	      (actual-message (progn (generate--print-final-test-group-stats test-group-stats group-name) (s-join "\n" (reverse messages)))))
	(should (s-contains-p (number-to-string total-test-count) actual-message))
	(should (s-contains-p group-name actual-message))
	(should (s-contains-p expected-outcome-string actual-message))
	(should (s-contains-p (number-to-string duration) actual-message))))))

(generate-ert-deftest-n-times generate--print-final-test-group-stats-for-unexpected-outcomes ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-unexpected-ert-test-outcome))
	      ((test-group-con group-name total-test-count outcomes-count-plist) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	      (expected-compatible-outcome (generate--plist-get :compatible (generate--plist-get test-outcome generate--DEFAULT-OUTCOMES-PLIST)))
	      (expected-count (generate--plist-get expected-compatible-outcome outcomes-count-plist))
	      (expected-outcome-string (funcall (generate--plist-get :summary-message (generate--plist-get expected-compatible-outcome generate--DEFAULT-OUTCOMES-PLIST)) total-test-count))
	      ((_ . test-group-stats) test-group-con)
	      ((&plist :duration) test-group-stats)
	      (actual-message (progn (generate--print-final-test-group-stats test-group-stats group-name) (s-join "\n" (reverse messages)))))
	(should (s-contains-p group-name actual-message))
	(should (s-contains-p (number-to-string total-test-count) actual-message))
	(should (s-contains-p (number-to-string expected-count) actual-message))
	(should (s-contains-p expected-outcome-string actual-message))
	(should (s-contains-p (number-to-string duration) actual-message))))))

(generate-ert-deftest-n-times generate--maybe-print-final-group-stats-should-not-print-anything ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-ert-test-outcome))
	      ((test-group-con) (generate--fake-mid-run-tests-groups-con-for-x-type test-outcome nil))
	      ((test-name . test-group-stats) test-group-con)
	      (actual-message (progn (generate--maybe-print-final-group-stats test-group-stats test-name) (s-join "\n" (reverse messages)))))
	(should (string-equal actual-message ""))))))

(generate-ert-deftest-n-times generate--maybe-print-final-group-stats-should-print-something ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-ert-test-outcome))

	      (((_ . test-group-stats) group-name total-test-count outcomes-count-plist) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	      (actual-message (progn (generate--maybe-print-final-group-stats test-group-stats group-name) (s-join "\n" (reverse messages)))))
	(should (g--len-gt0 actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-started ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-selector (generate-random-string))
	      ((tests-groups-alist ert-test-stats expected-total-test-count _) (generate--fake-fresh-tests-groups-alist-and-stats))
	      (test-event-args (list ert-test-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-started test-selector tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p (format "Running %s tests" expected-total-test-count) actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-ended-expected-no-message ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-expected-ert-test-outcome))
	      ((tests-groups-alist ert-stats
				   ert-test ert-test-result
				   currently-executing-test-group-name next-test-index
				   completed-tests-count-for-group total-tests-count-for-group
				   absolute-outcomes-counts-plist)
	       (generate--fake-mid-run-data-for-group-with-more-than-one-test-left test-outcome))
	      ((&plist :test-start-times previous-start-times :test-end-times previous-end-times :duration previous-duration) (map-elt tests-groups-alist currently-executing-test-group-name))
	      (test-event-args (list ert-stats ert-test ert-test-result))
	      (message (progn (generate--run-tests-batch-handle-test-ended tests-groups-alist test-event-args) messages))
	      ((&plist :completed-tests :test-results :test-start-times :test-end-times :duration) (map-elt tests-groups-alist currently-executing-test-group-name)))
	(should (> duration previous-duration))
	(should (equal completed-tests (1+ completed-tests-count-for-group)))
	(should (member ert-test-result test-results))
	(should (length= test-start-times (1+ (length previous-start-times))))
	(should (length= test-start-times (1+ (length previous-end-times))))
	(should-not message)))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-ended-fail-unexpected ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-unexpected-ert-test-outcome))
	      ((tests-groups-alist ert-stats
				   ert-test ert-test-result
				   currently-executing-test-group-name next-test-index
				   completed-tests-count-for-group total-tests-count-for-group
				   absolute-outcomes-counts-plist)
	       (generate--fake-mid-run-data-for-group-with-more-than-one-test-left test-outcome))
	      ((&plist :test-start-times previous-start-times :test-end-times previous-end-times :duration previous-duration) (map-elt tests-groups-alist currently-executing-test-group-name))
	      (test-event-args (list ert-stats ert-test ert-test-result))
	      (message (progn (generate--run-tests-batch-handle-test-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages))))
	      ((&plist :completed-tests :test-results :test-start-times :test-end-times :duration) (map-elt tests-groups-alist currently-executing-test-group-name)))
	(should (> duration previous-duration))
	(should (equal completed-tests (1+ completed-tests-count-for-group)))
	(should (member ert-test-result test-results))
	(should (length= test-start-times (1+ (length previous-start-times))))
	(should (length= test-start-times (1+ (length previous-end-times))))
	(should (s-contains-p (symbol-name (ert-test-name ert-test)) message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-group-finished-executing-with-exclusive-outcome ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-exclusive-ert-test-outcome))
	      (expected-outcome-function (generate--get-ert-outcome-summary-message-function test-outcome))
	      ((tests-groups-alist ert-stats
				   ert-test ert-test-result
				   currently-executing-test-group-name next-test-index
				   completed-tests-count-for-group total-tests-count-for-group
				   absolute-outcomes-counts-plist)
	       (generate--fake-mid-run-data-for-group-with-one-more-test-left test-outcome))
	      (expected-outcome-string (funcall expected-outcome-function total-tests-count-for-group))
	      (test-event-args (list ert-stats ert-test ert-test-result))
	      (message (progn (generate--run-tests-batch-handle-test-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p currently-executing-test-group-name message))
	(should (s-contains-p (format "%s/%s" total-tests-count-for-group total-tests-count-for-group) message))
	(should (s-contains-p expected-outcome-string message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-group-finished-executing-with-nonexclusive-outcome ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-non-exclusive-ert-test-outcome))
	      (test-compatible-outcome (generate--get-compatible-outcome test-outcome))
	      (expected-outcome-function (generate--get-ert-outcome-summary-message-function test-compatible-outcome))
	      ((tests-groups-alist ert-stats
				   ert-test ert-test-result
				   currently-executing-test-group-name next-test-index
				   completed-tests-count-for-group total-tests-count-for-group
				   absolute-outcomes-counts-plist)
	       (generate--fake-mid-run-data-for-group-with-one-more-test-left test-outcome))
	      (expected-outcome-string (funcall expected-outcome-function total-tests-count-for-group))
	      (test-event-args (list ert-stats ert-test ert-test-result))
	      (message (progn (generate--run-tests-batch-handle-test-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p currently-executing-test-group-name message))
	(should (s-contains-p (number-to-string completed-tests-count-for-group) message))
	(should (s-contains-p expected-outcome-string message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-ended-success ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-ert-test-outcome))
	      (expected-outcome-string (generate--get-ert-outcome-breakdown-message test-outcome))
	      ((tests-groups-alist ert-stats group-names-for-requested-outcome other-group-names absolute-total-tests-count expected-absolute-outcomes-counts-plist expected-relative-outcomes-counts-plist)
	       (generate--fake-completed-tests-groups-alist-and-stats test-outcome))
	      (test-event-args (list ert-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages))))
	      (expected-breakdown-list-length (funcall (-compose #'length #'map-remove) (lambda (_ v) (zerop v)) expected-relative-outcomes-counts-plist))
	      (actual-breakdown-list (funcall (-compose (apply-partially #'seq-filter #'generate--TEST-SUMMARY-STRING-PREDICATES) #'s-split) "\n" actual-message)))
	(should (s-contains-p "Start at" actual-message))
	(should (s-contains-p "Duration" actual-message))
	(should (s-contains-p "Total tests" actual-message))
	(should (s-contains-p "Breakdown:" actual-message))
	(should (s-contains-p expected-outcome-string actual-message))
	(should (length= actual-breakdown-list expected-breakdown-list-length))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-ended-aborted ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-ert-test-outcome))
	      ((tests-groups-alist ert-test-stats expected-tests-count outcomes-counts-plist group-names)
	       (generate--fake-completed-tests-groups-alist-and-stats test-outcome))
	      (abortedp t)
	      (test-event-args (list ert-test-stats abortedp))
	      (actual-message (progn (generate--run-tests-batch-handle-run-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p "Aborted" actual-message))
	(should-not (s-contains-p "Start at" actual-message))
	(should-not (s-contains-p "Duration" actual-message))
	(should-not (s-contains-p "Total tests" actual-message))
	(should-not (s-contains-p "Breakdown:" actual-message))))))

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
