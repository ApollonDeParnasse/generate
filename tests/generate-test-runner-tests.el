;;; generate-test-runner-tests.el --- Tests for the generate test-runner  -*- lexical-binding: t; -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: testing
;; Package-Requires: ((emacs "30") (org "9.7") (dash "2.20.0") (s "1.13.1") (compat "29"))
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

(require 'ert)
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

(defun generate--get-ert-outcome-attribute (attribute)
  (lambda (outcome)
    (funcall (-compose (-partial #'generate--plist-get attribute) (-rpartial #'generate--plist-get generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS)) outcome)))

(defalias 'generate--get-ert-outcome-message (generate--get-ert-outcome-attribute :message))
(defalias 'generate--get-ert-outcome-summary-message (generate--get-ert-outcome-attribute :summary))
(defalias 'generate--get-ert-outcome-slot-func (generate--get-ert-outcome-attribute :slot))

(generate-ert-deftest-n-times generate-ert-test ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-number (generate-random-nat-number))
	 (test-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFITER test-number))
	 (test (generate-ert-test test-name)))
    (should (ert-test-p test))))

(generate-ert-deftest-n-times generate-passing-should-form ()
  :num-runs 0
  (let* ((actual-should (generate-passing-should-form)))
    (should (equal (car actual-should) 'should))
    (should (cdr actual-should))))

(generate-ert-deftest-n-times generate-passing-should-not-form ()
  :num-runs 0
  (let* ((actual-should (generate-passing-should-not-form)))
    (should (equal (car actual-should) 'should-not))
    (should (cdr actual-should))))

(generate-ert-deftest-n-times generate-failing-should-form ()
  :num-runs 0
  (let* ((actual-should (generate-failing-should-form)))
    (should (equal (car actual-should) 'should))
    (should (cdr actual-should))))

(generate-ert-deftest-n-times generate-failing-should-not-form ()
  :num-runs 0
  (let* ((actual-should (generate-failing-should-not-form)))
    (should (equal (car actual-should) 'should-not))
    (should (cdr actual-should))))

(generate-ert-deftest-n-times generate-random-passing-should ()
  :num-runs 0
  (let* ((actual-should (generate-random-passing-should)))
    (should (member (car actual-should) (list 'should 'should-not)))
    (should (cdr actual-should))))

(generate-ert-deftest-n-times generate-random-failing-should ()
  :num-runs 0
  (let* ((actual-should (generate-random-failing-should)))
    (should (member (car actual-should) (list 'should 'should-not)))
    (should (cdr actual-should))))

(generate-ert-deftest-n-times generate-random-should ()
  :num-runs 0
  (let* ((actual-should (generate-random-should)))
    (should (member (car actual-should) (list 'should 'should-not)))
    (should (cdr actual-should))))

(generate-ert-deftest-n-times generate-list-of-n-passing-should-forms ()
  :num-runs 0
  (let* ((test-n (generate--random-nat-number-in-range-10))
	 (actual-list-of-passing-should-forms (generate-list-of-n-passing-should-forms test-n)))
    (should (= (seq-count (-compose (-rpartial #'member (list 'should 'should-not)) #'car) actual-list-of-passing-should-forms) (length actual-list-of-passing-should-forms) test-n))))

(generate-ert-deftest-n-times generate-list-of-n-should-forms-with-a-fail ()
  :num-runs 0
  (let* ((test-n (generate--random-nat-number-in-range-10))
	 (actual-list-of-should-forms (generate-list-of-n-should-forms-with-a-fail test-n)))
    (should (= (seq-count (-compose (-rpartial #'member (list 'should 'should-not)) #'car) actual-list-of-should-forms) (length actual-list-of-should-forms) test-n))))

(generate-ert-deftest-n-times generate-random-list-of-should-forms-with-a-fail ()
  :num-runs 0
  (let* ((actual-list-of-should-forms (generate-random-list-of-should-forms-with-a-fail))
	 (actual-car (car (generate-seq-take-random-value-from-seq actual-list-of-should-forms))))
    (should (symbolp actual-car))))

(generate-ert-deftest-n-times generate-random-list-of-passing-should-forms ()
  :num-runs 0
  (let* ((actual-list-of-passing-should-forms (generate-random-list-of-passing-should-forms))
	 (actual-car (car (generate-seq-take-random-value-from-seq actual-list-of-passing-should-forms))))
    (should (symbolp actual-car))))

(generate-ert-deftest-n-times generate-ert-test-failed-error ()
  :num-runs 0
  (-let* ((test-should (generate-random-failing-should))
	 ((actual-error actual-val) (generate-ert-test-failed-error test-should)))
    (should (equal actual-error 'ert-test-failed))
    (should (symbolp (caar actual-val)))))

(generate-ert-deftest-n-times generate-ert-test-failed-condition ()
  :num-runs 0
  (-let* ((test-should (generate-random-failing-should))
	 ((actual-error _) (generate-ert-test-failed-condition test-should)))
    (should (symbolp actual-error))))

(generate-ert-deftest-n-times generate-ert-test-skipped-condition ()
  :num-runs 0
  (-let* ((test-should (generate-random-failing-should))
	 ((actual-condition-symbol actual-val) (generate-ert-test-skipped-condition test-should)))
    (should (equal actual-condition-symbol 'ert-test-skipped))
    (should (symbolp (caar actual-val)))))

(generate-ert-deftest-n-times generate-ert-test-result-object-passed-expected-unexpected ()
  :num-runs 0
  (let* ((test-duration (generate-random-nat-number))
	 (test-outcome (generate-seq-take-random-value-from-seq (list :passed-expected :passed-unexpected)))
	 (actual-ert-test-result (generate-ert-test-result-object test-outcome test-duration)))
    (should (ert-test-result-type-p actual-ert-test-result :passed))
    (should (stringp (ert-test-result-messages actual-ert-test-result)))
    (should (ert-test-result-should-forms actual-ert-test-result))
    (should (numberp (ert-test-result-duration actual-ert-test-result)))))

(generate-ert-deftest-n-times generate-ert-test-result-object-failed-skipped ()
  :num-runs 0
  (-let* ((test-duration (generate-random-nat-number))
	  (test-outcome (generate-seq-take-random-value-from-seq (list :failed-unexpected :failed-expected :skipped)))
	  ((expected-condition-symbol expected-result-type) (generate--plist-get test-outcome (list :failed-unexpected (list 'ert-test-failed :failed)
								:failed-expected (list 'ert-test-failed :failed)
								:skipped (list 'ert-test-skipped :skipped))))
	  (actual-ert-test-result (generate-ert-test-result-object test-outcome test-duration)))
    (print actual-ert-test-result)
    (should (ert-test-result-type-p actual-ert-test-result expected-result-type))
    (should (stringp (ert-test-result-with-condition-messages actual-ert-test-result)))
    (should (equal (ert-test-result-with-condition-duration actual-ert-test-result) test-duration))
    (should (symbolp (car (ert-test-result-with-condition-condition actual-ert-test-result))))
    (should (backtrace-frame-p (generate-seq-take-random-value-from-seq (ert-test-result-with-condition-backtrace actual-ert-test-result))))))

(generate-ert-deftest-n-times generate--plist-of-ert-test-result-objects ()
  :num-runs 0
  (-let* ((test-durations (generate-list-of-nat-numbers :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-duration-pairs (-zip-lists generate--DEFAULT-OUTCOMES test-durations))
	  (actual-plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs)))
    (should (ert-test-result-p (car (generate-map-random-value actual-plist-of-ert-test-result-objects))))))

(generate-ert-deftest-n-times generate--take-from-plist-of-ert-test-results ()
  :num-runs 0
  (-let* ((test-durations (generate-list-of-nat-numbers :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-counts (generate--list-of-n-nat-numbers-in-range-10 :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-duration-pairs (-zip-lists generate--DEFAULT-OUTCOMES test-durations))
	  (test-outcome-counts-plist (-interleave generate--DEFAULT-OUTCOMES test-outcome-counts))
	  (test-plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs))
	  ((test-results test-reasons) (generate--take-from-plist-of-ert-test-results test-plist-of-ert-test-result-objects test-outcome-counts-plist))
	  (expected-length (funcall (-compose #'-sum #'map-values) test-outcome-counts-plist))
	  ((random-outcome random-pred) (generate-seq-take-random-value-from-seq generate--OUTCOME-PREDS-MAPPING))
	  (expected-count-for-random-outcome (generate--plist-get random-outcome test-outcome-counts-plist)))
    (should (length= test-results expected-length))
    (should (stringp (generate-seq-take-random-value-from-seq test-reasons)))
    (should (length= test-reasons expected-length))))
    (should (ert-test-result-p (generate-seq-take-random-value-from-seq test-results)))))

(generate-ert-deftest-n-times generate--generate-test-simple ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-number (generate-random-nat-number))
	 (actual-test (generate--generate-test test-group-name test-number)))
    (should (ert-test-p actual-test))
    (should (s-contains-p generate--TEST-IDENTIFITER (ert-test-name actual-test)))))

(generate-ert-deftest-n-times generate--generate-test-with-expected-result-type ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-number (generate-random-nat-number))
	 (expected-result-type (generate--random-ert-expected-result-type))
	 (actual-test (generate--generate-test test-group-name test-number :expected-result-type expected-result-type)))
    (should (ert-test-p actual-test))
    (should (equal (ert-test-expected-result-type actual-test) expected-result-type))
    (should (s-contains-p generate--TEST-IDENTIFITER (ert-test-name actual-test)))))

(generate-ert-deftest-n-times generate--test-name-unfolder ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-count (generate--random-nat-number-in-range-10))
	 (actual-names (generate--test-name-unfolder (cons test-count test-group-name)))
	 (expected-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFITER (generate--random-nat-number-between-0-and test-count))))
    (should (member expected-name actual-names))))

(generate-ert-deftest-n-times generate--generate-test-unfolder ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-count (generate--random-nat-number-in-range-10))
	 (expected-result-type (generate--random-ert-expected-result-type))
	 (actual-tests (generate--generate-test-unfolder test-group-name test-count expected-result-type))
	 (actual-random-test (generate-seq-take-random-value-from-seq actual-tests))
	 (expected-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFITER (generate--random-nat-number-between-0-and test-count))))
    (should (length= actual-tests test-count))
    (should (equal (ert-test-expected-result-type actual-random-test) expected-result-type))
    (should (-any (lambda (actual-test) (string-equal (ert-test-name actual-test) expected-name)) actual-tests))))

(generate-ert-deftest-n-times generate--create-ert-tests-for-test-group-basic ()
  :num-runs 0
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  (outcome-expected-result-type (generate--plist-get :expected-result-type (generate--plist-get test-outcome generate--DEFAULT-OUTCOMES-PLIST)))
	  (((test-group-name . test-stats)) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	  ((&plist :total-tests) test-stats)
	  (actual-tests (generate--create-ert-tests-for-test-group test-group-name test-stats))
	  (actual-random-test (generate-seq-take-random-value-from-seq actual-tests)))
    (should (length= actual-tests total-tests))
    (should (equal (ert-test-expected-result-type actual-random-test) outcome-expected-result-type))
    (should (s-starts-with-p test-group-name (ert-test-name actual-random-test)))))

(generate-ert-deftest-n-times generate--create-list-of-tests-for-tests-groups-alist ()
  :num-runs 0
  (-let* (((tests-groups-alist _ expected-total-absolute-tests-count test-group-names) (generate--fake-fresh-tests-groups-alist))
	  (expected-random-group-name (generate-seq-take-random-value-from-seq test-group-names))
	  (expected-random-group-name-test-count (generate--plist-get :total-tests (map-elt tests-groups-alist expected-random-group-name)))
	  (expected-random-index (generate--random-nat-number-between-0-and expected-random-group-name-test-count))
	  (actual-tests (generate--create-list-of-tests-for-tests-groups-alist tests-groups-alist)))
    (should (length= actual-tests expected-total-absolute-tests-count))
    (should (-first (lambda (actual-test) (let ((actual-name (ert-test-name actual-test))) (and (s-starts-with-p expected-random-group-name actual-name) (s-ends-with-p (number-to-string expected-random-index) actual-name)))) actual-tests))))

(generate-ert-deftest-n-times generate--fake-fresh-tests-groups-alist ()
  :num-runs 0
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
    (should (g--gt0 total-tests))
    (should (length= test-results 0))
    (should (length= test-start-times 0))
    (should (length= test-end-times 0))
    (should (g--len-gt0 actual-group-names))
    (should (g--gt0 actual-total-relative-test-count))
    (should (g--gt0 actual-total-absolute-test-count))))

(generate-ert-deftest-n-times generate--create-fresh-ert-stats-for-tests-groups-alist ()
  :num-runs 0
  (-let* (((tests-groups-alist expected-total-test-count expected-group-names) (generate--fake-fresh-tests-groups-alist))
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
  :num-runs 0
  (-let* (((actual-tests-groups-alist actual-ert-stats actual-total-relative-tests-count actual-total-absolute-tests-count actual-group-names) (generate--fake-fresh-tests-groups-alist-and-stats))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert--stats-p actual-ert-stats))
    (should (not (zerop actual-total-relative-tests-count)))
    (should (not (zerop actual-total-absolute-tests-count)))
    (should (g--len-gt0 actual-group-names))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-con-for-x-type-more-than-one-test-left ()
  :num-runs 0
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  (((_ . actual-group-stats) actual-ert-test-name actual-next-test-index-for-group actual-completed-tests-count-for-group
			   actual-total-tests-count-for-group actual-absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-con-for-x-type test-outcome 'nil))
	  ((&plist
	    :total-tests
	    :completed-tests
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (g--lt (1+ completed-tests) total-tests))
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-con-for-x-type-one-more-test-left ()
  :num-runs 0
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  (((_ . actual-group-stats) actual-ert-test-name actual-next-test-index-for-group actual-completed-tests-count-for-group
			   actual-total-tests-count-for-group actual-absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-con-for-x-type test-outcome 't))
	  ((&plist
	    :total-tests
	    :completed-tests
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (equal (1+ completed-tests) total-tests))
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-alist-for-x-type-more-than-one-test-left ()
  :num-runs 0
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
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome 'nil))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-ert-test-name))
	  ((&plist
	    :total-tests
	    :completed-tests
	    :test-start-times
	    :test-end-times
	    :test-results)
	   actual-group-stats)
	  (actual-random-result (generate-seq-take-random-value-from-seq test-results)))
    (should (g--lt (1+ completed-tests) total-tests))
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group actual-absolute-total-tests-count))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))
    (should (consp actual-currently-executing-test-group))
    (should (consp actual-fresh-tests-groups-alist))))

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-alist-for-x-type-one-more-test-left ()
  :num-runs 0
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
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome 't))
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
    (mapc (lambda (x) (should (natnump x))) (list actual-next-test-index-for-group actual-completed-tests-count-for-group actual-total-tests-count-for-group actual-absolute-total-tests-count))
    (mapc (lambda (x) (should (listp x))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (length= x completed-tests))) (list test-results test-start-times test-end-times))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (ert-test-result-p actual-random-result))
    (should (plistp actual-absolute-outcomes-counts-plist))
    (should (consp actual-currently-executing-test-group))
    (should (consp actual-fresh-tests-groups-alist))))

(generate-ert-deftest-n-times generate--fake-mid-run-ert-stats-for-tests-groups-alist-with-more-than-one-test-left ()
  :num-runs 0
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
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome 'nil))
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
      (should (equal actual-selector 't))
      ;; tests
      (should (vectorp actual-tests))
      (should (length= actual-tests expected-absolute-total-tests-count))
      ;; test-map
      (should (hash-table-p actual-test-map))
      (should (length= (map-keys actual-test-map) expected-absolute-total-tests-count))
      ;; test-results
      (should (vectorp actual-test-results))
      (should (length= actual-test-results expected-absolute-total-tests-count))
      (should (g--lt (1+ (seq-count #'ert-test-result-p actual-test-results)) expected-total-tests-count-for-group))
      ;; test-start-times
      (should (vectorp actual-test-start-times))
      (should (length= actual-test-start-times expected-absolute-total-tests-count))
      (should (g--lt (1+ (seq-count #'generate--lisp-timestampp actual-test-start-times)) expected-total-tests-count-for-group))
      ;; test-end-times
      (should (vectorp actual-test-end-times))
      (should (length= actual-test-end-times expected-absolute-total-tests-count))
      (should (g--lt (1+ (seq-count #'generate--lisp-timestampp actual-test-end-times)) expected-total-tests-count-for-group))
      ;; start-time
      (should (generate--lisp-timestampp actual-start-time))
      ;; end-time
      (should (generate--lisp-timestampp actual-end-time)))
    (with-slots ((actual-name name))
	actual-ert-test
      (should (s-starts-with-p expected-test-group-name actual-name))
      (should (s-ends-with-p (number-to-string test-next-test-index-for-group) actual-name)))))

(generate-ert-deftest-n-times generate--create-mid-run-ert-stats-for-tests-groups-alist-one-test-left ()
  :num-runs 0
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
	   (generate--fake-mid-run-tests-groups-alist-for-x-type test-outcome 't))
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
      (should (equal actual-selector 't))
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
      (should (s-starts-with-p expected-test-group-name actual-name))
      (should (s-ends-with-p (number-to-string test-next-test-index-for-group) actual-name)))))

(generate-ert-deftest-n-times generate--fake-mid-run-data-for-x-type ()
  :num-runs 0
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

(generate-ert-deftest-n-times generate--fake-mid-run-tests-groups-alist-and-stats-for-x-type-one-test-left ()
  :num-runs 0
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  ((actual-tests-groups-alist actual-ert-stats actual-group-name actual-next-test-index actual-completed-tests-count-for-group actual-total-tests-count-for-group actual-absolute-outcomes-counts-plist)
	   (generate--fake-mid-run-tests-groups-alist-and-stats-for-x-type test-outcome 'nil)))
    (should (map-elt actual-tests-groups-alist actual-group-name))
    (should (ert--stats-p actual-ert-stats))
    (should (natnump actual-next-test-index))
    (should (natnump actual-total-tests-count-for-group))
    (should (plistp actual-absolute-outcomes-counts-plist))))

(generate-ert-deftest-n-times generate--fake-completed-test-group-con-for-outcome-x-for-non-exclusive-outcomes ()
  :num-runs 0
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
    (should (g--gte actual-duration test-duration))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-start-times) test-start-time))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-end-times) test-end-time))
    (should (seq-every-p #'ert-test-result-p actual-results))
    (should (plistp actual-outcomes-counts-plist))
    (should (numberp (generate-map-random-value actual-outcomes-counts-plist)))
    (should (g--gt0 actual-absolute-total-tests))))

(generate-ert-deftest-n-times generate--fake-completed-test-group-con-for-outcome-x-for-exclusive-outcomes ()
  :num-runs 0
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
    (should (g--gte actual-duration test-duration))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-start-times) test-start-time))
    (should (equal (generate-seq-take-random-value-from-seq actual-test-end-times) test-end-time))
    (should (seq-every-p #'ert-test-result-p actual-results))
    (should (plistp actual-outcomes-counts-plist))
    (should (numberp (generate-map-random-value actual-outcomes-counts-plist)))
    (should (g--gt0 actual-absolute-total-tests))))

(generate-ert-deftest-n-times generate--random-fake-completed-test-group-con-for-outcome-x ()
  :num-runs 0
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
  :num-runs 0
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
  :num-runs 0
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
  :num-runs 0
  (-let* ((test-outcome (generate--random-ert-test-outcome))
	  ((tests-groups-alist test-group-names-for-requested-outcome test-other-group-names test-absolute-total-tests-count _)
	   (generate--fake-completed-tests-groups-alist test-outcome))
	  (actual-ert-stats (generate--create-completed-ert-stats-for-tests-groups-alist test-absolute-total-tests-count tests-groups-alist)))
    ;; selector
    (should (equal (ert--stats-selector actual-ert-stats) 't))
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
  :num-runs 0
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
  :num-runs 0
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

(generate-ert-deftest-n-times generate--stats ()
  :num-runs 0
  (-let* (((expected-outcome stats-func) (generate-seq-take-random-value-from-seq generate--OUTCOMES-FOR-STATS-TESTS))
	  ((tests-groups-alist group-names-for-requested-outcome other-group-names absolute-total-tests-count absolute-outcomes-counts-plist)
	  (generate--fake-completed-tests-groups-alist expected-outcome))
	  (expected-random-test-name (generate-seq-take-random-value-from-seq group-names-for-requested-outcome))
	  (expected-outcome-count (length group-names-for-requested-outcome))
	  ((actual-outcome . actual-outcome-count) (funcall stats-func tests-groups-alist)))
    (should (equal actual-outcome expected-outcome))
    (should (equal actual-outcome-count expected-outcome-count))))

(generate-ert-deftest-n-times generate--print-final-test-group-stats-for-expected-outcomes ()
  :num-runs 0
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
  :num-runs 0
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
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-ert-test-outcome))
	      ((test-group-con) (generate--fake-mid-run-tests-groups-con-for-x-type test-outcome nil))
	      ((test-name . test-group-stats) test-group-con)
	      (actual-message (progn (generate--maybe-print-final-group-stats test-group-stats test-name) (s-join "\n" (reverse messages)))))
	(should (string-equal actual-message ""))))))

(generate-ert-deftest-n-times generate--maybe-print-final-group-stats-should-print-something ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate--random-ert-test-outcome))

	      (((_ . test-group-stats) group-name total-test-count outcomes-count-plist) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	      (actual-message (progn (generate--maybe-print-final-group-stats test-group-stats group-name) (s-join "\n" (reverse messages)))))
	(should (g--len-gt0 actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-started ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-selector (generate-random-string))
	     ((tests-groups-alist ert-test-stats expected-total-test-count _) (generate--fake-fresh-tests-groups-alist-and-stats))
	     (test-event-args (list ert-test-stats))
	     (actual-message (progn (generate--run-tests-batch-handle-run-started test-selector tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(print actual-message)
	(should (s-contains-p (format "Running %s tests" expected-total-test-count) actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-ended-no-message ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* (((test-outcome test-outcome-string) (funcall (-compose (-juxt #'identity #'generate--get-ert-outcome-message) #'generate-seq-take-random-value-from-seq) (list :passed-expected :skipped)))
	      ((random-other-outcome random-other-outcome-string) (funcall (-compose
									    (-juxt #'identity #'generate--get-ert-outcome-message)
									    #'generate-seq-take-random-value-from-seq
									    #'remove)
									   test-outcome generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))
	      ((tests-groups-alist test-stats expected-tests-count outcomes-counts-plist group-names)
	       (generate--fake-completed-tests-groups-alist-and-stats test-outcome))
	      (test-event-args (list test-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-started 't tests-groups-alist test-event-args test) (apply #'concat (reverse messages)))))
	(should (string-equal actual-message ""))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-ended-passed-unexpected ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* (((tests-groups-alist test-stats test _ _)
	       (generate--random-fake-tests-groups-alist-stats-and-test :passed-unexpected))
	      (test-event-args (list test-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-started test-selector tests-groups-alist test-event-args test) (apply #'concat (reverse messages)))))
	(should (s-contains-p "passed-unexpectedly" actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-ended-success ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate-seq-take-random-value-from-seq generate--DEFAULT-OUTCOMES))
	      (expected-outcome-string (generate--get-ert-outcome-summary-message test-outcome))
	      ((tests-groups-alist test-stats expected-tests-count outcomes-counts-plist group-names)
	       (generate--fake-completed-tests-groups-alist-and-stats test-outcome))
	      (test-event-args (list test-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p "Start at" actual-message))
	(should (s-contains-p "Duration" actual-message))
	(should (s-contains-p "Total tests" actual-message))
	(should (s-contains-p "Breakdown:" actual-message))
	(should (s-contains-p expected-outcome-string actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-ended-aborted ()
  :num-runs 100
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-string &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate-seq-take-random-value-from-seq generate--DEFAULT-OUTCOMES))
	      ((tests-groups-alist test-stats expected-tests-count outcomes-counts-plist group-names)
	       (generate--fake-completed-tests-groups-alist-and-stats test-outcome))
	      (abortedp 't)
	      (test-event-args (list test-stats abortedp))
	      (actual-message (progn (generate--run-tests-batch-handle-run-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p "Aborted" actual-message))
	(should-not (s-contains-p "Start at" actual-message))
	(should-not (s-contains-p "Duration" actual-message))
	(should-not (s-contains-p "Total tests" actual-message))
	(should-not (s-contains-p "Breakdown:" actual-message))))))

(generate-ert-deftest-n-times generate--create-run-tests-batch-listener ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (let* (((test-list-of-events expected-messages) (generate--random-list-of-ert-events))
	     (actual-listener (generate--create-run-tests-batch-listener 't tests-groups-alist))
	     (actual-messages (progn (mapc (-lambda ((event event-args)) (funcall actual-listener event event-args)) test-list-of-event) (s-join "\n" (reverse messages)))))
	(mapc (lambda (expected-message) (should (s-contains-p expected-message actual-messages))) expected-messages)))))

;;; generate-test-runner-tests.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
