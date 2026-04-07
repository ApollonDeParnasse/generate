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

(defconst generate--OUTCOMES-FOR-EVENT-LISTENERS-TESTS
  (list :passed-expected :failed-expected :passed-unexpected :failed-unexpected :skipped :aborted :quit))

(generate-ert-deftest-n-times generate-ert-test ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-number (generate-random-nat-number))
	 (test-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFITER test-number))
	 (test (generate-ert-test test-name)))
    (should (ert-test-p test))))

(generate-ert-deftest-n-times generate--test-name-unfolder ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-count (generate--random-nat-number-in-range-10))
	 (actual-names (generate--test-name-unfolder (cons test-count test-group-name)))
	 (expected-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFITER (generate--random-nat-number-between-0-and test-count))))
    (should (member expected-name actual-names))))

(generate-ert-deftest-n-times generate--ert-test-unfolder ()
  :num-runs 0
  (let* ((test-group-name (generate-random-word))
	 (test-count (generate--random-nat-number-in-range-10))
	 (actual-tests (generate--ert-test-unfolder (cons test-count test-group-name)))
	 (expected-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFITER (generate--random-nat-number-between-0-and test-count))))
    (should (length= actual-tests test-count))
    (should (-any (lambda (actual-test) (string-equal (ert-test-name actual-test) expected-name)) actual-tests))))

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
  (-let* ((test-durations (generate-list-of-n-nat-numbers :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-duration-pairs (-zip-lists generate--DEFAULT-OUTCOMES test-durations))
	  (actual-plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs)))
    (should (ert-test-result-p (car (generate-map-random-value actual-plist-of-ert-test-result-objects))))))

(generate-ert-deftest-n-times generate--take-from-plist-of-ert-test-results ()
  :num-runs 0
  (-let* ((test-durations (generate-list-of-n-nat-numbers :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-counts (generate--list-of-n-nat-numbers-in-range-10 :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-duration-pairs (-zip-lists generate--DEFAULT-OUTCOMES test-durations))
	  (test-outcome-counts-plist (flatten-tree (-zip-lists generate--DEFAULT-OUTCOMES test-outcome-counts)))
	  (test-plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs))
	  ((test-results test-reasons) (generate--take-from-plist-of-ert-test-results test-plist-of-ert-test-result-objects test-outcome-counts-plist))
	  (expected-length (funcall (-compose #'-sum #'map-values) test-outcome-counts-plist))
	  ((random-outcome random-pred) (generate-seq-take-random-value-from-seq generate--OUTCOME-PREDS-MAPPING))
	  (expected-count-for-random-outcome (generate--plist-get random-outcome test-outcome-counts-plist)))
    (should (length= test-results expected-length))
    (should (stringp (generate-seq-take-random-value-from-seq test-reasons)))
    (should (length= test-reasons expected-length))))

(generate-ert-deftest-n-times generate--fake-fresh-tests-groups-alist ()
  :num-runs 0
  (-let* (((actual-tests-groups-alist actual-total-test-count actual-group-names) (generate--fake-fresh-tests-groups-alist))
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
	    :reasons
	    :results
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
    (should (length= results 0))
    (should (length= reasons 0))
    (should (length= test-start-times 0))
    (should (length= test-end-times 0))
    (should (g--len-gt0 actual-group-names))
    (should (g--gt0 actual-total-test-count))))

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
  :num-runs 100
  (-let* (((actual-tests-groups-alist actual-ert-stats actual-total-test-count actual-group-names) (generate--fake-fresh-tests-groups-alist-and-stats))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert--stats-p actual-ert-stats))
    (should (not (zerop actual-total-test-count)))
    (should (g--len-gt0 actual-group-names))))

(generate-ert-deftest-n-times generate--random-fake-tests-groups-alist-stats-and-test-with-backtrace ()
  :num-runs 0
  (-let* ((test-outcome (generate-seq-take-random-value-from-seq generate--OUTCOMES-FOR-EVENT-LISTENERS-TESTS))
	  ((actual-tests-groups-alist actual-test-stats actual-test actual-result actual-backtrace)
	   (generate--random-fake-tests-groups-alist-stats-and-test test-outcome))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert-test-p actual-test))
    (should (ert-test-result-p actual-result))
    (should (stringp actual-result-with-condition-backtrace))
    (should (g--len-gt0 actual-backtrace-string))))

(generate-ert-deftest-n-times generate--random-fake-tests-groups-alist-stats-and-test-without-backtrace ()
  :num-runs 0
  (-let* ((test-outcome (generate-seq-take-random-value-from-seq generate--OUTCOMES-FOR-EVENT-LISTENERS-TESTS))
	  ((actual-tests-groups-alist actual-test-stats actual-test actual-result actual-backtrace)
	   (generate--random-fake-tests-groups-alist-stats-and-test test-outcome))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert-test-p actual-test))
    (should-not (ert-test-result-p actual-result))
    (should (stringp actual-result-with-condition-backtrace))))

(generate-ert-deftest-n-times generate--fake-completed-test-group-con-for-outcome-x ()
  :num-runs 0
  (-let* ((test-requested-outcome (generate--random-ert-test-outcome))
	  (test-other-outcomes (remove requested-outcome generate--DEFAULT-OUTCOMES))
	  ((expected-count-for-requested-outcome test-counts-for-other-outcomes) (generate--times 2 #'generate--random-nat-number-in-range-10))
	  (test-counts-for-requested-outcome (ensure-list expected-count-for-requested-outcome))
	  (expected-total-test (+ expected-count-for-requested-outcome (-sum test-counts-for-other-outcomes)))
	  ((test-results test-reasons) (generate--plist-of-ert-test-result-objects))
	  ((test-start-times test-end-times test-durations) (generate--list-of-n-unzipped-starts-ends-durations 1))
	  (test-group-name (generate-random-string))
	  (test-index 0)
	  ((actual-group-name . actual-stats) (generate--fake-completed-test-group-con-for-outcome-x test-requested-outcome
													test-other-outcomes
													test-counts-for-requested-outcome
													test-counts-for-other-outcomes
													test-reasons
													test-results
													test-durations
													test-start-times
													test-end-times
													test-group-name
													test-index))
    	  ((&plist
	    :total-tests actual-total-tests
	    :duration actual-duration
	    :test-start-times actual-test-start-times
	    :test-end-times actual-test-end-times
	    :reasons actual-reasons
	    :results actual-results)
	   actual-stats)
	  (actual-requested-outcome-count (generate--plist-get expected-outcome actual-group-stats)))
    (should (equal actual-requested-outcome-count expected-count-for-requested-outcome))
    (should (equal actual-total-tests expected-total-tests))
    (should (equal actual-duration (-sum test-durations)))
    (should (equal actual-test-start-times test-start-times))
    (should (equal actual-test-end-times test-end-times))
    (should (seq-every-p #'stringp reasons))
    (should (seq-every-p #'ert-test-result-p results))))

(generate-ert-deftest-n-times generate--random-fake-completed-test-groups-con-for-outcome-x ()
  :num-runs 0
  (-let* ((test-requested-outcome (generate--random-ert-test-outcome))
	  (test-other-outcomes (remove requested-outcome generate--DEFAULT-OUTCOMES))
	  ((actual-group-name . actual-stats) (generate--random-fake-completed-test-groups-con-for-outcome-x test-requested-outcome))
    	  ((&plist
	    :total-tests actual-total-tests
	    :duration actual-duration
	    :test-start-times actual-test-start-times
	    :test-end-times actual-test-end-times
	    :reasons actual-reasons
	    :results actual-results)
	   actual-stats))
    (should (and (numberp actual-total-tests) (not (zerop actual-total-tests))))
    (should (and (numberp actual-duration) (not (zerop actual-duration))))
    (should (consp (generate-seq-take-random-value-from-seq actual-test-start-times)))
    (should (consp (generate-seq-take-random-value-from-seq actual-test-end-times)))
    (should (stringp (generate-seq-take-random-value-from-seq reasons)))
    (should (ert-test-result-p (generate-seq-take-random-value-from-seq reasons)))))

(generate-ert-deftest-n-times generate--fake-completed-tests-groups-alist ()
  :num-runs 0
  (-let* ((expected-outcome (generate-map-random-key generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))
	  ((actual-tests-groups-alist actual-count actual-group-names actual-outcome-string-count-pairs) (generate--fake-completed-tests-groups-alist expected-outcome))
	  (actual-random-group-name (generate-seq-take-random-value-from-seq actual-group-names))
	  ((actual-random-outcome actual-random-outcome-count) (generate-seq-take-random-value-from-seq actual-outcome-string-count-pairs))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-random-group-name))
	  ((&plist
	    :total-tests
	    :duration
	    :test-start-times
	    :test-end-times
	    :reasons
	    :results)
	   actual-group-stats)
	  ((actual-random-reason actual-random-result) (mapcar #'generate-seq-take-random-value-from-seq (list results reasons)))
	  (actual-requested-outcome-count (generate--plist-get expected-outcome actual-group-stats)))
    (mapc (lambda (x) (should (g--gt0 x))) (list total-tests duration actual-requested-outcome-count))
    (mapc (lambda (x) (should (listp x))) (list reasons results test-start-times test-end-times))
    (mapc (lambda (x) (should (g--len-gt0 x))) (list reasons results actual-group-names actual-outcome-string-count-pairs))
    (mapc (lambda (x) (should (decode-time (generate-seq-take-random-value-from-seq x)))) (list test-start-times test-end-times))
    (should (stringp actual-random-reason))
    (should (ert-test-result-p actual-random-result))))

(generate-ert-deftest-n-times generate--random-fake-completed-tests-groups-alist ()
  :num-runs 0
  (-let* (((actual-tests-groups-alist actual-total-test-count actual-outcome-string-count-pairs actual-group-names) (generate--random-fake-completed-tests-groups-alist))
	  ((actual-random-outcome actual-random-outcome-count) (generate-seq-take-random-value-from-seq actual-outcome-string-count-pairs))
	  (actual-random-group-name (generate-seq-take-random-value-from-seq actual-group-names))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-random-group-name)))
    (should (plistp actual-group-stats))
    (should (listp actual-group-names))
    (should (g--gt0 actual-total-test-count))
    (should (stringp actual-random-outcome))
    (should (g--gt0 actual-random-count))
    (should (stringp actual-random-group-name))))

(generate-ert-deftest-n-times generate--random-fake-completed-tests-groups-alist-and-stats ()
  :num-runs 0
  (-let* (((actual-tests-groups-alist actual-ert-stats actual-total-test-count actual-outcome-string-count-pairs actual-group-names) (generate--random-fake-completed-tests-groups-alist-and-stats))
	  ((actual-random-outcome actual-random-outcome-count) (generate-seq-take-random-value-from-seq actual-outcome-string-count-pairs))
	  ((actual-group-name . actual-group-stats) (generate-seq-take-random-value-from-seq actual-tests-groups-alist)))
    (should (stringp actual-group-name))
    (should (plistp actual-group-stats))
    (should (ert--stats-p actual-ert-stats))
    (should (not (zerop actual-total-test-count)))
    (should (stringp actual-random-outcome))
    (should (natnump actual-random-count))
    (should (g--len-gt0 actual-group-names))))

(generate-ert-deftest-n-times generate--chop-each-test-name ()
  :num-runs 0
  (-let* (((test-groups test-names) (funcall (-compose (-juxt (-partial #'seq-map-indexed (lambda (x i) (cons (1+ i) x))) #'identity) #'generate-random-list-of-strings)))
	  ((expected-count . expected-test-group) (generate-seq-take-random-value-from-seq test-groups))
	  (expected-random-number (generate--random-nat-number-between-0-and expected-count))
	  (tests (-flatten-n 1 (seq-map #'generate--ert-test-unfolder test-groups)))
	  (actual-name-number-cons (generate--chop-each-test-name tests)))
    (should (-contains-p actual-name-number-cons (cons expected-test-group expected-random-number)))))

(generate-ert-deftest-n-times generate--get-name-count-cons-for-list-of-tests ()
  :num-runs 0
  (-let* (((test-cons test-names) (funcall (-compose (-juxt (-partial #'seq-map-indexed (lambda (x i) (cons (+ i test-count) x))) #'identity) #'generate-random-list-of-strings)))
	  ((expected-count . expected-test-group) (generate-seq-take-random-value-from-seq test-groups))
	  (actual-name-count-cons (generate--get-name-count-cons-for-list-of-tests test-cons))
	  (actual-random-count (map-elt actual-name-count-cons expected-test-group)))
    (should (length= actual-name-count-cons (length test-groups)))
    (should (equal actual-random-count expected-count))))

(generate-ert-deftest-n-times generate--create-tests-groups-alist ()
  :num-runs 0
  (-let* ((test-groups (funcall (-compose (-partial #'seq-map-indexed (lambda (x i) (cons (+ i 1) x))) #'generate-random-list-of-unique-strings)))
	  ((expected-count . expected-test-group) (generate-seq-take-random-value-from-seq test-groups))
	  (tests (-flatten-n 1 (seq-map #'generate--ert-test-unfolder test-groups)))
	  (actual-tests-groups-alist (generate--create-tests-groups-alist tests))
	  ((&plist :total-tests :passed-expected :passed-unexpected :failed-unexpected :skipped :failed-expected :failed-unexpected :duration :reasons :results)
	   (map-elt actual-tests-groups-alist expected-test-group)))
    (mapc (lambda (actual-value) (zerop actual-value)) (list passed-expected passed-unexpected failed-unexpected skipped failed-expected failed-unexpected duration))
    (should (equal total-tests expected-count))
    (should (length= results 0))
    (should (length= reasons 0))))

(generate-ert-deftest-n-times generate--stats ()
  :num-runs 0
  (-let* (((expected-outcome stats-func) (generate-seq-take-random-value-from-seq generate--OUTCOMES-FOR-STATS-TESTS))
	  ((tests-groups-alist expected-count expected-group-names) (generate--fake-completed-tests-groups-alist expected-outcome))
	  (expected-random-test-name (generate-seq-take-random-value-from-seq expected-group-names))
	  ((actual-passed-test-count actual-passed-tests) (funcall stats-func tests-groups-alist)))
  (should (equal actual-passed-test-count expected-count))
  (should (map-elt actual-passed-tests expected-random-test-name))))

(generate-ert-deftest-n-times generate--summary-message ()
  :num-runs 0
  (-let* ((test-zero-strings (generate-random-list-of-strings))
	  (test-nonzero-strings (generate-random-list-of-strings))
	  (test-random-zero-string (generate-seq-take-random-value-from-seq test-zero-strings))
	  (test-random-nonzero-string (generate-seq-take-random-value-from-seq test-nonzero-strings))
	  (test-counts (generate-list-of-nat-numbers :exact-length (length test-nonzero-strings)))
	  (test-zeros-cons (-zip-pair test-zero-strings (make-list (length test-zero-strings) 0)))
	  (test-nonzero-cons (-zip-pair test-nonzero-strings test-counts))
	  (test-values (generate-shuffle-list (map-merge 'alist test-zeros-cons test-nonzero-cons)))
	  (test-initial-message (generate-random-string))
	  (actual-result (generate--summary-message test-initial-message test-values)))
    (should (s-contains-p test-initial-message actual-result))
    (should (s-contains-p test-random-nonzero-string actual-result))))

(generate-ert-deftest-n-times generate--default-message-printer-without-reasons-and-results ()
  :num-runs 0
  (let ((messages)
	(test-verbose-setting (generate-seq-take-random-value-from-seq (list "EMACS_TEST_VERBOSE" ""))))
    (cl-letf* (((symbol-function 'message)
		(lambda (format-string &rest args)
                  (push (apply #'format format-string args) messages)))
	       ((symbol-function 'print)
		(lambda (string)
                  (push string messages)))
	       ((symbol-function 'getenv)
		(lambda (_)
                  test-verbose-setting)))
      (-let* ((testable-outcomes (map-remove (lambda (_ v) (identity (generate--plist-get :with-reasons-and-results v))) generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))
	      ((test-outcome . (&plist :message expected-message)) (generate-seq-take-random-value-from-seq testable-outcomes))
	      ((test-group-con &as expected-name . test-stats) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	      ((&plist :duration expected-duration) test-stats)
	      (actual-message (progn (generate--default-message-printer test-outcome expected-message 'nil test-group-con) (s-join "\n" (reverse messages)))))
	(should (s-contains-p expected-name actual-message))
	(should (s-contains-p (number-to-string expected-duration) actual-message))
	(should (s-contains-p expected-message actual-message))))))

(generate-ert-deftest-n-times generate--default-message-printer-with-result-only ()
  :num-runs 0
  (let ((messages))
    (cl-letf* (((symbol-function 'message)
		(lambda (format-string &rest args)
                  (push (apply #'format format-string args) messages)))
	       ((symbol-function 'print)
		(lambda (string)
                  (push string messages)))
	       ((symbol-function 'getenv)
		(lambda (_)
                  "")))
      (-let* ((testable-outcomes (map-filter (lambda (_ v) (identity (generate--plist-get :with-reasons-and-results v))) generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))
	      ((test-outcome . (&plist :message expected-message)) (generate-seq-take-random-value-from-seq testable-outcomes))
	      ((test-group-con &as expected-name . test-stats) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	      ((&plist :duration expected-duration :reasons expected-reasons) test-stats)
	      (expected-reason (generate-seq-take-random-value-from-seq expected-reasons))
	      (actual-message (progn (generate--default-message-printer test-outcome expected-message 't test-group-con) (s-join "\n" (reverse messages)))))
	(should (s-contains-p expected-name actual-message))
	(should (s-contains-p (number-to-string expected-duration) actual-message))
	(should (s-contains-p expected-reason actual-message))))))

(generate-ert-deftest-n-times generate--default-message-printer-with-reasons-and-results ()
  :num-runs 0
  (let ((messages))
    (cl-letf* (((symbol-function 'message)
		(lambda (format-string &rest args)
                  (push (apply #'format format-string args) messages)))
	       ((symbol-function 'getenv)
		(lambda (_)
                  "EMACS_TEST_VERBOSE")))
      (-let* ((testable-outcomes (map-filter (lambda (_ v) (identity (generate--plist-get :with-reasons-and-results v))) generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))
	      ((test-outcome . (&plist :message expected-message)) (generate-seq-take-random-value-from-seq testable-outcomes))
	      ((test-group-con &as expected-name . test-stats) (generate--random-fake-completed-test-group-con-for-outcome-x test-outcome))
	      ((&plist :duration expected-duration :reasons expected-reasons :results expected-results) test-stats)
	      ((expected-result expected-reason) (mapcar #'generate-seq-take-random-value-from-seq (list expected-reasons expected-results)))
	      (actual-message (progn (generate--default-message-printer test-outcome expected-message 't test-group-con) (s-join "\n" (reverse messages)))))
	(should (s-contains-p expected-name actual-message))
	(should (s-contains-p (number-to-string expected-duration) actual-message))
	(should (s-contains-p expected-message actual-message))
	(should (s-contains-p expected-result actual-message))
	(should (s-contains-p expected-reason actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-started ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-selector (generate-random-string))
	     ((tests-groups-alist test-stats expected-total-test-count _) (generate--fake-fresh-tests-groups-alist-and-stats))
	     (test-event-args (list test-stats))
	     (actual-message (progn (generate--run-tests-batch-handle-run-started test-selector tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p (format "Running %s tests" expected-total-test-count) actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-run-ended ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* (((tests-groups-alist test-stats expected-total-test-count expected-outcome-string-count-pairs _)
	      (generate--random-fake-completed-tests-groups-alist-and-stats))
	      (expected-outcome-strings (funcall (-compose (-partial #'mapcar #'car) (-partial #'map-remove (lambda (x y) (zerop y))))  expected-outcome-string-count-pairs))
	     (test-event-args (list test-stats))
	     (actual-message (progn (generate--run-tests-batch-handle-run-ended tests-groups-alist test-event-args) (apply #'concat (reverse messages)))))
	(should (s-contains-p (format "Ran %s tests" expected-test-count) actual-message))
	(mapc (lambda (expected-outcome-string) (should (s-contains-p expected-outcome-string actual-message))) expected-outcome-strings)))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-ended-no-message ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* ((test-outcome (generate-seq-take-random-value-from-seq (list :passed-expected :failed-expected :skipped)))
	      ((tests-groups-alist test-stats test _ _)
	       (generate--random-fake-tests-groups-alist-stats-and-test test-outcome))
	      (test-event-args (list test-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-started test-selector tests-groups-alist test-event-args test) (apply #'concat (reverse messages)))))
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

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-ended-passed-unexpected ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* (((tests-groups-alist test-stats test _ _)
	       (generate--random-fake-tests-groups-alist-stats-and-test :aborted))
	      (test-event-args (list test-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-started test-selector tests-groups-alist test-event-args test) (apply #'concat (reverse messages)))))
	(should (s-contains-p "aborted with non-local exit" actual-message))))))

(generate-ert-deftest-n-times generate--run-tests-batch-handle-test-ended-passed-unexpected ()
  :num-runs 0
  (let ((messages))
    (cl-letf (((symbol-function 'message) (lambda (format-strings &rest args)
					    (push (apply #'format format-string args) messages))))
      (-let* (((tests-groups-alist test-stats test _ _)
	       (generate--random-fake-tests-groups-alist-stats-and-test :quit))
	      (test-event-args (list test-stats))
	      (actual-message (progn (generate--run-tests-batch-handle-run-started test-selector tests-groups-alist test-event-args test) (apply #'concat (reverse messages)))))
	(should (s-contains-p "Quit during" actual-message))))))

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
