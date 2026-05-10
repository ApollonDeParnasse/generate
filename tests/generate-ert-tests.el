;;; generate-ert-tests.el --- Tests for generators of ert objects  -*- lexical-binding: t; -*-

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

(require 'dash)
(require 'generate)
(require 'ert)

(generate-ert-deftest-n-times generate-ert-test ()
  :num-runs 100
  (let* ((test-group-name (generate-random-word))
	 (test-number (generate-random-nat-number))
	 (test-name (format "%s-%s-%s" test-group-name generate--TEST-IDENTIFIER test-number))
	 (actual-test (generate-ert-test test-name)))
    (should (ert-test-p actual-test))
    (should (string-equal (symbol-name (ert-test-name actual-test)) test-name))))

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

(generate-ert-deftest-n-times generate--ert-test-failed-error ()
  :num-runs 100
  (-let* ((test-should (generate-random-failing-should))
	 ((actual-error actual-val) (generate--ert-test-failed-error test-should)))
    (should (equal actual-error 'ert-test-failed))
    (should (symbolp (caar actual-val)))))

(generate-ert-deftest-n-times generate--ert-test-failed-condition ()
  :num-runs 100
  (-let* ((test-should (generate-random-failing-should))
	 ((actual-error _) (generate--ert-test-failed-condition test-should)))
    (should (symbolp actual-error))))

(generate-ert-deftest-n-times generate--ert-test-skipped-condition ()
  :num-runs 100
  (-let* ((test-should (generate-random-failing-should))
	 ((actual-condition-symbol actual-val) (generate--ert-test-skipped-condition test-should)))
    (should (equal actual-condition-symbol 'ert-test-skipped))
    (should (symbolp (caar actual-val)))))

(generate-ert-deftest-n-times generate-ert-test-result-object-passed-expected-unexpected ()
  :num-runs 100
  (let* ((test-duration (generate-random-nat-number))
	 (test-outcome (generate-seq-take-random-value-from-seq (list :passed-expected :passed-unexpected)))
	 (actual-ert-test-result (generate-ert-test-result-object test-outcome test-duration)))
    (should (ert-test-result-type-p actual-ert-test-result :passed))
    (should (stringp (ert-test-result-messages actual-ert-test-result)))
    (should (ert-test-result-should-forms actual-ert-test-result))
    (should (numberp (ert-test-result-duration actual-ert-test-result)))))

(generate-ert-deftest-n-times generate-ert-test-result-object-failed-skipped ()
  :num-runs 100
  (-let* ((test-duration (generate-random-nat-number))
	  (test-outcome (generate-seq-take-random-value-from-seq (list :failed-unexpected :failed-expected :skipped)))
	  ((expected-condition-symbol expected-result-type) (generate--plist-get test-outcome (list :failed-unexpected (list 'ert-test-failed :failed)
								:failed-expected (list 'ert-test-failed :failed)
								:skipped (list 'ert-test-skipped :skipped))))
	  (actual-ert-test-result (generate-ert-test-result-object test-outcome test-duration)))
    (should (ert-test-result-type-p actual-ert-test-result expected-result-type))
    (should (stringp (ert-test-result-with-condition-messages actual-ert-test-result)))
    (should (equal (ert-test-result-with-condition-duration actual-ert-test-result) test-duration))
    (should (symbolp (car (ert-test-result-with-condition-condition actual-ert-test-result))))
    (should (backtrace-frame-p (generate-seq-take-random-value-from-seq (ert-test-result-with-condition-backtrace actual-ert-test-result))))))

(generate-ert-deftest-n-times generate--plist-of-ert-test-result-objects ()
  :num-runs 100
  (-let* ((test-durations (generate-list-of-nat-numbers :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-duration-pairs (-zip-lists generate--DEFAULT-OUTCOMES test-durations))
	  (actual-plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs)))
    (should (ert-test-result-p (car (generate-map-random-value actual-plist-of-ert-test-result-objects))))))

(generate-ert-deftest-n-times generate--take-from-plist-of-ert-test-results ()
  :num-runs 100
  (-let* ((test-durations (generate-list-of-nat-numbers :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-counts (generate--list-of-n-nat-numbers-in-range-10 :exact-length (length generate--DEFAULT-OUTCOMES)))
	  (test-outcome-duration-pairs (-zip-lists generate--DEFAULT-OUTCOMES test-durations))
	  (test-outcome-counts-plist (-interleave generate--DEFAULT-OUTCOMES test-outcome-counts))
	  (test-plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs))
	  (test-results (generate--take-from-plist-of-ert-test-results test-plist-of-ert-test-result-objects test-outcome-counts-plist))
	  (expected-length (funcall (-compose #'-sum #'map-values) test-outcome-counts-plist)))
    (should (length= test-results expected-length))
    (should (ert-test-result-p (generate-seq-take-random-value-from-seq test-results)))))

;;; generate-ert-tests.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
