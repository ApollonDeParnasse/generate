;;; generate.el --- Random testing for Emacs Lisp -*- lexical-binding: t; no-byte-compile: t -*-

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

;; Generate random data.

;;; Code:

(require 'generate-file-extensions)
(require 'ert)
(require 'seq)
(require 'map)
(require 'thunk)
(require 'time-date)
(require 'calc-comb)
(require 'calc-prog)
(require 'color)
(require 'org)
(require 'org-table)
(require 'cl-lib)
(require 'gv)
(require 'dash)
(require 's)

(defvar generate--FILE-EXTENSIONS)
(defconst generate--TEST-IDENTIFIER
  "gen-ert")
(defconst generate--DEFAULT-OUTCOMES
  (list :passed-expected :passed-unexpected :skipped :failed-unexpected :failed-expected))

(defconst generate--EXPECTED-RESULT-TYPES
  (list ':passed ':skipped ':failed))

(defconst generate--DEFAULT-OUTCOMES-PLIST
  (list :passed-expected
	(list :exclusive 't :expectedp 't :expected-result-type ':passed :summary-message (cl-constantly "passed as expected") :breakdown-message "Passed as expected" :slot #'ert--stats-passed-expected :compatible ':failed-unexpected :test-outcome-sign "✓")
	:passed-unexpected
	(list :exclusive 'nil :expectedp 'nil :expected-result-type ':failed :summary-message (cl-constantly "passed unexpectedly") :breakdown-message "Passed unexpectedly" :slot #'ert--stats-passed-unexpected :compatible ':failed-expected :test-outcome-sign "×")
	:skipped
	(list :exclusive 't :expectedp 't :expected-result-type ':skipped :summary-message (lambda (count) (if (> count 1) "were skipped" "was skipped")) :breakdown-message "Skipped" :slot #'ert--stats-skipped :compatible ':skipped :test-outcome-sign "↓")
	:failed-expected
	(list :exclusive 't :expectedp 't :expected-result-type ':failed :summary-message (cl-constantly "failed as expected") :breakdown-message "Failed as expected" :slot #'ert--stats-failed-expected :compatible ':passed-unexpected :test-outcome-sign "✓")
	:failed-unexpected
	(list :exclusive 'nil :expectedp 'nil :expected-result-type ':passed :summary-message (cl-constantly "failed unexpectedly") :breakdown-message "Failed unexpectedly" :slot #'ert--stats-failed-unexpected :compatible ':passed-expected :test-outcome-sign "×")))

(defconst generate--TEST-GROUPS-PLIST
  (list
   :total-tests 0
   :completed-tests 0
   :passed-expected 0
   :passed-unexpected 0
   :skipped 0
   :failed-expected 0
   :failed-unexpected 0
   :test-results nil
   :duration 0
   :test-start-times nil
   :test-end-times nil
   ;; for testing purposes only
   ;; otherwise,this key is never referenced
   :expected-result-type ':passed))

(defconst generate--PREDICATES
  (list
   #'numberp
   #'integerp
   #'floatp
   #'stringp
   #'listp
   #'consp
   #'mapp
   #'seqp
   #'characterp
   #'booleanp))

(defconst generate--SECONDS-IN-AN-HOUR
  (* 60 60))
(defconst generate--SECONDS-IN-A-DAY
  (* 60 60 24))
(defconst generate--SECONDS-IN-A-MONTH
  (* 60 60 24 30))
(defconst generate--SECONDS-IN-A-YEAR
  (* 60 60 24 30 12))

(defconst generate--FIVERANGE
  (list 1 5))
(defconst generate--ZEROTENRANGE
  (list 0 10))
(defconst generate--TENRANGE
  (list 1 10))
(defconst generate--ZEROTONINE
  (list 0 9))
(defconst generate--UPPERALPHA
  (list 65 90) "All uppercase numbers of the alphabet.")
(defconst generate--LOWERALPHA
  (list 97 122) "All lowercase numbers of the alphabet.")
(defconst generate--ZEROTOSIXTY
  (list 0 60))
(defconst generate--ONETOTHIRTEEN
  (list 1 13))
(defconst generate--ZEROTOTWENTYFOUR
  (list 0 24))
(defconst generate--THREETOTWENTY
  (list 3 20))
(defconst generate--YEARRANGE
  (list 1960 3000))
(defconst generate--ONETOTWENTYFIVE
  (list 1 25))
(defconst generate--THREETOTWENTYFIVE
  (list 3 25))
(defconst generate--TWOTOTWENTYFIVE
  (list 2 25))
(defconst generate--FIVETOTWENTYFIVERANGE
  (list 5 25))
(defconst generate--FIFTYRANGE
  (list 1 50))
(defconst generate--DEFAULTRANDOMNUMBERRANGE
  (list 1 255))
(defconst generate--FIVEHUNDREDRANGE
  (list 1 500))
(defconst generate--NEGATIVENUMS
  (list most-negative-fixnum -1) "Negative numbers range.")
(defconst generate--NATURALNUMBERS
  (list 1 most-positive-fixnum))
(defconst generate--TRILLION
  1000000000000)
(defconst generate--PUNCTUATION
  (list "," ":" "." ";" "/" "-"))

(defgroup generate nil
  "Random generators for testing."
  :tag "generate"
  :group 'lisp)

(defcustom generate-lisp-timestamp-range-size generate--SECONDS-IN-AN-HOUR
  "The size of the range from which random timestamps will be taken."
  :group 'generate
  :type 'natnum)

(defalias 'generate--gte-one (-rpartial #'>= 1) "greater-than-or-equal 1?")
(defalias 'generate--gte-zero (-rpartial #'>= 0) "greater-than-or-equal 0?")
(defalias 'generate--lt0 (-rpartial #'<= 0) "less-than-or-equal 0?")
(defalias 'generate--gt0 (-rpartial #'> 0) "greater-than-or-equal 0?")
(defalias 'generate--gt1 (-rpartial #'> 1) "greater-than-or-equal 1?")
(defalias 'generate--equal-zero (apply-partially #'eql 0) "equal 0?")
(defalias 'generate--equal-one (apply-partially #'eql 1) "equal 1?")
(defalias 'generate--not-equal (-not #'equal) "not equal?")
(defalias 'generate--len-gt0 (-rpartial #'length> 0) "less-than-or-equal 0?")

(defun generate-nth-mod (n list &optional delta)
  "Return the N + DELTA element of LIST.
N counts from zero.  Thanks to mod, a value will always be returned
even if N is greater than the length of LIST."
  (declare (side-effect-free t))
  (nth (% (+ n (or delta 1)) (length list)) list))

(defmacro generate--plural! (macro args)
  "Use ARGS to create a plural verson of MACRO."
  `(progn
     ,@(seq-map (lambda (p) `(,macro ,p))
	     (symbol-value args))))

(defun generate--plist-get (prop plist)
  "Extract value of PROP from PLIST.
This is just `plist-get' with the
comparison function always set to `equal'."
  (declare (side-effect-free t))
  (plist-get plist prop #'equal))

(defun generate--plist-put (prop val plist)
  "Change value in PLIST of PROP to VAL.
This is just `plist-put' with the
comparison function always set to `equal'."
  (declare (side-effect-free t))
  (let ((new-plist (copy-sequence plist)))
    (plist-put new-plist prop val #'equal)))

;; code was copied from gv
(gv-define-expander generate--plist-get
  (lambda (do prop plist)
    (macroexp-let2 macroexp-copyable-p key prop
      (gv-letplace (getter setter) plist
        (macroexp-let2 nil p `(cdr (plist-member ,getter ,key))
          (funcall do
                   `(car ,p)
                   (lambda (val)
                     `(if ,p
                          (setcar ,p ,val)
                        ,(funcall setter
                                  `(cons ,key (cons ,val ,getter)))))))))))

(defalias 'generate--iterate-plus-one  (-partial #'-iterate #'1+))
(defalias 'generate--applify-iterate-plus-one  (-applify #'generate--iterate-plus-one))
(defalias 'generate--applify-rpartial (-applify #'-rpartial))
(defalias 'generate--applify-partial (-applify #'-partial))
(defalias 'generate--applify-subtract (-applify #'-))
(defalias 'generate--applify-multiply (-applify #'*))
(defalias 'generate--applify-divide (-applify #'/))
(defalias 'generate--applify-zip  (-applify #'-zip))
(defalias 'generate--applify-zip-pair  (-applify #'-zip-pair))
(defalias 'generate--applify-cons  (-applify #'cons))
(defalias 'generate--applify-concat  (-applify #'concat))
(defalias 'generate--applify-vconcat  (-applify #'vconcat))
(defalias 'generate--applify-mapcar  (-applify #'mapcar))
(defalias 'generate--applify-seq-split (-applify #'seq-split))
(defalias 'generate--applify-seq-take (-applify #'seq-take))
(defalias 'generate--applify-vector (-applify #'vector))
(defalias 'generate--applify-map-elt (-applify #'map-elt))
(defalias 'generate--seq-elt-flipped (-flip #'seq-elt))
(defalias 'generate--applify-seq-elt-flipped (-applify #'generate--seq-elt-flipped))
(defalias 'generate--identity-and-seq-length (-juxt #'identity #'seq-length))

(defun generate--times-helper (func n call-num)
  "Unless CALL-NUM is equal to N, call FUNC with CALL-NUM as arg."
  (unless (equal call-num n)
    (cons (funcall func call-num) (1+ call-num))))

(defun generate--times (n func)
  "Call FUNC N times and collect the results into an array.
Each function call will receive the current call number as its argument."
  (-unfold (apply-partially #'generate--times-helper func n) 0))

(defun generate--times-no-args (n func)
  "Call a FUNC N times with no args and collect the results into an array."
  (generate--times n (lambda (_) (funcall func))))

(defalias 'generate--times-no-args-twice (apply-partially #'generate--times-no-args 2) "Call FUNC twice.

\(fn FUNC)")

(cl-defun generate--zip-pair-longest-helper ((order-short short-list))
  "Helper function for `generate--zip-pair-longest'.
Uses ORDER-SHORT in order to determine if the shorter
list is the first list or the second list.
Returns a closure that be used to zip SHORT-LIST with LONG-LIST."
  (if (zerop order-short)
      (lambda (long-x index)
	(let ((short-x (generate-nth-mod index short-list)))
	  (cons short-x long-x)))
    (lambda (long-x index)
      (let ((short-x (generate-nth-mod index short-list)))
	(cons long-x short-x)))))

(defun generate--zip-pair-longest (list-one list-two)
  "Zip LIST-ONE and LIST-TWO together.

Make a pair with the head of each list, followed by a pair with
the second element of each list, and so on.  The number of pairs
returned is equal to the length of the longest input list."
  (thunk-let* ((length-one (length list-one))
	       (length-two (length list-two))
	       (sorted-lists (if (g--gt length-one length-two) (list (list 0 list-one) (list 1 list-two)) (list (list 1 list-two) (list 0 list-one))))
	       (longest-list (cadr (car sorted-lists)))
	       (shortest-list (cadr sorted-lists)))
    (if (equal length-one length-two)
	(-zip-pair list-one list-two)
      (seq-map-indexed (generate--zip-pair-longest-helper shortest-list) longest-list))))

(defun generate--zip-pair-first (list-one list-two)
  "Zip LIST-ONE and LIST-TWO together.

Make a pair with the head of each list,
followed by a pair with the second element of
each list, and so on.  The number of pairs
returned is equal to the length of LIST-ONE."
  (declare (pure t) (side-effect-free t))
  (-let* (((length-one length-two) (mapcar #'length (list list-one list-two))))
    (if (<= length-one length-two)
	(-zip-pair list-one list-two)
      (seq-map-indexed (generate--zip-pair-longest-helper (list 1 list-two)) list-one))))

(defun generate--collect-keywords (keys-and-body collection)
  "Helper function for `generate--parse-keys-and-body'.
Converts takes key-value pairs from
KEYS-AND-BODY and adds them to COLLECTION."
  (let ((first (car keys-and-body))
	(second (cadr keys-and-body))
	(rest (cddr keys-and-body)))
    (cond
     ((and (not first) (not second)) collection)
     ((and (keywordp first) (not second)) (error "Value expected after keyword %S in %S"
						 first keys-and-body))
     ((and (keywordp first) second) (generate--collect-keywords rest (generate--plist-put first second collection)))
     ((and first (not second)) (generate--plist-put :body first collection))
     ((and first second) (error "Not sure what you did here %S"
 				keys-and-body)))))

(defun generate--parse-keys-and-body (docstring-keys-and-body)
  "Converts DOCSTRING-KEYS-AND-BODY into a plist."
  (-let (((documentation keys-and-body) (if (stringp (car docstring-keys-and-body))
					    (list (list :documentation (car docstring-keys-and-body)) (cdr docstring-keys-and-body))
					  (list (list :documentation 'nil) docstring-keys-and-body))))
    (generate--collect-keywords keys-and-body documentation)))

;;;###autoload
(cl-defmacro generate-ert-deftest-n-times (name () &body docstring-keys-and-body)
  "Define NAME (a symbol) as a `ert-deftest' n times where n = NUM-RUNS.
NUM-RUNS can be specified as a keyword argument in addition to
the normal values of DOCSTRING-KEYS-AND-BODY.
If NUM-RUNS is not specified, your test will be defined 100 times.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] [:num-runs INTEGER] BODY...)"
  (declare (debug (&define [&name "test@" symbolp]
			   sexp [&optional stringp]
			   [&rest keywordp sexp]
			   def-body))
	   (doc-string 3)
	   (indent 2))
  (let ((run-symbol (gensym)))
    (cl-destructuring-bind
	(&key
	 (documentation nil documentation-supplied-p)
	 (expected-result nil expected-result-supplied-p)
	 (tags nil tags-supplied-p)
	 (num-runs 100)
	 (body nil))
	(generate--parse-keys-and-body docstring-keys-and-body)
      `(cl-macrolet ((skip-when (form) `(ert--skip-when ,form))
		     (skip-unless (form) `(ert--skip-unless ,form)))
	 (dotimes (run-symbol ,num-runs)
	   (ert-set-test (intern (format "%s-%s-%s" ',name generate--TEST-IDENTIFIER run-symbol))
			 (make-ert-test
			  :name (intern (format "%s-%s-%s" ',name generate--TEST-IDENTIFIER run-symbol))
			  ,@(when documentation-supplied-p
			      `(:documentation ,documentation))
			  ,@(when expected-result-supplied-p
			      `(:expected-result-type ,expected-result))
			  ,@(when tags-supplied-p
			      `(:tags ,tags))
			  :body (lambda () ,body nil)
			  :file-name ,(or (macroexp-file-name) buffer-file-name))))))))

(defun generate--get-ert-outcome-attribute (constant attribute)
  "Returns a closure that returns the value of ATTRIBUTE from CONSTANT for OUTCOME."
  (lambda (outcome)
    (:documentation (format "Returns the value of %s for OUTCOME." attribute))
    (funcall (-compose (apply-partially #'generate--plist-get attribute) (-rpartial #'generate--plist-get constant)) outcome)))

(defalias 'generate--get-ert-outcome-summary-message-function (generate--get-ert-outcome-attribute generate--DEFAULT-OUTCOMES-PLIST :summary-message))
(defalias 'generate--get-ert-outcome-breakdown-message (generate--get-ert-outcome-attribute generate--DEFAULT-OUTCOMES-PLIST :breakdown-message))
(defalias 'generate--get-ert-outcome-slot-func (generate--get-ert-outcome-attribute generate--DEFAULT-OUTCOMES-PLIST :slot))
(defalias 'generate--get-expected-result-type (generate--get-ert-outcome-attribute generate--DEFAULT-OUTCOMES-PLIST :expected-result-type))
(defalias 'generate--get-compatible-outcome (generate--get-ert-outcome-attribute generate--DEFAULT-OUTCOMES-PLIST :compatible))

(defun generate--get-group-name-and-index-for-test-base (test-identifier)
  "Uses TEST-IDENTIFIER to get the name and index of a given TEST."
  (lambda (test)
    "Returns the name and index of a given TEST."
    (let* ((test-name (symbol-name (ert-test-name test)))
	   (name-end-index (1- (s-index-of test-identifier test-name)))
	   (test-number-start-index (+ name-end-index (length test-identifier) 2))
	   (name (substring test-name 0 name-end-index))
	   (test-number (substring test-name test-number-start-index)))
      (cons name (string-to-number test-number)))))

(defalias 'generate--get-group-name-and-index-for-test (generate--get-group-name-and-index-for-test-base generate--TEST-IDENTIFIER)
  "Default implementation of generate--get-group-name-and-index-for-test.")

(defalias 'generate--get-group-name-and-index-for-each-test (apply-partially #'mapcar #'generate--get-group-name-and-index-for-test)  "Converts of a list of TEST-IDENTIFIERS into a list of test group symbols.

\(fn LIST)")

(cl-defun generate--get-name-count-cons-for-list-of-tests-helper ((name . vals))
  "Helper function for `generate--get-name-count-cons-for-list-of-tests'.
Get the max test index for the group from VALS.
Returns a cons cell with NAME as the car
and the test count for the group as the cdr."
  (let* ((counts (mapcar #'cdr vals)))
    (cons name (1+ (-max counts)))))

(defun generate--get-name-count-cons-for-list-of-tests (cons)
  "Converts CONS into a list of name-count con cells."
  (let* ((groups (-group-by #'car cons)))
    (mapcar #'generate--get-name-count-cons-for-list-of-tests-helper groups)))

(defun generate--create-test-group-con (stats-plist key-to-set)
  "Returns a closure that can be used to create a new test-group-con.
STATS-PLIST will be the cdr of the test-group-con.
KEY-TO-SET will receive the value of the total tests
for the new test-group-con."
  (-lambda ((name . count))
    (cons name (generate--plist-put key-to-set count stats-plist))))

(defun generate--create-tests-groups-alist-base (stats-plist key-to-set tests)
  "Creates a new a tests-groups-alist for a test run.
Test group symbols and counts are extracted from the values of
TESTS.  Those values are used to fill STATS-PLIST, an object
that will hold the statistics of generate test run.
KEY-TO-SET is the key that will hold the total tests
for each test group."
  (let* ((split-tests (generate--get-group-name-and-index-for-each-test tests))
	 (name-count-cons (generate--get-name-count-cons-for-list-of-tests split-tests)))
    (mapcar (generate--create-test-group-con stats-plist key-to-set) name-count-cons)))

(defalias 'generate--create-tests-groups-alist (apply-partially #'generate--create-tests-groups-alist-base generate--TEST-GROUPS-PLIST :total-tests)
  "Default implementaiton of generate--create-tests-groups-alist.")

(defun generate--creates-stats-predicate (list-of-outcomes outcome exclusivep _ test-group-plist)
  "Helper function for `generate--stats-default'.
Retrieves the value of OUTCOME from TEST-GROUP-PLIST.  When EXCLUSIVEP is t,
the count values of each other in LIST-OF-OUTCOMES are also retrieved.
Returns t when the count for an OUTCOME is greater than zero
if the OUTCOME is not exclusive or the OUTCOME is exclusive
and the exclusivity requirement was met."
  (thunk-let* ((outcome-value (generate--plist-get outcome test-group-plist))
	       (other-outcomes (-remove (apply-partially #'equal outcome) list-of-outcomes))
	       (other-outcome-values (mapcar (lambda (other-outcome) (generate--plist-get other-outcome test-group-plist)) other-outcomes))
	       (exclusive-check (if exclusivep (seq-every-p (apply-partially #'equal 0) other-outcome-values) 't)))
    (and (g--gt0 outcome-value) exclusive-check)))


(defun generate--stats-default (list-of-outcomes outcome exclusivep tests-groups-alist)
  "Returns a con cell with the stats of a given OUTCOME for a TESTS-GROUPS-ALIST.
When EXCLUSIVEP is t, a given test will be only be counted for OUTCOME
if every single one of its results matches OUTCOME.
In other words, for a given test, none of the other outcomes in LIST-OF-OUTCOMES
can have a value greater than zero for a given test-group, if EXCLUSIVEP is t."
  (let ((result (map-filter (apply-partially #'generate--creates-stats-predicate list-of-outcomes outcome exclusivep) tests-groups-alist)))
    (cons outcome (length result))))

(defalias 'generate--stats (apply-partially #'generate--stats-default generate--DEFAULT-OUTCOMES) "Default implementation of `generate--stats-default'.
Returns a con cell with the stats of a given OUTCOME
for a TESTS-GROUPS-ALIST.  When EXCLUSIVEP is t,
a given test will be only be counted for OUTCOME
if every single one of its results matches OUTCOME.

\(fn ERT-OUTCOME EXCLUSIVEP TESTS-GROUPS-ALIST)")

(defalias 'generate--stats-passed-expected (apply-partially #'generate--stats :passed-expected 't) "Returns a con cell with the count of test-groups that passed as expected.

\(fn TESTS-GROUPS-ALIST)")

(defalias 'generate--stats-failed-expected (apply-partially #'generate--stats :failed-expected 't) "Returns a con cell with the count of test-groups that failed as expected.

\(fn TESTS-GROUPS-ALIST)")

(defalias 'generate--stats-skipped (apply-partially #'generate--stats :skipped 't) "Returns a con cell with the count of test-groups that were skipped.

\(fn TESTS-GROUPS-ALIST)")

(defalias 'generate--stats-failed-unexpected (apply-partially #'generate--stats :failed-unexpected 'nil) "Returns a con cell with the count of test-groups that failed unexpectedly.

\(fn TESTS-GROUPS-ALIST)")

(defalias 'generate--stats-passed-unexpected (apply-partially #'generate--stats :passed-unexpected 'nil) "Returns a con cell with the count of test-groups that passed unexpectedly.

\(fn TESTS-GROUPS-ALIST)")

(defalias 'generate--create-final-test-stats (-juxt #'generate--stats-passed-expected
						     #'generate--stats-failed-expected
						     #'generate--stats-skipped
						     #'generate--stats-failed-unexpected
						     #'generate--stats-passed-unexpected)
  "Returns an with the test stats of a test run.

\(fn TESTS-GROUPS-ALIST)")

(defsubst generate--print-unexpected-outcome-message-for-test-group (default-outcomes-plist test-outcome test-group-stats test-name total-tests duration)
  (-let* ((sign (generate--plist-get :test-outcome-sign (generate--plist-get test-outcome default-outcomes-plist)))
	  (expected-outcome (generate--plist-get :compatible (generate--plist-get test-outcome default-outcomes-plist)))
	  (expected-outcome-count (generate--plist-get expected-outcome test-group-stats)))
    (generate--print-expected-outcome-message-for-test-group default-outcomes-plist expected-outcome test-name expected-outcome-count total-tests duration sign)))

(cl-defsubst generate--print-expected-outcome-message-for-test-group (default-outcomes-plist test-outcome test-name outcome-count total-tests duration &optional sign)
  (let* ((outcome-attributes (generate--plist-get test-outcome default-outcomes-plist))
	 (outcome-string (funcall (generate--plist-get :summary-message outcome-attributes) outcome-count))
	 (outcome-sign (or sign (generate--plist-get :test-outcome-sign outcome-attributes))))
    (message "%s %s > %s/%s %s %s (%f sec)" outcome-sign test-name outcome-count total-tests (if (> outcome-count 1) "tests" "test") outcome-string duration)))

(defun generate--print-final-test-group-stats-base (default-outcomes-plist)
  (lambda (test-group-stats test-name)
    (-let* ((alist (list (cons test-name test-group-stats)))
	    (zipped-outcomes (generate--create-final-test-stats alist))
	   ((&plist :total-tests :duration) test-group-stats)
	   ((test-outcome . _) (car (map-filter (lambda (_ val) (not (zerop val))) zipped-outcomes)))
	   (expectedp (generate--plist-get :expectedp (generate--plist-get test-outcome default-outcomes-plist))))
      (if expectedp
	  (generate--print-expected-outcome-message-for-test-group default-outcomes-plist test-outcome test-name total-tests total-tests duration)
	(generate--print-unexpected-outcome-message-for-test-group default-outcomes-plist test-outcome test-group-stats test-name total-tests duration)))))

(defalias 'generate--print-final-test-group-stats (generate--print-final-test-group-stats-base generate--DEFAULT-OUTCOMES-PLIST))

(defun generate--maybe-print-final-group-stats (test-group-stats test-name)
  (-when-let* (((&plist :total-tests :completed-tests) test-group-stats)
	       (done (equal total-tests completed-tests)))
    (generate--print-final-test-group-stats test-group-stats test-name)))

(defun generate--print-test-run-stats-base (default-outcomes-plist)
  "Print the final statistics for a test run.
DEFAULT-OUTCOMES-PLIST should be a plist
where each key is an ert-outcome.  Each value should
be matched with a plist that contains
the key breakdown-message."
  (lambda (initial-message tests-groups-alist)
    (let* ((zipped-outcomes (generate--create-final-test-stats tests-groups-alist))
	   (stat-messages (map-apply (lambda (key val) (cons (generate--plist-get :breakdown-message (generate--plist-get key default-outcomes-plist)) val)) zipped-outcomes)))
      (message "%s" (generate--summary-message initial-message stat-messages)))))

(defalias 'generate--print-test-run-stats (generate--print-test-run-stats-base generate--DEFAULT-OUTCOMES-PLIST))

(cl-defun generate--summary-message-helper (summary-message (outcome-string . outcome-total))
  "Helper function for `generate--summary-message'.
OUTCOME-STRING will be concatenated to SUMMARY-MESSAGE if
OUTCOME-TOTAL is greater than 0."
  (if (not (zerop outcome-total))
      (concat summary-message "\n" (format "%s  %s" outcome-string outcome-total))
    summary-message))

(defun generate--summary-message (initial-value zipped-outcomes)
  "Returns the summary message of a test run report.
INITIAL-VALUE will be the very first line of the report.
ZIPPED-OUTCOMES should be a list of con cells
where each key is an ert-outcome and each value is the corresponding
count of tests that ended with that outcome."
  (seq-reduce #'generate--summary-message-helper zipped-outcomes initial-value))

(cl-defun generate--print-messages-for-unexpected-outcomes (test result)
  "Print messages for unexpected test results.
RESULT is used to determine the
condition of TEST.  Backtrace
will be printed for TEST
if the test failed unexpectedly.
This was lifted directly from
`generate-run-tests-batch'."
  (unless (ert-test-result-expected-p test result)
    (cl-etypecase result
      (ert-test-passed
       (message "Test %S passed unexpectedly" (ert-test-name test)))
      (ert-test-result-with-condition
       (message "Test %S backtrace:" (ert-test-name test))
       (with-temp-buffer
	 (let ((backtrace-line-length
		(if (eq ert-batch-backtrace-line-length t)
                    backtrace-line-length
                  ert-batch-backtrace-line-length))
               (print-level ert-batch-print-level)
               (print-length ert-batch-print-length))
           (insert (backtrace-to-string
                    (ert-test-result-with-condition-backtrace result))))
	 (if (not ert-batch-backtrace-right-margin)
             (message "%s"
                      (buffer-substring-no-properties (point-min)
                                                      (point-max)))
           (goto-char (point-min))
           (while (not (eobp))
             (let ((start (point))
                   (end (line-end-position)))
               (setq end (min end
                              (+ start
				 ert-batch-backtrace-right-margin)))
               (message "%s" (buffer-substring-no-properties
                              start end)))
             (forward-line 1))))
       (with-temp-buffer
	 (ert--insert-infos result)
	 (insert "    ")
	 (let ((print-escape-newlines t)
               (print-level ert-batch-print-level)
               (print-length ert-batch-print-length))
           (ert--pp-with-indentation-and-newline
            (ert-test-result-with-condition-condition result)))
	 (goto-char (1- (point-max)))
	 (cl-assert (looking-at "\n"))
	 (delete-char 1)
	 (message "Test %S condition:" (ert-test-name test))
	 (message "%s" (buffer-string))))
      (ert-test-aborted-with-non-local-exit
       (message "Test %S aborted with non-local exit"
		(ert-test-name test)))
      (ert-test-quit
       (message "Quit during %S" (ert-test-name test))))))

(defun generate--create-test-result-key (expected-result matches-expected-result)
  (pcase-exhaustive (list expected-result matches-expected-result)
    (`(:passed t) :passed-expected)
    (`(:failed t) :failed-expected)
    (`(:passed nil) :passed-unexpected)
    (`(:failed nil) :failed-expected)
    (`(:skipped ,_) :skipped)))

(defun generate--run-tests-batch-handle-run-started (selector tests-groups-alist event-args)
  (cl-destructuring-bind (stats) event-args
    (message "Running %s tests (%s, selector `%S')"
	     (length tests-groups-alist)
	     (ert--format-time-iso8601 (ert--stats-start-time stats))
	     selector)))

(cl-defun generate--run-tests-batch-handle-test-ended (tests-groups-alist (stats test result))
  (generate--maybe-print-backtrace stats test result)
  (-let* ((test-name (ert-test-name test))
	 ((test-group-name) (generate--get-group-name-and-index-for-test test))
	 (test-absolute-index (map-elt (ert--stats-test-map stats) test-name))
	 (test-start-time (seq-elt (ert--stats-test-start-times stats) test-absolute-index))
	 (test-end-time (seq-elt (ert--stats-test-end-times stats) test-absolute-index))
	 (test-duration (ert-test-result-duration result))
	 (expected-result (ert-test-expected-result-type test))
	 (matches-expected-result (ert-test-result-expected-p test result))
	 (test-result-key (generate--create-test-result-key expected-result matches-expected-result)))
    (cl-incf (generate--plist-get test-result-key (map-elt tests-groups-alist test-group-name)))
    (cl-incf (generate--plist-get :completed-tests (map-elt tests-groups-alist test-group-name)))
    (cl-incf (generate--plist-get :duration (map-elt tests-groups-alist test-group-name)) test-duration)
    (push result (generate--plist-get :test-results (map-elt tests-groups-alist test-group-name)))
    (push test-start-time (generate--plist-get :test-start-times (map-elt tests-groups-alist test-group-name)))
    (push test-end-time (generate--plist-get :test-end-times (map-elt tests-groups-alist test-group-name)))
    (generate--maybe-print-final-group-stats (map-elt tests-groups-alist test-group-name) test-group-name)))

(defun generate--run-tests-batch-handle-run-ended (tests-groups-alist event-args)
  (-let* (((stats abortedp) event-args)
	  (duration (generate--time-diff (ert--stats-end-time stats) (ert--stats-start-time stats)))
	  (start-time (format-time-string "%T" (ert--stats-start-time stats)))
	  (start-at-message (format "Start at  %s" start-time))
	  (duration-message (format "Duration  %s" duration))
	  (total-tests-message (format "Total tests  %s" (length tests-groups-alist)))
	  (breakdown-message "\nBreakdown:\n")
	  (initial-message (s-join "\n" (list start-at-message duration-message total-tests-message breakdown-message))))
    (if abortedp
	(message "Aborted")
      (generate--print-test-run-stats initial-message tests-groups-alist))))

(defun generate--create-run-tests-batch-listener (selector tests-groups-alist)
  "Creates a closure that will run the tests specified by SELECTOR.

SELECTOR selects which tests to run as described in `ert-select-tests' when
called with its second argument t, except if SELECTOR is nil, in which case
all tests rather than none will be run.  TESTS-GROUPS-ALIST
contains the test-groups that will be ran."
  (lambda (event-type &rest event-args)
    (pcase-exhaustive event-type
      ('run-started (generate--run-tests-batch-handle-run-started selector tests-groups-alist event-args))
      ('run-ended (generate--run-tests-batch-handle-run-ended tests-groups-alist event-args))
      ('test-started)
      ('test-ended (generate--run-tests-batch-handle-test-ended tests-groups-alist event-args)))))

;;;###autoload
(defun generate-run-tests-batch (&optional selector)
    "Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR selects which tests to run as described in `ert-select-tests' when
called with its second argument t, except if SELECTOR is nil, in which case
all tests rather than none will be run; this makes the command line
 \"emacs -batch -l my-tests.el -f generate-run-tests-batch-and-exit\" useful.

Returns the stats object.

This is generate's implementation of `ert-run-tests-batch'."
  (let* ((tests (ert-select-tests selector t))
	 (tests-groups-alist (generate--create-tests-groups-alist tests))
	 (listener (generate--create-run-tests-batch-listener selector tests-groups-alist)))
    (ert-run-tests selector listener)))

;;;###autoload
(cl-defun generate-run-tests-batch-and-exit (&optional (selector t))
    "Like `generate-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the tool detected an error outside
of the tests (e.g. invalid SELECTOR or bug in the code that runs
the tests).

This is generate's implementation of `ert-run-tests-batch-and-exit'."
  (let ((tests (ert-select-tests selector t)))
    (cl-letf (((symbol-function 'ert-run-tests-batch)
	       (symbol-function 'generate-run-tests-batch))
	      ((symbol-function 'ert-select-tests)
	       (lambda (&rest _) tests)))
      (ert-run-tests-batch-and-exit selector))))

(defalias 'generate--cons-vec (apply-partially #'cons 'vec) "Convert a list into a calc vector.

\(fn LIST)")

(defalias 'generate-shuffle-list (-compose #'cdr (-applify #'math-shuffle-list) (-juxt #'seq-length #'seq-length #'generate--cons-vec) #'cl-copy-list) "Convert LIST into a calc vector shuffle it with math-shuffle-list.

\(fn LIST)")

(defalias 'generate-append-and-shuffle (-compose #'generate-shuffle-list #'append) "Concatenate all the arguments, make the result a list and the shuffle it.
The result is a list whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.


\(fn &rest SEQUENCES)")

(defun generate--convert-calc-value-into-lisp (calc-value)
  "Converts CALC-VALUE into an emacs-lisp value."
  (read (math-format-value calc-value)))

(cl-defun generate--in-range-exclusive-p ((range-min range-max) number)
 "Is NUMBER greater than RANGE-MIN and less than or equal RANGE-MAX?"
 (and (>= number range-min) (< number range-max)))

(defalias 'generate--between-1-and-255-exclusive-p (apply-partially #'generate--in-range-exclusive-p (list 1 255)) "Is VALUE greater than or equal to 1 and less than 255?

\(fn INTEGER)")
(defalias 'generate--between-0-and-1-exclusive-p (apply-partially #'generate--in-range-exclusive-p (list 0 1)) "Is VALUE greater than or equal to zero and less than 1?

\(fn INTEGER)")
(defalias 'generate--between-1-and-x-exclusive-p (-compose #'generate--applify-apply-partially (apply-partially #'list #'generate--in-range-exclusive-p) (apply-partially #'list 1)) "Is value in range [1,x).
In other words, is VALUE greater than or equal to one
and less than or equal to the given number?

\(fn INTEGER)")
(defalias 'generate--between-0-and-x-exclusive-p (-compose #'generate--applify-apply-partially (apply-partially #'list #'generate--in-range-exclusive-p) (apply-partially #'list 0)) "Is value in range [0,x).
Is VALUE greater than or equal to zero
and less than or equal to the given number?

\(fn INTEGER)")

(defalias 'generate--range-size (-compose #'generate--applify-subtract #'reverse) "Get size of RANGE.

\(fn RANGE)")

(cl-defun generate--in-range-inclusive-p ((min max) x)
  "Is X greater than or equal to MIN and less than or equal to MAX?"
  (<= min x max))

(defalias 'generate--between-0-and-1-inclusive-p (apply-partially #'generate--in-range-inclusive-p (list 0 1)) "Is VALUE greater than or equal to zero and less than or equal 1?

\(fn INTEGER)")

(cl-defgeneric generate--get-min-lisp-timestamp (timestamps)
  "Returns the earliest timestamp in TIMESTAMPS."
  (--min-by (> (car it) (car other)) timestamps))

(cl-defmethod generate--get-min-lisp-timestamp ((timestamps vector))
  "Returns the earliest timestamp in TIMESTAMPS."
  (generate--get-min-lisp-timestamp (seq-into timestamps 'list)))

(cl-defgeneric generate--get-max-lisp-timestamp (timestamps)
  "Returns the latest timestamp in TIMESTAMPS."
  (--max-by (> (car it) (car other)) timestamps))

(cl-defmethod generate--get-max-lisp-timestamp ((timestamps vector))
  "Returns the latest timestamp in TIMESTAMPS."
  (generate--get-max-lisp-timestamp (seq-into timestamps 'list)))

(defun generate--lisp-timestampp (val)
  "Is VAL a timestamp."
  (when val
    (let ((current-time-list nil))
      (ignore-errors (when (decode-time val) 't)))))

(defalias 'generate--divide-list-values-by-max-list-value (-compose #'generate--applify-mapcar (-juxt (-compose #'generate--applify-rpartial (apply-partially #'list #'/) #'float #'1+ #'-max) #'identity)) "Divide each value in LIST by the max value of LIST.")

(cl-defun generate--non-zero-bounded-modular-addition ((range-min range-max) increase current-number)
  "Allows you to perform modular addition with ranges where RANGE-MIN is not 0.
CURRENT-NUMBER can be larger than RANGE-MAX or even smaller than RANGE-MIN.
INCREASE can also be larger than RANGE-MAX or even smaller than RANGE-MIN."
  (when (> range-min range-max)
    (user-error "Range-min %d is not less than range-max %d" range-min range-max))
  (let* ((range-size (- range-max range-min))
       (adjusted-increase (% increase range-size))
       (current-number-index (max (- current-number range-min) 0))
       (adjusted-current-number-index (% current-number-index range-size))
       (new-number-index (% (+ adjusted-current-number-index adjusted-increase) range-size))
       (new-number (+ range-min new-number-index)))
  new-number))

(defalias 'generate--get-next-lower-alpha-character (apply-partially #'generate--non-zero-bounded-modular-addition generate--LOWERALPHA 1) "Convert N into a lower alphabetic character.")
(defalias 'generate--get-next-lower-alpha-string (-compose #'char-to-string #'generate--get-next-lower-alpha-character) "Convert N into a lower alphabetic string character.")

(defalias 'generate--get-next-upper-alpha-character (apply-partially #'generate--non-zero-bounded-modular-addition generate--UPPERALPHA 1) "Convert N into a upper alphabetic character.")
(defalias 'generate--get-next-upper-alpha-string (-compose #'char-to-string #'generate--get-next-upper-alpha-character) "Convert N into a upper alphabetic string character.")

(defalias 'generate--get-next-number-between-zero-and-nine (apply-partially #'generate--non-zero-bounded-modular-addition generate--ZEROTONINE 1) "Convert N into a number between 0 and 9.")
(defalias 'generate--get-next-num-between-zero-and-nine-string (-compose #'number-to-string #'generate--get-next-number-between-zero-and-nine) "Convert N into a string that is a number between 0 and 9.")

(defalias 'generate--random-nat-number (apply-partially #'calcFunc-random most-positive-fixnum))

(defun generate-random-float-between-0-and-1 ()
  "Returns a float that is greater than 0 and less than 1."
  (funcall (-compose #'generate--convert-calc-value-into-lisp #'math-random-float)))

(cl-defun generate-random-nat-number-in-range ((min max))
  "Returns a random number that is greater than or equal to MIN less than MAX.
In other words, use an exclusive range: [MIN, MAX)"
  (if (eql min max)
      min
    (generate--non-zero-bounded-modular-addition (list min max) 0 (generate--random-nat-number))))

(cl-defun generate-two-random-nat-numbers-in-range ((min max))
  "Returns two random numbers that are greater than or equal to MIN less than MAX."
  (thunk-let* ((rand-one (generate-random-nat-number-in-range (list min max)))
	       (distance-from-min (- rand-one min))
	       (distance-from-max (- max rand-one))
	       (min-max (if (g--gt distance-from-max distance-from-min) (list (1+ rand-one) max) (list min (1- rand-one))))
	       (rand-two (generate-random-nat-number-in-range min-max)))
  (if (eql min max)
      (list min min)
    (list rand-one rand-two))))

(defalias 'generate-two-sorted-random-nat-numbers-in-range (-compose #'sort #'generate-two-random-nat-numbers-in-range)  "Returns two sorted random numbers.
Each number will be that are greater than or equal to MIN less than MAX.

\(fn RANGE)")

(defalias 'generate-random-nat-number (apply-partially #'generate-random-nat-number-in-range generate--NATURALNUMBERS) "Returns a random natural number.")

(defalias 'generate-random-nat-number-twice (apply-partially #'generate--times-no-args-twice #'generate-random-nat-number)
  "Returns two random natural numbers.")

(defalias 'generate-random-nat-number-string (-compose #'number-to-string #'generate-random-nat-number) "Returns a random natural number as a string.")

(defalias 'generate-random-negative-number (-partial #'generate-random-nat-number-in-range generate--NEGATIVENUMS) "Returns a random negative number.")

(defalias 'generate--random-nat-number-in-range-1-to-5 (-partial #'generate-random-nat-number-in-range generate--FIVERANGE) "Returns a random number that is greater than or equal to 1 and less than 5.")

(defalias 'generate--random-nat-number-in-range-10 (-partial #'generate-random-nat-number-in-range generate--TENRANGE) "Returns a random number that is greater than or equal to 1 and less than 10.")

(defalias 'generate--random-nat-number-in-range-0-10 (-partial #'generate-random-nat-number-in-range generate--ZEROTENRANGE) "Returns a random number that is greater than or equal to 0 and less than 10.")

(defalias 'generate--two-random-nat-numbers-in-range-10 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-10)))

(defalias 'generate--random-nat-number-in-range-25 (-partial #'generate-random-nat-number-in-range generate--ONETOTWENTYFIVE) "Returns a random number that is greater than or equal to 1 and less than 25.")

(defalias 'generate--random-nat-number-in-range-3-25 (-partial #'generate-random-nat-number-in-range generate--THREETOTWENTYFIVE) "Returns a random number that is greater than or equal to 3 and less than 25.")

(defalias 'generate--random-nat-number-in-range-5-25 (-partial #'generate-random-nat-number-in-range generate--FIVETOTWENTYFIVERANGE) "Returns a random number that is greater than or equal to 5 and less than 25.")

(defalias 'generate--two-random-nat-numbers-in-range-25 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-25)))

(defalias 'generate--two-random-nat-numbers-in-range-3-25 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-3-25)))

(defalias 'generate--two-random-nat-numbers-in-range-5-25 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-5-25)))

(defalias 'generate--random-nat-number-in-range-50 (-partial #'generate-random-nat-number-in-range generate--FIFTYRANGE) "Returns a random number that is greater than or equal to 1 and less than 50.")
(defalias 'generate--random-nat-number-in-range-50 (apply-partially #'generate-random-nat-number-in-range generate--FIFTYRANGE) "Returns a random number that is greater than or equal to 1 and less than 50.")

(defalias 'generate--random-nat-number-in-range-255 (apply-partially #'generate-random-nat-number-in-range generate--DEFAULTRANDOMNUMBERRANGE) "Returns a random number that is greater than or equal to 1 and less than 255.")

(defalias 'generate--random-nat-number-in-range-500 (apply-partially #'generate-random-nat-number-in-range generate--FIVEHUNDREDRANGE) "Returns a random number that is greater than or equal to 1 and less than 500.")

(defalias 'generate--random-nat-number-in-range-3-to-20 (apply-partially #'generate-random-nat-number-in-range generate--THREETOTWENTY) "Returns a random number that is greater than or equal to 3 and less than 20.")

(defalias 'generate--random-nat-number-in-range-2-to-25 (apply-partially #'generate-random-nat-number-in-range generate--TWOTOTWENTYFIVE) "Returns a random number that is greater than or equal to 2 and less than 25.")

(defalias 'generate--random-nat-number-in-range-1-to-25 (apply-partially #'generate-random-nat-number-in-range generate--ONETOTWENTYFIVE) "Returns a random number that is greater than or equal to 1 and less than 25.")

(defalias 'generate--random-nat-number-between-0-and (-compose #'generate-random-nat-number-in-range (apply-partially #'list 0)) "Returns a random number that is greater than or equal to 0 and less than N.

\(fn INTEGER)")

(defalias 'generate--random-nat-number-between-1-and (-compose #'generate-random-nat-number-in-range (apply-partially #'list 1)) "Returns a random number that is greater than or equal to 1 and less than N.

\(fn INTEGER)")

(defalias 'generate--random-nat-number-between-3-and (-compose #'generate-random-nat-number-in-range (apply-partially #'list 3)) "Returns a random number that is greater than or equal to 3 and less than N.

\(fn INTEGER)")

(defalias 'generate--divide-by-random-value (funcall (-compose #'generate--applify-rpartial (apply-partially #'list #'/) (-compose #'float #'generate--random-nat-number-in-range-255))) "Divide N by a random number that is greater than or equal to 1 and less than 255")

(defalias 'generate-random-float (-compose #'generate--divide-by-random-value #'generate--random-nat-number-in-range-255)  "Returns a random float.")

(defalias 'generate-random-float-string (-compose #'number-to-string #'generate-random-float) "Returns a random float as a string.")

(defun generate-call-function-random-times (func)
  "Call a FUNC a random amount of times.

\(fn FUNCTION)"
  (generate--times-no-args (generate--random-nat-number-in-range-10) func))

(defalias 'generate-call-each-function-random-times (apply-partially #'mapcar #'generate-call-function-random-times) "Call each FUNC in LIST a random amount of times.
The results will be collected into a list.

\(fn LIST)")

(defalias 'generate-call-random-function (-compose #'funcall #'generate-seq-take-random-value-from-seq) "Take a random function from LIST, call it and then return the result.

\(fn LIST)")

(defun generate-call-random-function-n-times (n list)
  "Take a random function from LIST and call it N times.

\(fn INTEGER LIST)"
  (funcall (-compose (apply-partially #'generate--times-no-args n) #'generate-seq-take-random-value-from-seq) list))

(defalias 'generate-call-random-function-random-times (-compose #'generate-call-function-random-times #'generate-seq-take-random-value-from-seq) "Take a random function from LIST and call it a random amount of times.
The results will be collected into a list.

\(fn LIST)")

(defun generate-apply-random-function-to-single-arg (list args)
  "Take a random function from LIST and apply it on ARGS.

\(fn LIST LIST)"
  (funcall (-compose (lambda (func) (apply func args)) #'generate-seq-take-random-value-from-seq) list))

(defun generate-apply-random-function-to-rest-args (list &rest args)
  "Take a random function from LIST and funcall it with ARGS.

\(fn LIST LIST)"
  (funcall (-compose (lambda (func) (apply func args)) #'generate-seq-take-random-value-from-seq) list))

(defun generate-call-n-random-functions (n funcs)
  "Take N random FUNCS from LIST and call them."
  (funcall (-compose (apply-partially #'-map #'funcall) (apply-partially #'-take n) #'generate-shuffle-list) funcs))

(defalias 'generate-random-cl-constantly (-compose (-juxt #'cl-constantly #'identity) #'number-to-string #'generate--random-nat-number) "Returns a random cl-constantly and the value that it will return when called.")

(defun generate-default-convert-n-gen-to-random (generator-function)
  "Converts a GENERATOR-FUNCTION into a random generator.
GENERATOR-FUNCTION should only take one argument, N, the number of values
that will be generated."
  (-compose generator-function #'generate--random-nat-number-in-range-1-to-25))

(defun generate--convert-n-gen-to-random-with-arg (number-generator)
  "Use NUMBER-GENERATOR to convert a generator function into a random generator."
  (lambda (gen)
    (:documentation (format "Use %s to convert a generator function into a random generator." (symbol-name number-generator)))
    (lambda (arg)
      (:documentation (format "Random version of `%s'." (symbol-name gen)))
      (funcall gen (funcall number-generator) arg))))

(defalias 'generate--non-default-convert-n-gen-to-random (generate--convert-n-gen-to-random-with-arg #'generate-random-nat-number))

(defalias 'generate-default-convert-n-gen-to-random-with-arg (generate--convert-n-gen-to-random-with-arg #'generate--random-nat-number-in-range-1-to-25)
    "Converts a GENERATOR-FUNCTION into a random generator.
GENERATOR-FUNCTION should only take two arguments.
The first argument should correspond to the the number of values
that will be generated. The second argument can be anything.")

(defun generate-nat-number-range (size)
  "Returns a random n SIZE range.

\(fn INTEGER)"
  (funcall (-juxt #'identity (apply-partially #'+ size))
	 (generate--random-nat-number)))

(defalias 'generate-random-nat-number-range (generate-default-convert-n-gen-to-random #'generate-nat-number-range))

(defalias 'generate-random-list-of-cl-constantlys (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate--random-nat-number-list-in-range-255) "Returns a list of random cl-constantlys.
Also returns a list with the values
that each cl-constantly will return when called.")

(defun generate--random-nat-number-list (length)
  "Returns a N LENGTH list of random numbers.
Numbers will be taken from the range 1..1000."
  (funcall (-compose #'generate-shuffle-list #'-iota) length (1+ (generate--random-nat-number)) (generate--random-nat-number)))

(defalias 'generate--random-nat-number-list-in-range-255 (-compose #'generate--random-nat-number-list #'generate--random-nat-number-in-range-255) "Returns a list of random numbers whose LENGTH is random.")

(defalias 'generate--random-con-from-list (-compose #'generate--applify-cons #'generate-seq-two-random-values) "Returns a random con from LIST.

\(fn LIST))")

(defalias 'generate--divide-list-values-by-random-value (apply-partially #'mapcar #'generate--divide-by-random-value) "Divide each number in LIST by a random value.

\(fn LIST)")

(defalias 'generate--list-of-integer-member-predicates (-compose (-juxt #'seq-map-member #'identity) #'generate--random-nat-number-list) "Returns a list of is-member predicates.
Also returns the list of numbers used to create those predicates.")

(defalias 'generate--concat-two-cons-of-strings (-compose (apply-partially #'generate--map-on #'generate--applify-cons #'generate--applify-concat #'generate--applify-concat) #'list) "Converts con-one and con-two.

\(fn (CON-ONE CON-TWO)")

(defalias 'generate--concat-two-string-vector-cons (-compose (apply-partially #'generate--map-on #'generate--applify-cons #'generate--applify-concat #'generate--applify-vconcat) #'list))

(defalias 'generate--seq-map-char-to-string (apply-partially #'seq-map #'char-to-string))
(defalias 'generate--seq-map-cl-constantly (apply-partially #'seq-map #'cl-constantly))
(defalias 'generate--seq-map-vector (apply-partially #'seq-map #'vector))

(defalias 'generate--seq-max-plus-one (-compose #'1+ #'seq-max))
(defalias 'generate--seq-max-plus-1-and-random-chunk-length (-juxt #'generate--seq-max-plus-one  #'generate--seq-random-chunk-length))

(defalias 'generate--seq-every-p-nat-number (apply-partially #'seq-every-p #'natnump))
(defalias 'generate--seq-every-p-float (apply-partially #'seq-every-p #'floatp))
(defalias 'generate--seq-every-p-between-0-and-1 (apply-partially #'seq-every-p #'generate--between-0-and-1-exclusive-p))

(defalias 'generate--seq-every-p-between-0-and-1-inclusive (apply-partially #'seq-every-p #'generate--between-0-and-1-inclusive-p))

(defalias 'generate--seq-every-p-string (apply-partially #'seq-every-p #'stringp))
(defalias 'generate--seq-every-p-seq (apply-partially #'seq-every-p #'seqp))
(defalias 'generate--seq-every-p-map (apply-partially #'seq-every-p #'mapp))
(defalias 'generate--seq-every-p-list (apply-partially #'seq-every-p #'listp))
(defalias 'generate--seq-every-p-vector (apply-partially #'seq-every-p #'vectorp))
(defalias 'generate--seq-every-p-con (apply-partially #'seq-every-p #'-cons-pair-p))

(defalias 'generate--seq-take-one (-rpartial #'seq-take 1))
(defalias 'generate--seq-take-two (-rpartial #'seq-take 2))
(defalias 'generate--seq-take-three (-rpartial #'seq-take 3))

(defun generate--seq-take-last (n seq)
  "Get the last N values from SEQ."
  (funcall (-compose (apply-partially #'seq-subseq seq)  (-applify #'-)  #'nreverse (apply-partially #'list n) #'seq-length) seq))

(defalias 'generate--seq-last (-compose #'seq-first (apply-partially #'generate--seq-take-last 1)) "Get the last value from SEQ.

\(fn SEQ)")

(defun generate--seq-butlast (seq)
  "Get all but the last value of SEQ."
  (if (> (seq-length seq) 1)
      (funcall (-compose (apply-partially #'seq-subseq seq 0) #'1- #'seq-length) seq)
    'nil))

(defun generate--seq-cdr (seq)
  "Get the cdr of SEQ."
  (if (> (seq-length seq) 1)
      (funcall (-compose (apply-partially #'seq-subseq seq 1) #'seq-length) seq)
    'nil))

(cl-defgeneric generate--seq-reduce-right-indexed (function sequence initial-value)
  "Reduce the function FUNCTION across SEQUENCE, from right to left.
Start with INITIAL-VALUE.  Return the result of calling FUNCTION
with INITIAL-VALUE and the first element of SEQUENCE and the current index,
then calling FUNCTION with that result and the second element
of SEQUENCE and the current index, then with that result and the
third element of SEQUENCE and the current index, etc.
As we are iterating from the right, indices will decrease, e.g., 3-2-1,
as we iterate through the sequence.

If SEQUENCE is empty, return INITIAL-VALUE
and FUNCTION is not called."
  (if (seq-empty-p sequence)
      initial-value
    (let ((acc initial-value)
	  (len (seq-length sequence)))
      (seq-do-indexed
        (lambda (elt index) (setq acc (funcall function elt acc (- len index 1))))
      (reverse sequence))
      acc)))

(cl-defgeneric generate--seq-reduce-right (function sequence initial-value)
    "Reduce the function FUNCTION across SEQUENCE, from right to left.

Start with INITIAL-VALUE.  Return the result of calling FUNCTION
with INITIAL-VALUE and the first element of SEQUENCE and the current index,
then calling FUNCTION with that result and the second element
of SEQUENCE and the current index, then with that result and the
third element of SEQUENCE and the current index, etc.

If SEQUENCE is empty, return INITIAL-VALUE
and FUNCTION is not called."
  (generate--seq-reduce-right-indexed (lambda (elt acc _) (funcall function elt acc)) sequence initial-value))

(defalias 'generate-seq-take-random-value (-compose #'generate--seq-take-one #'generate-seq-shuffle) "Returns a list with one random value from SEQ.

\(fn SEQ)")

(defalias 'generate-seq-take-random-value-from-seq (-compose #'seq-first #'generate-seq-take-random-value) "Returns one random value from SEQ.

\(fn SEQ)")

(defalias 'generate-seq-two-random-values (-compose #'generate--seq-take-two #'generate-seq-shuffle) "Returns a list with two random values from SEQ.

\(fn SEQ)")

(defun generate--seq-random-chunk-length (seq)
  "Returns a random chunk length for SEQ.
The value is guaranteed to be greater than
or equal to 1 and less than the length of SEQ."
  (let ((max-length (max 1 (floor (seq-length seq) 2))))
    (if (equal max-length 1) 1 (generate--random-nat-number-between-1-and max-length))))

(defun generate-seq-random-chunk-of-size-n (chunk-length seq)
  "Returns a random chunk of size CHUNK-LENGTH from SEQ."
  (let* ((chunks (seq-split seq chunk-length))
     (correct-chunks (seq-filter (-rpartial #'length= chunk-length) chunks)))
    (generate-seq-take-random-value-from-seq correct-chunks)))

(defalias 'generate--applify-seq-random-chunk-of-size-n (-applify #'generate-seq-random-chunk-of-size-n) "Returns a random chunk of size CHUNK-LENGTH from SEQ.


\(fn (CHUNK-LENGTH SEQ))")

(defalias 'generate-seq-random-chunk (-compose #'generate--applify-seq-random-chunk-of-size-n (-juxt #'generate--seq-random-chunk-length #'identity)) "Returns a random chunk of from SEQ.

The length of chunk will be greater than or equal to 1
and less than the length of SEQ.

\(fn SEQ)")

(defalias 'generate-seq-random-position (-compose #'generate--random-nat-number-between-0-and #'seq-length) "Returns a random position from SEQ.

\(fn SEQ)")

(defalias 'generate-seq-split-random (-compose #'generate--applify-seq-split (-juxt #'identity #'generate--seq-random-chunk-length)) "Splits a SEQ into random chunks of random size.

\(fn SEQ)")

(defun generate-seq-n-random-values (n seq)
  "Returns N random values from SEQ."
  (funcall (-compose (-rpartial #'seq-take n) #'generate-seq-shuffle) seq))

(defun generate-seq-random-values (seq)
  "Returns a random number of values from SEQ."
  (funcall (-compose (-rpartial #'generate-seq-n-random-values seq) #'generate--seq-random-chunk-length) seq))

(cl-defgeneric generate--seq-random-iterate-from-max (seq)
  "Creates a new sequence starting from the max of SEQ.
The length of the new sequence
will be greater than or equal to
1 and less than the length of SEQ."
  (funcall (-compose #'generate--applify-iterate-plus-one #'generate--seq-max-plus-1-and-random-chunk-length) seq))

(cl-defmethod generate--seq-random-iterate-from-max ((seq vector))
  "Creates a new sequence starting from the max of SEQ.
The length of the new sequence
will be greater than or equal to
1 and less than the length of SEQ."
  (funcall (-compose #'seq--into-vector #'generate--applify-iterate-plus-one #'generate--seq-max-plus-1-and-random-chunk-length) seq))

(cl-defmethod generate--seq-random-iterate-from-max ((seq string))
  "Creates a new sequence starting from the max of SEQ.
The length of the new sequence
will be greater than or equal to
1 and less than the length of SEQ."
  (funcall (-compose #'seq--into-string #'generate--applify-iterate-plus-one #'generate--seq-max-plus-1-and-random-chunk-length) seq))

(defalias 'generate-seq-random-value-with-position (-compose (-juxt #'generate--applify-seq-elt-flipped #'seq-first) (-juxt #'generate-seq-random-position #'identity)) "Returns a random item with its position from SEQ.

\(fn SEQ)")

(cl-defgeneric generate-seq-take-infinite (n seq)
  "Take N values from SEQ.
When n is larger than the length of SEQ, we loop back around."
  (funcall (-compose (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defmethod generate-seq-take-infinite (n (seq vector))
    "Take N values from SEQ.
When n is larger than the length of SEQ, we loop back around."
  (funcall (-compose #'seq--into-vector (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defmethod generate-seq-take-infinite (n (seq string))
      "Take N values from SEQ.
When n is larger than the length of SEQ, we loop back around."
  (funcall (-compose #'seq--into-string (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defgeneric generate-seq-shuffle (seq)
  "Returns a shuffled SEQ."
  (generate-shuffle-list seq))

(cl-defmethod generate-seq-shuffle ((seq vector))
  "Returns a shuffled SEQ (vector)."
  (funcall (-compose #'seq--into-vector #'generate-shuffle-list #'seq--into-list) seq))

(cl-defmethod generate-seq-shuffle ((seq string))
  "Returns a shuffled SEQ (string)."
   (funcall (-compose #'seq--into-string #'generate-shuffle-list #'seq--into-list) seq))

(defun generate-seq-subseq-infinite (seq start end)
  "Return the elements of SEQ from START to END.
END is exclusive.  If END is omitted,
it defaults to the length of the sequence.
If START or END is negative, we counts from the end.
If END is greater than the length of the list,
we wrap back around."
  (cond
    ((or (cl-minusp start) (cl-minusp end))
	(error "Positions can not be negative"))
    ((> start end)
	(error "Start can not be greater than end"))
    (t
	(let* ((length (seq-length seq)))
	 (if (> end length)
	   (funcall (-compose (-rpartial #'seq-subseq start end) (-rpartial #'generate-seq-take-infinite seq) (-partial #'+ length) (-partial #'- end)) length)
	   (seq-subseq seq start end))))))

(cl-defun generate--seq-n-random-subseqs-reducer (seq-to-slice (slices last-slice-end) curr-slice-size)
  (let* ((current-slice-end (+ last-slice-end curr-slice-size))
	 (current-slice (generate-seq-subseq-infinite seq-to-slice last-slice-end current-slice-end)))
	 (list (cons current-slice slices) current-slice-end)))

(defun generate-seq-n-random-infinite-subseqs (n seq)
  "Returns N random subseqs from SEQ.
The length of the subseqs from may be longer
than the length of SEQ."
  (let* ((lengths (generate-data :exact-length n :item-transformer (apply-partially #'generate--non-zero-bounded-modular-addition generate--FIVERANGE 1)))
	 (sum-of-lengths (-sum lengths))
	 (lengths-butlast (generate--seq-butlast lengths))
	 (sum-of-lengths-butlast (-sum lengths))
	 (seq-to-slice-length (seq-length seq))
	 (slice-lengths (if (< sum-of-lengths seq-to-slice-length) (append lengths-butlast (list (- seq-to-slice-length sum-of-lengths-butlast))) lengths))
	 (slice-ends (or (generate--seq-cdr (-running-sum slice-lengths))))
	 (first-slice-end (seq-first slice-ends))
	 (initial-value (list (list (generate-seq-subseq-infinite seq 0 first-slice-end)) first-slice-end)))
    (seq-first (seq-reduce (apply-partially #'generate--seq-n-random-subseqs-reducer seq) slice-ends initial-value))))

(defun generate-seq-split-infinite (length seq)
    "Split SEQ into a list of sub-sequences.
LENGTH will be the length of each sub-sequence.
If LENGTH is greater than the actual length
of the list, we wrap back around."
  (let* ((start-index (1- (seq-length seq)))
	 (seq-to-reduce (generate--seq-butlast seq))
	 (initial-value (generate-seq-subseq-infinite seq (* start-index length) (* (1+ start-index) length))))
    (generate--seq-reduce-right-indexed
     (lambda (_ acc index)
       (append (list (generate-seq-subseq-infinite seq (* index length) (* (1+ index) length))) acc))
    seq-to-reduce
    (list initial-value))))

(defun generate-seq-n-random-chunks-of-size-x (length n seq)
  "Returns N random chunks from SEQ.
Each chunk will be LENGTH long."
  (funcall (-compose (apply-partially #'generate-seq-take-infinite n) #'generate-seq-shuffle #'generate-seq-split-infinite) length seq))

(defalias 'generate-seq-take-infinite-shuffled (-compose #'generate-seq-shuffle #'generate-seq-take-infinite))

(defun generate-seq-n-random-chunks-of-random-size (n seq)
  "Returns N random chunks from SEQ."
  (funcall (-compose (-rpartial #'generate-seq-n-random-chunks-of-size-x n seq) #'generate--seq-random-chunk-length) seq))

(defalias 'generate--applify-seq-n-random-chunks-of-random-size (-applify #'generate-seq-n-random-chunks-of-random-size))

(defalias 'generate--map-into-alist (-rpartial #'map-into 'alist))
(defalias 'generate--map-into-plist (-rpartial #'map-into 'plist))
(defalias 'generate--map-into-hash-table (-rpartial #'map-into 'hash-table))

(defalias 'generate--alistp (apply-partially #'seq-every-p #'-cons-pair-p) "Is LIST an alist?

\(fn LIST)")

(defalias 'generate-map-random-key (-compose #'generate-seq-take-random-value-from-seq #'map-keys) "Returns one random key from MAP.

\(fn MAP)")
(defalias 'generate-seq-map-random-map-key (apply-partially #'seq-map #'generate-map-random-key) "Returns one random key from each map in SEQ.

\(fn MAP)")

(defalias 'generate-map-random-value (-compose #'generate--applify-map-elt (-juxt #'identity #'generate-map-random-key)) "Returns one random value from MAP.

\(fn MAP)")

(defalias 'generate-map-random-pair (-compose (-juxt #'cadr #'generate--applify-map-elt) (-juxt #'identity #'generate-map-random-key)) "Returns one random key-value pair from MAP.

\(fn MAP)")

(defun generate--map-on (op keys-func values-func map)
  "Apply KEYS-FUNC to the MAP keys.
Apply VALUES-FUNC to the MAP values.
Finally, apply OP to MAP."
   (funcall (-compose op (-juxt (-compose keys-func #'map-keys) (-compose values-func #'map-values))) map))

(defun generate--map-merge-with-plus-plist (list-of-plists)
  (if (length= list-of-plists 1)
      (car list-of-plists)
    (apply (apply-partially #'map-merge-with 'plist #'+) list-of-plists)))

(cl-defun generate-data (&key (item-transformer #'identity) (list-transformer #'generate-shuffle-list)
			      min-length max-length exact-length)
  "Return a random list.
The length of the list can be optionally specified
using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH.
If EXACT-LENGTH and MIN-LENGTH or MAX-LENGTH are used together,
an error will be signaled.  :ITEM-TRANSFORMER must be a unary
function.  The function will called on each number
in the random list.  :LIST-TRANSFORMER will be called
with the random list as its only argument.
:LIST-TRANSFORMER will always been called after :ITEM-TRANSFORMER
has transformed each item of the list."
  (when (and min-length max-length (> min-length max-length))
    (error "Min-length must be less than max-length"))
  (when (and exact-length min-length)
    (error "Exact-length and min-length can not be used together"))
  (when (and exact-length max-length)
    (error "Exact-length and max-length can not be used together"))
  (let* ((min-items (or exact-length min-length 1))
	 (max-items (or exact-length max-length 50))
	 (range-length (generate-random-nat-number-in-range (list min-items max-items)))
	 (list-items (generate--random-nat-number-list range-length)))
    (funcall (-on list-transformer (apply-partially #'mapcar item-transformer)) list-items)))

(defalias 'generate--seq-map-next-lower-alpha-character (apply-partially #'seq-map #'generate--get-next-lower-alpha-character) "Converts LIST into a list of lowercase alphabetic characters.")

(cl-defun generate--n-words-reducer (string-of-characters (words last-end) current-end)
      "Helper function used by `generate--n-words-helper'.
Takes a subseq from STRING-OF-CHARACTERS.
The subseq will start at LAST-END
and end at CURRENT-END.  The subseq
will be consed onto WORDS."
  (let* ((current-word-end (+ last-end current-end))
       (current-word (seq-subseq string-of-characters last-end current-word-end)))
    (list (cons current-word words) current-word-end)))

(defun generate--list-of-n-words-helper (word-lengths string-of-characters)
  "Helper function used by generate-list-of-n-words.
Chops STRING-OF-CHARACTERS into a list of words.
The length of each word corresponds to a value in WORD-LENGTHS."
  (-let* (((first-word-length rest-of-list) (funcall (-juxt #'car #'cdr) word-lengths))
      (first-word (list (seq-subseq string-of-characters 0 first-word-length)))
      (initial-value (list first-word first-word-length)))
  (funcall (-compose #'seq-first (apply-partially #'seq-reduce (apply-partially #'generate--n-words-reducer string-of-characters))) rest-of-list initial-value)))

(defun generate-n-alpha-string-characters (character-count)
  "Returns a random list of alphabetic string characters.
The length of the list will be equal
to CHARACTER-COUNT."
  (generate-data :exact-length character-count :item-transformer #'generate--get-next-lower-alpha-string))

(defun generate-n-length-word (character-count)
  "Returns a random word whose length will be equal to CHARACTER-COUNT.
\(fn INTEGER)"
  (generate-data :exact-length character-count :item-transformer #'generate--get-next-lower-alpha-string :list-transformer #'generate--applify-concat))

(defalias 'generate-random-word (apply-partially #'generate-data :item-transformer #'generate--get-next-lower-alpha-string :list-transformer #'generate--applify-concat :min-length 2) "Returns a random word.")

(defalias 'generate-random-string #'generate-random-word
  "Returns a random string. Alias for `generate-random-word'.")

(defun generate-list-of-n-words (word-count)
  "Returns a random list of words.
The number of words will be equal to WORD-COUNT."
  (-let* (((word-lengths character-count) (funcall (-compose (-juxt #'identity #'-sum) #'generate-shuffle-list #'-iota) word-count (generate-random-nat-number-in-range (list 3 6))))
       (string-of-characters (generate-n-length-word character-count))
       (words (generate--list-of-n-words-helper word-lengths string-of-characters)))
    (if (length> words word-count) (butlast words) words)))

(defalias 'generate-list-of-n-strings #'generate-list-of-n-words
    "Returns a random list of words.
The number of words will be equal
to WORD-COUNT. Alias for `generate-list-of-n-words'.")

(defalias 'generate-random-list-of-words (generate-default-convert-n-gen-to-random #'generate-list-of-n-words) "Returns a random list of words.")

(defalias 'generate-random-list-of-strings #'generate-random-list-of-words
  "Returns a random list of words.
Alias for `generate-random-list-of-words'.")

(defun generate-random-sentence ()
  "Returns a random sentence."
  (concat (s-join " " (generate-random-list-of-words)) "."))

(defalias 'generate-random-list-of-unique-strings (-compose #'-uniq #'generate-random-list-of-words)
  "Returns a random list of strings.
Each string is guranteed to be unique.")

(defun generate--list-of-n-sentences-base (sentence-count &optional extra-generators)
      "Returns a list of sentences.
The number of lines will be equal to SENTENCE-COUNT.
The keyword :EXTRA-GENERATORS takes a list.
Each generator must take no arguments and a return a string.
Each generator will be called a random number of times."
  (-let* ((multiple (generate-random-nat-number-in-range (list 3 10)))
      (word-count (* multiple sentence-count))
      (list-of-regular-words (generate-list-of-n-words word-count))
      (list-of-words-from-gens (-flatten-n 1 (generate-call-each-function-random-times extra-generators)))
      ((sentence-slices all-words) (funcall (-compose (-juxt (-compose (apply-partially #'take sentence-count) (-rpartial #'seq-split multiple)) #'identity) #'generate-shuffle-list #'append) list-of-regular-words list-of-words-from-gens))
      (sentences (seq-map (-compose (-rpartial #'concat ".") (apply-partially #'s-join " ")) sentence-slices)))
    (list sentences all-words list-of-regular-words list-of-words-from-gens)))

(defun generate-list-of-n-sentences (n)
  "Returns a list of N sentences."
  (funcall (-compose #'car #'generate--list-of-n-sentences-base) n))

(defun generate--random-list-of-sentences-base ()
  "Returns a random list of sentences."
  (generate--list-of-n-sentences-base (generate-random-nat-number-in-range (list 3 5))))

(defun generate-random-list-of-sentences ()
  "Returns a random list of sentences."
  (funcall (-compose #'car #'generate--random-list-of-sentences-base)))

(defun generate--string-with-n-lines-base (line-count &optional extra-generators)
  "Returns a multiline string.
The string is formed by a joining a random list of sentences
on new-lines.  The number of lines
will be equal to LINE-COUNT.  The keyword
:EXTRA-GENERATORS takes a list of functions.
Each function must take no arguments and a
return a string.  Each generator will be called
a random number of times."
  (-let* (((list-of-sentences list-of-all-words list-of-alpha-words list-of-words-from-gens) (generate--list-of-n-sentences-base line-count extra-generators)))
    (list (s-join "\n" list-of-sentences) list-of-sentences list-of-all-words list-of-alpha-words list-of-words-from-gens)))

(defun generate-string-with-n-lines (n)
  "Returns a string with N lines."
  (funcall (-compose #'car #'generate--string-with-n-lines-base) n))

(defun generate--random-multiline-string-base (&optional extra-generators)
  "Returns a multiline string.
The string is formed by a joining
a random list of sentences on new-lines.
The keyword :EXTRA-GENERATORS takes a
list of functions.  Each function must
take no arguments and a return a string.
Each generator will be called
a random number of times."
  (generate--string-with-n-lines-base (generate-random-nat-number-in-range (list 2 5)) extra-generators))

(defalias 'generate-random-multiline-string (-compose #'car #'generate--random-multiline-string-base)  "Returns a random string.
The string is guranteed to have multiple lines.")

(defalias 'generate-list-of-nat-numbers #'generate-data "Returns a random list of natural numbers.
The length of the list can be optionally
specified using :MIN-LENGTH and
:MAX-LENGTH or simply :EXACT-LENGTH.

\(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")

(defalias 'generate-list-of-nat-number-strings (apply-partially #'generate-data :item-transformer #'number-to-string) "Returns a random list of strings.
Each string will be a natural number.
The length of the list can be optionally
specified using :MIN-LENGTH and
:MAX-LENGTH or simply :EXACT-LENGTH.

\(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")

(defalias 'generate-list-of-floats-between-0-and-1 (apply-partially #'generate-data :list-transformer (-compose #'generate--divide-list-values-by-max-list-value #'generate-seq-shuffle)) "Returns a random list of floats.
Each float will be greater than or equal to
zero and less than 1.  The length of the list
can be optionally specified using :MIN-LENGTH
and :MAX-LENGTH or simply :EXACT-LENGTH.

\(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")

(defalias 'generate-list-of-floats (apply-partially #'generate-data :list-transformer (-compose #'generate--divide-list-values-by-random-value #'generate-seq-shuffle)) "Returns a random list of floats.
The length of the list can be optionally
specified using :MIN-LENGTH and :MAX-LENGTH
or simply :EXACT-LENGTH.

\(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")

(defalias 'generate-random-list-of-lists-nat-numbers (apply-partially #'generate-data :list-transformer #'generate-seq-split-random) "Returns a random list of lists of natural numbers.")

(cl-defun generate-list-of-nat-numbers-in-range (range &key (list-transformer #'generate-shuffle-list) min-length max-length exact-length)
    "Returns a list with COUNT numbers.
Each number will be within the bounds of RANGE.
LIST-TRANSFORMER can be used to transform the
list itself.  MIN-LENGTH, MAX-LENGTH and EXACT-LENGTH
can be used to control the size of the list.  If
EXACT-LENGTH and MIN-LENGTH or MAX-LENGTH
are used together, an error will be signaled."
  (generate-data :list-transformer list-transformer
		 :item-transformer (apply-partially #'generate--non-zero-bounded-modular-addition range 0)
		 :exact-length exact-length
		 :min-length min-length
		 :max-length max-length))

(defalias 'generate--list-of-n-nat-numbers-in-range-5 (apply-partially #'generate-list-of-nat-numbers-in-range generate--FIVERANGE))

(defalias 'generate--list-of-n-nat-numbers-in-range-10 (apply-partially #'generate-list-of-nat-numbers-in-range generate--TENRANGE))

(defalias 'generate--list-of-nat-numbers-in-range-25 (apply-partially #'generate-list-of-nat-numbers-in-range generate--ONETOTWENTYFIVE))

(defconst generate--LIST-ITEM-TRANSFORMERS
  (list #'generate--get-next-lower-alpha-character
	#'generate--get-next-upper-alpha-character
	#'generate--nth-mod-file-extensions
	#'generate--divide-by-random-value
	#'identity
	#'number-to-string
	#'vector
	#'list)
  "Functions that will be used to create list generator variants.")

(defun generate--list-of-n-random-values (transformers)
  "Returns a closure that will generate a list of N values.
TRANSFORMERS should be a list of unary function.
For each run, a random function will be selected
from transformers and then applied to every value
in the underlying list of numbers that is used
to create the final list."
  (lambda (n)
    (:documentation (format "Returns a list with N random values. %s
Values will be created by taking
a random function from the following it list
and applying it to the underlying list of
numbers that is used to create the final list: " (mapcar (lambda (x) (format "%s\n" x)) transformers)))
    (let ((item-transformer (generate-seq-take-random-value-from-seq transformers)))
      (generate-list-of-nat-numbers :item-transformer item-transformer :exact-length n))))

(defalias 'generate-list-of-n-random-values (generate--list-of-n-random-values generate--LIST-ITEM-TRANSFORMERS))

(defalias 'generate-vector-of-n-nat-numbers (-compose #'generate--applify-vector #'generate-list-of-nat-numbers) "Returns a random vector of natural numbers.
The length of the vector can be optionally
specified using :MIN-LENGTH and :MAX-LENGTH
or simply :EXACT-LENGTH.

\(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")

(defalias 'generate-vector-of-floats-between-0-and-1 (-compose #'generate--applify-vector #'generate-list-of-floats-between-0-and-1) "Returns a random vector of floats.
Each float will be greater than or equal to zero and less than 1.
The length of the vector can be optionally
specified using :MIN-LENGTH and :MAX-LENGTH
or simply :EXACT-LENGTH.
\(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")

(defalias 'generate-vector-of-floats (-compose #'generate--applify-vector #'generate-list-of-floats) "Returns a random vector of floats.
The length of the vector can be optionally specified
using :MIN-LENGTH and :MAX-LENGTH
or simply :EXACT-LENGTH.

\(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")

(defalias 'generate-random-vector-of-strings (-compose #'generate--applify-vector #'generate-random-list-of-strings) "Returns a random vector of strings.")

(defalias 'generate-random-vector-of-lists-nat-numbers (apply-partially #'generate-data :list-transformer (-compose #'generate--applify-vector #'generate-seq-split-random)) "Returns a random vector of lists of natural numbers.")

(defalias 'generate-random-vector-of-vectors-nat-numbers (-compose #'generate--seq-map-vector #'generate-random-vector-of-lists-nat-numbers) "Returns a random vector of vectors of natural numbers.")

(defalias 'generate-random-alist-of-nat-numbers (apply-partially #'generate-data :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse #'generate-seq-shuffle))) "Returns a random alist.
Both the keys and the values will be natural numbers.")

(defalias 'generate-random-alist-of-strings (apply-partially #'generate-data :item-transformer #'generate--get-next-lower-alpha-string :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse #'generate-seq-shuffle))) "Returns a random alist.
Both the keys and the values will be strings.")

(defalias 'generate-random-alist-of-string-nat-number-cons (apply-partially #'generate-data :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-zip (-juxt (-compose #'generate--seq-map-char-to-string #'seq-reverse) #'generate-seq-shuffle))) "Returns a random alist.
The keys will be strings and the
values will be natural numbers.")

(defalias 'generate-random-alist-of-nat-number-string-cons (apply-partially #'generate-data :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse (-compose #'generate--seq-map-char-to-string #'generate-seq-shuffle)))) "Returns a random alist.
The keys will be natural numbers
and the values will be strings.")

(defalias 'generate-random-plist-of-nat-numbers (-compose #'generate--map-into-plist #'generate-random-alist-of-nat-numbers) "Returns a random plist.
Both the keys and the values will be natural numbers.")

(defalias 'generate-random-plist-of-strings (-compose #'generate--map-into-plist #'generate-random-alist-of-strings) "Returns a random plist
Both the keys and the values will be strings.")

(defalias 'generate-random-plist-of-string-nat-number-pairs (-compose #'generate--map-into-plist #'generate-random-alist-of-string-nat-number-cons) "Returns a random plist.
The keys will be strings and the
values will be natural numbers.")

(defalias 'generate-random-plist-of-nat-number-string-pairs (-compose #'generate--map-into-plist #'generate-random-alist-of-nat-number-string-cons) "Returns a random plist.
The keys will be natural numbers
and the values will be strings.")

(defalias 'generate-random-hash-table-of-nat-numbers (-compose #'generate--map-into-hash-table #'generate-random-alist-of-nat-numbers) "Returns a random hash-table
Both the keys and the values will be natural numbers.")

(defalias 'generate-random-hash-table-of-strings (-compose #'generate--map-into-hash-table #'generate-random-alist-of-strings) "Returns a random hash-table.
Both the keys and the values will be strings.")

(defalias 'generate-random-hash-table-of-string-nat-number-pairs (-compose #'generate--map-into-hash-table #'generate-random-alist-of-string-nat-number-cons) "Returns a random hash-table.
The keys will be strings and the
values will be natural numbers.")

(defalias 'generate-random-hash-table-of-nat-number-string-pairs (-compose #'generate--map-into-hash-table #'generate-random-alist-of-nat-number-string-cons) "Returns a random hash-table.
The keys will be natural numbers
and the values will be strings.")

(defalias 'generate-random-con-of-nat-numbers (apply-partially #'generate-data :exact-length 2 :list-transformer #'generate--random-con-from-list) "Returns a random cons cell.
 The car and the cdr will be natural numbers.")

(defalias 'generate-random-con-of-floats (apply-partially #'generate-data :exact-length 2 :list-transformer (-compose #'generate--random-con-from-list #'generate--divide-list-values-by-max-list-value)) "Returns a random cons cell.
The car and the cdr will be floats.")

(defalias 'generate-random-con-of-strings (apply-partially #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-string :list-transformer #'generate--random-con-from-list) "Returns a random cons cell.
The car and the cdr will be strings.")

(defalias 'generate-random-string-nat-number-con (apply-partially #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-cons (-juxt (-compose #'char-to-string #'-first-item) #'-second-item) #'generate-seq-two-random-values)) "Returns a random cons cell.
The car will be string.
The cdr will be a natural number.")

(defalias 'generate-random-nat-number-string-con (apply-partially #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-cons (-juxt #'-first-item (-compose #'char-to-string #'-second-item)) #'generate-seq-two-random-values)) "Returns a random cons cell.
The car will be a natural number
The cdr will be a string.")

(defalias 'generate-random-string-vector-of-nat-numbers-con (apply-partially #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-cons (-juxt (-compose #'char-to-string #'-first-item) (-compose #'generate--applify-vector #'cdr)))) "Returns a random cons cell.
The car will be a string.
The cdr will b a vector.")

(defalias 'generate-random-symbol (-compose #'make-symbol #'generate-random-word) "Returns a random symbol.")

(defalias 'generate-list-of-n-symbols (-compose (apply-partially #'mapcar #'make-symbol) #'generate-list-of-n-words)  "Returns a list with N symbols.")

(defalias 'generate-random-list-of-symbols (-compose (apply-partially #'mapcar #'make-symbol) #'generate-random-list-of-unique-strings) "Returns a random list of symbols.")

(defalias 'generate-random-boolean (apply-partially #'generate-seq-take-random-value-from-seq (list 't 'nil))
  "Returns a random boolean.")

(defun generate-list-of-n-booleans (n)
  "Returns a list with N booleans."
  (generate-data :item-transformer #'math-oddp :exact-length n))

(defalias 'generate--random-nat-number-between-zero-and-60 (apply-partially #'generate-random-nat-number-in-range generate--ZEROTOSIXTY) "Returns a random number that is greater than or equal to 0 and less than 60.")

(defalias 'generate--random-nat-number-between-1-and-13 (apply-partially #'generate-random-nat-number-in-range generate--ONETOTHIRTEEN) "Returns a random number that is greater than or equal to 1 and less than 13.")

(defalias 'generate--random-nat-number-between-zero-and-24 (apply-partially #'generate-random-nat-number-in-range generate--ZEROTOTWENTYFOUR) "Returns a random number that is greater than or equal to 0 and less than 24.")

(defalias 'generate--pad-zeros (apply-partially #'s-pad-left 2 "0") "For dates and times")
(defalias 'generate--number-to-padded-string (-compose #'generate--pad-zeros #'number-to-string))
(defalias 'generate--seq-map-format-pad (apply-partially #'seq-map #'generate--number-to-padded-string))
(defalias 'generate--join-time-values (apply-partially #'s-join ":"))

(defun generate--time-diff (end-time start-time)
  "Return the difference between END-TIME and START-TIME, in seconds.
T1 and T2 are time values (as returned by `current-time' for example).
Stolen from tramp."
  (float-time (time-subtract end-time start-time)))

(defalias 'generate--get-lisp-timestamp-range-duration (-compose (-applify #'generate--time-diff) #'reverse))

(defalias 'generate--lisp-timestamp-range-duration-helper (-juxt #'car #'cadr #'generate--get-lisp-timestamp-range-duration))

(defun generate--timestamp-range-index-to-timestamp (hz min index)
  (cons (+ (* index hz) min) hz))

(defun generate--timestamp-range-indices-to-timestamps (hz min range-indices)
  (mapcar (apply-partially #'generate--timestamp-range-index-to-timestamp hz min) range-indices))

(cl-defun generate--create-timestamp-range-around-current-time (minus-bottom plus-top)
  "Uses `current-time' to create a timestamp.
Returns a list that contains the MIN and MAX
of the range along with the HZ value used for the
calculations and the length of the range relative
to the HZ.  MINUS-BOTTOM and PLUS-TOP can be used
to widen or shrink the range of timestamps."
  (-let* ((current-time-list nil)
	  ((curr-secs . hz) (current-time))
	  (min (- curr-secs (* hz minus-bottom)))
	  (range-length (+ plus-top minus-bottom)))
    (list min hz range-length)))

(cl-defun generate--lisp-timestamp-helper (minus-bottom plus-top)
  "Returns a random Lisp timestamp.
The timestamp will be in the (TICKS . HZ) format.
MINUS-BOTTOM and PLUS-TOP can be used to widen
or shrink the range of possible timestamps."
  (-let* (((min hz range-length) (generate--create-timestamp-range-around-current-time minus-bottom plus-top))
	  (range-index (generate--random-nat-number-between-0-and range-length)))
    (generate--timestamp-range-index-to-timestamp hz min range-index)))

(cl-defun generate--lisp-timestamp-range-helper (minus-bottom PLUS-TOP)
  "Returns a random Lisp timestamp.
The timestamp will be in the (TICKS . HZ) format.
MINUS-BOTTOM and PLUS-TOP can be used to widen
or shrink the range of possible timestamps."
  (-let* (((min hz range-length) (generate--create-timestamp-range-around-current-time minus-bottom PLUS-TOP))
	  (range-indices (generate-two-sorted-random-nat-numbers-in-range (list 0 range-length))))
    (generate--timestamp-range-indices-to-timestamps hz min range-indices)))

(defun generate-random-12-hour-time-string ()
  "Returns a random time string in 12-hour format."
  (format "%s:%s" (generate--random-nat-number-between-1-and-13) (generate--number-to-padded-string (generate--random-nat-number-between-zero-and-60))))

(defun generate-random-24-hour-time-string ()
  "Returns a random time string in 24-hour format."
  (funcall (-compose #'generate--join-time-values #'generate--seq-map-format-pad #'list) (generate--random-nat-number-between-zero-and-24) (generate--random-nat-number-between-zero-and-60)))

(defalias 'generate-random-time-string (apply-partially #'generate-call-random-function (list #'generate-random-24-hour-time-string #'generate-random-12-hour-time-string)))

(cl-defun generate-random-lisp-timestamp (&optional (range-size generate-lisp-timestamp-range-size))
  "Returns a random Lisp timestamp.
RANGE-SIZE can be used to widen or shrink the range.
It will be used to create the range of times from
which the timestamp will be selected.
Each timestamp will be in the
\(TICKS . HZ) format."
  (generate--lisp-timestamp-helper (floor range-size 2) (floor range-size 2)))

(cl-defun generate-random-lisp-timestamp-range (&optional (range-size generate-lisp-timestamp-range-size))
  "Returns a random Lisp timestamp range.
RANGE-SIZE can be used to widen or shrink the range.
It will be used to create the range of times from
which the timestamp will be selected.
Each timestamp will be in the
\(TICKS . HZ) format."
  (generate--lisp-timestamp-range-helper (floor range-size 2) (floor range-size 2)))

(defalias 'generate-random-lisp-timestamp-range-with-duration (-compose #'generate--lisp-timestamp-range-duration-helper #'generate-random-lisp-timestamp-range))

(defun generate--list-of-n-lisp-timestamp-ranges-helper (n minus-bottom PLUS-TOP)
    (-let* (((min hz range-length) (generate--create-timestamp-range-around-current-time minus-bottom PLUS-TOP))
	    (range-indices (generate-list-of-nat-numbers-in-range (list 0 range-length) :exact-length (* n 2)))
	    (timestamps (generate--timestamp-range-indices-to-timestamps hz min range-indices))
	    (raw-ranges (-partition 2 timestamps)))
      (mapcar (-rpartial #'sort :key #'car) raw-ranges)))

(cl-defun generate-list-of-n-lisp-timestamp-ranges (n &optional (range-size generate--SECONDS-IN-AN-HOUR))
  "Returns N Lisp timestamp ranges.
RANGE-SIZE can be used to widen or shrink the range.
Timestamps will be in the (TICKS . HZ) format."
  (generate--list-of-n-lisp-timestamp-ranges-helper n (floor range-size 2) (floor range-size 2)))

(defalias 'generate--list-of-n-unzipped-starts-ends-durations (-compose #'-unzip-lists (apply-partially #'mapcar #'generate--lisp-timestamp-range-duration-helper) #'generate-list-of-n-lisp-timestamp-ranges))

(defalias 'generate-random-month-number #'generate--random-nat-number-between-1-and-13 "Returns a random month number.")

(defalias 'generate-random-year-number (apply-partially #'generate-random-nat-number-in-range generate--YEARRANGE) "Returns a random year number.")

(defun generate-random-day-number (year month)
  "Returns a valid random day number given a YEAR and a MONTH."
  (let ((days-in-month (date-days-in-month year month)))
    (generate-random-nat-number-in-range (list 1 days-in-month))))

(defalias 'generate--day-month-year (lambda (join-on year month day) (concat day join-on month join-on year)) "Join YEAR, MONTH and DAY with JOIN-ON.
Returns a date in D M YYYY
or DD MM YYYY format.")
(defalias 'generate--month-day-year (lambda (join-on year month day) (concat month join-on day join-on year)) "Join YEAR, MONTH and DAY with JOIN-ON.
Returns a date in M D YYYY
or MM DD YYYY format.")
(defalias 'generate--year-month-day (lambda (join-on year month day) (concat year join-on month join-on day)) "Join YEAR, MONTH and DAY with JOIN-ON.
Returns a date in YYYY M D
or YYYY MM DD format.")

(defalias 'generate--call-random-full-date-formatter (apply-partially #'generate-apply-random-function-to-single-arg (list #'generate--day-month-year #'generate--month-day-year #'generate--year-month-day)) "Returns a random date formatter.")

;; use keywords args for with-padding
(cl-defun generate--create-random-full-date-string (join-on &key (with-padding nil with-padding-supplied-p))
    "Returns a random date where parts are joined with JOIN-ON.
If WITH-PADDING is true, the month and day
will always be at least two characters,
e.g. 01 instead of 1 for the 1st."
  (-let* ((padding-p (or (and with-padding-supplied-p with-padding) (generate-random-boolean)))
	(string--converter (or (and padding-p #'generate--number-to-padded-string) #'number-to-string))
	((year-number year-string) (funcall (-juxt #'identity #'number-to-string) (generate-random-year-number)))
	((month-number month-string) (funcall (-juxt #'identity string--converter) (generate-random-month-number)))
	(day-string (funcall (-compose string--converter #'generate-random-day-number) year-number month-number)))
    (generate--call-random-full-date-formatter (list join-on year-string month-string day-string))))

(defalias 'generate--month-full-year (lambda (join-on year month) (concat month join-on year)) "Join YEAR and MONTH with JOIN-ON,
Returns a date in M YYYY
or MM YYYY format.")

(defalias 'generate--full-year-month (lambda (join-on year month) (concat year join-on month)) "Join YEAR and MONTH with JOIN-ON.
Returns a date in YYYY M
or YYYY MM format.")

(defalias 'generate--month-half-year (lambda (join-on year month) (concat month join-on (s-chop-left 2 year))) "Join YEAR and MONTH with JOIN-ON.
Returns a date in M YY or MM YY format.")

(defalias 'generate--half-year-month (lambda (join-on year month) (concat (s-chop-left 2 year) join-on month)) "Join YEAR and MONTH with JOIN-ON.
Returns a date in YY M or YY MM format.")

(defalias 'generate--call-random-short-date-formatter (apply-partially #'generate-apply-random-function-to-single-arg (list #'generate--month-full-year #'generate--full-year-month #'generate--month-half-year #'generate--half-year-month)) "Returns a random short date formatter.")

;; use keywords args for with-padding
(cl-defun generate--create-random-short-date-string (join-on &key (with-padding nil with-padding-supplied-p))
  "Returns a random short date where parts are joined with JOIN-ON.
If WITH-PADDING is true, the month will always
be at least two characters, e.g. 01 instead
of 1 for january."
  (let* ((padding-p (or (and with-padding-supplied-p with-padding) (generate-random-boolean)))
	  (string--converter (or (and padding-p #'generate--number-to-padded-string) #'number-to-string))
	  (year-string (funcall (-compose #'number-to-string #'generate-random-year-number)))
	  (month-string (funcall (-compose string--converter #'generate-random-month-number))))
    (generate--call-random-short-date-formatter (list join-on year-string month-string))))

(defalias 'generate-random-full-slash-date-string (apply-partially #'generate--create-random-full-date-string "/") "Returns a random date
The parts are joined with dashes.
Format will be one of the following:
YYYY/M/D
YYYY/MM/DD
M/D/YYYY
MM/DD/YYYY
D/M/YYYY
DD/MM/YYYY")

(defalias 'generate-random-full-dash-date-string (apply-partially #'generate--create-random-full-date-string "-") "Returns a random date
The parts are joined with dashes.
Format will be one of the following:
YYYY-M-D
YYYY-MM-DD
M-D-YYYY
MM-DD-YYYY
D-M-YYYY
DD-MM-YYYY")

(defalias 'generate-random-short-slash-date-string (apply-partially #'generate--create-random-short-date-string "/") "Returns a random short date.
The parts are joined with slashes.
Format will be one of the following:
YY/M
YY/MM
M/YY
MM/YY
YYYY/M
YYYY/MM
M/YYYY
MM/YYYY")

(defalias 'generate-random-short-dash-date-string (apply-partially #'generate--create-random-short-date-string "-") "Returns a random short date.
The parts will be joined with dashes.
Format will be one of the following:
YY-M
YY-MM
M-YY
MM-YY
YYYY-M
YYYY-MM
M-YYYY
MM-YYYY ")

(defalias 'generate-random-date-string (apply-partially #'generate-call-random-function (list #'generate-random-full-dash-date-string #'generate-random-full-slash-date-string #'generate-random-short-dash-date-string #'generate-random-short-slash-date-string)))

(defalias 'generate--create-random-regular-phone-number (-compose (apply-partially #'s-join "-") (apply-partially #'seq-map (apply-partially #'s-join "")) (-juxt (apply-partially #'-take 3) (-rpartial #'-slice 3 6) (apply-partially #'-take-last 4))) "Helper used to convert LIST into a random U.S. style phone number.")
(defalias 'generate-random-regular-phone-number (apply-partially #'generate-data :min-length 10 :max-length 10 :item-transformer #'generate--get-next-num-between-zero-and-nine-string :list-transformer #'generate--create-random-regular-phone-number) "Returns a random U.S. style phone number.")

(defalias 'generate--create-random-1-800-number (-compose (apply-partially #'concat "1-800-") (apply-partially #'s-join "-") (apply-partially #'seq-map (apply-partially #'s-join "")) (-juxt (apply-partially #'-take 3) (apply-partially #'-take-last 4))) "Helper used to convert LIST into a random 1-800 number.")

(defalias 'generate-random-1-800-number (apply-partially #'generate-data :min-length 7 :max-length 7 :item-transformer #'generate--get-next-num-between-zero-and-nine-string :list-transformer #'generate--create-random-1-800-number) "Returns a random 1-800 number.")

(defalias 'generate-random-phone-number (apply-partially #'generate-call-random-function (list #'generate-random-regular-phone-number #'generate-random-1-800-number)) "Returns a random phone number.")

(defalias 'generate--create-random-card-number (-compose (apply-partially #'s-join "-") (apply-partially #'seq-map (apply-partially #'s-join "")) (-rpartial #'seq-split 4)) "Helper used to convert LIST into a card number string.")
(defalias 'generate-random-card-number (apply-partially #'generate-data :min-length 16 :max-length 16 :item-transformer #'generate--get-next-num-between-zero-and-nine-string :list-transformer #'generate--create-random-card-number) "Returns a random 16-digit card number.")

(defun generate--random-identifier-string (item-transformer)
 "Returns a random identifier string.
The string will contain at least one numeric character
and at least two characters from the values
created by ITEM-TRANSFORMER."
 (let* ((letters (generate-data :min-length 2 :max-length 10 :item-transformer item-transformer))
      (nums (generate-data :min-length 1 :max-length 10 :item-transformer #'number-to-string)))
   (funcall (-compose (apply-partially #'s-join "") #'generate-shuffle-list #'append) letters nums)))

(defalias 'generate-random-string-of-lower-alphanums (apply-partially #'generate--random-identifier-string #'generate--get-next-lower-alpha-string) "Create a random alphanumeric identifier string.
All alphabetic characters will be in lowercase.")

(defalias 'generate-random-string-of-upper-alphanums (apply-partially #'generate--random-identifier-string #'generate--get-next-upper-alpha-string) "Create a random alphanumeric identifier string.
All alphabetic characters will be uppercase.")

(defmacro generate-with-buffer-with-text (buffer-text &rest body)
  "Run BODY in a temporary buffer with holding BUFFER-TEXT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,buffer-text)
     (goto-char (point-min))
     ,@body))

(defalias 'generate--basic-tbl (-rpartial #'orgtbl-to-orgtbl '()))
(defalias 'generate--join-with-new-lines (apply-partially #'s-join "\n"))
(defalias 'generate--surround-table-row (lambda (x) (format "| %s |" x)))
(defalias 'generate--join-table-cells (apply-partially #'s-join " | "))
(defalias 'generate--interpose-hlines (apply-partially #'-interpose 'hline) "Add hlines to a list of org-table row strings.")

(defalias 'generate--create-table-rows (apply-partially #'seq-map (-compose #'generate--surround-table-row #'generate--join-table-cells)) "Convert LIST into a list of strings.
Each string is an org-table row.

\(fn LIST)")

(defun generate--org-table-val-generator-caller (val-generator rows columns cell-num)
  (let ((current-col (1+ (% cell-num columns)))
	(current-row (1+ (floor cell-num columns))))
  (funcall val-generator (list current-row current-col))))

(defun generate--org-table-cell-values-helper (val-generator rows columns)
 (funcall (-compose (apply-partially #'-partition columns) (-rpartial #'generate--times (apply-partially #'generate--org-table-val-generator-caller val-generator rows columns)) #'*) rows columns))

(defun generate--org-table-without-hlines (val-generator rows columns)
  "Use ROWS, COLUMNS and VAL-GENERATOR to create an org-table.
Rows and columns should be integers.
VAL-GENERATOR should take one argument,
a list that will contain the current
row and column number.  This function
returns a tuple where the first value
is the table itself and the second
value is a list of the values
in the table.  The returned table
will not have hlines."
  (-let* (((test-row-strings test-rows-as-lists) (funcall (-compose (-juxt #'generate--create-table-rows #'identity) #'generate--org-table-cell-values-helper) val-generator rows columns)))
    (list (generate--join-with-new-lines test-row-strings) test-rows-as-lists)))

(defun generate--org-table-with-hlines (val-generator rows columns)
  "Use ROWS, COLUMNS and VAL-GENERATOR to create an org-table.
ROWS and COLUMNS should be integers.
VAL-GENERATOR should take one argument,
a list that will contain the current row
and column number.  This function returns
a tuple where the first value is the
table itself and the second value
is a list of the values in the table.
The returned table will have hlines."
  (-let* (((table-with-hlines _) (funcall (-compose (-juxt #'generate--interpose-hlines #'identity) #'generate--org-table-cell-values-helper) val-generator rows columns)))
    (list (generate--basic-tbl table-with-hlines) table-with-hlines)))

(defconst generate-ORG-TABLE-GENS
  (list #'generate--org-table-without-hlines #'generate--org-table-with-hlines))

(defalias 'generate--org-table (apply-partially #'generate-apply-random-function-to-rest-args generate-ORG-TABLE-GENS) "Use ROWS, COLUMNS and VAL-GENERATOR to create an org-table.
ROWS and COLUMNS should be integers. VAL-GENERATOR
should take one argument, a list that will contain
the current row and column number.
This function returns a tuple where the
first value is the table itself and
the second value is a list of the
values in the table. The returned table
may or may not have hlines.

 \(fn VAL-GENERATOR ROWS COLUMNS)")

(defalias 'generate-org-table-without-hlines (-compose #'car #'generate--org-table-without-hlines) "Use ROWS, COLUMNS and VAL-GENERATOR to create an org-table.
ROWS and COLUMNS should be integers. VAL-GENERATOR
should take one argument, a list that will contain
the current row and column number.
The returned table will not have hlines.

\(fn FUNCTION VAL-GENERATOR ROWS COLUMNS)")

(defalias 'generate-org-table-with-hlines (-compose #'car #'generate--org-table-with-hlines) "Use ROWS, COLUMNS and VAL-GENERATOR to create an org-table.
ROWS and COLUMNS should be integers. VAL-GENERATOR
should take one argument, a list that will contain
the current row and column number.
The returned table will have hlines.

\(fn FUNCTION VAL-GENERATOR ROWS COLUMNS)")

(defalias 'generate-org-table (-compose #'car #'generate--org-table) "Use ROWS, COLUMNS and VAL-GENERATOR to create an org-table.
ROWS and COLUMNS should be integers.
VAL-GENERATOR should take one argument,
a list that will contain the current row and column
number.  The returned table may or may
not have hlines.

\(fn FUNCTION VAL-GENERATOR ROWS COLUMNS)")

(defun generate--with-buffer-with-org-table-helper (gen gen-args body)
    "Use GEN and GEN-ARGS to create an org-table.
Then, execute BODY in buffer with the org-table."
  (setq org-hook 'nil)
  (cl-with-gensyms (org-table)
    `(let ((,org-table (apply #',gen ,gen-args)))
       (with-temp-buffer
       (org-mode)
       (insert ,org-table)
       (goto-char (org-table-begin))
       (font-lock-ensure (point-min) (point-max))
       ,@body))))

(cl-defmacro generate-with-buffer-with-org-table-without-hlines (org-table-args &rest body)
    "Use ORG-TABLE-ARGS and use them to create a buffer with a table.
The table will not have hlines.
BODY will be executed in the buffer
with the point at the beginning
of the table."
  (declare (indent 1) (debug t))
  (generate--with-buffer-with-org-table-helper #'generate-org-table-without-hlines org-table-args body))

(cl-defmacro generate-with-buffer-with-org-table-with-hlines (org-table-args &rest body)
    "Use ORG-TABLE-ARGS and use them to create a buffer with a table.
The table will not have hlines.
BODY will be executed in the buffer
with the point at the beginning
of the table."
  (declare (indent 1) (debug t))
  (generate--with-buffer-with-org-table-helper #'generate-org-table-with-hlines org-table-args body))

(cl-defmacro generate-with-buffer-with-org-table (org-table-args &rest body)
    "Use ORG-TABLE-ARGS and use them to create a buffer with a table.
The table may or may not have hlines.
BODY will be executed in the buffer
with the point at the beginning
of the table."
  (declare (indent 1) (debug t))
  (generate--with-buffer-with-org-table-helper #'generate-org-table org-table-args body))

(defun generate--random-void-x-error (symbol)
  "Returns a closure that will generate a random void type error.
SYMBOL should be void-function or void-variable."
  (lambda ()
    (:documentation (format "Returns a random %s error." symbol))
    (list symbol (generate-random-symbol))))

(defalias 'generate-random-void-function-error (generate--random-void-x-error 'void-function))

(defalias 'generate-random-void-variable-error (generate--random-void-x-error 'void-variable))

(defun generate-random-wrong-type-argument-error ()
  "Returns a random wrong-type-argument error."
  (let* ((random-val (generate-random-value))
	 (pred (funcall (-compose (apply-partially #'-first (lambda (func) (not (funcall func random-val)))) #'generate-shuffle-list) generate--PREDICATES)))
  (list 'wrong-type-argument pred random-val)))

(defalias 'generate-arith-error (cl-constantly (list 'arith-error nil)) "Returns a random arith-error.")

(defconst generate--ERROR-GENERATORS
  (list
   #'generate-random-void-function-error
   #'generate-random-void-variable-error
   #'generate-random-wrong-type-argument-error
   #'generate-arith-error))

(defalias 'generate-random-error (apply-partially #'generate-call-random-function generate--ERROR-GENERATORS) "Returns a random error.")

(defun generate-random-backtrace-frame ()
  "Returns a random backtrace frame."
  (let* ((evald (generate-random-boolean))
	 (fun (generate-random-symbol))
	 (args (generate-random-list-of-symbols))
	 (locals (generate-random-alist))
	 (flags (generate-seq-take-random-value-from-seq (list :debug-on-exit :source-available nil)))
	 (pos (generate-random-nat-number)))
    (backtrace-make-frame
     :evald evald
     :fun fun
     :args args
     :flags flags
     :locals locals
     :buffer nil
     :pos pos)))

(defun generate-list-of-n-backtrace-frames (n)
  "Returns a list with N random backtrace frames."
  (let* ((list-of-evalds (generate-list-of-n-booleans n))
	 (list-of-funs (generate-list-of-n-symbols n))
	 (list-of-args (generate-list-of-n-symbols n))
	 (list-of-locals (generate-list-of-n-alists n))
	 (list-of-flags (generate-seq-take-infinite-shuffled n (list :debug-on-exit :source-available nil)))
	 (list-of-positions (sort (generate-list-of-nat-numbers :exact-length n))))
    (seq-map-indexed (lambda (pos i) (backtrace-make-frame
				      :evald (nth i list-of-evalds)
				      :fun (nth i list-of-funs)
				      :args (nth i list-of-args)
				      :flags (nth i list-of-flags)
				      :locals (nth i list-of-locals)
				      :buffer nil
				      :pos pos))
		     list-of-positions)))

(defalias 'generate-random-list-of-backtrace-frames (generate-default-convert-n-gen-to-random #'generate-list-of-n-backtrace-frames))

(defalias 'generate--random-ert-test-outcome (-partial #'generate-seq-take-random-value-from-seq generate--DEFAULT-OUTCOMES))

(defalias 'generate--random-ert-expected-result-type (-partial #'generate-seq-take-random-value-from-seq generate--EXPECTED-RESULT-TYPES))

(defun generate--random-x-type-ert-test-outcome-base (default-outcomes-plist)
  (lambda (type expected-val)
    (lambda ()
      (let ((filtered-outcomes (map-filter (lambda (_ attributes) (equal (generate--plist-get type attributes) expected-val)) default-outcomes-plist)))
	(generate-map-random-key filtered-outcomes)))))

(defalias 'generate--random-x-type-ert-test-outcome (generate--random-x-type-ert-test-outcome-base generate--DEFAULT-OUTCOMES-PLIST))

(defalias 'generate--random-exclusive-ert-test-outcome (generate--random-x-type-ert-test-outcome :exclusive 't))

(defalias 'generate--random-non-exclusive-ert-test-outcome (generate--random-x-type-ert-test-outcome :exclusive 'nil))

(defalias 'generate--random-expected-ert-test-outcome (generate--random-x-type-ert-test-outcome :expectedp 't))

(defalias 'generate--random-unexpected-ert-test-outcome (generate--random-x-type-ert-test-outcome :expectedp 'nil))

(defun generate--make-should-form-gen-for-type-x (pcase-randomizer)
  (lambda (passing assert-symbol random-val)
    (let* ((random-n (generate-random-nat-number))
	   (should-val (funcall pcase-randomizer random-n random-val)))
      (pcase-exhaustive (list passing assert-symbol)
	(`(t should) (list assert-symbol should-val))
	(`(nil should) (list assert-symbol (list 'not should-val)))
	(`(t should-not) (list assert-symbol (list 'not should-val)))
	(`(nil should-not) (list assert-symbol should-val))))))

(defun generate--catchall-should-pcase (n val)
  (pcase-exhaustive (mod n 3)
    (0 (list 'equal val val))
    (1 (list 'equal (list val) (list val)))
    (2 (list 'equal (vector val) (vector val)))))

(defalias 'generate--catchall-should (generate--make-should-form-gen-for-type-x #'generate--catchall-should-pcase))

(defun generate--number-should-pcase (n number)
  (pcase-exhaustive (mod n 5)
    (0 (list 'numberp number))
    (1 (list 'plusp (abs number)))
    (2 (list 'minusp (* 1 (abs number))))
    (3 (list 'floatp (* 1.0 number)))
    (4 (generate--catchall-should-pcase n number))))

(defalias 'generate--number-should (generate--make-should-form-gen-for-type-x #'generate--number-should-pcase))

(defun generate--symbol-should-pcase (n symbol)
  (pcase-exhaustive (mod n 3)
    (0 (list 'symbolp symbol))
    (1 (equal (list 'symbol-name symbol) (list 'symbol-name symbol)))
    (2 (generate--catchall-should-pcase n symbol))))

(defalias 'generate--symbol-should (generate--make-should-form-gen-for-type-x #'generate--symbol-should-pcase))

(defun generate--seq-should-pcase (n seq)
  (pcase-exhaustive (mod n 5)
    (0 (list 'seqp seq))
    (1 (list 'equal (list 'seq-positions seq) (list 'seq-positions seq)))
    (2 (list 'equal (list 'seq-uniq seq) (list 'seq-uniq seq)))
    (3 (list 'seq-contains-p seq (seq-first seq)))
    (4 (generate--catchall-should-pcase n seq))))

(defalias 'generate--seq-should (generate--make-should-form-gen-for-type-x #'generate--seq-should-pcase))

(defun generate--map-should-pcase (n map)
  (pcase-exhaustive (mod n 5)
    (0 (list 'mapp map))
    (1 (list 'equal (list 'map-keys map) (list 'map-keys map)))
    (2 (list 'equal (list 'map-values map) (list 'map-values map)))
    (3 (list 'map-elt (car (map-keys map)) map))
    (4 (generate--catchall-should-pcase n map))))

(defalias 'generate--map-should (generate--make-should-form-gen-for-type-x #'generate--map-should-pcase))

;; is-type, is-equal-to-self, exists, extra
(cl-defun generate--should-form-for-type-x (&key passing assert-symbol)
  (cl-function (lambda (&optional val)
	       (let ((random-val (or val (generate-random-value))))
		 (pcase-exhaustive random-val
		   ((pred seqp) (generate--seq-should passing assert-symbol random-val))
		   ((pred mapp) (generate--map-should passing assert-symbol random-val))
		   ((pred symbolp) (generate--symbol-should passing assert-symbol random-val))
		   (_ (generate--catchall-should passing assert-symbol random-val)))))))

(defalias 'generate-passing-should-form (generate--should-form-for-type-x :passing t :assert-symbol 'should)
  "Returns a random passing should form.")

(defalias 'generate-passing-should-not-form (generate--should-form-for-type-x :passing t :assert-symbol 'should-not)
  "Returns a random passing should-not form.")

(defalias 'generate-failing-should-form (generate--should-form-for-type-x :passing 'nil :assert-symbol 'should)
  "Returns a random failing should form.")

(defalias 'generate-failing-should-not-form (generate--should-form-for-type-x :passing 'nil :assert-symbol 'should-not)
  "Returns a random failing should-not form.")

(defalias 'generate-random-passing-should (apply-partially #'generate-call-random-function (list #'generate-passing-should-form
											  #'generate-passing-should-not-form))
  "Returns a random passing should form.
The actual asserter will be either `should' or `should-not'.")

(defalias 'generate-random-failing-should (apply-partially #'generate-call-random-function (list #'generate-failing-should-form
											  #'generate-failing-should-not-form))
  "Returns a random failing should form.
The actual asserter will be either `should' or `should-not'.")

(defalias 'generate-random-should (apply-partially #'generate-call-random-function (list #'generate-passing-should-form
										  #'generate-passing-should-not-form
										  #'generate-failing-should-form
										  #'generate-failing-should-not-form))
  "Returns a random should form.
The actual asserter will be either `should' or `should-not'.
The form itself may be a passing form or a failing form.")

(cl-defun generate--list-of-n-passing-should-forms-helper (val (passing assert-symbol))
  (funcall (generate--should-form-for-type-x :passing passing :assert-symbol assert-symbol) val))

(defun generate-list-of-n-passing-should-forms (n)
  "Returns a list of N passing shoulds forms."
  (let* ((vals (generate-list-of-n-random-values n))
	 (should-count (generate--random-nat-number-between-0-and n))
	 (should-not-count (- n should-count))
	 (shoulds (generate-seq-take-infinite should-count (list (list 't 'should))))
	 (should-nots (generate-seq-take-infinite should-not-count (list (list 't 'should-not))))
	 (all-should-args (generate-append-and-shuffle shoulds should-nots)))
    (-zip-with #'generate--list-of-n-passing-should-forms-helper vals all-should-args)))

(defun generate-list-of-n-should-forms-with-a-fail (n)
  "Returns a list of N shoulds forms.
One of those forms will be a failing form."
  (let* ((passing-forms (generate-list-of-n-passing-should-forms (1- n)))
	 (failing-form (generate-random-failing-should)))
    (generate-append-and-shuffle passing-forms (list failing-form))))

(defalias 'generate-random-list-of-should-forms-with-a-fail (generate-default-convert-n-gen-to-random #'generate-list-of-n-should-forms-with-a-fail)
  "Returns a random list of should forms.
One of those forms will be a failing form.")

(defalias 'generate-random-list-of-passing-should-forms (generate-default-convert-n-gen-to-random #'generate-list-of-n-passing-should-forms)
  "Returns a random list of passing should forms.")

(defun generate--ert-test-failed-error (failing-should)
    "Returns a valid `ert-test-failed' error.
FAILING-SHOULD should be a valid `should', e.g., (should (equal x y))."
  (list 'ert-test-failed (list failing-should :form (cadr failing-should) :value nil)))

(defun generate--ert-test-failed-condition-helper (error-generators)
  (lambda (failing-should)
    (let* ((funcs (cons (apply-partially #'generate--ert-test-failed-error failing-should) error-generators)))
      (generate-call-random-function funcs))))

(defalias 'generate--ert-test-failed-condition (generate--ert-test-failed-condition-helper generate--ERROR-GENERATORS)
  "Returns a valid `ert-test-failed' condition.
FAILED-SHOULD should be a valid `should', e.g., (should (equal x y)).

\(fn FAILING-SHOULD)")

(defun generate--ert-test-skipped-condition (skipped-should)
  "Returns a valid `ert-test-skipped' condition.
SKIPPED-SHOULD should be a valid `should', e.g., (should (equal x y))."
  (list 'ert-test-skipped (list skipped-should :form (cadr skipped-should) :value 't)))

(cl-defun generate-ert-test-result-object (outcome duration)
  "Returns an ert-test-result object.
OUTCOME must be a valid ert-test outcome.
DURATION must be a number."
  (thunk-let* ((passing-should-forms (generate-random-list-of-passing-should-forms))
	       (random-ert-skipped-condition (generate--ert-test-skipped-condition (generate-seq-take-random-value-from-seq passing-should-forms)))
	       (list-of-should-forms-with-a-fail (generate-random-list-of-should-forms-with-a-fail))
	       (should-forms-with-a-fail (car list-of-should-forms-with-a-fail))
	       (failing-should (cadr should-forms-with-a-fail))
	       (random-ert-failed-condition (generate--ert-test-failed-condition failing-should))
	       (backtrace-frames (generate-random-list-of-backtrace-frames))
	       (random-message (generate-random-sentence)))
    (pcase outcome
      (:passed-expected (make-ert-test-passed :messages ""
					      :should-forms passing-should-forms
					      :duration duration))
      (:passed-unexpected (make-ert-test-passed :messages ""
						:should-forms passing-should-forms
						:duration duration))
      (:skipped (make-ert-test-skipped :messages ""
				       :should-forms passing-should-forms
				       :duration duration
				       :condition random-ert-skipped-condition
				       :backtrace backtrace-frames
				       :infos nil))
      (:failed-unexpected (make-ert-test-failed :messages random-message
						:should-forms should-forms-with-a-fail
						:duration duration
						:condition random-ert-failed-condition
						:backtrace backtrace-frames
						:infos nil))
      (:failed-expected (make-ert-test-failed :messages random-message
					      :should-forms should-forms-with-a-fail
					      :duration duration
					      :condition random-ert-failed-condition
					      :backtrace backtrace-frames
					      :infos nil)))))

(defun generate--plist-of-ert-test-result-objects (all-outcome-duration-pairs)
  (-flatten-n 1 (mapcar (-juxt #'car (-compose #'list (-applify #'generate-ert-test-result-object))) all-outcome-duration-pairs)))

(defun generate--take-from-plist-of-ert-test-results-helper (test-plist-of-ert-test-result-objects)
  (-lambda (outcome cnt-to-take)
    (generate-seq-take-infinite cnt-to-take (generate--plist-get outcome test-plist-of-ert-test-result-objects))))

(defun generate--take-from-plist-of-ert-test-results (test-plist-of-ert-test-result-objects test-outcomes-count-plist)
  (funcall (-compose (apply-partially #'-flatten-n 1) #'map-apply) (generate--take-from-plist-of-ert-test-results-helper test-plist-of-ert-test-result-objects) test-outcomes-count-plist))

(cl-defun generate-ert-test (test-name &key documentation tags file-name expected-result-type)
  "Returns an ert-test object named TEST-NAME.
DOCUMENTATION, TAGS, FILE-NAME and EXPECTED-RESULT-TYPE
are optional values that can be used to further customize
the ert-test object."
  (let ((test-symbol (intern test-name))
	(test-func-body (generate-random-should)))
    (make-ert-test
     :name test-symbol
     :tags (or tags '())
     :documentation (or documentation (generate-random-sentence))
     :body (lambda () test-func-body nil)
     :expected-result-type (or expected-result-type ':passed)
     :file-name (or file-name (generate-random-file-name)))))

(defalias 'generate-random-file-extension (apply-partially #'generate-seq-take-random-value-from-seq generate--FILE-EXTENSIONS) "Returns a random file extension.")

(defun generate-random-file-name ()
  "Returns a random file name."
  (concat (generate-random-word) "." (generate-random-file-extension)))

(defalias 'generate--nth-mod-file-extensions (-rpartial #'generate-nth-mod generate--FILE-EXTENSIONS))

(defconst generate--NUMBER-GENS
  (vector #'generate-random-float-between-0-and-1 #'generate-random-nat-number #'generate-random-negative-number))

(defconst generate--LIST-GENS
  (vector #'generate-list-of-nat-numbers
      #'generate-list-of-floats-between-0-and-1
      #'generate-list-of-floats
      #'generate-random-list-of-strings
      #'generate-random-list-of-lists-nat-numbers))

(defconst generate--HASH-TABLE-GENS
  (vector #'generate-random-hash-table-of-nat-numbers
    #'generate-random-hash-table-of-strings
    #'generate-random-hash-table-of-string-nat-number-pairs
    #'generate-random-hash-table-of-nat-number-string-pairs))

(defconst generate--VECTOR-GENS
  (vector
   #'generate-vector-of-n-nat-numbers
   #'generate-vector-of-floats
   #'generate-vector-of-floats-between-0-and-1
   #'generate-random-vector-of-strings
   #'generate-random-vector-of-vectors-nat-numbers
   #'generate-random-vector-of-lists-nat-numbers))

(defconst generate--ALIST-GENS
  (vector #'generate-random-alist-of-nat-numbers
    #'generate-random-alist-of-strings
    #'generate-random-alist-of-string-nat-number-cons
    #'generate-random-alist-of-nat-number-string-cons))

(defconst generate--PLIST-GENS
    (vector #'generate-random-plist-of-nat-numbers
      #'generate-random-plist-of-strings
      #'generate-random-plist-of-string-nat-number-pairs
      #'generate-random-plist-of-nat-number-string-pairs))


(defconst generate--STRING-GENS
  (vector #'generate-random-word #'generate-random-multiline-string))

(defconst generate--SEQ-GENS
  (vconcat generate--LIST-GENS generate--VECTOR-GENS generate--STRING-GENS))

(defconst generate--MAP-GENS
  (vconcat generate--LIST-GENS generate--HASH-TABLE-GENS generate--LIST-GENS))


(defconst generate--TYPE-GEN-MAP
  (list
   (cons "number" generate--NUMBER-GENS)
   (cons "list" generate--LIST-GENS)
   (cons "vector" generate--VECTOR-GENS)
   (cons "alist" generate--ALIST-GENS)
   (cons "plist" generate--PLIST-GENS)
   (cons "hash-table" generate--HASH-TABLE-GENS)
   (cons "seq" generate--SEQ-GENS)
   (cons "map" generate--MAP-GENS))
  "Each type of generator in this list will available at run time.")

(defalias 'generate--get-random-generator-type (-partial (-compose #'generate-seq-take-random-value-from-seq #'map-keys) generate--TYPE-GEN-MAP) "Get a random generator type from generate--TYPE-GEN-MAP.")
(defalias 'generate--get-generators-of-type-x (-partial #'map-elt generate--TYPE-GEN-MAP) "Get the vector of generators for TYPE from generate--TYPE-GEN-MAP.")
(defalias 'generate--get-random-generator (-compose #'generate-seq-take-random-value-from-seq (-partial #'generate-map-random-value generate--TYPE-GEN-MAP)) "Get a random generator from generate--TYPE-GEN-MAP.")

(cl-defmacro generate--create-generate-random-x ((type . generators-list))
  "Create a generate-random-x-type function for TYPE.
When the resulting function is called,
generate-call-random-function
will select a function from GENERATORS-LIST."
  (cl-with-gensyms (alias-name)
    `(let ((,alias-name (intern (format "generate-random-%s" ,type))))
       (defalias ,alias-name (apply-partially #'generate-call-random-function ,generators-list)))))

(cl-defmacro generate--create-list-of-n-xs ((type . generators-list))
  "Create a generate-list-of-n-xs function for TYPE.
When the resulting function is called,
generate-call-random-function-n-times
will select a function from GENERATORS-LIST."
  (cl-with-gensyms (alias-name)
    `(let ((,alias-name (intern (format "generate-list-of-n-%ss" ,type))))
       (defalias ,alias-name (-rpartial #'generate-call-random-function-n-times ,generators-list)))))

(cl-defmacro generate--create-generate-random-x-type-twice ((type . generators-list))
  "Create a generate-random-x-type-twice function for TYPE.
When the resulting function is called,
generate-call-random-function-n-times
will select a function from GENERATORS-LIST.
The selected function will be called twice."
  (cl-with-gensyms (alias-name)
    `(let ((,alias-name (intern (format "generate-random-%s-type-twice" ,type))))
       (defalias ,alias-name (apply-partially #'generate-call-random-function-n-times 2 ,generators-list)))))

(cl-defmacro generate--create-random-list-of-xs ((type . generators-list))
  "Create a generate-random-list-of-xs function for TYPE.
When the resulting function is called,
generate-call-random-function-random-times
will select a function from GENERATORS-LIST.
The selected function will be
called a random amount of times."
  (cl-with-gensyms (alias-name)
    `(let ((,alias-name (intern (format "generate-random-list-of-%ss" ,type))))
       (defalias ,alias-name (apply-partially #'generate-call-random-function-random-times ,generators-list)))))

(defmacro generate--create-list-of-generate-random-x (args)
  "Call generate--create-generate-random-x for each cons cell in ARGS."
  `(generate--plural! generate--create-generate-random-x ,args))

(defmacro generate--create-list-of-generate--create-list-of-n-xs (args)
  "Call generate-list-of-n-xs for each cons cell in ARGS."
  `(generate--plural! generate--create-list-of-n-xs ,args))

(defmacro generate--create-list-of-generate-random-x-type-twice (args)
  "Call generate--create-generate-random-x-type-twice for each cons cell in ARGS."
  `(generate--plural! generate--create-generate-random-x-type-twice ,args))

(defmacro generate--create-list-of-generate--create-random-list-of-xs (args)
  "Call generate-random-list-of-xs for each cons cell in ARGS."
  `(generate--plural! generate--create-random-list-of-xs ,args))

;; Make functions available at run time
(generate--create-list-of-generate-random-x generate--TYPE-GEN-MAP)
(generate--create-list-of-generate--create-list-of-n-xs generate--TYPE-GEN-MAP)
(generate--create-list-of-generate-random-x-type-twice generate--TYPE-GEN-MAP)
(generate--create-list-of-generate--create-random-list-of-xs generate--TYPE-GEN-MAP)

(defalias 'generate-random-value (-compose #'funcall #'generate--get-random-generator) "Returns a random value.")

(defalias 'generate-random-punctuation (apply-partially #'generate-seq-take-random-value-from-seq generate--PUNCTUATION) "Returns a random member of generate-PUNCTUATION.")

(defalias 'generate-random-color (-compose (-applify #'color-rgb-to-hex) (apply-partially #'generate-list-of-floats-between-0-and-1 :exact-length 3)))

(defun generate-list-of-n-colors (n)
  "Returns a list of N colors.
Values are hexadecimals."
  (let* ((float-count (* n 3))
	 (floats (generate-list-of-floats-between-0-and-1 :exact-length float-count)))
    (funcall (-compose (apply-partially #'mapcar (-applify #'color-rgb-to-hex)) #'-partition) 3 floats)))

(defalias 'generate-random-list-of-colors (generate-default-convert-n-gen-to-random #'generate-list-of-n-colors))

(defun generate--activate-font-lock-keywords ()
"Activate font-lock keywords for some of ERT's symbols."
(font-lock-add-keywords
 nil
 '(("(\\(\\<generate-ert-deftest-n-times\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face nil t)
    (2 font-lock-function-name-face nil t)))))

(add-hook 'emacs-lisp-mode-hook #'generate--activate-font-lock-keywords)
;;add hook for ob-src-blocks

(provide 'generate)
;;; generate.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
