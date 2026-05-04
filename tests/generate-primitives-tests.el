;;; generate-primitives-tests.el --- Tests for primitive generators  -*- lexical-binding: t; -*-

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

(require 'seq)
(require 'map)
(require 'time-date)
(require 'calc-comb)
(require 'hex-util)
(require 's)
(require 'dash)
(require 'generate)
(require 'ert)

(defconst MAX-PRECISON
  10000000 "I can not gurantee results will be accurate for numbers larger than 1000000.")

(generate-ert-deftest-n-times generate--times ()
  :num-runs 100
  (-let* ((((expected-num test-func) test-calls) (funcall (-juxt (-compose (-juxt #'identity #'cl-constantly) #'generate--random-nat-number-in-range-255) #'generate--random-nat-number-in-range-255)))
	  ((actual-seq actual-seq-length) (funcall (-compose #'generate--identity-and-seq-length #'generate--times) test-calls test-func)))
    (should (seq-every-p (-partial #'eql expected-num) actual-seq))
    (should (eql actual-seq-length test-calls))))

(generate-ert-deftest-n-times generate--zip-pair-longest ()
  :num-runs 100
  (-let* (((test-group-one test-group-two) (generate--times-no-args-twice #'generate-random-list-of-strings))
	  (expected-length (-max (mapcar #'length (list test-group-one test-group-two))))
	  (actual-list (generate--zip-pair-longest test-group-one test-group-two))
	  ((actual-car . actual-cdr) (generate-seq-take-random-value-from-seq actual-list)))
    (should (length= actual-list expected-length))
    (should (member actual-car test-group-one))
    (should (member actual-cdr test-group-two))))

(generate-ert-deftest-n-times generate--zip-pair-first ()
  :num-runs 100
  (-let* (((test-group-one test-group-two) (generate--times-no-args-twice #'generate-random-list-of-strings))
	  (expected-length (length test-group-one))
	  (actual-list (generate--zip-pair-first test-group-one test-group-two))
	  ((actual-car . actual-cdr) (generate-seq-take-random-value-from-seq actual-list)))
    (should (length= actual-list expected-length))
    (should (member actual-car test-group-one))
    (should (member actual-cdr test-group-two))))

(generate-ert-deftest-n-times generate--convert-calc-value-into-lisp ()
  :num-runs 100
  (should (floatp (generate--convert-calc-value-into-lisp (math-gaussian-float)))))

(generate-ert-deftest-n-times generate--between-1-and-x-exclusive-p-true ()
  :num-runs 100
  (-let* (((test-p test-nat-number) (funcall (-compose (-juxt #'1+ #'identity) #'generate--random-nat-number-in-range-255))))
    (should (funcall (generate--between-1-and-x-exclusive-p test-p) test-nat-number))))

(generate-ert-deftest-n-times generate--between-1-and-x-exclusive-p-false ()
  :num-runs 100
  (-let* (((test-nat-number test-p) (funcall (-compose (-juxt #'1+ #'identity) #'generate--random-nat-number-in-range-255))))
    (should-not (funcall (generate--between-1-and-x-exclusive-p test-p) test-nat-number))))

(generate-ert-deftest-n-times generate--non-zero-bounded-modular-addition-max-test ()
  :num-runs 100
  (let* ((range-max (random MAX-PRECISON))
  	 (range-min (- range-max (random range-max) 2))
  	 (increase 1)
  	 (expected-result range-min)
  	 (current-number (1- range-max))
  	 (actual-result (generate--non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
    (should (eql actual-result expected-result))))

(generate-ert-deftest-n-times generate--non-zero-bounded-modular-addition-min-test ()
  :num-runs 100
  (let* ((range-max (random MAX-PRECISON))
	 (range-min (- range-max (random range-max) 2))
	 (increase 1)
	 (expected-result (1+ range-min))
	 (current-number range-min)
	 (actual-result (generate--non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
    (should (eql actual-result expected-result))))

(generate-ert-deftest-n-times generate--non-zero-bounded-modular-addition-basic-nat-number-test ()
  :num-runs 100
  (let* ((range-max (random MAX-PRECISON))
	 (range-min (- range-max (random range-max) 2))
	 (increase (random range-max))
	 (current-number (random range-max))
	 (actual-result (generate--non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
    (should (and (g--gte actual-result range-min) (g--lt actual-result range-max)))))

(generate-ert-deftest-n-times generate--divide-list-values-by-max-list-value ()
  :num-runs 100
  (let ((actual-list (funcall (-compose #'generate--divide-list-values-by-max-list-value #'generate--random-nat-number-list-in-range-255))))
    (should (generate--seq-every-p-between-0-and-1-inclusive actual-list))))

(generate-ert-deftest-n-times generate--lisp-timestampp-true ()
  :num-runs 100
  (should (decode-time (generate-random-lisp-timestamp))))

(generate-ert-deftest-n-times generate--lisp-timestampp-false-one ()
  :num-runs 100
  (should-not (generate--lisp-timestampp (generate-list-of-nat-numbers))))

(generate-ert-deftest-n-times generate--lisp-timestampp-false-two ()
  :num-runs 100
  (should-not (generate--lisp-timestampp (cons (generate-random-nat-number) 0))))

(generate-ert-deftest-n-times generate-random-float ()
  :num-runs 100
  (should (floatp (generate-random-float))))

(generate-ert-deftest-n-times generate-random-float-between-0-and-1 ()
  :num-runs 100
  (let ((actual-float (generate-random-float-between-0-and-1)))
    (should (floatp actual-float))
    (should (g--lt1 actual-float))
    (should (g--gte actual-float 0))))

(generate-ert-deftest-n-times generate-random-nat-number-in-range ()
:num-runs 100
      (let* ((test-max (random MAX-PRECISON))
  	   (test-min (- test-max (random test-max) 2))
  	   (actual-nat-number (generate-random-nat-number-in-range (list test-min test-max))))
	(should (natnump actual-nat-number))
        (should (g--gte actual-nat-number test-min))
        (should (g--lt actual-nat-number test-max))))

(generate-ert-deftest-n-times generate-two-random-nat-numbers-in-range-min-not-equal-max ()
  :num-runs 100
   (-let* ((test-max (random MAX-PRECISON))
	  (test-min (- test-max (random test-max) 1))
	  ((actual-rand-one actual-rand-two) (generate-two-random-nat-numbers-in-range (list test-min test-max)))
	  (actual-random-nat-number (generate-seq-take-random-value-from-seq (list actual-rand-one actual-rand-two))))
     (should (natnump actual-random-nat-number))
     (should (g--gte actual-random-nat-number test-min))
     (should (g--lt actual-random-nat-number test-max))
     (should-not (equal actual-rand-one actual-rand-two))))

(generate-ert-deftest-n-times generate-two-random-nat-numbers-in-range-min-equal-max ()
  :num-runs 100
   (-let* ((test-max (random MAX-PRECISON))
	  ((actual-rand-one actual-rand-two) (generate-two-random-nat-numbers-in-range (list test-max test-max)))
     (should (natnump actual-rand-one))
     (should (natnump actual-rand-two))
     (should (= actual-rand-one actual-rand-two test-max)))))

(generate-ert-deftest-n-times generate--random-nat-number-list ()
:num-runs 100
  (let ((actual-list (funcall (-compose #'generate--random-nat-number-list #'calcFunc-random) 255)))
  (should (generate--seq-every-p-nat-number actual-list))))

(generate-ert-deftest-n-times generate-nat-number-range ()
:num-runs 100
  (-let (((actual-range expected-range-length) (funcall (-juxt #'generate-nat-number-range #'identity) (generate-random-nat-number-in-range (list 1 10)))))
  (should (eql (generate--range-size actual-range) expected-range-length))))

(generate-ert-deftest-n-times generate--divide-by-random-value ()
:num-runs 100
  (-let (((actual-result actual-input-value) (funcall (-compose (-juxt #'generate--divide-by-random-value #'identity) #'generate--random-nat-number-in-range-255))))
    (should (floatp actual-result))
    (should (g--lte actual-result actual-input-value))))

(generate-ert-deftest-n-times generate-call-random-function ()
  :num-runs 100
  (-let* (((expected-super-set test-list) (funcall (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate-list-of-nat-numbers)))
	  (actual-value (generate-call-random-function test-list)))
    (should (member actual-value expected-super-set))))

(generate-ert-deftest-n-times generate-call-random-function-n-times ()
  :num-runs 100
  (-let* ((((expected-super-set test-list) test-calls) (funcall (-juxt (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate-list-of-nat-numbers) #'generate--random-nat-number-in-range-255)))
	  (actual-seq (generate-call-random-function-n-times test-calls test-list)))
    (should (cl-subsetp actual-seq expected-super-set))
    (should (g--len-eq actual-seq test-calls))))

(generate-ert-deftest-n-times generate-call-n-random-functions ()
  :num-runs 100
  (-let* (((expected-super-set test-list) (funcall (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate-list-of-nat-numbers)))
	  (test-n (generate--seq-random-chunk-length expected-super-set))
	  (actual-list (generate-call-n-random-functions test-n test-list)))
    (should (cl-subsetp actual-list expected-super-set))
    (should (g--len-eq actual-list test-n))))

(generate-ert-deftest-n-times generate-call-function-random-times ()
  :num-runs 100
  (-let* (((test-cl-constantly) (generate-random-cl-constantly))
	  (actual-values (generate-call-function-random-times test-cl-constantly)))
    (should actual-values)))

(generate-ert-deftest-n-times generate--divide-list-values-by-random-value ()
:num-runs 100
  (let ((actual-list (funcall (-compose #'generate--divide-list-values-by-random-value #'generate--random-nat-number-list-in-range-255))))
    (should (generate--seq-every-p-float actual-list))))

(generate-ert-deftest-n-times generate-random-list-of-cl-constantlys ()
:num-runs 100
  (-let* (((expected-super-set test-list) (generate-random-list-of-cl-constantlys)))
    (should (eql (seq-length expected-super-set) (seq-length test-list)))
    (should (generate--seq-every-p-nat-number expected-super-set))
    (should (generate--seq-every-p-function test-list))))

(generate-ert-deftest-n-times generate--seq-take-last-for-lists ()
  :num-runs 100
  (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'generate--seq-random-chunk-length) #'generate-list-of-nat-numbers)))
	  ((actual-result actual-result-length) (funcall (-compose #'generate--identity-and-seq-length #'generate--seq-take-last) test-chunk-length test-list)))
    (should (eql actual-result-length test-chunk-length))
    (should (cl-subsetp actual-result test-list))))

(generate-ert-deftest-n-times generate--seq-take-last-for-vectors ()
  :num-runs 100
  (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'generate--seq-random-chunk-length) #'generate-vector-of-n-nat-numbers)))
	  ((actual-result actual-result-length) (funcall (-compose #'generate--identity-and-seq-length #'generate--seq-take-last) test-chunk-length test-list)))
    (should (eql actual-result-length test-chunk-length))
    (should (vectorp actual-result))))

(generate-ert-deftest-n-times generate--seq-take-last-for-strings ()
  :num-runs 100
  (-let* (((test-string test-chunk-length) (funcall (-compose (-juxt #'identity #'generate--seq-random-chunk-length) #'generate-random-word)))
	  ((actual-result actual-result-length) (funcall (-compose #'generate--identity-and-seq-length #'generate--seq-take-last) test-chunk-length test-string)))
    (should (eql actual-result-length test-chunk-length))
    (should (stringp actual-result))))

(generate-ert-deftest-n-times generate--seq-butlast ()
  :num-runs 100
  (-let* ((test-seq (generate-random-seq))
	  (expected-last-item-as-seq (funcall (-compose #'list #'generate--seq-last) test-seq))
	  ((actual-seq actual-seq-length) (funcall (-compose (-juxt #'identity #'seq-length) #'generate--seq-butlast) test-seq)))
    (should (eql actual-seq-length (1- (seq-length test-seq))))))

(generate-ert-deftest-n-times generate--seq-reduce-right-indexed ()
  :num-runs 100
  (-let* ((test-seq (generate-random-seq))
	 (test-initial-value (seq-first test-seq))
	 ((actual-result actual-index) (generate--seq-reduce-right-indexed (lambda (a b i) (list a i)) test-seq test-initial-value)))
    (should (equal actual-result (seq-first test-seq)))
    (should (zerop actual-index))))

(generate-ert-deftest-n-times generate--seq-reduce-right ()
  :num-runs 100
  (let* ((test-seq (generate-random-seq))
	 (test-initial-value (seq-first test-seq))
	 (actual-result (generate--seq-reduce-right (lambda (a b) a) test-seq test-initial-value)))
    (should (equal actual-result (seq-first test-seq)))))

(generate-ert-deftest-n-times generate-seq-shuffle-list ()
  :num-runs 100
  (-let* (((actual-shuffled-list test-list) (funcall (-compose (-juxt #'generate-seq-shuffle #'identity) #'generate-random-list-of-strings))))
    (should (seq-set-equal-p actual-shuffled-list test-list))))

(generate-ert-deftest-n-times generate-seq-shuffle-vector ()
:num-runs 100
  (-let* (((actual-shuffled-vector test-vector) (funcall (-compose (-juxt #'generate-seq-shuffle #'identity) #'generate-vector-of-n-nat-numbers))))
    (should (vectorp actual-shuffled-vector))
    (should (seq-set-equal-p actual-shuffled-vector test-vector))))

(generate-ert-deftest-n-times generate-seq-shuffle-string ()
:num-runs 100
  (-let* (((actual-shuffled-string test-string) (funcall (-compose (-juxt #'generate-seq-shuffle #'identity) #'generate-random-word))))
        (should (stringp actual-shuffled-string))
	(should-not (seq-difference actual-shuffled-string test-string))))

(generate-ert-deftest-n-times generate--seq-random-chunk-length ()
:num-runs 100
    (-let* (((test-chunk-length test-list-length) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'seq-length) #'generate-list-of-nat-numbers) :min-length 2)))
      (should (g--lt test-chunk-length test-list-length))
      (should (g--gte test-chunk-length 1))))

;; this can take :min-length 2?
(generate-ert-deftest-n-times generate-seq-n-random-values-list ()
  :num-runs 100
  (-let* (((test-count test-list) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-random-list-of-strings)))
	  (actual-length (funcall (-compose #'seq-length #'generate-seq-n-random-values) test-count test-list)))
    (should (eql actual-length test-count))))

(generate-ert-deftest-n-times generate-seq-n-random-values-vector ()
:num-runs 100
  (-let* (((test-count test-vector) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-vector-of-n-nat-numbers)))
	 ((actual-vector actual-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-n-random-values) test-count test-vector)))
    (should (vectorp actual-vector))
    (should (eql actual-length test-count))))

(generate-ert-deftest-n-times generate-seq-n-random-values-string ()
:num-runs 100
  (-let* (((test-count test-string) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-random-word)))
	 ((actual-string actual-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-n-random-values) test-count test-string)))
    (should (stringp actual-string))
    (should (eql actual-length test-count))))

(generate-ert-deftest-n-times generate-seq-random-values-lists ()
  :num-runs 100
  (-let* ((((actual-list actual-list-length) (test-list test-list-length))
	   (funcall (-compose (-partial #'seq-map #'generate--identity-and-seq-length) (-juxt #'generate-seq-random-values #'identity) #'generate-random-list-of-strings))))
    (should (listp actual-list))
    (should (g--lte actual-list-length test-list-length))))

(generate-ert-deftest-n-times generate-seq-random-values-vectors ()
:num-runs 100
  (-let* ((((actual-vector actual-vector-length) (test-vector test-vector-length))
	  (funcall (-compose (-partial #'seq-map #'generate--identity-and-seq-length) (-juxt #'generate-seq-random-values #'identity) #'generate-vector-of-n-nat-numbers))))
    (should (vectorp actual-vector))
    (should (g--lte actual-vector-length test-vector-length))))

(generate-ert-deftest-n-times generate-seq-random-values-strings ()
:num-runs 100
  (-let* ((((actual-string actual-string-length) (test-string test-string-length))
	  (funcall (-compose (-partial #'seq-map #'generate--identity-and-seq-length) (-juxt #'generate-seq-random-values #'identity) #'generate-random-word))))
    (should (stringp actual-string))
    (should (g--lte actual-string-length test-string-length))))

(generate-ert-deftest-n-times generate--seq-random-iterate-from-max-lists ()
:num-runs 100
  (-let* (((actual-list test-list-max) (funcall (-compose (-juxt #'generate--seq-random-iterate-from-max #'seq-max) #'generate-list-of-floats))))
    (should (seq-every-p (-rpartial #'g--gte test-list-max) actual-list))))

(generate-ert-deftest-n-times generate--seq-random-iterate-from-max-vectors ()
:num-runs 100
  (-let* (((actual-vector test-vector-max) (funcall (-compose (-juxt #'generate--seq-random-iterate-from-max #'seq-max) #'generate-vector-of-n-nat-numbers))))
    (should (vectorp actual-vector))
    (should (seq-every-p (-rpartial #'g--gte test-vector-max) actual-vector))))

(generate-ert-deftest-n-times generate--seq-random-iterate-from-max-strings ()
:num-runs 100
  (-let* (((actual-string test-string-max) (funcall (-compose (-juxt #'generate--seq-random-iterate-from-max #'seq-max) #'generate-random-word))))
    (should (stringp actual-string))
    (should (seq-every-p (-rpartial #'g--gte test-string-max) actual-string))))

(generate-ert-deftest-n-times generate-seq-random-position-lists ()
:num-runs 100
  (-let* ((((test-list test-list-length) actual-position) (funcall (-compose (-juxt #'generate--identity-and-seq-length #'generate-seq-random-position) #'generate-random-list-of-strings))))
	(should (funcall (generate--between-0-and-x-exclusive-p test-list-length) actual-position))))

(generate-ert-deftest-n-times generate-seq-random-position-vectors ()
:num-runs 100
  (-let* ((((test-vector test-vector-length) actual-position) (funcall (-compose (-juxt #'generate--identity-and-seq-length #'generate-seq-random-position) #'generate-vector-of-n-nat-numbers))))
	(should (funcall (generate--between-0-and-x-exclusive-p test-vector-length) actual-position))))

(generate-ert-deftest-n-times generate-seq-random-position-strings ()
:num-runs 100
  (-let* ((((test-string test-string-length) actual-position) (funcall (-compose (-juxt #'generate--identity-and-seq-length #'generate-seq-random-position) #'generate-random-word))))
	(should (funcall (generate--between-0-and-x-exclusive-p test-string-length) actual-position))))

(generate-ert-deftest-n-times generate-seq-random-value-with-position ()
:num-runs 100
  (-let* ((test-seq (generate-random-seq))
	  ((actual-item actual-position) (generate-seq-random-value-with-position test-seq)))
    (should (seq-contains-p test-seq actual-item))
    (should (funcall (generate--between-0-and-x-exclusive-p (seq-length test-seq)) actual-position))))

(generate-ert-deftest-n-times generate-seq-split-random-list ()
:num-runs 100
  (let ((actual-list (funcall (-compose #'generate-seq-split-random #'generate-random-list-of-strings))))
    (should (generate--seq-every-p-list actual-list))))

(generate-ert-deftest-n-times generate-seq-split-random-vector ()
:num-runs 100
 (let ((actual-vector (funcall (-compose #'generate-seq-split-random #'generate-vector-of-n-nat-numbers) :min-length 2)))
    (should (generate--seq-every-p-vector actual-vector))))

(generate-ert-deftest-n-times generate-seq-split-random-string ()
:num-runs 100
  (let ((actual-string (funcall (-compose #'generate-seq-split-random #'generate-random-word))))
    (should (generate--seq-every-p-string actual-string))))

(generate-ert-deftest-n-times generate-seq-random-chunk-of-size-n-string ()
:num-runs 100
    (-let* (((test-chunk-length test-string) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-random-word)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-random-chunk-of-size-n) test-chunk-length test-string)))
      (should (stringp actual-chunk))
      (should (s-contains? actual-chunk test-string))))

(generate-ert-deftest-n-times generate-seq-random-chunk-of-size-n-list ()
:num-runs 100
    (-let* (((test-chunk-length test-list) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-list-of-nat-numbers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-random-chunk-of-size-n) test-chunk-length test-list)))
      (should (listp actual-chunk))
      (should (cl-subsetp actual-chunk test-list))))

(generate-ert-deftest-n-times generate-seq-random-chunk-of-size-n-vector ()
:num-runs 100
    (-let* (((test-chunk-length test-vector) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-vector-of-n-nat-numbers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-random-chunk-of-size-n) test-chunk-length test-vector)))
      (should (vectorp actual-chunk))
      (should-not (seq-difference (seq-union actual-chunk test-vector) (seq--into-list test-vector)))))

(generate-ert-deftest-n-times generate-seq-take-infinite ()
  :num-runs 100
  (-let* (((test-seq test-n) (funcall (-juxt #'generate-random-seq #'generate--random-nat-number-in-range-500)))
	  ((actual-seq actual-seq-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-take-infinite) test-n test-seq)))
    (should (equal (cl-type-of actual-seq) (cl-type-of test-seq)))
    (should (eql actual-seq-length test-n))))

(generate-ert-deftest-n-times generate-seq-subseq-infinite ()
:num-runs 100
  (-let* (((test-seq test-subseq-start) (funcall (-compose (-juxt #'identity #'generate-seq-random-position) #'generate-random-seq)))
	  ((test-subseq-end expected-subseq-length) (funcall (-compose (-juxt #'identity (-rpartial #'- test-subseq-start)) (-partial #'+ test-subseq-start) #'generate--random-nat-number-in-range-255)))
	  ((actual-seq actual-seq-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-subseq-infinite) test-seq test-subseq-start test-subseq-end)))
    (should (eql actual-seq-length expected-subseq-length))))

(generate-ert-deftest-n-times generate-seq-n-random-infinite-subseqs ()
  :num-runs 100
  (-let* ((test-subseqs-count (1+ (generate--random-nat-number-in-range-10)))
	  (test-seq (generate-random-seq))
	  (actual-subseqs (generate-seq-n-random-infinite-subseqs test-subseqs-count test-seq)))
    (should (length= actual-subseqs test-subseqs-count))))

(generate-ert-deftest-n-times generate-seq-split-infinite ()
  :num-runs 100
  (-let* ((test-chunk-size 5)
	  (test-seq (generate-random-seq))
	  ((actual-seq actual-random-chunk) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'generate-seq-split-infinite) test-chunk-size test-seq)))
    (should (length= actual-random-chunk test-chunk-size))))

(generate-ert-deftest-n-times generate-seq-n-random-chunks-of-size-x ()
:num-runs 100
  (-let* (((test-chunk-length test-chunk-count test-seq) (funcall (-juxt #'generate--random-nat-number-in-range-50 #'generate--random-nat-number-in-range-3-25 #'generate-random-seq)))
	  ((actual-chunked-seq actual-random-chunk) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'generate-seq-n-random-chunks-of-size-x) test-chunk-length test-chunk-count test-seq)))
  (should (length= actual-chunked-seq test-chunk-count))
  (should (cl-typep actual-random-chunk (cl-type-of test-seq)))
  (should (length= actual-random-chunk test-chunk-length))))

(generate-ert-deftest-n-times generate-seq-n-random-chunks-of-random-size ()
  :num-runs 100
  (-let* (((test-chunk-count test-seq) (funcall (-juxt #'generate--random-nat-number-in-range-255 #'generate-random-seq)))
	  ((actual-chunked-seq actual-random-chunk) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'generate-seq-n-random-chunks-of-random-size) test-chunk-count test-seq)))
    (should (length= actual-chunked-seq test-chunk-count))
    (should (cl-typep actual-random-chunk (cl-type-of test-seq)))))

(generate-ert-deftest-n-times generate--map-on-alist-of-nat-numbers ()
:num-runs 100
  (-let* ((test-alist (generate-random-alist-of-nat-numbers))
	  ((actual-car . actual-cdr) (generate--map-on #'generate--applify-cons #'-sum #'-sum test-alist)))
  (should (natnump actual-car))
  (should (natnump actual-cdr))))

(generate-ert-deftest-n-times generate--map-on-alist-of-strings ()
  :num-runs 100
  (-let* (((test-alist test-alist-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-random-alist-of-strings)))
	  (actual-result (generate--map-on #'identity (-applify #'concat) (-applify #'concat) test-alist)))
    (should (generate--seq-every-p-string actual-result))
    (should (seq-every-p (-rpartial #'length= test-alist-length) actual-result))))

(generate-ert-deftest-n-times generate--map-on-alist-of-string-nat-number-cons ()
  :num-runs 100
  (-let* (((test-alist test-alist-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-random-alist-of-string-nat-number-cons)))
	  ((actual-string actual-sum) (generate--map-on #'identity  (-applify #'concat) #'-sum test-alist)))
    (should (stringp actual-string))
    (should (length= actual-string test-alist-length))
    (should (integerp actual-sum))))

(generate-ert-deftest-n-times generate-map-random-key ()
  :num-runs 100
  (-let* ((test-map (generate-random-map))
	  (actual-key (generate-map-random-key test-map))
	  (expected-keys (map-keys test-map)))
    (should (member actual-key expected-keys))))

(generate-ert-deftest-n-times generate-map-random-value ()
  :num-runs 100
  (-let* ((test-map (generate-random-map))
	  (actual-value (generate-map-random-value test-map))
	  (expected-values (map-values test-map)))
    (should (member actual-value expected-values))))

(generate-ert-deftest-n-times generate-map-random-pair ()
  :num-runs 100
  (-let* ((test-map (generate-random-map))
	  ((actual-key actual-value) (generate-map-random-pair test-map)))
    (should (equal (map-elt test-map actual-key #'equal) actual-value))))

(generate-ert-deftest-n-times generate--plist-get ()
  :num-runs 100
  (-let* ((test-plist (generate-random-plist))
	  ((test-key-to-get expected-value) (generate-map-random-pair test-plist))
    (should (equal (generate--plist-get test-key-to-get test-plist) expected-value)))))

(generate-ert-deftest-n-times generate--plist-put ()
  :num-runs 100
  (-let* ((test-plist (generate-random-plist))
	  (test-key-to-update (generate-map-random-key test-plist))
	  (test-new-value (generate-random-value))
	  (updated-plist (generate--plist-put test-key-to-update test-new-value test-plist)))
    (should (equal (generate--plist-get test-key-to-update updated-plist) test-new-value))
    ;; old value should not change
    (should-not (equal (generate--plist-get test-key-to-update test-plist) test-new-value))))

(generate-ert-deftest-n-times generate-data-fail ()
:num-runs 100
  (-let* (((test-max-length test-plus) (generate-random-nat-number-twice))
	   (test-min-length (+ test-max-length test-plus)))
    (should-error (generate-data :min-length test-max :max-length test-min-length))))

(generate-ert-deftest-n-times generate-list-of-nat-numbers ()
:num-runs 100
   (let ((test-list (generate-list-of-nat-numbers)))
    (should (generate--seq-every-p-nat-number test-list))))

(generate-ert-deftest-n-times generate-list-of-nat-numbers-with-exact-length ()
:num-runs 100
   (let* ((test-exact-length (generate--random-nat-number-in-range-25))
	 (test-list (generate-list-of-nat-numbers :exact-length test-exact-length)))
    (should (equal (seq-count #'natnump test-list) test-exact-length))))

(generate-ert-deftest-n-times generate-list-of-floats-1 ()
:num-runs 100
  (let ((test-list (generate-list-of-floats)))
    (should (generate--seq-every-p-float test-list))))

(generate-ert-deftest-n-times generate-list-of-floats-2 ()
:num-runs 100
    (let ((test-list (generate-list-of-floats-between-0-and-1)))
    (should (generate--seq-every-p-float test-list))))

(generate-ert-deftest-n-times generate-list-of-lists-of-nat-numbers ()
:num-runs 100
    (let ((test-list (generate-random-list-of-lists-nat-numbers)))
    (should (generate--seq-every-p-list test-list))))

(generate-ert-deftest-n-times generate-list-of-nat-numbers-in-range ()
  :num-runs 100
  (let* ((test-range (generate-random-nat-number-range))
	 (test-list-size (generate--random-nat-number-in-range-25))
	 (actual-list (generate-list-of-nat-numbers-in-range test-range :exact-length test-list-size))
	 (actual-random-value (generate-seq-take-random-value-from-seq actual-list)))
    (should (generate--in-range-exclusive-p test-range actual-random-value))
    (should (length= actual-list test-list-size))))

(generate-ert-deftest-n-times generate-list-of-n-random-values ()
  :num-runs 100
  (let* ((test-list-size (generate--random-nat-number-in-range-25))
	 (actual-list (generate-list-of-n-random-values test-list-size)))
    (should (length= actual-list test-list-size))))

(generate-ert-deftest-n-times generate-n-alpha-string-characters ()
:num-runs 100
  (-let* ((test-character-count (generate--random-nat-number-in-range-25))
	((actual-string-characters actual-random-string-character) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq)
										      #'generate-n-alpha-string-characters)
									    test-character-count)))
    (should (stringp actual-random-string-character))
    (should (length= actual-string-characters test-character-count))))

(generate-ert-deftest-n-times generate-random-word ()
:num-runs 100
  (-let ((actual-word (generate-random-word)))
    (should (stringp actual-word))
    (should (g--len-gt actual-word 1))))

(generate-ert-deftest-n-times generate-list-of-n-words ()
:num-runs 100
  (-let* ((word-count (1+ (generate--random-nat-number-in-range-25)))
	  (actual-words (generate-list-of-n-words word-count)))
    (should (generate--seq-every-p-string actual-words))
    (should (length= actual-words word-count))))

(defconst TESTGENS
  (list #'generate-random-card-number #'generate-random-time-string #'generate-random-date-string #'generate-random-phone-number #'generate-random-string-of-lower-alphanums #'generate-random-string-of-upper-alphanums))

(generate-ert-deftest-n-times generate--list-of-n-sentences-base-default ()
  :num-runs 100
  (-let* ((sentence-count (generate--random-nat-number-in-range-10))
	((actual-result actual-all-words) (generate--list-of-n-sentences-base sentence-count))
	(actual-random-value (generate-seq-take-random-value-from-seq actual-result)))
    (should (s-ends-with-p "." actual-random-value))
    (should (length= actual-result sentence-count))
    (should (listp actual-all-words))))

(generate-ert-deftest-n-times generate--list-of-n-sentences-base-with-extra ()
  :num-runs 100
  (-let* ((test-gen (generate-seq-take-random-value TESTGENS))
      (sentence-count (generate--random-nat-number-in-range-10))
      ((actual-result actual-all-words actual-list-of-regular-words actual-list-of-words-from-gens) (generate--list-of-n-sentences-base sentence-count test-gen))
      (actual-random-value (generate-seq-take-random-value-from-seq actual-result)))
  (should (s-ends-with-p "." actual-random-value))
  (should (length= actual-result sentence-count))
  (should (listp actual-all-words))
  (should (listp actual-list-of-regular-words))
  (should (listp actual-list-of-words-from-gens))))

(generate-ert-deftest-n-times generate--random-list-of-sentences-base ()
  :num-runs 100
  (-let (((actual-sentences) (generate--random-list-of-sentences-base)))
    (should (g--gt (seq-count #'stringp actual-sentences) 2))))

(generate-ert-deftest-n-times generate--string-with-n-lines-base-default ()
  :num-runs 100
  (-let* ((line-count (generate--random-nat-number-in-range-10))
       ((actual-buffer-lines actual-list-of-sentences actual-all-words) (generate--string-with-n-lines-base line-count)))
    (should (equal (s-count-matches "\n" actual-buffer-lines) (1- line-count)))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))))

(generate-ert-deftest-n-times generate--string-with-n-lines-base-with-extra ()
  :num-runs 100
  (-let* ((test-gens (generate-seq-two-random-values TESTGENS))
	(line-count (1+ (generate--random-nat-number-in-range-10)))
       ((actual-buffer-lines actual-list-of-sentences actual-all-words actual-regular-words actual-words-from-gens) (generate--string-with-n-lines-base line-count test-gens)))
    (should (equal (s-count-matches "\n" actual-buffer-lines) (1- line-count)))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))
    (should (listp actual-regular-words))
    (should (generate--len-gt actual-words-from-gens 1))))

(generate-ert-deftest-n-times generate--random-multiline-string-base-default ()
  :num-runs 100
  (-let (((actual-buffer-lines actual-list-of-sentences actual-all-words) (generate--random-multiline-string-base)))
    (should (stringp actual-buffer-lines))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))))

(generate-ert-deftest-n-times generate--random-multiline-string-base-with-extra ()
  :num-runs 100
  (-let* ((test-gens (generate-seq-two-random-values TESTGENS))
       ((actual-buffer-lines actual-list-of-sentences actual-all-words actual-regular-words actual-words-from-gens) (generate--random-multiline-string-base test-gens)))
    (should (stringp actual-buffer-lines))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))
    (should (listp actual-regular-words))
    (should (generate--len-gt actual-words-from-gens 1))))

(generate-ert-deftest-n-times generate-random-alist-of-nat-numbers ()
  :num-runs 100
    (-let (((actual-alist actual-car-and-actual-cdr) (funcall (-compose (-juxt #'identity (-compose #'-cons-to-list #'generate-seq-take-random-value-from-seq)) #'generate-random-alist-of-nat-numbers))))
      (should (generate--seq-every-p-nat-number actual-car-and-actual-cdr))
      (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-alist-of-strings ()
  :num-runs 100
    (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity (-compose #'generate-seq-take-random-value-from-seq)) #'generate-random-alist-of-strings))))
      (should (stringp actual-random-car))
      (should (stringp actual-random-cdr))
      (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-alist-of-strings-nat-number-cons ()
  :num-runs 100
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'generate-random-alist-of-string-nat-number-cons))))
    (should (stringp actual-random-car))
    (should (natnump actual-random-cdr))
    (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-alist-of-nat-number-strings-cons ()
  :num-runs 100
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'generate-random-alist-of-nat-number-string-cons))))
    (should (natnump actual-random-car))
    (should (stringp actual-random-cdr))
    (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-plist-of-nat-numbers ()
  :num-runs 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-nat-numbers))))
      (should (plistp actual-plist))
      (should (natnump (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-plist-of-strings ()
  :num-runs 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-strings))))
      (should (plistp actual-plist))
      (should (stringp (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-plist-of-strings-nat-number-pairs ()
  :num-runs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-string-nat-number-pairs))))
    (should (plistp actual-plist))
    (should (natnump (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-plist-of-nat-number-strings-pairs ()
  :num-runs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-nat-number-string-pairs))))
    (should (plistp actual-plist))
    (should (stringp (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-nat-numbers ()
  :num-runs 100
    (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-nat-numbers))))
      (should (hash-table-p actual-hash-table))
      (should (natnump (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-strings ()
  :num-runs 100
    (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-strings))))
      (should (hash-table-p actual-hash-table))
      (should (stringp (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-strings-nat-number-pairs ()
  :num-runs 100
  (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-string-nat-number-pairs))))
      (should (hash-table-p actual-hash-table))
      (should (natnump (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-nat-number-string-pairs ()
  :num-runs 100
  (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-nat-number-string-pairs))))
      (should (hash-table-p actual-hash-table))
      (should (stringp (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-con-of-nat-numbers ()
:num-runs 100
  (-let (((actual-con (actual-car . actual-cdr))(funcall (-compose (-juxt #'identity #'identity) #'generate-random-con-of-nat-numbers))))
	 (should (-cons-pair-p actual-con))
	 (should (natnump actual-car))
	 (should (natnump actual-cdr))))

(generate-ert-deftest-n-times generate-random-con-of-floats ()
:num-runs 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-con-of-floats))))
	 (should (-cons-pair-p actual-con))
	 (should (floatp actual-car))
	 (should (floatp actual-cdr))))

(generate-ert-deftest-n-times generate-random-con-of-strings ()
:num-runs 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-con-of-strings))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (stringp actual-cdr))))

(generate-ert-deftest-n-times generate-random-string-nat-number-con ()
:num-runs 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-string-nat-number-con))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (natnump actual-cdr))))

(generate-ert-deftest-n-times generate-random-nat-number-string-con ()
:num-runs 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-nat-number-string-con))))
	 (should (-cons-pair-p actual-con))
	 (should (natnump actual-car))
	 (should (stringp actual-cdr))))

(generate-ert-deftest-n-times generate-random-string-vector-of-nat-numbers-con ()
:num-runs 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-string-vector-of-nat-numbers-con))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (vectorp actual-cdr))))

(generate-ert-deftest-n-times generate-vector-of-n-nat-numbers ()
:num-runs 100
    (let ((actual-vector (generate-vector-of-n-nat-numbers)))
      (should (vectorp actual-vector))
      (should (generate--seq-every-p-nat-number actual-vector))))

(generate-ert-deftest-n-times generate-random-12-hour-time-string ()
:num-runs 100
  (-let* (((actual-hour actual-minute) (s-split ":" (generate-random-12-hour-time-string))))
    (should (generate--between-0-and-x-exclusive-p 13 (string-to-number actual-hour)))
    (should (length= actual-minute 2))
    (should (generate--between-0-and-x-exclusive-p 60 (string-to-number actual-minute)))))

(generate-ert-deftest-n-times generate-random-24-hour-time-string ()
:num-runs 100
  (-let* (((actual-hour actual-minute) (s-split ":" (generate-random-24-hour-time-string))))
    (should (length= actual-hour 2))
    (should (generate--between-0-and-x-exclusive-p 24 (string-to-number actual-hour)))
    (should (length= actual-minute 2))
    (should (generate--between-0-and-x-exclusive-p 60 (string-to-number actual-minute)))))

(generate-ert-deftest-n-times generate-random-lisp-timestamp ()
  :num-runs 100
  (-let* (((actual-ticks . actual-hz) (generate-random-lisp-timestamp)))
    (mapc (lambda (x) (should (natnump x))) (list actual-ticks actual-hz))
    (should-not (zerop actual-hz))))

(generate-ert-deftest-n-times generate-random-lisp-timestamp-range ()
  :num-runs 100
  (-let ((((actual-start-ticks . actual-start-hz) (actual-end-ticks . actual-end-hz)) (generate-random-lisp-timestamp-range)))
    (should (g--gt actual-end-ticks actual-start-ticks))
    (should (equal actual-start-hz actual-end-hz))
    (mapc (lambda (x) (should (g--gt0 x))) (list actual-start-ticks actual-end-ticks actual-start-hz actual-end-hz))))

(generate-ert-deftest-n-times generate-random-lisp-timestamp-range-with-duration ()
  :num-runs 100
  (-let* ((((actual-start-ticks . actual-start-hz) (actual-end-ticks . actual-end-hz) actual-duration) (generate-random-lisp-timestamp-range-with-duration)))
    (mapc (lambda (x) (should-not (zerop x))) (list actual-start-ticks actual-end-ticks actual-start-hz actual-end-hz actual-duration))))

(generate-ert-deftest-n-times generate-list-of-n-lisp-timestamp-ranges ()
  :num-runs 100
  (-let* ((test-n (generate--random-nat-number-in-range-25))
	  (actual-list (generate-list-of-n-lisp-timestamp-ranges test-n))
	  (((actual-random-start-ticks . actual-random-start-hz) (actual-random-end-ticks . actual-random-end-hz)) (generate-seq-take-random-value-from-seq actual-list)))
    (should (length= actual-list test-n))
    (mapc (lambda (x) (should (numberp x))) (list actual-random-start-ticks actual-random-end-ticks actual-random-start-hz actual-random-end-hz))
    (should (g--gte actual-random-end-ticks actual-random-start-ticks))))

(generate-ert-deftest-n-times generate--list-of-n-unzipped-starts-ends-durations ()
  :num-runs 100
  (-let* ((test-n (generate--random-nat-number-in-range-10))
	  (actual-list (generate--list-of-n-unzipped-starts-ends-durations test-n))
	  (random-n (generate--random-nat-number-between-0-and test-n))
	  (((actual-random-start-ticks . actual-random-start-hz) (actual-random-end-ticks . actual-random-end-hz) actual-random-duration) (mapcar (-partial #'nth random-n) actual-list)))
    (mapc (lambda (x) (should (length= x test-n))) actual-list)
    (mapc (lambda (x) (should x)) (list actual-random-start-ticks actual-random-end-ticks actual-random-start-hz actual-random-end-hz actual-random-duration))))

(generate-ert-deftest-n-times generate-random-month-number ()
:num-runs 100
  (should (generate--between-1-and-x-exclusive-p 13 (generate-random-month-number))))

(generate-ert-deftest-n-times generate-random-year-number ()
:num-runs 100
  (should (generate--in-range-exclusive-p (list 1960 3000) (generate-random-year-number))))

(generate-ert-deftest-n-times generate-random-day-number ()
:num-runs 100
  (let* ((test-month-number (generate-random-month-number))
	(test-year-number (generate-random-year-number))
	(expected-days-in-month (date-days-in-month test-year-number test-month-number)))
    (should (generate--between-1-and-x-exclusive-p (1+ expected-days-in-month) (generate-random-day-number test-year-number test-month-number)))))

(generate-ert-deftest-n-times generate--create-random-full-date-string-with-padding ()
:num-runs 100
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-full-date-string test-join-on :with-padding t))))
    (should (equal (seq-count (-rpartial #'length= 4) actual-date-parts) 1))
    (should (equal (seq-count (-rpartial #'length= 2) actual-date-parts) 2))))

(generate-ert-deftest-n-times generate--create-random-full-date-string-without-padding ()
:num-runs 100
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-full-date-string test-join-on :with-padding nil))))
    (should (equal (seq-count (-rpartial #'length= 4) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate--create-random-full-date-string-random-padding ()
:num-runs 100
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-full-date-string test-join-on))))
    (should (equal (seq-count (-rpartial #'length= 4) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate--create-random-short-date-string-with-padding ()
:num-runs 100
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-short-date-string test-join-on :with-padding t))))
     (should (equal (seq-count (-rpartial #'g--len-gt 1) actual-date-parts) 2))))

(generate-ert-deftest-n-times generate--create-random-short-date-string-without-padding ()
:num-runs 100
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-short-date-string test-join-on :with-padding nil))))
    (should (generate--between-1-and-x-exclusive-p 2 (seq-count (-rpartial #'g--len-gt 1) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate--create-random-short-date-string-random-padding ()
:num-runs 100
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-short-date-string test-join-on))))
    (should (generate--between-1-and-x-exclusive-p 2 (seq-count (-rpartial #'g--len-gt 1) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate-random-phone-number ()
:num-runs 100
   (-let* (((actual-part-one actual-part-two actual-part-three) (s-split "-" (generate-random-regular-phone-number))))
     (should (length= actual-part-one 3))
     (should (length= actual-part-two 3))
     (should (length= actual-part-three 4))))

(generate-ert-deftest-n-times generate-random-1-800-number ()
:num-runs 100
  (-let* (((actual-part-one actual-part-two actual-part-three actual-part-four) (s-split "-" (generate-random-1-800-number))))
     (should (string-equal actual-part-one "1"))
     (should (string-equal actual-part-two "800"))
     (should (length= actual-part-three 3))
     (should (length= actual-part-four 4))))

(generate-ert-deftest-n-times generate-random-card-number ()
:num-runs 100
 (let ((actual-parts (s-split "-" (generate-random-card-number))))
   (should (length= actual-parts 4))
   (should (length= (generate-seq-take-random-value-from-seq actual-parts) 4))))

(generate-ert-deftest-n-times generate-random-string-of-lower-alphanums ()
:num-runs 100
 (let ((actual-string (generate-random-string-of-lower-alphanums)))
   (should (g--len-gt actual-string 2))
   (should (string-equal (downcase actual-string) actual-string))))

(generate-ert-deftest-n-times generate-random-string-of-upper-alphanums ()
:num-runs 100
 (let ((actual-string (generate-random-string-of-upper-alphanums)))
   (should (g--len-gt actual-string 2))
   (should (string-equal (upcase actual-string) actual-string))))

(generate-ert-deftest-n-times generate-random-map ()
:num-runs 100
  (let* ((actual-map (generate-random-map)))
    (should (mapp actual-map))))

(generate-ert-deftest-n-times generate-random-hash-table ()
:num-runs 100
  (-let* ((actual-hash-table (generate-random-hash-table)))
    (should (hash-table-p actual-hash-table))))

(generate-ert-deftest-n-times generate-random-alist ()
:num-runs 100
  (-let* ((actual-alist (generate-random-alist)))
    (should (generate--alistp actual-alist))))

(generate-ert-deftest-n-times generate-random-plist ()
:num-runs 100
  (-let* ((actual-plist (generate-random-plist)))
    (should (plistp actual-plist))))

(generate-ert-deftest-n-times generate-list-of-n-maps ()
  :num-runs 100
  (-let* (((actual-list-of-maps expected-count) (funcall (-compose (-juxt #'generate-list-of-n-maps #'identity) #'generate--random-nat-number-in-range-10))))
    (should (generate--seq-every-p-map actual-list-of-maps))
    (should (g--len-eq actual-list-of-maps expected-count))))

(generate-ert-deftest-n-times generate-random-list-of-maps ()
:num-runs 100
  (-let* ((actual-maps (generate-random-list-of-maps)))
    (should (generate--seq-every-p-map actual-maps))))

(generate-ert-deftest-n-times generate-random-seq ()
:num-runs 100
  (let* ((actual-seq (generate-random-seq)))
    (should (seqp actual-seq))))

(generate-ert-deftest-n-times generate-random-list ()
:num-runs 100
  (-let* ((actual-list (generate-random-list)))
    (should (proper-list-p actual-list))))

(generate-ert-deftest-n-times generate-list-of-n-seqs ()
  :num-runs 100
  (-let* (((actual-list-of-seqs expected-count) (funcall (-compose (-juxt #'generate-list-of-n-seqs #'identity) #'generate--random-nat-number-in-range-10))))
    (should (generate--seq-every-p-seq actual-list-of-seqs))
    (should (g--len-eq actual-list-of-seqs expected-count))))

(generate-ert-deftest-n-times generate-random-list-of-seqs ()
  :num-runs 100
  (-let* ((actual-seqs (generate-random-list-of-seqs)))
    (should (generate--seq-every-p-seq actual-seqs))))

(generate-ert-deftest-n-times generate-with-buffer-with-text ()
:num-runs 100
  (-let* (((test-buffer-lines test-list-of-sentences test-words) (generate--random-multiline-string-base))
	(expected-string (generate-seq-take-random-value-from-seq test-list-of-sentences)))
    (should (generate-with-buffer-with-text test-buffer-lines
	      (search-forward expected-string)))))

(generate-ert-deftest-n-times generate-random-file-extension ()
  :num-runs 100
  (let* ((actual-file-name (generate-random-file-extension)))
    (should (member actual-file-name generate--FILE-EXTENSIONS))))

(generate-ert-deftest-n-times generate-random-file-name ()
  :num-runs 100
  (-let* ((actual-file-name (generate-random-file-name))
	  ((actual-name actual-extension) (s-split "\\." actual-file-name)))
    (should (g--len-gt actual-name 1))
    (should (g--len-gt actual-extension 0))))

(generate-ert-deftest-n-times generate-random-symbol ()
  :num-runs 100
  (should (symbolp (generate-random-symbol))))

(generate-ert-deftest-n-times generate-list-of-n-symbols ()
  :num-runs 100
  (let* ((test-n (generate--random-nat-number-in-range-10))
	 (actual-symbols (generate-list-of-n-symbols test-n)))
    (should (length= actual-symbols test-n))
    (should (symbolp (generate-seq-take-random-value-from-seq actual-symbols)))))

(generate-ert-deftest-n-times generate-random-list-of-symbols ()
  :num-runs 100
  (let* ((actual-symbols (generate-random-list-of-symbols)))
    (should (symbolp (generate-seq-take-random-value-from-seq actual-symbols)))))

(generate-ert-deftest-n-times generate--random-void-x-error ()
  :num-runs 100
  (-let* ((test-symbol (generate-seq-take-random-value-from-seq (list 'void-function 'void-variable)))
	 ((actual-symbol actual-val) (funcall (generate--random-void-x-error test-symbol))))
    (should (equal test-symbol actual-symbol))
    (should (symbolp actual-val))))

(generate-ert-deftest-n-times generate-random-wrong-type-argument-error ()
  :num-runs 100
  (-let (((actual-error actual-pred actual-val) (generate-random-wrong-type-argument-error)))
    (should (equal actual-error 'wrong-type-argument))
    (should-not (funcall actual-pred actual-val))))

(generate-ert-deftest-n-times generate-random-error ()
  :num-runs 100
  (-let (((actual-error _) (generate-random-error)))
    (should (symbolp actual-error))))

(generate-ert-deftest-n-times generate-random-backtrace-frame ()
  :num-runs 100
  (let* ((actual-backtrace-frame (generate-random-backtrace-frame)))
    (should (booleanp (backtrace-frame-evald actual-backtrace-frame)))
    (should (symbolp (backtrace-frame-fun actual-backtrace-frame)))
    (should (seq-every-p #'symbolp (backtrace-frame-args actual-backtrace-frame)))
    (should (generate--alistp (backtrace-frame-locals actual-backtrace-frame)))
    (should (natnump (backtrace-frame-pos actual-backtrace-frame)))))

(generate-ert-deftest-n-times generate-list-of-n-backtrace-frames ()
  :num-runs 100
  (let* ((actual-list-of-backtrace-frames (generate-random-list-of-backtrace-frames))
	 (actual-random-backtrace-frame (generate-seq-take-random-value-from-seq actual-list-of-backtrace-frames)))
    (mapc (lambda (func) (should (funcall func actual-random-backtrace-frame))) (list  #'backtrace-frame-fun
										       #'backtrace-frame-args
										       #'backtrace-frame-locals
										       #'backtrace-frame-pos))))

(generate-ert-deftest-n-times generate-random-list-of-backtrace-frames ()
  :num-runs 100
  (let* ((actual-list-of-backtrace-frames (generate-random-list-of-backtrace-frames)))
    (should (backtrace-frame-p (generate-seq-take-random-value-from-seq actual-list-of-backtrace-frames)))))

(generate-ert-deftest-n-times generate-list-of-n-booleans ()
  :num-runs 100
  (let* ((test-n (generate--random-nat-number-in-range-10))
	 (actual-booleans (generate-list-of-n-booleans test-n)))
    (should (length= actual-booleans test-n))
    (should (booleanp (generate-seq-take-random-value-from-seq actual-booleans)))))

(generate-ert-deftest-n-times generate-random-color ()
  :num-runs 100
  (let* ((actual-color (generate-random-color))
	 (actual-hex-value (s-chop-left 1 actual-color)))
    (should (decode-hex-string actual-hex-value))))

(generate-ert-deftest-n-times generate-list-of-n-colors ()
  :num-runs 100
  (let* ((test-n (generate--random-nat-number-in-range-10))
	(actual-colors (generate-list-of-n-colors test-n))
	(actual-random-color (generate-seq-take-random-value-from-seq actual-colors))
    (should (length= actual-colors test-n))
    (should (decode-hex-string (s-chop-left 1 actual-random-color))))))

(generate-ert-deftest-n-times generate-random-list-of-n-colors ()
  :num-runs 100
  (let* ((actual-colors (generate-random-list-of-colors))
	(actual-random-color (generate-seq-take-random-value-from-seq actual-colors)))
    (should (stringp actual-random-color))))

;;; generate-primitives-tests.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
