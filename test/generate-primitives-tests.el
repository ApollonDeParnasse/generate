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
(require 'org)
(require 'org-element)
(require 's)
(require 'dash)
(require 'generate)

(defconst MAX-PRECISON
  10000000 "I can not gurantee results will be accurate for numbers larger than 1000000.")

(generate-ert-deftest-n-times generate--times ()
  (-let* ((((expected-num test-func) test-calls) (funcall (-juxt (-compose (-juxt #'identity #'cl-constantly) #'generate--random-nat-number-in-range-255) #'generate--random-nat-number-in-range-255)))
	 ((actual-seq actual-seq-length) (funcall (-compose #'generate--identity-and-seq-length #'generate--times) test-calls test-func)))
    (should (seq-every-p (-partial #'eql expected-num) actual-seq))
    (should (eql actual-seq-length test-calls))))

(generate-ert-deftest-n-times generate--convert-calc-value-into-lisp ()
  (should (floatp (generate--convert-calc-value-into-lisp (math-gaussian-float)))))

(generate-ert-deftest-n-times generate--between-1-and-p-true ()
  (-let* (((test-p test-nat-number) (funcall (-compose (-juxt #'1+ #'identity) #'generate--random-nat-number-in-range-255))))
    (should (funcall (generate--between-1-and-p test-p) test-nat-number))))

(generate-ert-deftest-n-times generate--between-1-and-p-false ()
  (-let* (((test-nat-number test-p) (funcall (-compose (-juxt #'1+ #'identity) #'generate--random-nat-number-in-range-255))))
    (should-not (funcall (generate--between-1-and-p test-p) test-nat-number))))

(generate-ert-deftest-n-times generate--non-zero-bounded-modular-addition-max-test ()
  (let* ((range-max (random MAX-PRECISON))
	 (range-min (- range-max (random range-max) 2))
	 (increase 1)
	 (expected-result range-min)
	 (current-number (1- range-max))
	 (actual-result (generate--non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
			    (should (eql actual-result expected-result))))

(generate-ert-deftest-n-times generate--non-zero-bounded-modular-addition-min-test ()
    (let* ((range-max (random MAX-PRECISON))
	 (range-min (- range-max (random range-max) 2))
	 (increase 1)
	 (expected-result (1+ range-min))
	 (current-number range-min)
	 (actual-result (generate--non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
			    (should (eql actual-result expected-result))))

(generate-ert-deftest-n-times generate--non-zero-bounded-modular-addition-basic-nat-number-test ()
  (let* ((range-max (random MAX-PRECISON))
	 (range-min (- range-max (random range-max) 2))
	 (increase (random range-max))
	 (current-number (random range-max))
	 (actual-result (generate--non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
			 (should (and (g--gte actual-result range-min) (g--lt actual-result range-max)))))

(generate-ert-deftest-n-times generate--scale-float-to-range ()
    (let* ((test-max (random MAX-PRECISON))
	   (test-min (- test-max (random test-max) 2))
	   (test-float-to-scale (generate--convert-calc-value-into-lisp (math-random-float)))
	   (actual-float (generate--scale-float-to-range (list test-min test-max) test-float-to-scale)))
      (should (g--gte actual-float test-min))
      (should (g--lt actual-float test-max))))

(generate-ert-deftest-n-times generate--divide-list-values-by-max-list-value ()
  (let ((actual-list (funcall (-compose #'generate--divide-list-values-by-max-list-value #'generate--random-nat-number-list-in-range-255))))
    (should (generate--seq-every-p-between-0-and-1 actual-list))))

(generate-ert-deftest-n-times generate-random-float-between-0-and-1 ()
  (should (floatp (generate-random-float-between-0-and-1))))

(generate-ert-deftest-n-times generate-random-nat-number-in-range ()
      (let* ((test-max (random MAX-PRECISON))
  	   (test-min (- test-max (random test-max) 2))
  	   (actual-nat-number (generate-random-nat-number-in-range (list test-min test-max))))
	(should (natnump actual-nat-number))
        (should (g--gte actual-nat-number test-min))
        (should (g--lt actual-nat-number test-max))))

(generate-ert-deftest-n-times generate--random-nat-number-list ()
  (let ((actual-list (funcall (-compose #'generate--random-nat-number-list #'calcFunc-random) 255)))
  (should (generate--seq-every-p-nat-number actual-list))))

(generate-ert-deftest-n-times generate-random-nat-number-range ()
  (-let (((actual-range expected-range-length) (funcall (-juxt #'generate-random-nat-number-range #'identity) (generate-random-nat-number-in-range (list 1 10)))))
  (should (eql (generate--range-size actual-range) expected-range-length))))

(generate-ert-deftest-n-times generate--divide-by-random-value ()
  (-let (((actual-result actual-input-value) (funcall (-compose (-juxt #'generate--divide-by-random-value #'identity) #'generate--random-nat-number-in-range-255))))
    (should (floatp actual-result))
    (should (g--lte actual-result actual-input-value))))

(generate-ert-deftest-n-times generate-call-random-function ()
  (-let* (((expected-super-set test-list) (funcall (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate-list-of-nat-numbers)))
	 (actual-value (generate-call-random-function test-list)))
    (should (member actual-value expected-super-set))))

(generate-ert-deftest-n-times generate-call-random-function-n-times ()
  (-let* ((((expected-super-set test-list) test-calls) (funcall (-juxt (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate-list-of-nat-numbers) #'generate--random-nat-number-in-range-255)))
	 (actual-seq (generate-call-random-function-n-times test-calls test-list)))
    (should (cl-subsetp actual-seq expected-super-set))
    (should (g--len-eq actual-seq test-calls))))

(generate-ert-deftest-n-times generate-call-n-random-functions ()
  (-let* (((expected-super-set test-list) (funcall (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate-list-of-nat-numbers)))
	 (test-n (generate--seq-random-chunk-length expected-super-set))
	 (actual-list (generate-call-n-random-functions test-n test-list)))
    (should (cl-subsetp actual-list expected-super-set))
    (should (g--len-eq actual-list test-n))))

(generate-ert-deftest-n-times generate-call-function-random-times ()
  (-let* (((test-cl-constantly) (generate-random-cl-constantly))
	 (actual-values (generate-call-function-random-times test-cl-constantly)))
    (should actual-values)))

(generate-ert-deftest-n-times generate--divide-list-values-by-random-value ()
  (let ((actual-list (funcall (-compose #'generate--divide-list-values-by-random-value #'generate--random-nat-number-list-in-range-255))))
    (should (generate--seq-every-p-float actual-list))))

(generate-ert-deftest-n-times generate-random-list-of-cl-constantlys ()
  (-let* (((expected-super-set test-list) (generate-random-list-of-cl-constantlys)))
    (should (eql (seq-length expected-super-set) (seq-length test-list)))
    (should (generate--seq-every-p-nat-number expected-super-set))
    (should (generate--seq-every-p-function test-list))))

(generate-ert-deftest-n-times generate-seq-shuffle-list ()
  (-let* (((actual-shuffled-list test-list) (funcall (-compose (-juxt #'generate-seq-shuffle #'identity) #'generate-random-list-of-strings))))
	(should (seq-set-equal-p actual-shuffled-list test-list))))

(generate-ert-deftest-n-times generate-seq-shuffle-vector ()
  (-let* (((actual-shuffled-vector test-vector) (funcall (-compose (-juxt #'generate-seq-shuffle #'identity) #'generate-vector-of-nat-numbers))))
    (should (vectorp actual-shuffled-vector))
    (should (seq-set-equal-p actual-shuffled-vector test-vector))))

(generate-ert-deftest-n-times generate-seq-shuffle-string ()
  (-let* (((actual-shuffled-string test-string) (funcall (-compose (-juxt #'generate-seq-shuffle #'identity) #'generate-random-word))))
        (should (stringp actual-shuffled-string))
	(should-not (seq-difference actual-shuffled-string test-string))))

(generate-ert-deftest-n-times generate--seq-random-chunk-length ()
    (-let* (((test-chunk-length test-list-length) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'seq-length) #'generate-list-of-nat-numbers) :min-length 2)))
      (should (g--lt test-chunk-length test-list-length))
      (should (g--gte test-chunk-length 1))))

;; this can take :min-length 2?
(generate-ert-deftest-n-times generate-seq-n-random-values-list ()
  (-let* (((test-count test-list) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-random-list-of-strings)))
	  (actual-length (funcall (-compose #'seq-length #'generate-seq-n-random-values) test-count test-list)))
    (should (eql actual-length test-count))))

(generate-ert-deftest-n-times generate-seq-n-random-values-vector ()
  (-let* (((test-count test-vector) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-vector-of-nat-numbers)))
	 ((actual-vector actual-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-n-random-values) test-count test-vector)))
    (should (vectorp actual-vector))
    (should (eql actual-length test-count))))

(generate-ert-deftest-n-times generate-seq-n-random-values-string ()
  (-let* (((test-count test-string) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-random-word)))
	 ((actual-string actual-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-n-random-values) test-count test-string)))
    (should (stringp actual-string))
    (should (eql actual-length test-count))))

(generate-ert-deftest-n-times generate-seq-random-values-lists ()
  (-let* ((((actual-list actual-list-length) (test-list test-list-length))
	  (funcall (-compose (-partial #'seq-map #'generate--identity-and-seq-length) (-juxt #'generate-seq-random-values #'identity) #'generate-random-list-of-strings))))
    (should (listp actual-list))
    (should (g--lte actual-list-length test-list-length))))

(generate-ert-deftest-n-times generate-seq-random-values-vectors ()
  (-let* ((((actual-vector actual-vector-length) (test-vector test-vector-length))
	  (funcall (-compose (-partial #'seq-map #'generate--identity-and-seq-length) (-juxt #'generate-seq-random-values #'identity) #'generate-vector-of-nat-numbers))))
    (should (vectorp actual-vector))
    (should (g--lte actual-vector-length test-vector-length))))

(generate-ert-deftest-n-times generate-seq-random-values-strings ()
  (-let* ((((actual-string actual-string-length) (test-string test-string-length))
	  (funcall (-compose (-partial #'seq-map #'generate--identity-and-seq-length) (-juxt #'generate-seq-random-values #'identity) #'generate-random-word))))
    (should (stringp actual-string))
    (should (g--lte actual-string-length test-string-length))))

(generate-ert-deftest-n-times generate--seq-random-iterate-from-max-lists ()
  (-let* (((actual-list test-list-max) (funcall (-compose (-juxt #'generate--seq-random-iterate-from-max #'seq-max) #'generate-list-of-floats))))
    (should (seq-every-p (-rpartial #'g--gte test-list-max) actual-list))))

(generate-ert-deftest-n-times generate--seq-random-iterate-from-max-vectors ()
  (-let* (((actual-vector test-vector-max) (funcall (-compose (-juxt #'generate--seq-random-iterate-from-max #'seq-max) #'generate-vector-of-nat-numbers))))
    (should (vectorp actual-vector))
    (should (seq-every-p (-rpartial #'g--gte test-vector-max) actual-vector))))

(generate-ert-deftest-n-times generate--seq-random-iterate-from-max-strings ()
  (-let* (((actual-string test-string-max) (funcall (-compose (-juxt #'generate--seq-random-iterate-from-max #'seq-max) #'generate-random-word))))
    (should (stringp actual-string))
    (should (seq-every-p (-rpartial #'g--gte test-string-max) actual-string))))

(generate-ert-deftest-n-times generate-seq-random-position-lists ()
  (-let* ((((test-list test-list-length) actual-position) (funcall (-compose (-juxt #'generate--identity-and-seq-length #'generate-seq-random-position) #'generate-random-list-of-strings))))
	(should (funcall (generate--between-0-and-p test-list-length) actual-position))))

(generate-ert-deftest-n-times generate-seq-random-position-vectors ()
  (-let* ((((test-vector test-vector-length) actual-position) (funcall (-compose (-juxt #'generate--identity-and-seq-length #'generate-seq-random-position) #'generate-vector-of-nat-numbers))))
	(should (funcall (generate--between-0-and-p test-vector-length) actual-position))))

(generate-ert-deftest-n-times generate-seq-random-position-strings ()
  (-let* ((((test-string test-string-length) actual-position) (funcall (-compose (-juxt #'generate--identity-and-seq-length #'generate-seq-random-position) #'generate-random-word))))
	(should (funcall (generate--between-0-and-p test-string-length) actual-position))))

(generate-ert-deftest-n-times generate-seq-random-value-with-position ()
  (-let* ((test-seq (generate-random-seq))
	  ((actual-item actual-position) (generate-seq-random-value-with-position test-seq)))
    (should (seq-contains-p test-seq actual-item))
    (should (funcall (generate--between-0-and-p (seq-length test-seq)) actual-position))))

(generate-ert-deftest-n-times generate-seq-split-random-list ()
  (let ((actual-list (funcall (-compose #'generate-seq-split-random #'generate-random-list-of-strings))))
    (should (generate--seq-every-p-list actual-list))))

(generate-ert-deftest-n-times generate-seq-split-random-vector ()
 (let ((actual-vector (funcall (-compose #'generate-seq-split-random #'generate-vector-of-nat-numbers) :min-length 2)))
    (should (generate--seq-every-p-vector actual-vector))))

(generate-ert-deftest-n-times generate-seq-split-random-string ()
  (let ((actual-string (funcall (-compose #'generate-seq-split-random #'generate-random-word))))
    (should (generate--seq-every-p-string actual-string))))

(generate-ert-deftest-n-times generate-seq-random-chunk-of-size-n-string ()
    (-let* (((test-chunk-length test-string) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-random-word)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-random-chunk-of-size-n) test-chunk-length test-string)))
      (should (stringp actual-chunk))
      (should (s-contains? actual-chunk test-string))))

(generate-ert-deftest-n-times generate-seq-random-chunk-of-size-n-list ()
    (-let* (((test-chunk-length test-list) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-list-of-nat-numbers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-random-chunk-of-size-n) test-chunk-length test-list)))
      (should (listp actual-chunk))
      (should (cl-subsetp actual-chunk test-list))))

(generate-ert-deftest-n-times generate-seq-random-chunk-of-size-n-vector ()
    (-let* (((test-chunk-length test-vector) (funcall (-compose (-juxt #'generate--seq-random-chunk-length #'identity) #'generate-vector-of-nat-numbers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-seq-random-chunk-of-size-n) test-chunk-length test-vector)))
      (should (vectorp actual-chunk))
      (should-not (seq-difference (seq-union actual-chunk test-vector) (seq--into-list test-vector)))))

(generate-ert-deftest-n-times generate--map-on-alist-of-nat-numbers ()
  (-let* ((test-alist (generate-random-alist-of-nat-numbers))
	  ((actual-car . actual-cdr) (generate--map-on #'generate--applify-cons #'-sum #'-sum test-alist)))
  (should (natnump actual-car))
  (should (natnump actual-cdr))))

(generate-ert-deftest-n-times generate--map-on-alist-of-strings ()
  (-let* (((test-alist test-alist-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-random-alist-of-nat-numbers)))
	(actual-result (generate--map-on #'identity #'concat #'concat test-alist)))
  (should (generate--seq-every-p-string actual-result))
  (should (seq-every-p (-rpartial #'length= test-alist-length) actual-result))))

(generate-ert-deftest-n-times generate--map-on-alist-of-string-nat-number-cons ()
  (-let* (((test-alist test-alist-length) (funcall (-compose #'generate--identity-and-seq-length #'generate-random-alist-of-nat-numbers)))
	((actual-string actual-sum) (generate--map-on #'identity #'concat #'-sum test-alist)))
  (should (stringp actual-string))
  (should (length= actual-string test-alist-length))
  (should (integerp actual-sum))))

(generate-ert-deftest-n-times generate-map-random-value ()
  (-let* ((test-map (generate-random-map))
	(actual-value (generate-map-random-value test-map))
	(expected-values (map-values test-map)))
    (should (member actual-value expected-values))))

(generate-ert-deftest-n-times generate-data-fail ()
  (-let* (((test-max-length test-plus) (generate-random-nat-number-twice))
	   (test-min-length (+ test-max-length test-plus)))
    (should-error (generate-data :min-length test-max :max-length test-min-length))))

(generate-ert-deftest-n-times generate-list-of-nat-numbers ()
   (let ((test-list (generate-list-of-nat-numbers)))
    (should (generate--seq-every-p-nat-number test-list))))

(generate-ert-deftest-n-times generate-list-of-nat-numbers-with-exact-length ()
   (let* ((test-exact-length (generate--random-nat-number-in-range-25))
	 (test-list (generate-list-of-nat-numbers :exact-length test-exact-length)))
    (should (equal (seq-count #'natnump test-list) test-exact-length))))

(generate-ert-deftest-n-times generate-list-of-floats-1 ()
  (let ((test-list (generate-list-of-floats)))
    (should (generate--seq-every-p-float test-list))))

(generate-ert-deftest-n-times generate-list-of-floats-2 ()
    (let ((test-list (generate-list-of-floats-between-0-and-1)))
    (should (generate--seq-every-p-float test-list))))

(generate-ert-deftest-n-times generate-list-of-lists-of-nat-numbers ()
    (let ((test-list (generate-random-list-of-lists-nat-numbers)))
    (should (generate--seq-every-p-list test-list))))

(generate-ert-deftest-n-times generate-n-alpha-string-characters ()
  (-let* ((test-character-count (generate--random-nat-number-in-range-25))
	((actual-string-characters actual-random-string-character) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq)
										      #'generate-n-alpha-string-characters)
									    test-character-count)))
    (should (stringp actual-random-string-character))
    (should (length= actual-string-characters test-character-count))))

(generate-ert-deftest-n-times generate-random-word ()
  (-let ((actual-word (generate-random-word)))
    (should (stringp actual-word))
    (should (g--len-gt actual-word 1))))

(generate-ert-deftest-n-times generate-n-words ()
  (-let* ((word-count (1+ (generate--random-nat-number-in-range-25)))
	  (actual-words (generate-n-words word-count)))
    (should (generate--seq-every-p-string actual-words))
    (should (length= actual-words word-count))))

(defconst TESTGENS
  (list #'generate-random-card-number #'generate-random-time-string #'generate-random-date-string #'generate-random-phone-number #'generate-random-string-of-lower-alphanums #'generate-random-string-of-upper-alphanums))

(generate-ert-deftest-n-times generate-n-sentences-default ()
  (-let* ((sentence-count (generate--random-nat-number-in-range-10))
	((actual-result actual-all-words) (generate-n-sentences sentence-count))
	(actual-random-value (generate-seq-take-random-value-from-seq actual-result)))
    (should (s-ends-with-p "." actual-random-value))
    (should (length= actual-result sentence-count))
    (should (listp actual-all-words))))

(generate-ert-deftest-n-times generate-n-sentences-with-extra ()
  (-let* ((test-gen (generate-seq-take-random-value TESTGENS))
      (sentence-count (generate--random-nat-number-in-range-10))
      ((actual-result actual-all-words actual-list-of-regular-words actual-list-of-words-from-gens) (generate-n-sentences sentence-count test-gen))
      (actual-random-value (generate-seq-take-random-value-from-seq actual-result)))
  (should (s-ends-with-p "." actual-random-value))
  (should (length= actual-result sentence-count))
  (should (listp actual-all-words))
  (should (listp actual-list-of-regular-words))
  (should (listp actual-list-of-words-from-gens))))

(generate-ert-deftest-n-times generate-random-list-of-sentences ()
  (-let (((actual-sentences) (generate-random-list-of-sentences)))
    (should (g--gt (seq-count #'stringp actual-sentences) 2))))

(generate-ert-deftest-n-times generate-string-with-n-lines-default ()
  (-let* ((line-count (generate--random-nat-number-in-range-10))
       ((actual-buffer-lines actual-list-of-sentences actual-all-words) (generate-string-with-n-lines line-count)))
    (should (equal (s-count-matches "\n" actual-buffer-lines) (1- line-count)))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))))

(generate-ert-deftest-n-times generate-string-with-n-lines-with-extra ()
  (-let* ((test-gens (generate-seq-two-random-values TESTGENS))
	(line-count (1+ (generate--random-nat-number-in-range-10)))
       ((actual-buffer-lines actual-list-of-sentences actual-all-words actual-regular-words actual-words-from-gens) (generate-string-with-n-lines line-count test-gens)))
    (should (equal (s-count-matches "\n" actual-buffer-lines) (1- line-count)))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))
    (should (listp actual-regular-words))
    (should (generate--len-gt actual-words-from-gens 1))))

(generate-ert-deftest-n-times generate-random-multiline-string-default ()
  (-let (((actual-buffer-lines actual-list-of-sentences actual-all-words) (generate-random-multiline-string)))
    (should (stringp actual-buffer-lines))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))))

(generate-ert-deftest-n-times generate-random-multiline-string-with-extra ()
  (-let* ((test-gens (generate-seq-two-random-values TESTGENS))
       ((actual-buffer-lines actual-list-of-sentences actual-all-words actual-regular-words actual-words-from-gens) (generate-random-multiline-string test-gens)))
    (should (stringp actual-buffer-lines))
    (should (listp actual-list-of-sentences))
    (should (listp actual-all-words))
    (should (listp actual-regular-words))
    (should (generate--len-gt actual-words-from-gens 1))))

(generate-ert-deftest-n-times generate-random-alist-of-nat-numbers ()
    (-let (((actual-alist actual-car-and-actual-cdr) (funcall (-compose (-juxt #'identity (-compose #'-cons-to-list #'generate-seq-take-random-value-from-seq)) #'generate-random-alist-of-nat-numbers))))
      (should (generate--seq-every-p-nat-number actual-car-and-actual-cdr))
      (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-alist-of-strings ()
    (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity (-compose #'generate-seq-take-random-value-from-seq)) #'generate-random-alist-of-strings))))
      (should (stringp actual-random-car))
      (should (stringp actual-random-cdr))
      (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-alist-of-strings-nat-number-cons ()
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'generate-random-alist-of-string-nat-number-cons))))
    (should (stringp actual-random-car))
    (should (natnump actual-random-cdr))
    (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-alist-of-nat-number-strings-cons ()
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'generate-random-alist-of-nat-number-string-cons))))
    (should (natnump actual-random-car))
    (should (stringp actual-random-cdr))
    (should (generate--seq-every-p-con actual-alist))))

(generate-ert-deftest-n-times generate-random-plist-of-nat-numbers ()
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-nat-numbers))))
      (should (plistp actual-plist))
      (should (natnump (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-plist-of-strings ()
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-strings))))
      (should (plistp actual-plist))
      (should (stringp (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-plist-of-strings-nat-number-pairs ()
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-string-nat-number-pairs))))
    (should (plistp actual-plist))
    (should (natnump (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-plist-of-nat-number-strings-pairs ()
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-plist-of-nat-number-string-pairs))))
    (should (plistp actual-plist))
    (should (stringp (map-elt actual-plist actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-nat-numbers ()
    (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-nat-numbers))))
      (should (hash-table-p actual-hash-table))
      (should (natnump (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-strings ()
    (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-strings))))
      (should (hash-table-p actual-hash-table))
      (should (stringp (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-strings-nat-number-pairs ()
  (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-string-nat-number-pairs))))
      (should (hash-table-p actual-hash-table))
      (should (natnump (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-hash-table-of-nat-number-string-pairs ()
  (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'generate-map-random-key) #'generate-random-hash-table-of-nat-number-string-pairs))))
      (should (hash-table-p actual-hash-table))
      (should (stringp (map-elt actual-hash-table actual-random-key)))))

(generate-ert-deftest-n-times generate-random-con-of-nat-numbers ()
  (-let (((actual-con (actual-car . actual-cdr))(funcall (-compose (-juxt #'identity #'identity) #'generate-random-con-of-nat-numbers))))
	 (should (-cons-pair-p actual-con))
	 (should (natnump actual-car))
	 (should (natnump actual-cdr))))

(generate-ert-deftest-n-times generate-random-con-of-floats ()
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-con-of-floats))))
	 (should (-cons-pair-p actual-con))
	 (should (floatp actual-car))
	 (should (floatp actual-cdr))))

(generate-ert-deftest-n-times generate-random-con-of-strings ()
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-con-of-strings))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (stringp actual-cdr))))

(generate-ert-deftest-n-times generate-random-string-nat-number-con ()
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-string-nat-number-con))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (natnump actual-cdr))))

(generate-ert-deftest-n-times generate-random-nat-number-string-con ()
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-nat-number-string-con))))
	 (should (-cons-pair-p actual-con))
	 (should (natnump actual-car))
	 (should (stringp actual-cdr))))

(generate-ert-deftest-n-times generate-random-string-vector-of-nat-numbers-con ()
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-random-string-vector-of-nat-numbers-con))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (vectorp actual-cdr))))

(generate-ert-deftest-n-times generate-vector-of-nat-numbers ()
    (let ((actual-vector (generate-vector-of-nat-numbers)))
      (should (vectorp actual-vector))
      (should (generate--seq-every-p-nat-number actual-vector))))

(generate-ert-deftest-n-times generate-random-12-hour-time-string ()
  (-let* (((actual-hour actual-minute) (s-split ":" (generate-random-12-hour-time-string))))
    (should (generate--between-0-and-p 13 (string-to-number actual-hour)))
    (should (length= actual-minute 2))
    (should (generate--between-0-and-p 60 (string-to-number actual-minute)))))

(generate-ert-deftest-n-times generate-random-24-hour-time-string ()
  (-let* (((actual-hour actual-minute) (s-split ":" (generate-random-24-hour-time-string))))
    (should (length= actual-hour 2))
    (should (generate--between-0-and-p 24 (string-to-number actual-hour)))
    (should (length= actual-minute 2))
    (should (generate--between-0-and-p 60 (string-to-number actual-minute)))))

(generate-ert-deftest-n-times generate-random-month-number ()
  (should (generate--between-1-and-p 13 (generate-random-month-number))))

(generate-ert-deftest-n-times generate-random-year-number ()
  (should (generate--range-member-exclusive-p (list 1960 3000) (generate-random-year-number))))

(generate-ert-deftest-n-times generate-random-day-number ()
  (let* ((test-month-number (generate-random-month-number))
	(test-year-number (generate-random-year-number))
	(expected-days-in-month (date-days-in-month test-year-number test-month-number)))
    (should (generate--between-1-and-p (1+ expected-days-in-month) (generate-random-day-number test-year-number test-month-number)))))

(generate-ert-deftest-n-times generate--create-random-full-date-string-with-padding ()
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-full-date-string test-join-on :with-padding t))))
    (should (equal (seq-count (-rpartial #'length= 4) actual-date-parts) 1))
    (should (equal (seq-count (-rpartial #'length= 2) actual-date-parts) 2))))

(generate-ert-deftest-n-times generate--create-random-full-date-string-without-padding ()
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-full-date-string test-join-on :with-padding nil))))
    (should (equal (seq-count (-rpartial #'length= 4) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate--create-random-full-date-string-random-padding ()
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-full-date-string test-join-on))))
    (should (equal (seq-count (-rpartial #'length= 4) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate--create-random-short-date-string-with-padding ()
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-short-date-string test-join-on :with-padding t))))
     (should (equal (seq-count (-rpartial #'g--len-gt 1) actual-date-parts) 2))))

(generate-ert-deftest-n-times generate--create-random-short-date-string-without-padding ()
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-short-date-string test-join-on :with-padding nil))))
    (should (generate--between-1-and-p 2 (seq-count (-rpartial #'g--len-gt 1) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate--create-random-short-date-string-random-padding ()
  (let* ((test-join-on (generate-seq-take-random-value-from-seq (list "-" "/")))
	 (actual-date-parts (s-split test-join-on (generate--create-random-short-date-string test-join-on))))
    (should (generate--between-1-and-p 2 (seq-count (-rpartial #'g--len-gt 1) actual-date-parts) 1))))

(generate-ert-deftest-n-times generate-random-phone-number ()
   (-let* (((actual-part-one actual-part-two actual-part-three) (s-split "-" (generate-random-regular-phone-number))))
     (should (length= actual-part-one 3))
     (should (length= actual-part-two 3))
     (should (length= actual-part-three 4))))

(generate-ert-deftest-n-times generate-random-1-800-number ()
  (-let* (((actual-part-one actual-part-two actual-part-three actual-part-four) (s-split "-" (generate-random-1-800-number))))
     (should (string-equal actual-part-one "1"))
     (should (string-equal actual-part-two "800"))
     (should (length= actual-part-three 3))
     (should (length= actual-part-four 4))))

(generate-ert-deftest-n-times generate-random-card-number ()
 (let ((actual-parts (s-split "-" (generate-random-card-number))))
   (should (length= actual-parts 4))
   (should (length= (generate-seq-take-random-value-from-seq actual-parts) 4))))

(generate-ert-deftest-n-times generate-random-string-of-lower-alphanums ()
 (let ((actual-string (generate-random-string-of-lower-alphanums)))
   (should (g--len-gt actual-string 2))
   (should (string-equal (downcase actual-string) actual-string))))

(generate-ert-deftest-n-times generate-random-string-of-upper-alphanums ()
 (let ((actual-string (generate-random-string-of-upper-alphanums)))
   (should (g--len-gt actual-string 2))
   (should (string-equal (upcase actual-string) actual-string))))

(generate-ert-deftest-n-times generate-random-map ()
  (let* ((actual-map (generate-random-map)))
    (should (mapp actual-map))))

(generate-ert-deftest-n-times generate-random-hash-table ()
  (-let* ((actual-hash-table (generate-random-hash-table)))
    (should (hash-table-p actual-hash-table))))

(generate-ert-deftest-n-times generate-random-alist ()
  (-let* ((actual-alist (generate-random-alist)))
    (should (generate--alistp actual-alist))))

(generate-ert-deftest-n-times generate-random-plist ()
  (-let* ((actual-plist (generate-random-plist)))
    (should (plistp actual-plist))))

(generate-ert-deftest-n-times generate-random-map-type-n-times ()
  (-let* (((actual-list-of-maps expected-count) (funcall (-compose (-juxt #'generate-random-map-type-n-times #'identity) #'generate--random-nat-number-in-range-10))))
    (should (generate--seq-every-p-map actual-list-of-maps))
    (should (g--len-eq actual-list-of-maps expected-count))))

(generate-ert-deftest-n-times generate-random-map-type-n-random-times ()
  (-let* ((actual-maps (generate-random-map-type-n-random-times)))
    (should (generate--seq-every-p-map actual-maps))))

(generate-ert-deftest-n-times generate-random-seq ()
  (let* ((actual-seq (generate-random-seq)))
    (should (seqp actual-seq))))

(generate-ert-deftest-n-times generate-random-list ()
  (-let* ((actual-list (generate-random-list)))
    (should (proper-list-p actual-list))))

(generate-ert-deftest-n-times generate-random-seq-type-n-times ()
  (-let* (((actual-list-of-seqs expected-count) (funcall (-compose (-juxt #'generate-random-seq-type-n-times #'identity) #'generate--random-nat-number-in-range-10))))
    (should (generate--seq-every-p-seq actual-list-of-seqs))
    (should (g--len-eq actual-list-of-seqs expected-count))))

(generate-ert-deftest-n-times generate-random-seq-type-n-random-times ()
  (-let* ((actual-seqs (generate-random-seq-type-n-random-times)))
    (should (generate--seq-every-p-seq actual-seqs))))

;;; generate-tests.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
