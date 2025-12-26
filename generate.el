;;; generate.el --- A suite of random data generators  -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: tools, maint
;; Package-Requires: ((emacs "30.1") (org "9.7") (dash "2.20.0") (s "1.13.1") (compat "29"))
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

(require 'ert)
(require 'seq)
(require 'map)
(require 'time-date)
(require 'calc-comb)
(require 'org)
(require 'org-table)
(require 'dash)
(require 's)

(defconst generate--FIVERANGE
  (list 1 5))
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
(defconst generate--THREETO20
  (list 3 20))
(defconst generate--YEARRANGE
  (list 1960 3000))
(defconst generate--TWENTYFIVERANGE
  (list 1 25))
(defconst generate--THREETOTWENTYFIVERANGE
  (list 3 25))
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
(defconst generate--PUNCTUATION
(list "," ":" "." ";" "/" "-"))

(defalias 'generate--lt #'< "less-than alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--lte #'<= "less-than-or-equal alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--len-eq #'length= "length-equal alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--len-gt #'length> "length-greater-than alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--gt #'> "greater-than alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--gte #'>= "greater-than-or-equal alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--gte-one (-rpartial #'>= 1) "greater-than-or-equal 1?")
(defalias 'generate--gte-zero (-rpartial #'>= 0) "greater-than-or-equal 0?")
(defalias 'generate--lt0 (-rpartial #'<= 0) "less-than-or-equal 0?")
(defalias 'generate--gt0 (-rpartial #'> 0) "greater-than-or-equal 0?")
(defalias 'generate--equal-zero (-partial #'eql 0) "equal 0?")
(defalias 'generate--equal-one (-partial #'eql 1) "equal 1?")
(defalias 'generate--not-equal (-not #'equal) "not equal?")

(defalias 'generate--first-and-last-item  (-juxt #'-first-item #'-last-item))
(defalias 'generate--iterate-plus-one  (-partial #'-iterate #'1+))
(defalias 'generate--applify-iterate-plus-one  (-applify #'generate--iterate-plus-one))


(defalias 'generate--applify-equal (-applify #'equal))
(defalias 'generate--applify-rpartial (-applify #'-rpartial))
(defalias 'generate--applify-partial (-applify #'-partial))

(defalias 'generate--applify-subtract (-applify #'-))
(defalias 'generate--applify-multiply (-applify #'*))
(defalias 'generate--applify-divide (-applify #'/))


(defalias 'generate--applify-zip  (-applify #'-zip))
(defalias 'generate--applify-zip-pair  (-applify #'-zip-pair))

(defalias 'generate--applify-juxt  (-applify #'-juxt))

(defalias 'generate--applify-cons  (-applify #'cons))
(defalias 'generate--applify-concat  (-applify #'concat))
(defalias 'generate--applify-vconcat  (-applify #'vconcat))
(defalias 'generate--applify-append (-applify #'append))


(defalias 'generate--applify-mapcar  (-applify #'mapcar))
(defalias 'generate--applify-cl-subsetp (-applify #'cl-subsetp))
(defalias 'generate--applify-seq-split (-applify #'seq-split))
(defalias 'generate--applify-seq-take (-applify #'seq-take))
(defalias 'generate--applify-vector (-applify #'vector))
(defalias 'generate--applify-map-elt (-applify #'map-elt))

(defalias 'generate--seq-take-flipped (-flip #'seq-take))
(defalias 'generate--applify-seq-take-flipped (-applify #'seq-take-flipped))
(defalias 'generate--seq-elt-flipped (-flip #'seq-elt))
(defalias 'generate--applify-seq-elt-flipped (-applify #'generate--seq-elt-flipped))


(defalias 'generate--identity-and-seq-length (-juxt #'identity #'seq-length))
(defalias 'generate---duplicate (-juxt #'identity #'identity))

(defalias 'generate--any-true (-partial #'-any-p #'identity))
(defalias 'generate---every-true (-partial #'-every-p #'identity))

(defalias 'generate--flatten-one-level (-partial #'-flatten-n 1))

(defun generate--times-helper (func n call-num)
  "Unless CALL-NUM is equal to N, call FUNC with CALL-NUM as arg."
  (unless (equal call-num n)
    (cons (funcall func call-num) (1+ call-num))))


(defun generate--times (n func)
  "Call FUNC N times and collect the results into an array.
Each function call will receive the current call number as its argument."
  (-unfold (-partial #'generate--times-helper func n) 0))

(defun generate--times-no-args (n func)
  "Call a FUNC N times with no args and collect the results into an array."
  (generate--times n (lambda (_) (funcall func))))

(defalias 'generate--times-no-args-twice (-partial #'generate--times-no-args 2) "Call FUNC twice.")

(cl-defmacro generate-ert-deftest-n-times (name () &body docstring-keys-and-body)
      "Define NAME (a symbol) as a `ert-deftest' n time where n = NUM-RUNS.
NUM-RUNS can be specified as a keyword argument in addition to
the normal values of DOCSTRING-KEYS-AND-BODY.
If NUM-RUNS is not specified, your test will be defined 100 times.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] [:num-runs INTEGERS] BODY...)"
(declare (debug (&define [&name "test@" symbolp]
			 sexp [&optional stringp]
			 [&rest keywordp sexp]
			 def-body))
	   (doc-string 3)
	   (indent 2))
  (let ((documentation nil)
	(documentation-supplied-p nil)
	(run-symbol (gensym)))
(when (stringp (car docstring-keys-and-body))
  (setq documentation (pop docstring-keys-and-body)
	documentation-supplied-p t))
(cl-destructuring-bind
    ((&key (expected-result nil expected-result-supplied-p)
	   (tags nil tags-supplied-p)
	 (num-runs 100))
     body)
    (ert--parse-keys-and-body docstring-keys-and-body)
  `(cl-macrolet ((skip-when (form) `(ert--skip-when ,form))
		 (skip-unless (form) `(ert--skip-unless ,form)))
     (dotimes (run-symbol ,num-runs)
       (ert-set-test (intern (format "%s-%s" ',name run-symbol))
		   (make-ert-test
		    :name (intern (format "%s-%s" ',name run-symbol))
		    ,@(when documentation-supplied-p
			`(:documentation ,documentation))
		    ,@(when expected-result-supplied-p
			`(:expected-result-type ,expected-result))
		    ,@(when tags-supplied-p
			`(:tags ,tags))
		    :body (lambda () ,@body nil)
		    :file-name ,(or (macroexp-file-name) buffer-file-name))))))))

(defmacro generate--plural! (macro args)
  "Use ARGS to create a plural verson of MACRO."
  `(progn
     ,@(seq-map (lambda (p) `(,macro ,p))
	     (symbol-value args))))

(defalias 'generate--cons-vec (-partial #'cons 'vec) "Convert a list into a calc vector.")

(defalias 'generate-shuffle-list (-compose #'cdr (-applify #'math-shuffle-list) (-juxt #'seq-length #'seq-length #'generate--cons-vec) #'cl-copy-list) "Convert LIST into a calc vector shuffle it with math-shuffle-list. \(fn LIST)")

(defun generate--convert-calc-value-into-lisp (calc-value)
  "Converts CALC-VALUE into an emacs-lisp value."
  (read (math-format-value calc-value)))

(cl-defun generate--range-member-exclusive-p ((range-min range-max) number)
 "Is NUMBER greater than RANGE-MIN and less than or equal RANGE-MAX?"
 (and (g--gte number range-min) (g--lt number range-max)))

(defalias 'generate--between-1-and-255-p (-partial #'generate--range-member-exclusive-p (list 1 255)) "Is VALUE greater than or equal to 1 and less than 255?")
(defalias 'generate--between-0-and-1-p (-partial #'generate--range-member-exclusive-p (list 0 1)) "Is VALUE greater than or equal to zero and less than 1?")
(defalias 'generate--between-1-and-p (-compose #'generate--applify-partial (-partial #'list #'generate--range-member-exclusive-p) (-partial #'list 1)) "Is VALUE greater than or equal to one and less than or equal to the given number?")
(defalias 'generate--between-0-and-p (-compose #'generate--applify-partial (-partial #'list #'generate--range-member-exclusive-p) (-partial #'list 0)) "Is VALUE greater than or equal to zero and less than or equal to the given number?")

(cl-defun generate--non-zero-bounded-modular-addition ((range-min range-max) increase current-number)
  "Allows you to perform modular addition with ranges where RANGE-MIN is not 0.
CURRENT-NUMBER can be larger than RANGE-MAX or even smaller than RANGE-MIN.
INCREASE can also be larger than RANGE-MAX or even smaller than RANGE-MIN."
  (when (g--gte range-min range-max)
    (user-error "Range-min %d is not less than range-max %d" range-min range-max))
  (let* ((range-size (- range-max range-min))
       (adjusted-increase (mod increase range-size))
       (current-number-index (max (- current-number range-min) 0))
       (adjusted-current-number-index (mod current-number-index range-size))
       (new-number-index (mod (+ adjusted-current-number-index adjusted-increase) range-size))
       (new-number (+ range-min new-number-index)))
  new-number))

(defalias 'generate--get-next-lower-alpha-character (-partial #'generate--non-zero-bounded-modular-addition generate--LOWERALPHA 1) "Convert N into a lower alphabetic character.")
(defalias 'generate--get-next-lower-alpha-string (-compose #'char-to-string #'generate--get-next-lower-alpha-character) "Convert N into a lower alphabetic string character.")

(defalias 'generate--get-next-upper-alpha-character (-partial #'generate--non-zero-bounded-modular-addition generate--UPPERALPHA 1) "Convert N into a upper alphabetic character.")
(defalias 'generate--get-next-upper-alpha-string (-compose #'char-to-string #'generate--get-next-upper-alpha-character) "Convert N into a upper alphabetic string character.")

(defalias 'generate--get-next-number-between-zero-and-nine (-partial #'generate--non-zero-bounded-modular-addition generate--ZEROTONINE 1) "Convert N into a number between 0 and 9.")
(defalias 'generate--get-next-num-between-zero-and-nine-string (-compose #'number-to-string #'generate--get-next-number-between-zero-and-nine) "Convert N into a string that is a number between 0 and 9.")

(defalias 'generate--range-size (-compose #'generate--applify-subtract #'reverse) "Get size of RANGE.")

(cl-defun generate--scale-float-to-range ((min max) float)
  "Scale FLOAT until it is greater than or equal to MIN and less than MAX."
  (when (g--gte min max)
    (user-error "Min must be less than max"))
  (let* ((min-ceiled (ceiling min))
       (max-floored (floor max))
       (min-max (- max-floored min-ceiled))
       (float-times-min-max (* float min-max))
       (plus-min-ceiled (+ float-times-min-max min-ceiled)))
  (floor plus-min-ceiled)))

(defalias 'generate--divide-list-values-by-max-list-value (-compose #'generate--applify-mapcar (-juxt (-compose #'generate--applify-rpartial (-partial #'list #'/) #'float #'1+ #'-max) #'identity)) "Divide each value in LIST by the max value of LIST.")

(defun generate-random-float-between-0-and-1 ()
  "Returns a float that is greater than 0 and less than 1."
  (funcall (-compose #'generate--convert-calc-value-into-lisp #'math-random-float)))

(cl-defun generate-random-nat-number-in-range ((min max))
  "Returns a random number that is greater than or equal to MIN less than MAX."
  (if (eql min max)
      min
    (funcall (-compose (-partial #'generate--scale-float-to-range (list min max)) #'generate-random-float-between-0-and-1))))

(defun generate-random-nat-number-range (size)
  "Returns a random n SIZE range.
\(fn INTEGER)"
  (funcall (-juxt #'identity (-partial #'+ size))
	 (math-random-three-digit-number)))

(defalias 'generate-random-nat-number (-partial #'generate-random-nat-number-in-range generate--NATURALNUMBERS) "Returns a random natural number.")

(defalias 'generate-random-nat-number-twice (-partial #'generate--times-no-args-twice #'generate-random-nat-number))

(defalias 'generate-random-nat-number-string (-compose #'number-to-string #'generate-random-nat-number) "Returns a random natural number as a string.")

(defalias 'generate-random-negative-number (-partial #'generate-random-nat-number-in-range generate--NEGATIVENUMS) "Returns a random negative number.")

(defalias 'generate-random-nat-number-in-range-1-to-5 (-partial #'generate-random-nat-number-in-range generate--FIVERANGE) "Returns a random number that is greater than or equal to 1 and less than 5.")

(defalias 'generate--random-nat-number-in-range-10 (-partial #'generate-random-nat-number-in-range generate--TENRANGE) "Returns a random number that is greater than or equal to 1 and less than 10.")

(defalias 'generate--two-random-nat-numbers-from-range-10 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-10)))

(defalias 'generate--random-nat-number-in-range-25 (-partial #'generate-random-nat-number-in-range generate--TWENTYFIVERANGE) "Returns a random number that is greater than or equal to 1 and less than 25.")

(defalias 'generate--random-nat-number-in-range-3-25 (-partial #'generate-random-nat-number-in-range generate--THREETOTWENTYFIVERANGE) "Returns a random number that is greater than or equal to 3 and less than 25.")

(defalias 'generate--random-nat-number-in-range-5-25 (-partial #'generate-random-nat-number-in-range generate--FIVETOTWENTYFIVERANGE) "Returns a random number that is greater than or equal to 5 and less than 25.")

(defalias 'generate--two-random-nat-numbers-from-range-25 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-25)))

(defalias 'generate--two-random-nat-numbers-from-range-3-25 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-3-25)))

(defalias 'generate--two-random-nat-numbers-from-range-5-25 (lambda () (generate--times-no-args-twice #'generate--random-nat-number-in-range-5-25)))

(defalias 'generate--random-nat-number-in-range-50 (-partial #'generate-random-nat-number-in-range generate--FIFTYRANGE) "Returns a random number that is greater than or equal to 1 and less than 50.")

(defalias 'generate--random-nat-number-in-range-255 (-partial #'generate-random-nat-number-in-range generate--DEFAULTRANDOMNUMBERRANGE) "Returns a random number that is greater than or equal to 1 and less than 255.")

(defalias 'generate--random-nat-number-in-range-500 (-partial #'generate-random-nat-number-in-range generate--FIVEHUNDREDRANGE) "Returns a random number that is greater than or equal to 1 and less than 500.")

(defalias 'generate--random-nat-number-in-range-3-to-20 (-partial #'generate-random-nat-number-in-range generate--THREETO20) "Returns a random number that is greater than or equal to 3 and less than 20.")

(defalias 'generate--random-nat-number-between-0-and (-compose #'generate-random-nat-number-in-range (-partial #'list 0)) "Returns a random number that is greater than or equal to 0 and less than N. \(fn INTEGER)")

(defalias 'generate--random-nat-number-between-1-and (-compose #'generate-random-nat-number-in-range (-partial #'list 1)) "Returns a random number that is greater than or equal to 1 and less than N. \(fn INTEGER)")

(defalias 'generate--random-nat-number-between-3-and (-compose #'generate-random-nat-number-in-range (-partial #'list 3)) "Returns a random number that is greater than or equal to 3 and less than N. \(fn INTEGER)")

(defalias 'generate--divide-by-random-value (funcall (-compose #'generate--applify-rpartial (-partial #'list #'/) (-compose #'float #'generate--random-nat-number-in-range-255))) "Divide N by a random number that is greater than or equal to 1 and less than 255")

(defun generate-random-float ()
  "Returns a random float."
  (generate--divide-by-random-value (generate--random-nat-number-in-range-255)) "Returns a random float.")

(defalias 'generate-random-float-string (-compose #'number-to-string #'generate-random-float) "Returns a random float as a string.")

(defun generate-call-function-random-times (func)
  "Call a FUNC a random amount of times.
\(fn FUNCTION)"
  (generate--times-no-args (generate--random-nat-number-in-range-10) func))

(defalias 'generate-call-each-function-random-times (-partial #'mapcar #'generate-call-function-random-times) "Call each FUNC in LIST a random amount of times. The results will be collected into a list.")

(defalias 'generate-call-random-function (-compose #'funcall #'generate-seq-take-random-value-from-seq) "Take a random function from LIST, call it and then return the result. \(fn LIST)")

(defun generate-call-random-function-n-times (n list)
  "Take a random function from LIST and call it N times.
\(fn INTEGER LIST)"
  (funcall (-compose (-partial #'generate--times-no-args n) #'generate-seq-take-random-value-from-seq) list))

(defalias 'generate-call-random-function-random-times (-compose #'generate-call-function-random-times #'generate-seq-take-random-value-from-seq) "Take a random function from LIST and call it a random amount of times. The results will be collected into a list. \(fn LIST)")

(defun generate-apply-random-function-to-single-arg (list args)
  "Take a random function from LIST and apply it on ARGS.
\(fn LIST LIST)"
  (funcall (-compose (lambda (func) (apply func args)) #'generate-seq-take-random-value-from-seq) list))

(defun generate-apply-random-function-to-rest-args (list &rest args)
  "Take a random function from LIST and funcall it with ARGS.
\(fn LIST LIST)"
  (funcall (-compose (lambda (func) (apply func args)) #'generate-seq-take-random-value-from-seq) list))

(defun generate-call-n-random-functions (n funcs)
  "Take N random FUNCS from LIST and call them.
\(fn INTEGER LIST)"
  (funcall (-compose (-partial #'-map #'funcall) (-partial #'-take n) #'generate-shuffle-list) funcs))

(defalias 'generate-random-cl-constantly (-compose (-juxt #'cl-constantly #'identity) #'number-to-string #'random) "Returns a random cl-constantly and the value that it will return when called.")

(defalias 'generate-random-list-of-cl-constantlys (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate--random-nat-number-list-in-range-255) "Returns a list of random cl-constantlys and the values that each cl-constantly will return when called.")

(defun generate--random-nat-number-list (length)
  "Returns a N LENGTH list of random numbers."
  (funcall (-compose #'generate-shuffle-list #'-iota) length (math-random-three-digit-number) (math-random-three-digit-number)))

(defalias 'generate--random-nat-number-list-in-range-255 (-compose #'generate--random-nat-number-list #'generate--random-nat-number-in-range-255) "Returns a list of random numbers whose LENGTH is random.")

(defalias 'generate--random-con-from-list (-compose #'generate--applify-cons #'generate-seq-two-random-values) "Returns a random con from LIST. \(fn LIST))")

(defalias 'generate--divide-list-values-by-random-value (-partial #'mapcar #'generate--divide-by-random-value) "Divide each function in LIST by a random value.")

(defalias 'generate--list-of-integer-member-predicates (-compose (-juxt #'seq-map-member #'identity) #'generate--random-nat-number-list) "Returns a list of is-member predicates and the list of numbers used to create those predicates.")

(defalias 'generate--concat-two-cons-of-strings (-compose (-partial #'map-on #'generate--applify-cons #'generate--applify-concat #'generate--applify-concat) #'list) "Converts con-one and con-two ")

(defalias 'generate--concat-two-string-vector-cons (-compose (-partial #'map-on #'generate--applify-cons #'generate--applify-concat #'generate--applify-vconcat) #'list))

(defalias 'generate--seq-map-add-one (-partial #'seq-map #'1+))
(defalias 'generate--seq-map-seq-length (-partial #'seq-map #'seq-length))
(defalias 'generate--seq-map-member (-partial #'seq-map (lambda (x) (-partial #'member x))))

(defalias 'generate--seq-map-map-size (-partial #'seq-map #'map-length))


(defalias 'generate--seq-map-seq--into-list (-partial #'seq-map #'seq--into-list))
(defalias 'generate--seq-map-char-to-string (-partial #'seq-map #'char-to-string))
(defalias 'generate--seq-map-string--to-char (-partial #'seq-map #'string-to-char))
(defalias 'generate--seq-map-number-to-string (-partial #'seq-map #'number-to-string))
(defalias 'generate--seq-map-string-to-number (-partial #'seq-map #'string-to-number))
(defalias 'generate--seq-map-cl-constantly (-partial #'seq-map #'cl-constantly))
(defalias 'generate--seq-map-vector (-partial #'seq-map #'vector))

(defalias 'generate--seq-min-length (-compose #'-min #'seq-map-seq-length))
(defalias 'generate--seq-sum-seq-lengths (-compose #'-sum #'seq-map-seq-length))
(defalias 'generate--seq-sum-map-sizes (-compose #'-sum #'seq-map-map-size))

(defalias 'generate--seq-max-plus-one (-compose #'1+ #'seq-max))
(defalias 'generate--seq-max-plus-1-and-random-chunk-length (-juxt #'generate--seq-max-plus-one  #'generate--seq-random-chunk-length))

(defalias 'generate--seq-every-p-integer (-partial #'seq-every-p #'integerp))
(defalias 'generate--seq-every-p-nat-number (-partial #'seq-every-p #'natnump))
(defalias 'generate--seq-every-p-float (-partial #'seq-every-p #'floatp))
(defalias 'generate--seq-every-p-between-0-and-1 (-partial #'seq-every-p #'generate--between-0-and-1-p))

(defalias 'generate--seq-every-p-string (-partial #'seq-every-p #'stringp))
(defalias 'generate--seq-every-p-seq (-partial #'seq-every-p #'seqp))
(defalias 'generate--seq-every-p-map (-partial #'seq-every-p #'mapp))
(defalias 'generate--seq-every-p-list (-partial #'seq-every-p #'listp))
(defalias 'generate--seq-every-p-proper-list (-partial #'seq-every-p #'proper-list-p))
(defalias 'generate--seq-every-p-vector (-partial #'seq-every-p #'vectorp))
(defalias 'generate--seq-every-p-con (-partial #'seq-every-p #'-cons-pair-p))
(defalias 'generate--seq-every-p-symbol (-partial #'seq-every-p #'symbolp))

(defalias 'generate--seq-every-p-function (-partial #'seq-every-p #'functionp))

(defalias 'generate--seq-take-one (-rpartial #'seq-take 1))
(defalias 'generate--seq-take-two (-rpartial #'seq-take 2))
(defalias 'generate--seq-take-three (-rpartial #'seq-take 3))

(cl-defgeneric generate-seq-shuffle (seq)
  "Returns a shuffled SEQ.
Base implementation use generate-shuffle-list."
  (generate-shuffle-list seq))

(cl-defmethod generate-seq-shuffle ((seq vector))
  "Returns a shuffled SEQ (vector)."
  (funcall (-compose #'seq--into-vector #'generate-shuffle-list #'seq--into-list) seq))

(cl-defmethod generate-seq-shuffle ((seq string))
  "Returns a shuffled SEQ (string)."
   (funcall (-compose #'seq--into-string #'generate-shuffle-list #'seq--into-list) seq))

(defalias 'generate-seq-take-random-value (-compose #'generate--seq-take-one #'generate-seq-shuffle) "Returns a list with one random value from SEQ. \(fn SEQ)")

(defalias 'generate-seq-take-random-value-from-seq (-compose #'seq-first #'generate-seq-take-random-value) "Returns one random value from SEQ. \(fn SEQ)")

(defalias 'generate-seq-two-random-values (-compose #'generate--seq-take-two #'generate-seq-shuffle) "Returns a list with two random values from SEQ. \(fn SEQ)")

(defun generate--seq-random-chunk-length (seq)
  "Returns a random chunk length that is greater than or equal to 1 and less than the length of SEQ."
  (let ((max-length (max 1 (floor (seq-length seq) 2))))
    (if (equal max-length 1) 1 (generate--random-nat-number-between-1-and max-length))))

(defun generate-seq-random-chunk-of-size-n (chunk-length seq)
  "Returns a random chunk of size CHUNK-LENGTH from SEQ.
\(fn INTEGER SEQ)"
  (let* ((chunks (seq-split seq chunk-length))
     (correct-chunks (seq-filter (-rpartial #'length= chunk-length) chunks)))
    (generate-seq-take-random-value-from-seq correct-chunks)))

(defalias 'generate--applify-seq-random-chunk-of-size-n (-applify #'generate-seq-random-chunk-of-size-n) "Returns a random chunk of size CHUNK-LENGTH from SEQ. This function takes a 1 argument instead of 2.")

(defalias 'generate-seq-random-chunk (-compose #'generate--applify-seq-random-chunk-of-size-n (-juxt #'generate--seq-random-chunk-length #'identity)) "Returns a random chunk of from SEQ whose length is greater than or equal to 1 and less than the length of SEQ. \(fn SEQ)")

(defalias 'generate-seq-random-position (-compose #'generate--random-nat-number-between-0-and #'seq-length) "Returns a random position from SEQ. \(fn SEQ)")

(defalias 'generate-seq-split-random (-compose #'generate--applify-seq-split (-juxt #'identity #'generate--seq-random-chunk-length)) "Splits a SEQ into random chunks of random size. \(fn SEQ)")

(defun generate-seq-n-random-values (n seq)
  "Returns N random values from SEQ.
\(fn INTEGER SEQ)"
  (funcall (-compose (-rpartial #'seq-take n) #'generate-seq-shuffle) seq))

(defun generate-seq-random-values (seq)
  "Returns a random number of values from SEQ.
\(fn SEQ)"
  (funcall (-compose (-rpartial #'generate-seq-n-random-values seq) #'generate--seq-random-chunk-length) seq))

(cl-defgeneric generate--seq-random-iterate-from-max (seq)
  "Returns a new sequence whose length is greater than or equal to 1 and less than the length of SEQ.
The min value of the new sequence will be equal to the MAX value of SEQ + 1."
  (funcall (-compose #'generate--applify-iterate-plus-one #'generate--seq-max-plus-1-and-random-chunk-length) seq))

(cl-defmethod generate--seq-random-iterate-from-max ((seq vector))
    "Returns a new sequence (vector) whose length is greater than or equal to 1 and less than the length of SEQ.
The min value of the new sequence will be equal to the MAX value of SEQ + 1."
  (funcall (-compose #'seq--into-vector #'generate--applify-iterate-plus-one #'generate--seq-max-plus-1-and-random-chunk-length) seq))

(cl-defmethod generate--seq-random-iterate-from-max ((seq string))
  "Returns a SEQ (string) whose length is random."
  (funcall (-compose #'seq--into-string #'generate--applify-iterate-plus-one #'generate--seq-max-plus-1-and-random-chunk-length) seq))

(defalias 'generate-seq-random-value-with-position (-compose (-juxt #'generate--applify-seq-elt-flipped #'seq-first) (-juxt #'generate-seq-random-position #'identity)) "Returns a random item with its position from SEQ. \(fn SEQ)")

(defalias 'generate--map-into-alist (-rpartial #'map-into 'alist))
(defalias 'generate--map-into-plist (-rpartial #'map-into 'plist))
(defalias 'generate--map-into-hash-table (-rpartial #'map-into 'hash-table))

(defalias 'generate--map-merge-alist (-partial #'map-merge 'alist))
(defalias 'generate--map-merge-plist (-partial #'map-merge 'plist))
(defalias 'generate--map-merge-hash-table (-partial #'map-merge 'hash-table))

(defalias 'generate--alistp (-partial #'seq-every-p #'-cons-pair-p) "Is LIST an alist?")

(defalias 'generate-map-random-key (-compose #'generate-seq-take-random-value-from-seq #'map-keys) "Returns one random key from MAP. \(fn MAP)")
(defalias 'generate-seq-map-random-map-key (-partial #'seq-map #'generate-map-random-key) "Returns one random key from each map in SEQ. \(fn MAP)")

(defalias 'generate-map-random-value (-compose #'generate--applify-map-elt (-juxt #'identity #'generate-map-random-key)) "Returns one random value from MAP. \(fn MAP)")

(defun generate--map-on (op keys-trans values-trans map)
  "Apply VALUES-TRANS to MAP value, KEYS-TRANS to MAP keys and finally OP to MAP.
\(fn FUNCTION FUNCTION FUNCTION MAP)"
   (funcall (-compose op (-juxt (-compose keys-trans #'map-keys) (-compose values-trans #'map-values))) map))

(cl-defun generate-data (&key (item-transformer #'identity) (list-transformer #'generate-shuffle-list)
				   min-length max-length exact-length)
    "Return a random list.
The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH.
:ITEM-TRANSFORMER must be a unary function.
The function will called on each number in the random list.
:LIST-TRANSFORMER will be called with the random list as its only argument.
:LIST-TRANSFORMER will always been called after :ITEM-TRANSFORMER has transformed each item of the list."
  (when (and min-length max-length (g--gt min-length max-length))
      (error "Min-length must be less than max-length"))
  (let* ((min-items (or exact-length min-length 1))
	 (max-items (or exact-length max-length 50))
	 (range-length (generate-random-nat-number-in-range (list min-items max-items)))
	 (list-items (generate--random-nat-number-list range-length)))
3    (funcall (-on list-transformer (-partial #'mapcar item-transformer)) list-items)))

(defalias 'generate--seq-map-next-lower-alpha-character (-partial #'seq-map #'generate--get-next-lower-alpha-character) "Converts LIST into a list of lowercase alphabetic characters.")

  (cl-defun generate--n-words-reducer (string-of-characters (words last-end) current-end)
      "Helper function used by generate--n-words-helper.
Takes of subseq from STRING-OF-CHARACTERS. The subseq will start at LAST-END and end at CURRENT-END.
The subseq will be cons onto WORDS."
  (let* ((current-word-end (+ last-end current-end))
       (current-word (seq-subseq string-of-characters last-end current-word-end)))
    (list (cons current-word words) current-word-end)))

(defun generate--n-words-helper (word-lengths string-of-characters)
  "Helper function used by generate-n-words.
Chops STRING-OF-CHARACTERS into a list of words.
The length of each word corresponds to a value in WORD-LENGTHS."
  (-let* (((first-word-length rest-of-list) (funcall (-juxt #'car #'cdr) word-lengths))
      (first-word (list (seq-subseq string-of-characters 0 first-word-length)))
      (initial-value (list first-word first-word-length)))
  (funcall (-compose #'seq-first (-partial #'seq-reduce (-partial #'generate--n-words-reducer string-of-characters))) rest-of-list initial-value)))











(defun generate-n-alpha-string-characters (character-count)
  "Returns a random list of alphabetic string characters whose length will be equal to CHARACTER-COUNT.
\(fn INTEGER)"
  (generate-data :exact-length character-count :item-transformer #'generate--get-next-lower-alpha-string))

(defun generate-n-length-word (character-count)
  "Returns a random word whose length will be equal to CHARACTER-COUNT.
\(fn INTEGER)"
  (generate-data :exact-length character-count :item-transformer #'generate--get-next-lower-alpha-string :list-transformer #'generate--applify-concat))

(defalias 'generate-random-word (-partial #'generate-data :item-transformer #'generate--get-next-lower-alpha-string :list-transformer #'generate--applify-concat :min-length 2) "Returns a random word.")

(defun generate-n-words (word-count)
  "Returns a random list of words.
The number of lines will be equal to WORD-COUNT.
\(fn INTEGER)"
  (-let* (((word-lengths character-count) (funcall (-compose (-juxt #'identity #'-sum) #'generate-shuffle-list #'-iota) word-count (generate-random-nat-number-in-range (list 2 5))))
       (string-of-characters (generate-n-length-word character-count))
       (words (generate--n-words-helper word-lengths string-of-characters)))
    (if (generate--len-gt words word-count) (butlast words) words)))

(defalias 'generate-random-list-of-words (-compose #'generate-n-words #'1+ #'generate--random-nat-number-in-range-25) "Returns a random list of words.")

(defun generate-n-sentences (sentence-count &optional extra-generators)
      "Returns a random list of sentences.
The number of lines will be equal to SENTENCE-COUNT.
The keyword :EXTRA-GENERATORS takes a list.
Each generator must take no arguments and a return a string.
Each generator will be called a random number of times."
  (-let* ((multiple (generate-random-nat-number-in-range (list 3 10)))
      (word-count (* multiple sentence-count))
      (list-of-regular-words (generate-n-words word-count))
      (list-of-words-from-gens (-flatten-n 1 (generate-call-each-function-random-times extra-generators)))
      ((sentence-slices all-words) (funcall (-compose (-juxt (-compose (-partial #'take sentence-count) (-rpartial #'seq-split multiple)) #'identity) #'generate-shuffle-list #'append) list-of-regular-words list-of-words-from-gens))
      (sentences (seq-map (-compose (-rpartial #'concat ".") (-partial #'s-join " ")) sentence-slices)))
    (list sentences all-words list-of-regular-words list-of-words-from-gens)))

(defun generate-random-list-of-sentences ()
  "Returns a random list of sentences."
  (generate-n-sentences (generate-random-nat-number-in-range (list 3 5))))

(defun generate-string-with-n-lines (line-count &optional extra-generators)
  "Returns a string that is formed by a random list of sentences that are joined on new-lines.
The number of lines will be equal to LINE-COUNT.
  The keyword :EXTRA-GENERATORS takes a list.
Each generator must take no arguments and a return a string.
Each generator will be called a random number of times."
  (-let* (((list-of-sentences list-of-all-words list-of-alpha-words list-of-words-from-gens) (generate-n-sentences line-count extra-generators)))
    (list (s-join "\n" list-of-sentences) list-of-sentences list-of-all-words list-of-alpha-words list-of-words-from-gens)))

(defun generate-random-multiline-string (&optional extra-generators)
  "Returns a string that is formed by a random list of sentences that are joined on new-lines.
The keyword :EXTRA-GENERATORS takes a list.
Each generator must take no arguments and a return a string.
Each generator will be called a random number of times."
  (generate-string-with-n-lines (generate-random-nat-number-in-range (list 2 5)) extra-generators))

(defalias 'generate-list-of-nat-numbers #'generate-data "Returns a random list of natural numbers. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-list-of-nat-number-strings (-partial #'generate-data :item-transformer #'number-to-string) "Returns a random list of strings where each string is a natural numbers. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-list-of-floats-between-0-and-1 (-partial #'generate-data :list-transformer (-compose #'generate--divide-list-values-by-max-list-value #'generate-seq-shuffle)) "Returns a random list of floats where each float is greater than or equal to zero and less than 1. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-list-of-floats (-partial #'generate-data :list-transformer (-compose #'generate--divide-list-values-by-random-value #'generate-seq-shuffle)) "Returns a random list of floats. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-random-list-of-strings (-partial #'generate-data :min-length 50 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate-seq-split-random #'seq--into-string)) "Returns a random list of strings.")
(defalias 'generate-random-list-of-lists-nat-numbers (-partial #'generate-data :list-transformer #'generate-seq-split-random) "Returns a random list of lists of natural numbers.")

(defalias 'generate-vector-of-nat-numbers (-compose #'generate--applify-vector #'generate-list-of-nat-numbers) "Returns a random vector of natural numbers. The length of the vector can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-vector-of-floats-between-0-and-1 (-compose #'generate--applify-vector #'generate-list-of-floats-between-0-and-1) "Returns a random vector of floats where each float is greater than or equal to zero and less than 1. The length of the vector can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-vector-of-floats (-compose #'generate--applify-vector #'generate-list-of-floats) "Returns a random vector of floats. The length of the vector can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-random-vector-of-strings (-compose #'generate--applify-vector #'generate-random-list-of-strings) "Returns a random vector of strings.")
(defalias 'generate-random-vector-of-lists-nat-numbers (-partial #'generate-data :list-transformer (-compose #'generate--applify-vector #'generate-seq-split-random)) "Returns a random vector of lists of natural numbers.")
(defalias 'generate-random-vector-of-vectors-nat-numbers (-compose #'generate--seq-map-vector #'generate-random-vector-of-lists-nat-numbers) "Returns a random vector of vectors of natural numbers.")

(defalias 'generate-random-alist-of-nat-numbers (-partial #'generate-data :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse #'generate-seq-shuffle))) "Returns a random alist were both the keys and the values are natural numbers.")
(defalias 'generate-random-alist-of-strings (-partial #'generate-data :item-transformer #'char-to-string :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse #'generate-seq-shuffle))) "Returns a random alist were both the keys and the values are strings.")
(defalias 'generate-random-alist-of-string-nat-number-cons (-partial #'generate-data :list-transformer (-compose #'generate--applify-zip (-juxt (-compose #'generate--seq-map-char-to-string #'seq-reverse) #'generate-seq-shuffle))) "Returns a random alist were both the keys are strings and the values are natural numbers.")
(defalias 'generate-random-alist-of-nat-number-string-cons (-partial #'generate-data :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse (-compose #'generate--seq-map-char-to-string #'generate-seq-shuffle)))) "Returns a random alist were the keys are natural numbers and the values are strings.")

(defalias 'generate-random-plist-of-nat-numbers (-compose #'generate--map-into-plist #'generate-random-alist-of-nat-numbers) "Returns a random plist were both the keys and the values are natural numbers.")
(defalias 'generate-random-plist-of-strings (-compose #'generate--map-into-plist #'generate-random-alist-of-strings) "Returns a random plist were both the keys and the values are strings.")
(defalias 'generate-random-plist-of-string-nat-number-pairs (-compose #'generate--map-into-plist #'generate-random-alist-of-string-nat-number-cons) "Returns a random plist were both the keys are strings and the values are natural numbers.")
(defalias 'generate-random-plist-of-nat-number-string-pairs (-compose #'generate--map-into-plist #'generate-random-alist-of-nat-number-string-cons) "Returns a random plist were the keys are natural numbers and the values are strings.")

(defalias 'generate-random-hash-table-of-nat-numbers (-compose #'generate--map-into-hash-table #'generate-random-alist-of-nat-numbers) "Returns a random hash-table were both the keys and the values are natural numbers.")
(defalias 'generate-random-hash-table-of-strings (-compose #'generate--map-into-hash-table #'generate-random-alist-of-strings) "Returns a random hash-table were both the keys and the values are strings.")
(defalias 'generate-random-hash-table-of-string-nat-number-pairs (-compose #'generate--map-into-hash-table #'generate-random-alist-of-string-nat-number-cons) "Returns a random hash-table were both the keys are strings and the values are natural numbers.")
(defalias 'generate-random-hash-table-of-nat-number-string-pairs (-compose #'generate--map-into-hash-table #'generate-random-alist-of-nat-number-string-cons) "Returns a random hash-table were the keys are natural numbers and the values are strings.")

(defalias 'generate-random-con-of-nat-numbers (-partial #'generate-data :exact-length 2 :list-transformer #'generate--random-con-from-list) "Returns a random cons cell where both values are natural numbers.")
(defalias 'generate-random-con-of-floats (-partial #'generate-data :exact-length 2 :list-transformer (-compose #'generate--random-con-from-list #'generate--divide-list-values-by-max-list-value)) "Returns a random cons cell where both values are floats.")
(defalias 'generate-random-con-of-strings (-partial #'generate-data :exact-length 2 :item-transformer #'char-to-string :list-transformer #'generate--random-con-from-list) "Returns a random cons cell where both values are strings.")

(defalias 'generate-random-string-nat-number-con (-partial #'generate-data :exact-length 2 :list-transformer (-compose #'generate--applify-cons (-juxt (-compose #'char-to-string #'-first-item) #'-second-item) #'generate-seq-two-random-values)) "Returns a random cons cell where the car is a string and con is natural number.")
(defalias 'generate-random-nat-number-string-con (-partial #'generate-data :exact-length 2 :list-transformer (-compose #'generate--applify-cons (-juxt #'-first-item (-compose #'char-to-string #'-second-item)) #'generate-seq-two-random-values)) "Returns a random cons cell where the car is a natural number and the cons is a string.")
(defalias 'generate-random-string-vector-of-nat-numbers-con (-partial #'generate-data :exact-length 2 :list-transformer (-compose #'generate--applify-cons (-juxt (-compose #'char-to-string #'-first-item) (-compose #'generate--applify-vector #'cdr)))) "Returns a random cons cell where the car is a string and the cons is a vector.")

(defalias 'generate--random-nat-number-between-zero-and-60 (-partial #'generate-random-nat-number-in-range generate--ZEROTOSIXTY) "Returns a random number that is greater than or equal to 0 and less than 60.")

(defalias 'generate--random-nat-number-between-1-and-13 (-partial #'generate-random-nat-number-in-range generate--ONETOTHIRTEEN) "Returns a random number that is greater than or equal to 1 and less than 13.")

(defalias 'generate--random-nat-number-between-zero-and-24 (-partial #'generate-random-nat-number-in-range generate--ZEROTOTWENTYFOUR) "Returns a random number that is greater than or equal to 0 and less than 24.")

(defalias 'generate--pad-zeros (-partial #'s-pad-left 2 "0") "For dates and times")
(defalias 'generate--number-to-padded-string (-compose #'generate--pad-zeros #'number-to-string))
(defalias 'generate--seq-map-format-pad (-partial #'seq-map #'generate--number-to-padded-string))
(defalias 'generate--join-time-values (-partial #'s-join ":"))

(defun generate-random-12-hour-time-string ()
  "Returns a random time string in 12-hour format."
  (format "%s:%s" (generate--random-nat-number-between-1-and-13) (generate--number-to-padded-string (generate--random-nat-number-between-zero-and-60))))

(defun generate-random-24-hour-time-string ()
  "Returns a random time string in 24-hour format."
  (funcall (-compose #'generate--join-time-values #'generate--seq-map-format-pad #'list) (generate--random-nat-number-between-zero-and-24) (generate--random-nat-number-between-zero-and-60)))

(defalias 'generate-random-time-string (-partial #'generate-call-random-function (list #'generate-random-24-hour-time-string #'generate-random-12-hour-time-string)))

(defalias 'generate-random-month-number #'generate--random-nat-number-between-1-and-13 "Returns a random month number.")

(defalias 'generate-random-year-number (-partial #'generate-random-nat-number-in-range generate--YEARRANGE) "Returns a random year number.")

(defun generate-random-day-number (year month)
  "Returns a valid random day number given a YEAR and a MONTH."
  (let ((days-in-month (date-days-in-month year month)))
    (generate-random-nat-number-in-range (list 1 days-in-month))))

(defalias 'generate--day-month-year (lambda (join-on year month day) (concat day join-on month join-on year)) "Join YEAR, MONTH and DAY with JOIN-ON to create a date in D M YYYY or DD MM YYYY format.")
(defalias 'generate--month-day-year (lambda (join-on year month day) (concat month join-on day join-on year)) "Join YEAR, MONTH and DAY with JOIN-ON to create a date in M D YYYY or MM DD YYYY format.")
(defalias 'generate--year-month-day (lambda (join-on year month day) (concat year join-on month join-on day)) "Join YEAR, MONTH and DAY with JOIN-ON to create a date in YYYY M D or YYYY MM DD format.")

(defalias 'generate--call-random-full-date-formatter (-partial #'generate-apply-random-function-to-single-arg (list #'generate--day-month-year #'generate--month-day-year #'generate--year-month-day)) "Returns a random date formatter.")

;; use keywords args for with-padding
(cl-defun generate--create-random-full-date-string (join-on &key (with-padding nil with-padding-supplied-p))
    "Returns a random date where parts are joined with JOIN-ON.
If WITH-PADDING is true, the month and day will always be at least two characters, e.g. 01 instead of 1 for the 1st."
  (-let* ((padding-p (or (and with-padding-supplied-p with-padding) (generate-random-boolean)))
	(string--converter (or (and padding-p #'generate--number-to-padded-string) #'number-to-string))
	((year-number year-string) (funcall (-juxt #'identity #'number-to-string) (generate-random-year-number)))
	((month-number month-string) (funcall (-juxt #'identity string--converter) (generate-random-month-number)))
	(day-string (funcall (-compose string--converter #'generate-random-day-number) year-number month-number)))
    (generate--call-random-full-date-formatter (list join-on year-string month-string day-string))))

(defalias 'generate--month-full-year (lambda (join-on year month) (concat month join-on year)) "Join YEAR and MONTH with JOIN-ON to create a date in M YYYY or MM YYYY format.")
(defalias 'generate--full-year-month (lambda (join-on year month) (concat year join-on month)) "Join YEAR and MONTH with JOIN-ON to create a date in YYYY M or YYYY MM format.")
(defalias 'generate--month-half-year (lambda (join-on year month) (concat month join-on (s-chop-left 2 year))) "Join YEAR and MONTH with JOIN-ON to create a date in M YY or MM YY format.")
(defalias 'generate--half-year-month (lambda (join-on year month) (concat (s-chop-left 2 year) join-on month)) "Join YEAR and MONTH with JOIN-ON to create a date in YY M or YY MM format.")

(defalias 'generate--call-random-short-date-formatter (-partial #'generate-apply-random-function-to-single-arg (list #'generate--month-full-year #'generate--full-year-month #'generate--month-half-year #'generate--half-year-month)) "Returns a random short date formatter.")

;; use keywords args for with-padding
(cl-defun generate--create-random-short-date-string (join-on &key (with-padding nil with-padding-supplied-p))
  "Returns a random short date where parts are joined with JOIN-ON.
If WITH-PADDING is true, the month will always be at least two characters, e.g. 01 instead of 1 for january."
  (-let* ((padding-p (or (and with-padding-supplied-p with-padding) (generate-random-boolean)))
      (string--converter (or (and padding-p #'generate--number-to-padded-string) #'number-to-string))
      ((year-number year-string) (funcall (-juxt #'identity #'number-to-string) (generate-random-year-number)))
      ((month-number month-string) (funcall (-juxt #'identity string--converter) (generate-random-month-number))))
    (generate--call-random-short-date-formatter (list join-on year-string month-string))))

(defalias 'generate-random-full-slash-date-string (-partial #'generate--create-random-full-date-string "/") "Returns a random date where parts are joined with dashes. Format will be one of the following: YYYY/M/D, YYYY/MM/DD, M/D/YYYY, MM/DD/YYYY, D/M/YYYY or DD/MM/YYYY")

(defalias 'generate-random-full-dash-date-string (-partial #'generate--create-random-full-date-string "-") "Returns a random date where parts are joined with dashes. Format will be one of the following: YYYY-M-D, YYYY-MM-DD, M-D-YYYY, MM-DD-YYYY, D-M-YYYY or DD-MM-YYYY")

(defalias 'generate-random-short-slash-date-string (-partial #'generate--create-random-short-date-string "/") "Returns a random short date where parts are joined with slashes. Format will be one of the following: YY/M, YY/MM, M/YY, MM/YY, YYYY/M, YYYY/MM, M/YYYY or MM/YYYY.")

(defalias 'generate-random-short-dash-date-string (-partial #'generate--create-random-short-date-string "-") "Returns a random short date where parts are joined with dashes. Format will be one of the following: YY-M, YY-MM, M-YY, MM-YY, YYYY-M, YYYY-MM, M-YYYY or MM-YYYY.")

(defalias 'generate-random-date-string (-partial #'generate-call-random-function (list #'generate-random-full-dash-date-string #'generate-random-full-slash-date-string #'generate-random-short-dash-date-string #'generate-random-short-slash-date-string)))

(defalias 'generate--create-random-regular-phone-number (-compose (-partial #'s-join "-") (-partial #'seq-map (-partial #'s-join "")) (-juxt (-partial #'-take 3) (-rpartial #'-slice 3 6) (-partial #'-take-last 4))) "Helper used to convert LIST into a random U.S. style phone number.")
(defalias 'generate-random-regular-phone-number (-partial #'generate-data :min-length 10 :max-length 10 :item-transformer #'generate--get-next-num-between-zero-and-nine-string :list-transformer #'generate--create-random-regular-phone-number) "Returns a random U.S. style phone number.")

(defalias 'generate--create-random-1-800-number (-compose (-partial #'concat "1-800-") (-partial #'s-join "-") (-partial #'seq-map (-partial #'s-join "")) (-juxt (-partial #'-take 3) (-partial #'-take-last 4))) "Helper used to convert LIST into a random 1-800 number.")
(defalias 'generate-random-1-800-number (-partial #'generate-data :min-length 7 :max-length 7 :item-transformer #'generate--get-next-num-between-zero-and-nine-string :list-transformer #'generate--create-random-1-800-number) "Returns a random 1-800 number.")

(defalias 'generate-random-phone-number (-partial #'generate-call-random-function (list #'generate-random-regular-phone-number #'generate-random-1-800-number)) "Returns a random phone number.")

(defalias 'generate--create-random-card-number (-compose (-partial #'s-join "-") (-partial #'seq-map (-partial #'s-join "")) (-rpartial #'seq-split 4)) "Helper used to convert LIST into a card number string.")
(defalias 'generate-random-card-number (-partial #'generate-data :min-length 16 :max-length 16 :item-transformer #'generate--get-next-num-between-zero-and-nine-string :list-transformer #'generate--create-random-card-number) "Returns a random 16-digit card number.")

(defun generate--random-identifier-string (item-transformer)
 "Returns a random identifier string with at least one numeric character and at least two characters from ITEM-TRANSFORMER."
 (let* ((letters (generate-data :min-length 2 :max-length 10 :item-transformer item-transformer))
      (nums (generate-data :min-length 1 :max-length 10 :item-transformer #'number-to-string)))
   (funcall (-compose (-partial #'s-join "") #'generate-shuffle-list #'append) letters nums)))

(defalias 'generate-random-string-of-lower-alphanums (-partial #'generate--random-identifier-string #'generate--get-next-lower-alpha-string) "Create a random alphanumeric identifier string. All alphabetic characters will be in lowercase.")

(defalias 'generate-random-string-of-upper-alphanums (-partial #'generate--random-identifier-string #'generate--get-next-upper-alpha-string) "Create a random alphanumeric identifier string. All alphabetic characters will be in uppercase.")

(defalias 'generate--basic-tbl (-rpartial #'orgtbl-to-orgtbl '()))
(defalias 'generate--join-with-new-lines (-partial #'s-join "\n"))
(defalias 'generate--surround-table-row (lambda (x) (format "| %s |" x)))
(defalias 'generate--join-table-cells (-partial #'s-join " | "))
(defalias 'generate--interpose-hlines (-partial #'-interpose 'hline) "Add hlines to a list of org-table row strings.")

(defalias 'generate--create-table-rows (-partial #'seq-map (-compose #'generate--surround-table-row #'generate--join-table-cells)) "Convert LIST into a list of strings where each string is an org-table row.")

(defun generate--org-table-without-hlines (val-generator rows columns)
    "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR.
Returns an org-table without hlines and the list of list of values of the org-table.
\(fn FUNCTION INTEGER INTEGER)"
  (-let* (((test-row-strings test-rows-as-lists) (funcall (-compose (-juxt #'generate--create-table-rows #'identity) (-partial #'-partition columns) (-rpartial #'generate--times val-generator) #'*) rows columns)))
    (list (generate--join-with-new-lines test-row-strings) test-rows-as-lists)))

(defun generate--org-table-with-hlines (val-generator rows columns)
  "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR.
Returns an org-table with hlines and the list of list of values of the org-table.
\(fn FUNCTION INTEGER INTEGER)"
  (-let* (((table-with-hlines test-rows-as-lists) (funcall (-compose (-juxt #'generate--interpose-hlines #'identity) (-partial #'-partition columns) (-rpartial #'generate--times val-generator) #'*) rows columns)))
    (list (generate--basic-tbl table-with-hlines) table-with-hlines)))

(defconst generate-ORG-TABLE-GENS
  (list #'generate--org-table-without-hlines #'generate--org-table-with-hlines))

(defalias 'generate--org-table (-partial #'generate-apply-random-function-to-rest-args generate-ORG-TABLE-GENS) "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR.Returns an org-table with hlines and the list of list of values of the org-table. \(fn FUNCTION INTEGER INTEGER)")

(defalias 'generate-org-table-without-hlines (-compose #'car #'generate--org-table-without-hlines) "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR. The resulting table will not have hlines. \(fn FUNCTION INTEGER INTEGER)")

(defalias 'generate-org-table-with-hlines (-compose #'car #'generate--org-table-with-hlines) "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR. The resulting table will have hlines. \(fn FUNCTION INTEGER INTEGER)")

(defalias 'generate-org-table (-compose #'car #'generate--org-table) "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR. The resulting table may or may not have hlines. \(fn FUNCTION INTEGER INTEGER)")

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
       (org-table-next-field)
       (font-lock-ensure (point-min) (point-max))
       ,@body))))

(cl-defmacro generate-with-buffer-with-org-table-without-hlines (org-table-args &rest body)
    "Take ORG-TABLE-ARGS and use them to create a buffer with a table generated by generate-org-table-without-hlines.
Execute BODY in buffer."
  (declare (indent 1) (debug t))
  (generate--with-buffer-with-org-table-helper #'generate-org-table-without-hlines org-table-args body))

(cl-defmacro generate-with-buffer-with-org-table-with-hlines (org-table-args &rest body)
  "Take ORG-TABLE-ARGS and use them to create a buffer with a table generated by generate-org-table-with-hlines.
Execute BODY in buffer."
  (declare (indent 1) (debug t))
  (generate--with-buffer-with-org-table-helper #'generate-org-table-with-hlines org-table-args body))

(cl-defmacro generate-with-buffer-with-org-table (org-table-args &rest body)
  "Take ORG-TABLE-ARGS and use them to create a buffer with a table generated by generate-org-table-with-hlines.
Execute BODY in buffer."
  (declare (indent 1) (debug t))
  (generate--with-buffer-with-org-table-helper #'generate-org-table org-table-args body))

(defalias 'generate-random-boolean (-partial #'generate-seq-take-random-value-from-seq (list 't 'nil)) "Returns a random boolean.")

(defalias 'generate-symbol (-partial #'generate-data :list-transformer (-compose #'make-symbol #'seq--into-string #'generate-seq-shuffle)) "Returns a random symbol.")

(defalias 'generate-random-punctuation (-partial #'generate-seq-take-random-value-from-seq generate--PUNCTUATION) "Returns a random member of generate-PUNCTUATION.")

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
   #'generate-vector-of-nat-numbers
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
(defalias 'generate--get-random-generator (-compose #'generate-seq-take-random-value-from-seq (-partial #'generate--map-random-value generate--TYPE-GEN-MAP)) "Get a random generator from generate--TYPE-GEN-MAP.")

(cl-defmacro generate--create-generate-random-x ((type . generators-list))
"Create a generate-random-x-type function for TYPE.
When the resulting function is called, generate-call-random-function will select a function from GENERATORS-LIST."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-random-%s" ,type))))
     (defalias ,alias-name (-partial #'generate-call-random-function ,generators-list)))))

(cl-defmacro generate--create-generate-random-x-type-n-times ((type . generators-list))
"Create a generate-random-x-type-n-times function for TYPE.
When the resulting function is called, generate-call-random-function-n-times will select a function from GENERATORS-LIST."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-random-%s-type-n-times" ,type))))
     (defalias ,alias-name (-rpartial #'generate-call-random-function-n-times ,generators-list)))))

(cl-defmacro generate--create-generate-random-x-type-twice ((type . generators-list))
"Create a generate-random-x-type-twice function for TYPE.
When the resulting function is called, generate-call-random-function-n-times will select a function from GENERATORS-LIST.
The selected function will be called twice."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-random-%s-type-twice" ,type))))
     (defalias ,alias-name (-partial #'generate-call-random-function-n-times 2 ,generators-list)))))

(cl-defmacro generate--create-generate-random-x-type-random-times ((type . generators-list))
"Create a generate-random-x-type-random-times function for TYPE.
When the resulting function is called, generate-call-random-function-random-times will select a function from GENERATORS-LIST.
The selected function will be called a random amount of times."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-random-%s-type-n-random-times" ,type))))
     (defalias ,alias-name (-partial #'generate-call-random-function-random-times ,generators-list)))))

(defmacro generate--create-list-of-generate-random-x (args)
"Call generate--create-generate-random-x for each cons cell in ARGS."
`(generate--plural! generate--create-generate-random-x ,args))

(defmacro generate--create-list-of-generate-random-x-type-n-times (args)
"Call generate--create-generate-random-x-type-n-times for each cons cell in ARGS."
`(generate--plural! generate--create-generate-random-x-type-n-times ,args))

(defmacro generate--create-list-of-generate-random-x-type-twice (args)
"Call generate--create-generate-random-x-type-twice for each cons cell in ARGS."
`(generate--plural! generate--create-generate-random-x-type-twice ,args))

(defmacro generate--create-list-of-generate-random-x-type-n-random-times (args)
"Call generate--create-generate-random-x-type-random-times for each cons cell in ARGS."
`(generate--plural! generate--create-generate-random-x-type-random-times ,args))

;; Make functions available at run time
(generate--create-list-of-generate-random-x generate--TYPE-GEN-MAP)
(generate--create-list-of-generate-random-x-type-n-times generate--TYPE-GEN-MAP)
(generate--create-list-of-generate-random-x-type-twice generate--TYPE-GEN-MAP)
(generate--create-list-of-generate-random-x-type-n-random-times generate--TYPE-GEN-MAP)

(defalias 'generate-random-value (-compose #'funcall #'generate--get-random-generator) "Returns a random value.")

(defun generate--activate-font-lock-keywords ()
"Activate font-lock keywords for some of ERT's symbols."
(font-lock-add-keywords
 nil
 '(("(\\(\\<generate-ert-deftest-n-times\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face nil t)
    (2 font-lock-function-name-face nil t)))))

(add-hook 'emacs-lisp-mode-hook #'generate--activate-font-lock-keywords)

(provide 'generate)
;;; generate.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
