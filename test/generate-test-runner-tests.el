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

(defconst generate--OUTCOMES-FOR-EVENT-LISTENERS-TESTS
  (list :passed-expected :failed-expected :passed-unexpected :failed-unexpected :skipped :aborted :quit))

(generate-ert-deftest-n-times generate--fake-completed-tests-groups-alist ()
  (-let* ((expected-outcome (generate-map-random-key generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))
	  ((actual-tests-groups-alist actual-count actual-group-names) (generate--fake-completed-tests-groups-alist expected-outcome))
	  (actual-random-group-name (generate-seq-take-random-value-from-seq actual-group-names))
	  (actual-group-stats (map-elt actual-tests-groups-alist actual-random-group-name))
	  ((&plist
	    :total-tests
	    :duration
	    :test-start-times
	    :test-end-times
	    :reasons
	    :results)
	   actual-group-stats)
	  (actual-random-reason-and-result (mapcar #'generate-seq-take-random-value-from-seq (list results reasons)))
	  (actual-requested-outcome-count (generate--plist-get expected-outcome actual-group-stats)))
    (mapc (lambda (x) (should (g--gt0 x))) (list total-tests duration actual-requested-outcome-count))
    (mapc (lambda (x) (should (listp x))) (list reasons results test-start-times test-end-times))
    (mapc (lambda (x) (should (g--len-gt0 x))) (list reasons results actual-group-names test-start-times test-end-times))
    (mapc (lambda (x) (should (stringp x))) actual-random-reason-and-result)))

;; Local Variables:
;; read-symbol-shorthands: (("g-" . "generate-"))
;; End:
