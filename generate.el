;;; generate.el --- Random testing for Emacs Lisp -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0.0
;; Keywords: tools, maint
;; Package-Requires: ((emacs "30.1") (compat "29") (org "9.7") (dash "2.20.0") (s "1.13.1"))
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

(defconst generate--TEST-IDENTIFITER
  "gen-ert")
(defconst generate--DEFAULT-OUTCOMES
  (list :passed-expected :passed-unexpected :skipped :failed-unexpected :failed-expected))
(defconst generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS
  (list :passed-expected
	(list :exclusive 't :with-reasons-and-results 'nil :message "passed as expected" :summary "Passed as expected" :slot #'ert--stats-passed-expected)
	:passed-unexpected
	(list :exclusive 'nil :with-reasons-and-results 'nil :message "passed unexpectedly" :summary "Passed unexpectedly" :slot #'ert--stats-passed-unexpected)
	:skipped
	(list :exclusive 't :with-reasons-and-results 'nil :message "was skipped" :summary "Skipped" :slot #'ert--stats-skipped)
	:failed-expected
	(list :exclusive 't :with-reasons-and-results 'nil :message "failed as expected" :summary "Failed as expected" :slot #'ert--stats-failed-expected)
	:failed-unexpected
	(list :exclusive 'nil :with-reasons-and-results 't :message "failed unexpectedly" :summary "Failed unexpectedly" :slot #'ert--stats-failed-unexpected)))
(defconst generate--TEST-GROUPS-PLIST
  (list
   :total-tests 0
   :passed-expected 0
   :passed-unexpected 0
   :failed-unexpected 0
   :skipped 0
   :failed-expected 0
   :failed-unexpected 0
   :reasons nil
   :results nil
   :duration 0
   :test-start-times nil
   :test-end-times nil))

(defconst generate--TEST-OUTCOME-STRINGS
  (list "passed as expected" "failed as expected" "were skipped" "failed unexpectedly" "passed unexpectedly"))


(defconst generate--TEST-SUMMARY-STRINGS
  (list "Passed as expected" "Failed as expected" "Skipped" "Failed unexpectedly" "Passed unexpectedly"))


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


(defconst generate--FILE-EXTENSIONS (list "ascii" "ascx" "asm" "asmx" "asp" "aspx" "atom" "au3" "awk" "bas"
					  "bash" "bashrc" "bat" "bbcolors" "bcp" "bdsgroup" "bdsproj" "bib"
					  "bowerrc" "c" "cbl" "cc" "cfc" "cfg" "cfm" "cfml" "cgi" "cjs" "clj"
					  "cljs" "cls" "cmake" "cmd" "cnf" "cob" "code-snippets" "coffee"
					  "coffeekup" "conf" "cp" "cpp" "cpt" "cpy" "crt" "cs" "csh" "cson"
					  "csproj" "csr" "css" "csslintrc" "csv" "ctl" "cts" "curlrc" "cxx" "d"
					  "dart" "dfm" "diff" "dof" "dpk" "dpr" "dproj" "dtd" "eco"
					  "editorconfig" "ejs" "el" "elm" "emacs" "eml" "ent" "erb" "erl"
					  "eslintignore" "eslintrc" "ex" "exs" "f" "f03" "f77" "f90" "f95"
					  "fish" "for" "fpp" "frm" "fs" "fsproj" "fsx" "ftn" "gemrc" "gemspec"
					  "gitattributes" "gitconfig" "gitignore" "gitkeep" "gitmodules" "go"
					  "gpp" "gradle" "graphql" "groovy" "groupproj" "grunit" "gtmpl"
					  "gvimrc" "h" "haml" "hbs" "hgignore" "hh" "hpp" "hrl" "hs" "hta"
					  "htaccess" "htc" "htm" "html" "htpasswd" "hxx" "iced" "iml" "inc"
					  "inf" "info" "ini" "ino" "int" "irbrc" "itcl" "itermcolors" "itk"
					  "jade" "java" "jhtm" "jhtml" "js" "jscsrc" "jshintignore" "jshintrc"
					  "json" "json5" "jsonld" "jsp" "jspx" "jsx" "ksh" "less" "lhs" "lisp"
					  "log" "ls" "lsp" "lua" "m" "m4" "mak" "map" "markdown" "master" "md"
					  "mdown" "mdwn" "mdx" "metadata" "mht" "mhtml" "mjs" "mk" "mkd" "mkdn"
					  "mkdown" "ml" "mli" "mm" "mts" "mxml" "nfm" "nfo" "noon" "npmignore"
					  "npmrc" "nuspec" "nvmrc" "ops" "pas" "pasm" "patch" "pbxproj" "pch"
					  "pem" "pg" "php" "php3" "php4" "php5" "phpt" "phtml" "pir" "pl" "pm"
					  "pmc" "pod" "pot" "prettierrc" "properties" "PROPS" "PT" "PUG" "PURS"
					  "PY" "PYX" "R" "RAKE" "RB" "RBW" "RC" "RDOC" "RDOC_OPTIONS" "RESX"
					  "REXX" "RHTML" "RJS" "RLIB" "RON" "RS" "RSS" "RST" "RTF" "RVMRC"
					  "RXML" "S" "SASS" "SCALA" "SCM" "SCSS" "SEESTYLE" "SH" "SHTML" "SLN"
					  "SLS" "SPEC" "SQL" "SQLITE" "SQLPROJ" "SRT" "SS" "SSS" "ST" "STRINGS"
					  "sty" "styl" "stylus" "sub" "sublime-build" "sublime-commands"
					  "sublime-completions" "sublime-keymap" "sublime-macro" "sublime-menu"
					  "sublime-project" "sublime-settings" "sublime-workspace" "sv" "svc"
					  "svg" "swift" "t" "tcl" "tcsh" "terminal" "tex" "text" "textile" "tg"
					  "tk" "tmLanguage" "tmpl" "tmTheme" "tpl" "ts" "tsv" "tsx" "tt" "tt2"
					  "ttml" "twig" "txt" "v" "vb" "vbproj" "vbs" "vcproj" "vcxproj" "vh"
					  "vhd" "vhdl" "vim" "viminfo" "vimrc" "vm" "vue" "webapp"
					  "webmanifest" "wsc" "x-php" "xaml" "xht" "xhtml" "xml" "xs" "xsd"
					  "xsl" "xslt" "y" "yaml" "yml" "zsh" "zshrc"))

(defgroup generate nil
  "Random generators for testing"
  :tag "generate")

(defcustom generate-lisp-timestamp-range-size generate--SECONDS-IN-AN-HOUR
  "The size of the range to be used functions which will generate random timestamps."
  :group 'generate
  :type 'natnum)

(defalias 'generate--lt #'< "less-than alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--lt1 #'< "less-than 1?")
(defalias 'generate--lte #'<= "less-than-or-equal alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--len-eq #'length= "length-equal alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--len-gt #'length> "length-greater-than alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--len-gt0 (-rpartial #'length> 0) "length-greater-than-zero alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--gt #'> "greater-than alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--gte #'>= "greater-than-or-equal alias in order to avoid issues with emacs-lisp syntax highlighting.")
(defalias 'generate--gte-one (-rpartial #'>= 1) "greater-than-or-equal 1?")
(defalias 'generate--gte-zero (-rpartial #'>= 0) "greater-than-or-equal 0?")
(defalias 'generate--lt0 (-rpartial #'<= 0) "less-than-or-equal 0?")
(defalias 'generate--gt0 (-rpartial #'> 0) "greater-than-or-equal 0?")
(defalias 'generate--gt1 (-rpartial #'> 1) "greater-than-or-equal 1?")
(defalias 'generate--equal-zero (-partial #'eql 0) "equal 0?")
(defalias 'generate--equal-one (-partial #'eql 1) "equal 1?")
(defalias 'generate--not-equal (-not #'equal) "not equal?")

(defun generate-nth-mod (n list &optional delta)
  (nth (mod (+ n (or delta 1)) (length list)) list))

(defalias 'generate--compact (-partial #'seq-filter #'identity))

(defun generate--plist-get (prop plist)
  (plist-get plist prop #'equal))

(defun generate--plist-put (prop val plist)
  (plist-put plist prop val #'equal))

(defun generate--nth-mod (n list &optional delta)
  (nth (mod (+ n (or delta 1)) (length list)) list))
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

(defalias 'generate--identity-and-length (-juxt #'identity #'length))

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

(cl-defun generate--zip-pair-longest-helper ((order-short short-list))
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
  "Zip LIST1 and LIST2 together.

Make a pair with the head of each list, followed by a pair with
the second element of each list, and so on.  The number of pairs
returned is equal to the length of LIST-ONE."
  (-let* (((length-one length-two) (mapcar #'length (list list-one list-two))))
    (if (g--lte length-one length-two)
	(-zip-pair list-one list-two)
      (seq-map-indexed (generate--zip-pair-longest-helper (list 1 list-two)) list-one))))

;;;###autoload
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
       (ert-set-test (intern (format "%s-%s-%s" ',name generate--TEST-IDENTIFITER run-symbol))
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

(defun generate--chop-each-test-name-helper-base (test-identifier)
  (lambda (test)
    (let* ((test-name (ert-test-name test))
	   (name-end-index (s-index-of (format "-%s" test-identifier) test-name))
	   (test-number-start-index (+ name-end-index (length test-identifier) 2))
	   (name (substring test-name 0 name-end-index))
	   (test-number (substring test-name test-number-start-index)))
      (cons name (string-to-number test-number)))))

(defalias 'generate--chop-each-test-name-helper (generate--chop-each-test-name-helper-base generate--TEST-IDENTIFITER))
(defalias 'generate--chop-each-test-name (-partial #'mapcar #'generate--chop-each-test-name-helper))

(cl-defun generate--get-name-count-cons-for-list-of-tests-helper ((name . vals))
  (let* ((counts (mapcar #'cdr vals)))
    (cons name (1+ (-max counts)))))

(defun generate--get-name-count-cons-for-list-of-tests (cons)
  (let* ((groups (-group-by #'car cons)))
    (mapcar #'generate--get-name-count-cons-for-list-of-tests-helper groups)))

(defun generate--create-test-group-con (stats key-to-set)
  (-lambda ((name . count))
    (cons name (generate--plist-put key-to-set count (copy-sequence stats)))))

(defun generate--create-tests-groups-alist-base (stats key-to-set tests)
  (let* ((split-tests (generate--chop-each-test-name tests))
	 (name-count-cons (generate--get-name-count-cons-for-list-of-tests split-tests)))
    (mapcar (generate--create-test-group-con stats key-to-set) name-count-cons)))

(defalias 'generate--create-tests-groups-alist (-partial #'generate--create-tests-groups-alist-base generate--TEST-GROUPS-PLIST :total-tests))

(defun generate--creates-stats-predicate (list-of-outcomes outcome exclusive _ test-group-plist)
  (thunk-let* ((outcome-value (plist-get test-group-plist outcome #'equal))
	       (other-outcomes (-remove (-partial #'equal outcome) list-of-outcomes))
	       (other-outcome-values (mapcar (lambda (other-outcome) (plist-get test-group-plist other-outcome #'equal)) other-outcomes))
	       (exclusive-check (if exclusive (seq-every-p (-partial #'equal 0) other-outcome-values) 't)))
    (and (g--gt0 outcome-value) exclusive-check)))


(defun generate--stats-default (list-of-outcomes outcome exclusive tests-groups-alist)
  (let ((result (map-filter (-partial #'generate--creates-stats-predicate list-of-outcomes outcome exclusive) tests-groups-alist)))
    (list (length result) result)))

(defalias 'generate--stats (-partial #'generate--stats-default generate--DEFAULT-OUTCOMES))

(defalias 'generate--stats-passed-expected (-partial #'generate--stats :passed-expected 't))

(defalias 'generate--stats-failed-expected (-partial #'generate--stats :failed-expected 't))

(defalias 'generate--stats-skipped (-partial #'generate--stats :skipped 't))

(defalias 'generate--stats-failed-unexpected (-partial #'generate--stats :failed-unexpected 'nil))

(defalias 'generate--stats-passed-unexpected (-partial #'generate--stats :passed-unexpected 'nil))

(defalias 'generate--create-final-tests-stats (-juxt #'length
						       #'generate--stats-passed-expected
						       #'generate--stats-failed-expected
						       #'generate--stats-skipped
						       #'generate--stats-failed-unexpected
						       #'generate--stats-passed-unexpected))

(cl-defun generate--summary-message-helper (summary-message (outcome-string . outcome-total))
  (if (not (zerop outcome-total))
      (concat summary-message "\n" (format "%s  %s" outcome-string outcome-total))
    summary-message))

(defun generate--summary-message (initial-value zipped-outcomes)
  (seq-reduce #'generate--summary-message-helper zipped-outcomes initial-value))

(cl-defun generate--default-message-printer (outcome message with-results (test-name . test-stats))
  (thunk-let* ((duration (plist-get test-stats :duration #'equal))
	       (reasons (plist-get test-stats :reasons #'equal))
	       (with-reasons (g--len-gt0 (getenv "EMACS_TEST_VERBOSE")))
	       (results (mapcar (-compose #'backtrace-to-string #'ert-test-result-with-condition-backtrace) (plist-get test-stats :results #'equal)))
	       (reasons-results (-zip-pair results reasons)))
    (message "%s %s \n Duration: %s" test-name message duration)
    (cond
     ((equal outcome :passed-as-expected))
     ((not with-results))
     ((and with-results (not with-reasons)) (mapc #'print reasons))
     ((and with-results with-reasons) (mapc (-lambda ((reason . result)) (message "%s \n %s" reason result)) reasons-results)))))

(defalias 'generate--passed-as-expected-message-printer (-partial #'generate--default-message-printer :passed-as-expected "passed as expected" 'nil))

(defalias 'generate--failed-as-expected-message-printer (-partial #'generate--default-message-printer :failed-expected "failed as expected" 'nil))

(defalias 'generate--skipped-message-printer (-partial #'generate--default-message-printer :skipped "was skipped" 'nil))

(defalias 'generate--failed-unexpected-message-printer (-partial #'generate--default-message-printer :failed-unexpected "failed unexpectedly" 't))

(defalias 'generate--passed-unexpected-message-printer (-partial #'generate--default-message-printer :passed-unexpected "passed unexpectedly" 'nil))

(cl-defun generate--maybe-print-backtrace ((stats test result))
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

(defun generate--create-test-result-key (actual-expected)
  (pcase actual-expected
    (`(:passed :expected) :passed-expected)
    (`(:failed :expected) :failed-expected)
    (`(:passed :unexpected) :passed-unexpected)
    (`(:failed :unexpected) :failed-expected)
    (`(:skipped ,_) :skipped)))

(defun generate--run-tests-batch-handle-run-started (selector tests-groups-alist event-args)
  (cl-destructuring-bind (stats) event-args
    (message "Running %s tests (%s, selector `%S')"
	     (length tests-groups-alist)
	     (ert--format-time-iso8601 (ert--stats-start-time stats))
	     selector)))

(defun generate--run-tests-batch-handle-run-ended-base (summary-strings tests-groups-alist event-args)
  (-let* (((stats abortedp) event-args)
	  ((total-tests (total-passed-as-expected . passed-as-expected)
			(total-failed-as-expected . failed-as-expected)
			(total-skipped . skipped)
			(total-failed-unexpected . failed-unexpected)
			(total-passed-unexpected . passed-unexpected))
	   (generate--create-final-tests-stats tests-groups-alist))
	  (zipped-outcomes (-zip-pair summary-strings (list total-passed-as-expected total-failed-as-expected total-skipped total-failed-unexpected total-passed-unexpected)))
	  (duration (generate--time-diff (ert--stats-end-time stats) (ert--stats-start-time stats)))
	  (start-time (format-time-string "%T" (ert--stats-start-time stats)))
	  (start-at-message (format "Start at  %s" start-time))
	  (duration-message (format "Duration  %s" duration))
	  (total-tests-message (format "Total tests  %s" (length tests-groups-alist)))
	  (breakdown-message "\nBreakdown:\n")
	  (initial-message (s-join "\n" (list start-at-message duration-message total-tests-message breakdown-message))))
    (if abortedp
	(message "Aborted")
      (message "%s" (generate--summary-message initial-message zipped-outcomes)))))

(defalias 'generate--run-tests-batch-handle-run-ended (-partial #'generate--run-tests-batch-handle-run-ended-base generate--TEST-SUMMARY-STRINGS))

(defalias 'generate--cons-vec (-partial #'cons 'vec) "Convert a list into a calc vector.")

(defalias 'generate-shuffle-list (-compose #'cdr (-applify #'math-shuffle-list) (-juxt #'seq-length #'seq-length #'generate--cons-vec) #'cl-copy-list) "Convert LIST into a calc vector shuffle it with math-shuffle-list. \(fn LIST)")

(defalias 'generate-append-and-shuffle (-compose #'generate-shuffle-list #'append))

(defun generate--convert-calc-value-into-lisp (calc-value)
  "Converts CALC-VALUE into an emacs-lisp value."
  (read (math-format-value calc-value)))

(cl-defun generate--in-range-exclusive-p ((range-min range-max) number)
 "Is NUMBER greater than RANGE-MIN and less than or equal RANGE-MAX?"
 (and (g--gte number range-min) (g--lt number range-max)))

(defalias 'generate--between-1-and-255-exclusive-p (-partial #'generate--in-range-exclusive-p (list 1 255)) "Is VALUE greater than or equal to 1 and less than 255?")
(defalias 'generate--between-0-and-1-exclusive-p (-partial #'generate--in-range-exclusive-p (list 0 1)) "Is VALUE greater than or equal to zero and less than 1?")
(defalias 'generate--between-1-and-x-exclusive-p (-compose #'generate--applify-partial (-partial #'list #'generate--in-range-exclusive-p) (-partial #'list 1)) "Is VALUE greater than or equal to one and less than or equal to the given number?")
(defalias 'generate--between-0-and-x-exclusive-p (-compose #'generate--applify-partial (-partial #'list #'generate--in-range-exclusive-p) (-partial #'list 0)) "Is VALUE greater than or equal to zero and less than or equal to the given number?")

(defalias 'generate--range-size (-compose #'generate--applify-subtract #'reverse) "Get size of RANGE.")

(cl-defun generate--in-range-inclusive-p ((min max) x)
  "Is X greater than or equal to MIN and less than or equal to MAX?"
  (<= min x max))

(defalias 'generate--between-0-and-1-inclusive-p (-partial #'generate--in-range-inclusive-p (list 0 1)) "Is VALUE greater than or equal to zero and less than or equal 1?")

(cl-defgeneric generate--get-min-lisp-timestamp (timestamps)
  (--min-by (> (car it) (car other)) timestamps))

(cl-defmethod generate--get-min-lisp-timestamp ((timestamps vector))
  (generate--get-min-lisp-timestamp (seq-into timestamps 'list)))

(cl-defgeneric generate--get-max-lisp-timestamp (timestamps)
  (--max-by (> (car it) (car other)) timestamps))

(cl-defmethod generate--get-max-lisp-timestamp ((timestamps vector))
  (generate--get-max-lisp-timestamp (seq-into timestamps 'list)))

(defun generate--lisp-timestampp (val)
  "Is VAL a timestamp."
  (when val
    (let ((current-time-list nil))
      (ignore-errors (when (decode-time val) 't)))))

(cl-defun generate--scale-float-to-range ((min max) float)
  "Scale FLOAT until it is greater than or equal to MIN and less than MAX."
  (let* ((float-min (- float (1- min)))
	 (max-min (- max min)))
    (* (/ (float min) max-min) max-min) float-min))
(generate--scale-float-to-range (list 1 10) 50)

(defalias 'generate--divide-list-values-by-max-list-value (-compose #'generate--applify-mapcar (-juxt (-compose #'generate--applify-rpartial (-partial #'list #'/) #'float #'1+ #'-max) #'identity)) "Divide each value in LIST by the max value of LIST.")

(cl-defun generate--non-zero-bounded-modular-addition ((range-min range-max) increase current-number)
  "Allows you to perform modular addition with ranges where RANGE-MIN is not 0.
CURRENT-NUMBER can be larger than RANGE-MAX or even smaller than RANGE-MIN.
INCREASE can also be larger than RANGE-MAX or even smaller than RANGE-MIN."
  (when (> range-min range-max)
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

(defalias 'generate--random-nat-number (-partial #'calcFunc-random most-positive-fixnum))

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

(defalias 'generate-two-sorted-random-nat-numbers-in-range (-compose #'sort #'generate-two-random-nat-numbers-in-range)
  "Returns two sorted random numbers that are greater than or equal to MIN less than MAX.")

(defalias 'generate-random-nat-number (-partial #'generate-random-nat-number-in-range generate--NATURALNUMBERS) "Returns a random natural number.")

(defalias 'generate-random-nat-number-twice (-partial #'generate--times-no-args-twice #'generate-random-nat-number))

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

(defalias 'generate--random-nat-number-in-range-255 (-partial #'generate-random-nat-number-in-range generate--DEFAULTRANDOMNUMBERRANGE) "Returns a random number that is greater than or equal to 1 and less than 255.")

(defalias 'generate--random-nat-number-in-range-500 (-partial #'generate-random-nat-number-in-range generate--FIVEHUNDREDRANGE) "Returns a random number that is greater than or equal to 1 and less than 500.")

(defalias 'generate--random-nat-number-in-range-3-to-20 (-partial #'generate-random-nat-number-in-range generate--THREETOTWENTY) "Returns a random number that is greater than or equal to 3 and less than 20.")

(defalias 'generate--random-nat-number-in-range-2-to-25 (-partial #'generate-random-nat-number-in-range generate--TWOTOTWENTYFIVE) "Returns a random number that is greater than or equal to 2 and less than 25.")

(defalias 'generate--random-nat-number-in-range-1-to-25 (-partial #'generate-random-nat-number-in-range generate--ONETOTWENTYFIVE) "Returns a random number that is greater than or equal to 1 and less than 25.")

(defalias 'generate--random-nat-number-between-0-and (-compose #'generate-random-nat-number-in-range (-partial #'list 0)) "Returns a random number that is greater than or equal to 0 and less than N. \(fn INTEGER)")

(defalias 'generate--random-nat-number-between-1-and (-compose #'generate-random-nat-number-in-range (-partial #'list 1)) "Returns a random number that is greater than or equal to 1 and less than N. \(fn INTEGER)")

(defalias 'generate--random-nat-number-between-3-and (-compose #'generate-random-nat-number-in-range (-partial #'list 3)) "Returns a random number that is greater than or equal to 3 and less than N. \(fn INTEGER)")

(defalias 'generate--divide-by-random-value (funcall (-compose #'generate--applify-rpartial (-partial #'list #'/) (-compose #'float #'generate--random-nat-number-in-range-255))) "Divide N by a random number that is greater than or equal to 1 and less than 255")

(defalias 'generate-random-float (-compose #'generate--divide-by-random-value #'generate--random-nat-number-in-range-255)  "Returns a random float.")

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

(defalias 'generate-random-cl-constantly (-compose (-juxt #'cl-constantly #'identity) #'number-to-string #'generate--random-nat-number) "Returns a random cl-constantly and the value that it will return when called.")

(defun generate-default-convert-n-gen-to-random (gen)
  (-compose gen #'generate--random-nat-number-in-range-1-to-25))

(defun generate--convert-n-gen-to-random-with-arg (number-generator)
  (lambda (gen)
    (lambda (arg) (funcall gen (funcall number-generator) arg))))

(defalias 'generate--non-default-convert-n-gen-to-random (generate--convert-n-gen-to-random-with-arg #'generate-random-nat-number))

(defalias 'generate-default-convert-n-gen-to-random-with-arg (generate--convert-n-gen-to-random-with-arg #'generate--random-nat-number-in-range-1-to-25))

(defun generate-nat-number-range (size)
  "Returns a random n SIZE range.
\(fn INTEGER)"
  (funcall (-juxt #'identity (-partial #'+ size))
	 (generate--random-nat-number)))

(defalias 'generate-random-nat-number-range (generate-default-convert-n-gen-to-random #'generate-nat-number-range))



(defalias 'generate-random-list-of-cl-constantlys (-compose (-juxt #'identity #'generate--seq-map-cl-constantly) #'generate--random-nat-number-list-in-range-255) "Returns a list of random cl-constantlys and the values that each cl-constantly will return when called.")

(defun generate--random-nat-number-list (length)
  "Returns a N LENGTH list of random numbers.
Numbers will be taken from the range 1..1000."
  (funcall (-compose #'generate-shuffle-list #'-iota) length (1+ (generate--random-nat-number)) (generate--random-nat-number)))

(defalias 'generate--random-nat-number-list-in-range-255 (-compose #'generate--random-nat-number-list #'generate--random-nat-number-in-range-255) "Returns a list of random numbers whose LENGTH is random.")

(defalias 'generate--random-con-from-list (-compose #'generate--applify-cons #'generate-seq-two-random-values) "Returns a random con from LIST. \(fn LIST))")

(defalias 'generate--divide-list-values-by-random-value (-partial #'mapcar #'generate--divide-by-random-value) "Divide each function in LIST by a random value.")

(defalias 'generate--list-of-integer-member-predicates (-compose (-juxt #'seq-map-member #'identity) #'generate--random-nat-number-list) "Returns a list of is-member predicates and the list of numbers used to create those predicates.")

(defalias 'generate--concat-two-cons-of-strings (-compose (-partial #'generate--map-on #'generate--applify-cons #'generate--applify-concat #'generate--applify-concat) #'list) "Converts con-one and con-two ")

(defalias 'generate--concat-two-string-vector-cons (-compose (-partial #'generate--map-on #'generate--applify-cons #'generate--applify-concat #'generate--applify-vconcat) #'list))

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
(defalias 'generate--seq-every-p-between-0-and-1 (-partial #'seq-every-p #'generate--between-0-and-1-exclusive-p))

(defalias 'generate--seq-every-p-between-0-and-1-inclusive (-partial #'seq-every-p #'generate--between-0-and-1-inclusive-p))

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

(defun generate--seq-take-last (n seq)
  (funcall (-compose (-partial #'seq-subseq seq)  (-applify #'-)  #'nreverse (-partial #'list n) #'seq-length) seq))

(defalias 'generate--seq-last (-compose #'seq-first (-partial #'generate--seq-take-last 1)))

(defun generate--seq-butlast (seq)
  (if (> (seq-length seq) 1)
      (funcall (-compose (-partial #'seq-subseq seq 0) #'1- #'seq-length) seq)
  'nil))

(defun generate--seq-cdr (seq)
  (if (> (seq-length seq) 1)
      (funcall (-compose (-partial #'seq-subseq seq 1) #'seq-length) seq)
    'nil))

(cl-defgeneric generate--seq-reduce-right-indexed (function sequence initial-value)
  "Reduce the function FUNCTION across SEQUENCE, from right to left, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQUENCE and the current index, then calling FUNCTION with that result
and the second element of SEQUENCE and the current index, then with that result and the
third element of SEQUENCE and the current index, etc. As we are iterating from the right,
indices will decrease, e.g., 3-2-1, as we iterate through the sequence.

If SEQUENCE is empty, return INITIAL-VALUE and FUNCTION is not called."
  (if (seq-empty-p sequence)
      initial-value
    (let ((acc initial-value)
	  (len (seq-length sequence)))
      (seq-do-indexed
        (lambda (elt index) (setq acc (funcall function elt acc (- len index 1))))
      (reverse sequence))
      acc)))

(cl-defgeneric generate--seq-reduce-right (function sequence initial-value)
    "Reduce the function FUNCTION across SEQUENCE, from right to left, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQUENCE, then calling FUNCTION with that result
and the second element of SEQUENCE, then with that result and the
third element of SEQUENCE, etc.  FUNCTION will be called with
INITIAL-VALUE (and then the accumulated value) as the first
argument, and the elements from SEQUENCE as the second argument.

If SEQUENCE is empty, return INITIAL-VALUE and FUNCTION is not called."
  (generate--seq-reduce-right-indexed (lambda (elt acc index) (funcall function elt acc)) sequence initial-value))

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

(cl-defgeneric generate-seq-take-infinite (n seq)
  "When n is larger than the generate-seq-length, we loop back around"
  (funcall (-compose (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defmethod generate-seq-take-infinite (n (seq vector))
  (funcall (-compose #'seq--into-vector (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defmethod generate-seq-take-infinite (n (seq string))
  (funcall (-compose #'seq--into-string (-rpartial #'seq-take n) #'-cycle) seq))

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

(defun generate-seq-subseq-infinite (seq start end)
  "We use seq-take-infinite so that we loop around"
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

(defun generate-seq-n-random-infinite-subseqs (count seq)
  (let* ((lengths (generate-data :exact-length count :item-transformer (-partial #'generate--non-zero-bounded-modular-addition generate--FIVERANGE 1)))
	 (sum-of-lengths (-sum lengths))
	 (lengths-butlast (generate--seq-butlast lengths))
	 (sum-of-lengths-butlast (-sum lengths))
	 (seq-to-slice-length (seq-length seq))
	 (slice-lengths (if (< sum-of-lengths seq-to-slice-length) (append lengths-butlast (list (- seq-to-slice-length sum-of-lengths-butlast))) lengths))
	 (slice-ends (or (generate--seq-cdr (-running-sum slice-lengths))))
	 (first-slice-end (seq-first slice-ends))
	 (initial-value (list (list (generate-seq-subseq-infinite seq 0 first-slice-end)) first-slice-end)))
    (seq-first (seq-reduce (-partial #'generate--seq-n-random-subseqs-reducer seq) slice-ends initial-value))))

(cl-defgeneric generate-seq-split-infinite (chunk-size seq)
  (let* ((chunks (ceiling (seq-length seq) chunk-size))
	 (start-index (1- (seq-length seq)))
	 (seq-to-reduce (generate--seq-butlast seq))
	 (initial-value (generate-seq-subseq-infinite seq (* start-index chunk-size) (* (1+ start-index) chunk-size))))
    (generate--seq-reduce-right-indexed
     (lambda (elt acc index)
       (append (list (generate-seq-subseq-infinite seq (* index chunk-size) (* (1+ index) chunk-size))) acc))
    seq-to-reduce
    (list initial-value))))

(defun generate-seq-n-random-chunks-of-size-x (chunk-size chunk-count seq)
  (funcall (-compose (-partial #'generate-seq-take-infinite chunk-count) #'generate-seq-shuffle #'generate-seq-split-infinite) chunk-size seq))

(defalias 'generate-seq-take-infinite-shuffled (-compose #'generate-seq-shuffle #'generate-seq-take-infinite))

(defun generate-seq-n-random-chunks-of-random-size (chunk-count seq)
  (funcall (-compose (-rpartial #'generate-seq-n-random-chunks-of-size-x chunk-count seq) #'generate--seq-random-chunk-length) seq))

(defalias 'generate--applify-seq-n-random-chunks-of-random-size (-applify #'generate-seq-n-random-chunks-of-random-size))

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

(defalias 'generate-map-random-pair (-compose (-juxt #'cadr #'generate--applify-map-elt) (-juxt #'identity #'generate-map-random-key)) "Returns one random key-value pair from MAP. \(fn MAP)")

(defun generate--map-on (op key-func values-func map)
  "Apply KEYS-FUNC to the MAP keys, VALUES-FUNC to the MAP values, and finally OP to MAP.
\(fn FUNCTION FUNCTION FUNCTION MAP)"
   (funcall (-compose op (-juxt (-compose key-func #'map-keys) (-compose values-func #'map-values))) map))

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
    (funcall (-on list-transformer (-partial #'mapcar item-transformer)) list-items)))

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

(defalias 'generate-random-string #'generate-random-word)

(defun generate-list-of-n-words (word-count)
  "Returns a random list of words.
The number of lines will be equal to WORD-COUNT.
\(fn INTEGER)"
  (-let* (((word-lengths character-count) (funcall (-compose (-juxt #'identity #'-sum) #'generate-shuffle-list #'-iota) word-count (generate-random-nat-number-in-range (list 3 6))))
       (string-of-characters (generate-n-length-word character-count))
       (words (generate--n-words-helper word-lengths string-of-characters)))
    (if (generate--len-gt words word-count) (butlast words) words)))

(defalias 'generate-list-of-n-strings #'generate-list-of-n-words)

(defalias 'generate-random-list-of-words (generate-default-convert-n-gen-to-random #'generate-list-of-n-words) "Returns a random list of words.")

(defalias 'generate-random-list-of-strings #'generate-random-list-of-words)

(defun generate-random-sentence ()
  "Returns a random sentence."
  (concat (s-join " " (generate-random-list-of-words)) "."))

(defalias 'generate-random-list-of-unique-strings (-compose #'-uniq #'generate-random-list-of-words))

(defun generate--list-of-n-sentences-base (sentence-count &optional extra-generators)
      "Returns a random list of sentences.
The number of lines will be equal to SENTENCE-COUNT.
The keyword :EXTRA-GENERATORS takes a list.
Each generator must take no arguments and a return a string.
Each generator will be called a random number of times."
  (-let* ((multiple (generate-random-nat-number-in-range (list 3 10)))
      (word-count (* multiple sentence-count))
      (list-of-regular-words (generate-list-of-n-words word-count))
      (list-of-words-from-gens (-flatten-n 1 (generate-call-each-function-random-times extra-generators)))
      ((sentence-slices all-words) (funcall (-compose (-juxt (-compose (-partial #'take sentence-count) (-rpartial #'seq-split multiple)) #'identity) #'generate-shuffle-list #'append) list-of-regular-words list-of-words-from-gens))
      (sentences (seq-map (-compose (-rpartial #'concat ".") (-partial #'s-join " ")) sentence-slices)))
    (list sentences all-words list-of-regular-words list-of-words-from-gens)))

(defalias 'generate-list-of-n-sentences (-compose #'car #'generate--list-of-n-sentences-base))

(defun generate--random-list-of-sentences-base ()
  "Returns a random list of sentences."
  (generate--list-of-n-sentences-base (generate-random-nat-number-in-range (list 3 5))))

(defalias 'generate-random-list-of-sentences (-compose #'car #'generate--random-list-of-sentences-base))

(defun generate--string-with-n-lines-base (line-count &optional extra-generators)
  "Returns a string that is formed by a random list of sentences that are joined on new-lines.
The number of lines will be equal to LINE-COUNT.
  The keyword :EXTRA-GENERATORS takes a list.
Each generator must take no arguments and a return a string.
Each generator will be called a random number of times."
  (-let* (((list-of-sentences list-of-all-words list-of-alpha-words list-of-words-from-gens) (generate--list-of-n-sentences-base line-count extra-generators)))
    (list (s-join "\n" list-of-sentences) list-of-sentences list-of-all-words list-of-alpha-words list-of-words-from-gens)))

(defalias 'generate-string-with-n-lines (-compose #'car #'generate--string-with-n-lines-base))

(defun generate--random-multiline-string-base (&optional extra-generators)
  "Returns a string that is formed by a random list of sentences that are joined on new-lines.
The keyword :EXTRA-GENERATORS takes a list.
Each generator must take no arguments and a return a string.
Each generator will be called a random number of times."
  (generate--string-with-n-lines-base (generate-random-nat-number-in-range (list 2 5)) extra-generators))

(defalias 'generate-random-multiline-string (-compose #'car #'generate--random-multiline-string-base))

(defalias 'generate-list-of-nat-numbers #'generate-data "Returns a random list of natural numbers. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-list-of-nat-number-strings (-partial #'generate-data :item-transformer #'number-to-string) "Returns a random list of strings where each string is a natural numbers. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-list-of-floats-between-0-and-1 (-partial #'generate-data :list-transformer (-compose #'generate--divide-list-values-by-max-list-value #'generate-seq-shuffle)) "Returns a random list of floats where each float is greater than or equal to zero and less than 1. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-list-of-floats (-partial #'generate-data :list-transformer (-compose #'generate--divide-list-values-by-random-value #'generate-seq-shuffle)) "Returns a random list of floats. The length of the list can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
;;(defalias 'generate-random-list-of-strings (-partial #'generate-data :min-length 20 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate-seq-split-random #'seq--into-string)) "Returns a random list of strings.")
(defalias 'generate-random-list-of-lists-nat-numbers (-partial #'generate-data :list-transformer #'generate-seq-split-random) "Returns a random list of lists of natural numbers.")

(cl-defun generate-list-of-nat-numbers-in-range (range &key (list-transformer #'generate-shuffle-list) min-length max-length exact-length)
    "Returns a list with COUNT numbers where each number is within the bounds of RANGE.
\(fn INTEGER INTEGER)"
  (generate-data :list-transformer list-transformer :item-transformer (-partial #'generate--non-zero-bounded-modular-addition range 0) :exact-length exact-length :min-length min-length :max-length max-length))

(defalias 'generate--list-of-n-nat-numbers-in-range-5 (-partial #'generate-list-of-nat-numbers-in-range generate--FIVERANGE))

(defalias 'generate--list-of-n-nat-numbers-in-range-10 (-partial #'generate-list-of-nat-numbers-in-range generate--TENRANGE))

(defalias 'generate--list-of-nat-numbers-in-range-25 (-partial #'generate-list-of-nat-numbers-in-range generate--ONETOTWENTYFIVE))

(defconst generate--LIST-ITEM-TRANSFORMERS
  (list #'generate--get-next-lower-alpha-character
	#'generate--get-next-upper-alpha-character
	#'generate--nth-mod-file-extensions
	#'generate--divide-by-random-value
	#'identity
	#'number-to-string
	#'vector
	#'list))

(defun generate--list-of-n-random-values (transformers)
  (lambda (n)
    (let ((item-transformer (generate-seq-take-random-value-from-seq transformers)))
      (generate-list-of-nat-numbers :item-transformer item-transformer :exact-length n))))

(defalias 'generate-list-of-n-random-values (generate--list-of-n-random-values generate--LIST-ITEM-TRANSFORMERS))

(defalias 'generate-vector-of-n-nat-numbers (-compose #'generate--applify-vector #'generate-list-of-nat-numbers) "Returns a random vector of natural numbers. The length of the vector can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-vector-of-floats-between-0-and-1 (-compose #'generate--applify-vector #'generate-list-of-floats-between-0-and-1) "Returns a random vector of floats where each float is greater than or equal to zero and less than 1. The length of the vector can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-vector-of-floats (-compose #'generate--applify-vector #'generate-list-of-floats) "Returns a random vector of floats. The length of the vector can be optionally specified using :MIN-LENGTH and :MAX-LENGTH or simply :EXACT-LENGTH. \(fn [:max-length INTEGER] [:min-length INTEGER] [:exact-length INTEGER])")
(defalias 'generate-random-vector-of-strings (-compose #'generate--applify-vector #'generate-random-list-of-strings) "Returns a random vector of strings.")
(defalias 'generate-random-vector-of-lists-nat-numbers (-partial #'generate-data :list-transformer (-compose #'generate--applify-vector #'generate-seq-split-random)) "Returns a random vector of lists of natural numbers.")
(defalias 'generate-random-vector-of-vectors-nat-numbers (-compose #'generate--seq-map-vector #'generate-random-vector-of-lists-nat-numbers) "Returns a random vector of vectors of natural numbers.")

(defalias 'generate-random-alist-of-nat-numbers (-partial #'generate-data :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse #'generate-seq-shuffle))) "Returns a random alist were both the keys and the values are natural numbers.")
(defalias 'generate-random-alist-of-strings (-partial #'generate-data :item-transformer #'generate--get-next-lower-alpha-string :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse #'generate-seq-shuffle))) "Returns a random alist were both the keys and the values are strings.")
(defalias 'generate-random-alist-of-string-nat-number-cons (-partial #'generate-data :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-zip (-juxt (-compose #'generate--seq-map-char-to-string #'seq-reverse) #'generate-seq-shuffle))) "Returns a random alist were both the keys are strings and the values are natural numbers.")
(defalias 'generate-random-alist-of-nat-number-string-cons (-partial #'generate-data :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-zip (-juxt #'seq-reverse (-compose #'generate--seq-map-char-to-string #'generate-seq-shuffle)))) "Returns a random alist were the keys are natural numbers and the values are strings.")

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
(defalias 'generate-random-con-of-strings (-partial #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-string :list-transformer #'generate--random-con-from-list) "Returns a random cons cell where both values are strings.")

(defalias 'generate-random-string-nat-number-con (-partial #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-cons (-juxt (-compose #'char-to-string #'-first-item) #'-second-item) #'generate-seq-two-random-values)) "Returns a random cons cell where the car is a string and con is natural number.")
(defalias 'generate-random-nat-number-string-con (-partial #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-cons (-juxt #'-first-item (-compose #'char-to-string #'-second-item)) #'generate-seq-two-random-values)) "Returns a random cons cell where the car is a natural number and the cons is a string.")
(defalias 'generate-random-string-vector-of-nat-numbers-con (-partial #'generate-data :exact-length 2 :item-transformer #'generate--get-next-lower-alpha-character :list-transformer (-compose #'generate--applify-cons (-juxt (-compose #'char-to-string #'-first-item) (-compose #'generate--applify-vector #'cdr)))) "Returns a random cons cell where the car is a string and the cons is a vector.")

(defalias 'generate--random-nat-number-between-zero-and-60 (-partial #'generate-random-nat-number-in-range generate--ZEROTOSIXTY) "Returns a random number that is greater than or equal to 0 and less than 60.")

(defalias 'generate--random-nat-number-between-1-and-13 (-partial #'generate-random-nat-number-in-range generate--ONETOTHIRTEEN) "Returns a random number that is greater than or equal to 1 and less than 13.")

(defalias 'generate--random-nat-number-between-zero-and-24 (-partial #'generate-random-nat-number-in-range generate--ZEROTOTWENTYFOUR) "Returns a random number that is greater than or equal to 0 and less than 24.")

(defalias 'generate--pad-zeros (-partial #'s-pad-left 2 "0") "For dates and times")
(defalias 'generate--number-to-padded-string (-compose #'generate--pad-zeros #'number-to-string))
(defalias 'generate--seq-map-format-pad (-partial #'seq-map #'generate--number-to-padded-string))
(defalias 'generate--join-time-values (-partial #'s-join ":"))

(defun generate--time-diff (end-time start-time)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example).
Stolen from tramp."
  (float-time (time-subtract end-time start-time)))

(defalias 'generate--get-lisp-timestamp-range-duration (-compose (-applify #'generate--time-diff) #'reverse))

(defalias 'generate--lisp-timestamp-range-duration-helper (-juxt #'car #'cadr #'generate--get-lisp-timestamp-range-duration))

(defun generate--timestamp-range-indices-to-timestamps (hz min range-indices)
  (mapcar (lambda (i) (cons (+ (* i hz) min) hz)) range-indices))

(cl-defun generate--create-timestamp-range-around-current-time (inc-bottom inc-top)
  "Uses `current-time' to create a timestamp.
Returns a list that contains the MIN and MAX
of the range along with the HZ value used for the
calculations and the length of the range relative
to the HZ. INC-BOTTOM and INC-TOP can be used
to widen or shrink the range of timestamps."
  (-let* ((current-time-list nil)
	  ((curr-secs . hz) (current-time))
	  (min (- curr-secs (* hz minus-bottom)))
	  (range-length (+ plus-top minus-bottom)))
    (list min hz range-length)))

(cl-defun generate--lisp-timestamp-helper (minus-bottom plus-top)
  "Returns a random Lisp timestamp.
The timestamp will be in the (TICKS . HZ) format.
INC-BOTTOM and INC-TOP can be sed to widen
or shrink the range of possible timestamps."
  (-let* (((min hz range-length) (generate--create-timestamp-range-around-current-time minus-bottom plus-top))
	  (range-index (generate--random-nat-number-between-0-and range-length)))
    (generate--timestamp-range-index-to-timestamp hz min range-index)))

(cl-defun generate--lisp-timestamp-range-helper (minus-bottom minus-top)
  "Returns a random Lisp timestamp.
The timestamp will be in the (TICKS . HZ) format.
INC-BOTTOM and INC-TOP can be used to widen
or shrink the range of possible timestamps."
  (-let* (((min hz range-length) (generate--create-timestamp-range-around-current-time minus-bottom minus-top))
	  (range-indices (generate-two-sorted-random-nat-numbers-in-range (list 0 range-length))))
    (generate--timestamp-range-indices-to-timestamps hz min range-indices)))

(defun generate-random-12-hour-time-string ()
  "Returns a random time string in 12-hour format."
  (format "%s:%s" (generate--random-nat-number-between-1-and-13) (generate--number-to-padded-string (generate--random-nat-number-between-zero-and-60))))

(defun generate-random-24-hour-time-string ()
  "Returns a random time string in 24-hour format."
  (funcall (-compose #'generate--join-time-values #'generate--seq-map-format-pad #'list) (generate--random-nat-number-between-zero-and-24) (generate--random-nat-number-between-zero-and-60)))

(defalias 'generate-random-time-string (-partial #'generate-call-random-function (list #'generate-random-24-hour-time-string #'generate-random-12-hour-time-string)))

(cl-defun generate-random-lisp-timestamp (&optional (range-size generate-lisp-timestamp-range-size))
  "Returns a random lisp timestamp.
RANGE-SIZE should be seconds. 
It will be used to create the range of times from
which the timestamp will be selected. Each timestamp will be in the (TICKS . HZ) format."
  (generate--lisp-timestamp-helper (floor range-size 2) (floor range-size 2)))

(cl-defun generate-random-lisp-timestamp-range (&optional (range-size generate-lisp-timestamp-range-size))
  "Returns a random lisp timestamp range.
RANGE-SIZE should be seconds. 
It will be used to create the range of times from
which the timestamp will be selected. Each timestamp will be in the (TICKS . HZ) format."
  (generate--lisp-timestamp-range-helper (floor range-size 2) (floor range-size 2)))

(defalias 'generate-random-lisp-timestamp-range-with-duration (-compose #'generate--lisp-timestamp-range-duration-helper #'generate-random-lisp-timestamp-range))

(defun generate--list-of-n-lisp-timestamp-ranges-helper (n minus-bottom minus-top)
    (-let* (((min hz range-length) (generate--create-timestamp-range-around-current-time minus-bottom minus-top))
	    (range-indices (generate-list-of-nat-numbers-in-range (list 0 range-length) :exact-length (* n 2)))
	    (timestamps (generate--timestamp-range-indices-to-timestamps hz min range-indices))
	    (raw-ranges (-partition 2 timestamps)))
      (mapcar (-rpartial #'sort :key #'car) raw-ranges)))

(cl-defun generate-list-of-n-lisp-timestamp-ranges (n &optional (range-size generate--SECONDS-IN-AN-HOUR))
  "Returns N lisp timestamp ranges.
Timestamps are in the (TICKS . HZ) format."
  (generate--list-of-n-lisp-timestamp-ranges-helper n (floor range-size 2) (floor range-size 2)))

(defalias 'generate--list-of-n-unzipped-starts-ends-durations (-compose #'-unzip-lists (-partial #'mapcar #'generate--lisp-timestamp-range-duration-helper) #'generate-list-of-n-lisp-timestamp-ranges))

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

(defmacro generate-buffer-with-text (buffer-text &rest body)
 (declare (indent 1) (debug t))
 `(with-temp-buffer
   (insert ,buffer-text)
   (goto-char (point-min))
   ,@body))

(defalias 'generate--basic-tbl (-rpartial #'orgtbl-to-orgtbl '()))
(defalias 'generate--join-with-new-lines (-partial #'s-join "\n"))
(defalias 'generate--surround-table-row (lambda (x) (format "| %s |" x)))
(defalias 'generate--join-table-cells (-partial #'s-join " | "))
(defalias 'generate--interpose-hlines (-partial #'-interpose 'hline) "Add hlines to a list of org-table row strings.")

(defalias 'generate--create-table-rows (-partial #'seq-map (-compose #'generate--surround-table-row #'generate--join-table-cells)) "Convert LIST into a list of strings where each string is an org-table row.")

(defun generate--org-table-val-generator-caller (val-generator rows columns cell-num)
  (let ((current-col (1+ (mod cell-num columns)))
	(current-row (1+ (floor cell-num columns))))
  (funcall val-generator (list current-row current-col))))

(defun generate--org-table-cell-values-helper (val-generator rows columns)
 (funcall (-compose (-partial #'-partition columns) (-rpartial #'generate--times (-partial #'generate--org-table-val-generator-caller val-generator rows columns)) #'*) rows columns))

(defun generate--org-table-without-hlines (val-generator rows columns)
    "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR.
Returns an org-table without hlines and the list of list of values of the org-table.
\(fn FUNCTION INTEGER INTEGER)"
  (-let* (((test-row-strings test-rows-as-lists) (funcall (-compose (-juxt #'generate--create-table-rows #'identity) #'generate--org-table-cell-values-helper) val-generator rows columns)))
    (list (generate--join-with-new-lines test-row-strings) test-rows-as-lists)))

(defun generate--org-table-with-hlines (val-generator rows columns)
  "Create an org-table with ROWS and COLUMNS where each value is generated by VAL-GENERATOR.
Returns an org-table with hlines and the list of list of values of the org-table.
\(fn FUNCTION INTEGER INTEGER)"
  (-let* (((table-with-hlines test-rows-as-lists) (funcall (-compose (-juxt #'generate--interpose-hlines #'identity) #'generate--org-table-cell-values-helper) val-generator rows columns)))
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

(defalias 'generate-random-file-extension (-partial #'generate-seq-take-random-value-from-seq generate--FILE-EXTENSIONS) "Returns a random file extension.")

(defalias 'generate--nth-mod-file-extensions (-rpartial #'generate--nth-mod generate--FILE-EXTENSIONS))



(defun generate-random-file-name ()
  "Returns a random file name."
  (concat (generate-random-word) "." (generate-random-file-extension)))

(defalias 'generate-random-symbol (-compose #'make-symbol #'generate-random-word) "Returns a random symbol.")

(defalias 'generate-list-of-n-symbols (-compose (-partial #'mapcar #'make-symbol) #'generate-list-of-n-words)
  "Returns a list with N symbols.")

(defalias 'generate-random-list-of-symbols (generate-default-convert-n-gen-to-random #'generate-list-of-n-symbols))

(defun generate--random-void-x-error (symbol)
  (lambda ()
    (list symbol (generate-random-symbol))))

(defalias 'generate-random-void-function-error (generate--random-void-x-error 'void-function))

(defalias 'generate-random-void-variable-error (generate--random-void-x-error 'void-variable))

(defun generate-random-wrong-type-argument-error ()
  (let* ((random-val (generate-random-value))
	 (pred (funcall (-compose (-partial #'-first (lambda (func) (not (funcall func random-val)))) #'generate-shuffle-list) generate--PREDICATES)))
  (list 'wrong-type-argument pred random-val)))

(defalias 'generate-arith-error (cl-constantly (list 'arith-error nil)))

(defconst generate--ERROR-GENERATORS
  (list
   #'generate-random-void-function-error
   #'generate-random-void-variable-error
   #'generate-random-wrong-type-argument-error
   #'generate-arith-error))

(defalias 'generate-random-error (-partial #'generate-call-random-function generate--ERROR-GENERATORS))

(defalias 'generate-random-boolean (-partial #'generate-seq-take-random-value-from-seq (list 't 'nil))
  "Returns a random boolean.")

(defun generate-list-of-n-booleans (n)
  "Returns a list with N booleans."
  (generate-data :item-transformer #'math-oddp :exact-length n))

(defalias 'generate-random-punctuation (-partial #'generate-seq-take-random-value-from-seq generate--PUNCTUATION) "Returns a random member of generate-PUNCTUATION.")

(defalias 'generate-random-color (-compose (-applify #'color-rgb-to-hex) (-partial #'generate-list-of-floats-between-0-and-1 :exact-length 3)))

(defun generate-list-of-n-colors (n)
  "Returns a list of N colors.
Values are hexadecimals."
  (let* ((float-count (* n 3))
	 (floats (generate-list-of-floats-between-0-and-1 :exact-length float-count)))
    (funcall (-compose (-partial #'mapcar (-applify #'color-rgb-to-hex)) #'-partition) 3 floats)))

(defalias 'generate-random-list-of-colors (generate-default-convert-n-gen-to-random #'generate-list-of-n-colors))

(defun generate-random-backtrace-frame ()
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

(cl-defun generate--test-name-unfolder-base (test-identifier (count . name))
  (generate--times count (lambda (index) (format "%s-%s-%s" name test-identifier index))))

(defalias 'generate--test-name-unfolder (-partial #'generate--test-name-unfolder-base generate--TEST-IDENTIFITER))

(cl-defun generate--ert-test-unfolder-base (test-identifier (count . name))
  (generate--times count (lambda (index) (generate-ert-test (format "%s-%s-%s" name test-identifier index)))))

(defalias 'generate--ert-test-unfolder (-partial #'generate--ert-test-unfolder-base generate--TEST-IDENTIFITER))

(defalias 'generate--random-ert-test-outcome (-partial #'generate-seq-take-random-value-from-seq generate--DEFAULT-OUTCOMES))

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
  (pcase (mod n 3)
    (0 (list 'equal val val))
    (1 (list 'equal (list val) (list val)))
    (2 (list 'equal (vector val) (vector val)))))

(defalias 'generate--catchall-should (generate--make-should-form-gen-for-type-x #'generate--catchall-should-pcase))

(defun generate--number-should-pcase (n number)
  (pcase (mod n 5)
    (0 (list 'numberp number))
    (1 (list 'plusp (abs number)))
    (2 (list 'minusp (* 1 (abs number))))
    (3 (list 'floatp (* 1.0 number)))
    (4 (generate--catchall-should-pcase n number))))

(defalias 'generate--number-should (generate--make-should-form-gen-for-type-x #'generate--number-should-pcase))

(defun generate--symbol-should-pcase (n symbol)
  (pcase (mod n 3)
    (0 (list 'symbolp symbol))
    (1 (equal (list 'symbol-name symbol) (list 'symbol-name symbol)))
    (2 (generate--catchall-should-pcase n symbol))))

(defalias 'generate--symbol-should (generate--make-should-form-gen-for-type-x #'generate--symbol-should-pcase))

(defun generate--seq-should-pcase (n seq)
  (pcase (mod n 5)
    (0 (list 'seqp seq))
    (1 (list 'equal (list 'seq-positions seq) (list 'seq-positions seq)))
    (2 (list 'equal (list 'seq-uniq seq) (list 'seq-uniq seq)))
    (3 (list 'seq-contains-p seq (seq-first seq)))
    (4 (generate--catchall-should-pcase n seq))))

(defalias 'generate--seq-should (generate--make-should-form-gen-for-type-x #'generate--seq-should-pcase))

(defun generate--map-should-pcase (n map)
  (pcase (mod n 5)
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
		 (pcase random-val
		   ((pred seqp) (generate--seq-should passing assert-symbol random-val))
		   ((pred mapp) (generate--map-should passing assert-symbol random-val))
		   ((pred symbolp) (generate--symbol-should passing assert-symbol random-val))
		   (_ (generate--catchall-should passing assert-symbol random-val)))))))

(defalias 'generate-passing-should-form (generate--should-form-for-type-x :passing t :assert-symbol 'should))

(defalias 'generate-passing-should-not-form (generate--should-form-for-type-x :passing t :assert-symbol 'should-not))

(defalias 'generate-failing-should-form (generate--should-form-for-type-x :passing 'nil :assert-symbol 'should))

(defalias 'generate-failing-should-not-form (generate--should-form-for-type-x :passing 'nil :assert-symbol 'should-not))

(defalias 'generate-random-passing-should (-partial #'generate-call-random-function (list #'generate-passing-should-form
									#'generate-passing-should-not-form)))

(defalias 'generate-random-failing-should (-partial #'generate-call-random-function (list #'generate-failing-should-form
									#'generate-failing-should-not-form)))

(defalias 'generate-random-should (-partial #'generate-call-random-function (list #'generate-passing-should-form
										  #'generate-passing-should-not-form
										  #'generate-failing-should-form
										  #'generate-failing-should-not-form)))

(cl-defun generate--list-of-n-passing-should-forms-helper (val (passing assert-symbol))
  (funcall (generate--should-form-for-type-x :passing passing :assert-symbol assert-symbol) val))

(defun generate-list-of-n-passing-should-forms (n)
  (let* ((vals (generate-list-of-n-random-values n))
	 (should-count (generate--random-nat-number-between-0-and n))
	 (should-not-count (- n should-count))
	 (shoulds (generate-seq-take-infinite should-count (list (list 't 'should))))
	 (should-nots (generate-seq-take-infinite should-not-count (list (list 't 'should-not))))
	 (all-should-args (generate-append-and-shuffle shoulds should-nots)))
    (-zip-with #'generate--list-of-n-passing-should-forms-helper vals all-should-args)))

(defun generate-list-of-n-should-forms-with-a-fail (n)
  (let* ((passing-forms (generate-list-of-n-passing-should-forms (1- n)))
	 (failing-form (generate-random-failing-should)))
    (generate-append-and-shuffle passing-forms (list failing-form))))

(defalias 'generate-random-list-of-should-forms-with-a-fail (generate-default-convert-n-gen-to-random #'generate-list-of-n-should-forms-with-a-fail))

(defalias 'generate-random-list-of-passing-should-forms (generate-default-convert-n-gen-to-random #'generate-list-of-n-passing-should-forms))

(defun generate-ert-test-failed-error (failing-should)
  (list 'ert-test-failed (list failing-should :form (cadr failing-should) :value nil)))

(defun generate-ert-test-failed-condition-helper (error-generators)
  (lambda (failing-should)
    (let* ((funcs (cons (-partial #'generate-ert-test-failed-error failing-should) error-generators)))
      (generate-call-random-function funcs))))

(defalias 'generate-ert-test-failed-condition (generate-ert-test-failed-condition-helper generate--ERROR-GENERATORS))

(defun generate-ert-test-skipped-condition (skipped-should)
  (list 'ert-test-skipped (list skipped-should :form (cadr skipped-should) :value 't)))

(cl-defun generate-ert-test-result-object (outcome duration)
  (thunk-let* ((passing-should-forms (generate-random-list-of-passing-should-forms))
	       (random-ert-skipped-condition (generate-ert-test-skipped-condition (generate-seq-take-random-value-from-seq passing-should-forms)))
	       (list-of-should-forms-with-a-fail (generate-random-list-of-should-forms-with-a-fail))
	       (should-forms-with-a-fail (car list-of-should-forms-with-a-fail))
	       (failing-should (cadr should-forms-with-a-fail))
	       (random-ert-failed-condition (generate-ert-test-failed-condition failing-should))
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
  (let* ((test-results (funcall (-compose (-partial #'-flatten-n 1) #'map-apply) (generate--take-from-plist-of-ert-test-results-helper test-plist-of-ert-test-result-objects) test-outcomes-count-plist))
	 (test-reasons (mapcar #'ert-reason-for-test-result test-results)))
    (list test-results test-reasons)))

(cl-defun generate-ert-test (test-name &key documentation tags file-name expected-result-type)
  (let ((test-symbol (intern test-name))
	(test-func-body (generate-random-boolean)))
    (make-ert-test
     :name test-name
     :documentation (or documentation (generate-random-sentence))
     :body (lambda () test-func-body)
     :expected-result-type (or expected-result-type ':passed)
     :file-name (or file-name (generate-random-file-name)))))

(defun generate--generate-test-base (test-identifier)
  (cl-function (lambda (test-name next-test-index-for-group &key documentation tags file-name expected-result-type)
    (generate-ert-test (format "%s-%s-%s" test-name test-identifier next-test-index-for-group) :documentation documentation :tags tags :file-name file-name :expected-result-type expected-result-type))))

(defalias 'generate--generate-test (generate--generate-test-base generate--TEST-IDENTIFIER))

(cl-defun generate--test-name-unfolder-base (test-identifier (count . name))
  (generate--times count (lambda (index) (format "%s-%s-%s" name test-identifier index))))

(defalias 'generate--test-name-unfolder (-partial #'generate--test-name-unfolder-base generate--TEST-IDENTIFIER))

(cl-defun generate--generate-test-unfolder (test-group-name total-tests expected-result-type)
  (generate--times total-tests (lambda (index) (generate--generate-test test-group-name index :expected-result-type expected-result-type))))

(defun generate--create-ert-tests-for-test-group (test-group-name test-stats)
  (-let* (((&plist :total-tests :expected-result-type) test-stats)
	  (tests (generate--generate-test-unfolder test-group-name total-tests expected-result-type)))
    tests))

(defalias 'generate--create-list-of-tests-for-tests-groups-alist (-compose (-partial #'-flatten-n 1) (-partial #'map-apply #'generate--create-ert-tests-for-test-group)))

(defalias 'generate--create-vector-of-tests-for-tests-groups-alist (-compose #'generate--applify-vector #'generate--create-list-of-tests-for-tests-groups-alist))

(defun generate--fake-fresh-test-group-con-base (stats)
  (lambda (counts name index)
    (let ((copied-stats (copy-sequence stats)))
      (cons name (generate--plist-put :total-tests (nth index counts) copied-stats)))))
(defalias 'generate--fake-fresh-test-group-con (generate--fake-fresh-test-group-con-base generate--TEST-GROUPS-PLIST))

(defun generate--fake-fresh-tests-groups-alist-base (all-outcomes)
  (lambda ()
    (let* ((group-names (generate-random-list-of-strings))
	   (total-test-count (length group-names))
	   (test-counts (generate--list-of-n-nat-numbers-in-range-5 :exact-length total-test-count))
	   (tests-groups-alist (seq-map-indexed (-partial #'generate--fake-fresh-test-group-con test-counts) group-names)))
      (list tests-groups-alist total-test-count group-names))))

(defalias 'generate--fake-fresh-tests-groups-alist (generate--fake-fresh-tests-groups-alist-base generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))

(defun generate--create-fresh-ert-tests-for-test-group (test-name test-stats)
  (-let* (((&plist :total-tests) test-stats)
	  (tests (generate--ert-test-unfolder (cons total-tests test-name))))
    tests))

(defun generate--create-fresh-ert-stats-for-tests-groups-alist (tests-groups-alist)
  (let ((tests (funcall (-compose (-partial #'-flatten-n 1) (-partial #'map-apply #'generate--create-fresh-ert-tests-for-test-group)) tests-groups-alist)))
    (ert--make-stats tests 't)))

(defun generate--fake-fresh-tests-groups-alist-and-stats ()
  (-let* (((tests-groups-alist total-test-count group-names) (generate--fake-fresh-tests-groups-alist))
	  (stats (generate--create-fresh-ert-stats-for-tests-groups-alist tests-groups-alist)))
    (list tests-groups-alist stats total-test-count group-names)))

(defun generate--fake-completed-test-group-con-for-outcome-x (requested-outcome other-outcomes counts-for-requested-outcome counts-for-other-outcomes plist-of-ert-test-result-objects durations test-start-times test-end-times group-name index)
  (-let* ((other-outcomes-plist (-interleave other-outcomes (generate-shuffle-list counts-for-other-outcomes)))
	 (count-for-requested-outcome (generate--nth-mod index counts-for-requested-outcome))
	 (absolute-outcomes-counts-plist (map-merge 'plist other-outcomes-plist (list requested-outcome count-for-requested-outcome)))
	 (absolute-total-tests (+ count-for-requested-outcome (-sum counts-for-other-outcomes)))
	 (duration (funcall (-compose (-partial #'* absolute-total-tests) #'generate--nth-mod) index durations))
	 ((test-results test-reasons) (generate--take-from-plist-of-ert-test-results plist-of-ert-test-result-objects absolute-outcomes-counts-plist))
	 (total-duration-reason-result (list :total-tests absolute-total-tests
					     :duration duration
					     :test-start-times (make-vector absolute-total-tests (generate--nth-mod index test-start-times))
					     :test-end-times (make-vector absolute-total-tests (generate--nth-mod index test-end-times))
					     :reasons test-reasons
					     :results (apply #'vector test-results)))
	 (all-stats (map-merge 'plist absolute-outcomes-counts-plist total-duration-reason-result)))
  (list (cons group-name all-stats) absolute-total-tests absolute-outcomes-counts-plist)))

(defun generate--random-fake-completed-test-group-con-for-outcome-x-base (all-outcomes requested-outcome)
  (-let* ((exclusivep (generate--plist-get :exclusive (generate--plist-get requested-outcome all-outcomes)))
	  (group-name (generate-random-word))
	  (other-outcomes (seq-remove (-partial #'equal requested-outcome) all-outcomes))
	  (length-of-other-outcomes (length other-outcomes))
	  (counts-for-other-outcomes (if exclusivep (make-list length-of-other-outcomes 0) (generate--list-of-n-nat-numbers-in-range-5 :exact-length length-of-other-outcomes)))
	  (test-durations (generate-list-of-nat-numbers :exact-length (length all-outcomes)))
	  (test-outcome-duration-pairs (-zip-lists all-outcomes test-durations))
	  (plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs))
	  ((start end duration) (generate-random-lisp-timestamp-range-with-duration))
	  ((test-start-times test-end-times test-durations) (mapcar #'list (list start end duration)))
	  (counts-for-requested-outcome (list (generate--random-nat-number-in-range-1-to-5))))

    (generate--fake-completed-test-group-con-for-outcome-x
     requested-outcome other-outcomes
     counts-for-requested-outcome
     counts-for-other-outcomes
     plist-of-ert-test-result-objects
     test-durations
     test-start-times
     test-end-times
     group-name
     0)))

(defalias 'generate--random-fake-completed-test-group-con-for-outcome-x (-partial #'generate--random-fake-completed-test-group-con-for-outcome-x-base generate--DEFAULT-OUTCOMES))

(cl-defun generate--create-data-for-fake-completed-tests-groups ((requested-outcome
								  other-outcomes
								  requested-counts-for-requested-outcome
								  requested-counts-for-other-outcomes
								  other-counts-for-requested-outcomes
								  other-counts-for-other-outcomes
								  plist-of-ert-test-result-objects
								  durations
								  test-start-times
								  test-end-times)
								 (group-name for-requested-outcome) index)
  (-let* (((counts-for-requested-outcome counts-for-other-outcomes) (if for-requested-outcome
									(list requested-counts-for-requested-outcome requested-counts-for-other-outcomes)
								      (list other-counts-for-requested-outcomes other-counts-for-other-outcomes))))
    (list requested-outcome
	  other-outcomes
	  counts-for-requested-outcome
	  counts-for-other-outcomes
	  plist-of-ert-test-result-objects
	  durations
	  test-start-times
	  test-end-times
	  group-name
	  index)))

(defun generate--fake-completed-tests-groups-alist-base (all-outcomes requested-outcome)
  (-let* ((exclusivep (generate--plist-get :exclusive (generate--plist-get requested-outcome all-outcomes)))
	  ((expected-group-names other-group-names) (generate--times-no-args-twice #'generate-random-list-of-strings))
	  (total-groups-for-requested-outcome (length expected-group-names))
	  (total-groups-for-other-outcomes (length other-group-names))
	  (groups-for-requested-outcome (mapcar (-rpartial #'list 't) expected-group-names))
	  (groups-for-other-outcomes (mapcar (-rpartial #'list 'nil) other-group-names))
	  (groups (generate-shuffle-list (append groups-for-other-outcomes groups-for-requested-outcome)))
	  (other-outcomes (funcall (-compose (-partial #'remove requested-outcome) #'map-keys) all-outcomes))
	  (outcomes-with-reasons-results (map-filter (lambda (_ v) (identity (generate--plist-get :with-reasons-and-results v))) all-outcomes))
	  (total-other-outcomes (length other-outcomes))
	  (total-outcomes-with-reasons-results (length outcomes-with-reasons-results))
	  (requested-counts-for-requested-outcome (generate--list-of-n-nat-numbers-in-range-5 :exact-length total-groups-for-requested-outcome))
	  (requested-counts-for-other-outcomes (if exclusivep (make-list total-other-outcomes 0) (generate--list-of-n-nat-numbers-in-range-5 :exact-length total-other-outcomes)))
	  ((test-start-times test-end-times test-durations) (generate--list-of-n-unzipped-starts-ends-durations (length groups)))
	  (other-counts-for-requested-outcomes (make-list total-groups-for-other-outcomes 0))
	  (other-counts-for-other-outcomes (generate--list-of-n-nat-numbers-in-range-5 :exact-length total-other-outcomes))
	  (test-outcome-duration-pairs (-zip-lists all-outcomes (generate-seq-take-infinite (length all-outcomes) test-durations)))
	  (plist-of-ert-test-result-objects (generate--plist-of-ert-test-result-objects test-outcome-duration-pairs))
	  (fake-data (list requested-outcome
			   other-outcomes
			   requested-counts-for-requested-outcome
			   requested-counts-for-other-outcomes
			   other-counts-for-requested-outcomes
			   other-counts-for-other-outcomes
			   plist-of-ert-test-result-objects
			   test-durations
			   test-start-times
			   test-end-times))
	  ((tests-groups-alist list-of-total-tests-count list-of-outcome-count-plists) (funcall (-compose #'-unzip #'seq-map-indexed) (-compose
																       (-applify #'generate--fake-completed-test-group-con-for-outcome-x)
																       (-partial #'generate--create-data-for-fake-completed-tests-groups fake-data))
												groups))
    (outcomes-counts-plist (if (length= list-of-outcome-count-plists 1)
			       (car list-of-outcome-count-plists)
			     (apply (-partial #'map-merge-with 'plist #'+) list-of-outcome-count-plists))))
    (list tests-groups-alist expected-group-names other-group-names (-sum list-of-total-tests-count) outcomes-counts-plist)))

(defalias 'generate--fake-completed-tests-groups-alist (-partial #'generate--fake-completed-tests-groups-alist-base generate--DEFAULT-OUTCOMES-FOR-SELF-TESTS))

(defalias 'generate--random-fake-completed-tests-groups-alist (-compose #'generate--fake-completed-tests-groups-alist #'generate--random-ert-test-outcome))

(defun generate--create-completed-ert-stats-for-tests-groups-alist-mapper (tests-groups-alist)
  (-lambda (test index)    
    (-let* (((name . number) (generate--chop-each-test-name-helper test))
	    (test-group-stats (map-elt tests-groups-alist name))
	    (test-result (aref (generate--plist-get :results test-group-stats) number))
	    (test-start-time (aref (generate--plist-get :test-start-times test-group-stats) number))
	    (test-end-time (aref (generate--plist-get :test-end-times test-group-stats) number)))
      (list (cons name index) test-result test-start-time test-end-time))))

(defun generate--create-completed-ert-stats-for-tests-groups-alist (total-tests tests-groups-alist)
  (-let* ((tests (funcall (-compose (-applify #'vector) (-partial #'-flatten-n 1) (-partial #'map-apply #'generate--create-fresh-ert-tests-for-test-group)) tests-groups-alist))	  
	  ((test-map test-results test-start-times test-end-times) (funcall (-compose
									     (-partial #'-flatten-n 1)
									     (-juxt (-compose (-rpartial #'map-into 'hash-table) #'car) (-compose (-partial #'mapcar (-applify #'vector)) #'cdr))
									     #'-unzip-lists
									     (-partial #'seq-map-indexed (generate--create-completed-ert-stats-for-tests-groups-alist-mapper tests-groups-alist)))
									     tests)))
    (make-ert--stats :selector 't
		     :start-time (--min-by (> (car it) (car other)) (seq-into test-start-times 'list))
		     :end-time (--max-by (> (car it) (car other)) (seq-into test-end-times 'list))
                     :tests tests
                     :test-map test-map
                     :test-results test-results
                     :test-start-times test-start-times
                     :test-end-times test-end-times)))

(defun generate--fake-completed-tests-groups-alist-and-stats (requested-outcome)
  (-let* (((tests-groups-alist group-names-for-requested-outcome other-group-names absolute-total-tests-count absolute-outcomes-counts-plist) (generate--fake-completed-tests-groups-alist requested-outcome))
	  (stats (generate--create-completed-ert-stats-for-tests-groups-alist absolute-total-tests-count tests-groups-alist)))
    (list tests-groups-alist stats group-names-for-requested-outcome other-group-names absolute-total-tests-count absolute-outcomes-counts-plist)))

(defalias 'generate--random-fake-completed-tests-groups-alist-and-stats (-compose #'generate--fake-completed-tests-groups-alist-and-stats #'generate--random-ert-test-outcome))

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
When the resulting function is called, generate-call-random-function will select a function from GENERATORS-LIST."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-random-%s" ,type))))
     (defalias ,alias-name (-partial #'generate-call-random-function ,generators-list)))))

(cl-defmacro generate--create-list-of-n-xs ((type . generators-list))
"Create a generate-list-of-n-xs function for TYPE.
When the resulting function is called, generate-call-random-function-n-times will select a function from GENERATORS-LIST."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-list-of-n-%ss" ,type))))
     (defalias ,alias-name (-rpartial #'generate-call-random-function-n-times ,generators-list)))))

(cl-defmacro generate--create-generate-random-x-type-twice ((type . generators-list))
"Create a generate-random-x-type-twice function for TYPE.
When the resulting function is called, generate-call-random-function-n-times will select a function from GENERATORS-LIST.
The selected function will be called twice."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-random-%s-type-twice" ,type))))
     (defalias ,alias-name (-partial #'generate-call-random-function-n-times 2 ,generators-list)))))

(cl-defmacro generate--create-random-list-of-xs ((type . generators-list))
"Create a generate-random-list-of-xs function for TYPE.
When the resulting function is called, generate-call-random-function-random-times will select a function from GENERATORS-LIST.
The selected function will be called a random amount of times."
(cl-with-gensyms (alias-name)
  `(let ((,alias-name (intern (format "generate-random-list-of-%ss" ,type))))
     (defalias ,alias-name (-partial #'generate-call-random-function-random-times ,generators-list)))))

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
