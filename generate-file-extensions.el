;;; generate-file-extensions.el --- Random testing for Emacs Lisp -*- lexical-binding: t; no-byte-compile: t -*-

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

;; Large constant for generate

;;; Code:

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

(provide 'generate-file-extensions)
;;; generate.el ends here
