;;; guix.scm --- Guix package for generate

(define-module (generate)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build))

(define-public generate
  (package
    (name "generate")
    (version "0.0")
    (source (local-file (getcwd) #:recursive? #t))
    (build-system emacs-build-system)  
    (inputs (list emacs-dash emacs-s emacs-compat))
    (native-inputs (list git))
    (synopsis "Random generation for Emacs Lisp")
    (description "Random generation for Emacs Lisp")
    (home-page "https://github.com/ApollonDeParnasse/generate")
    (license gpl3)))

generate
;;; guix.scm ends here

