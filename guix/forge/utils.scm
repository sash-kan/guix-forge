;;; guix-forge --- Guix software forge meta-service
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of guix-forge.
;;;
;;; guix-forge is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; guix-forge is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guix-forge.  If not, see
;;; <https://www.gnu.org/licenses/>.

(define-module (forge utils)
  #:use-module (ice-9 match)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:export (with-manifest
            with-packages))

(define (with-manifest manifest exp)
  "Return a gexp executing EXP, another gexp, in a profile defined by
MANIFEST."
  (with-imported-modules (source-module-closure '((guix search-paths)))
    #~(begin
        ;; Set the environment.
        ;; We pull out match-lambda using module-ref instead of using
        ;; use-modules because this gexp will be substituted into other
        ;; gexps and use-modules only works at the top-level.
        (let-syntax ((match-lambda (macro-transformer
                                    (module-ref (resolve-module '(ice-9 match))
                                                'match-lambda))))
          (let ((evaluate-search-paths (@@ (guix search-paths) evaluate-search-paths))
                (search-path-specification-variable (@@ (guix search-paths)
                                                        search-path-specification-variable))
                (sexp->search-path-specification (@@ (guix search-paths)
                                                     sexp->search-path-specification)))
            (for-each (match-lambda
                        ((specification . value)
                         (setenv (search-path-specification-variable specification)
                                 value)))
                      (evaluate-search-paths
                       (map sexp->search-path-specification
                            '#$(map search-path-specification->sexp
                                    (manifest-search-paths manifest)))
                       (list #$(profile
                                (content manifest)
                                (allow-collisions? #t)))))))
        ;; Run the provided expression.
        #$exp)))

(define (with-packages packages exp)
  "Return a gexp executing EXP, another gexp, in an environment where
PACKAGES are available and their search path environment variables
have been set."
  (with-manifest (packages->manifest packages)
                 exp))
