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

(define-module (forge build utils)
  #:use-module (rnrs exceptions)
  #:use-module (ice-9 match)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (with-profile))

;;;
;;; Commentary:
;;;
;;; This module provides various build-side utilities.
;;;

;;; Code:

(define (with-profile profile exp)
  "Return a gexp referencing PROFILE and executing EXP, another gexp,
in the environment of PROFILE."
  #~(begin
      ;; Reference the profile.
      #$profile
      ;; Set the environment.
      ;; We pull out match-lambda using module-ref instead of using
      ;; use-modules because this gexp will be substituted into other
      ;; gexps and use-modules only works at the top-level.
      (let-syntax ((match-lambda (macro-transformer
                                  (module-ref (resolve-module '(ice-9 match))
                                              'match-lambda))))
        (for-each (match-lambda
                    ((variable . value)
                     (setenv variable value)))
                  '#$(map (match-lambda
                            ((search-path . value)
                             (cons (search-path-specification-variable search-path)
                                   value)))
                          (profile-search-paths profile))))
      ;; Run the provided expression.
      #$exp))
