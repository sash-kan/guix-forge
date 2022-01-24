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
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:export (with-packages))

(define (profile-with-packages packages)
  "Return profile with PACKAGES."
  (with-store store
    (run-with-store store
      (mlet* %store-monad ((prof-drv (profile-derivation
                                      (packages->manifest packages)))
                           (profile -> (derivation->output-path prof-drv)))
        (mbegin %store-monad
          (built-derivations (list prof-drv))
          (return profile))))))

(define (environment-with-packages packages)
  "Return environment of a profile with PACKAGES. Return value is an
association list mapping the name of an environment variable to its
value."
  (map (match-lambda
         ((search-path . value)
          (cons (search-path-specification-variable search-path)
                value)))
       (profile-search-paths (profile-with-packages packages))))

(define (with-packages packages exp)
  "Return a gexp executing EXP, another gexp, in an environment where
PACKAGES are available and their search path environment variables
have been set."
  #~(begin
      (use-modules (ice-9 match))
      ;; Add a reference to the profile.
      #$(profile-with-packages packages)
      ;; Set the environment.
      (for-each (match-lambda
                  ((variable . value)
                   (setenv variable value)))
                '#$(environment-with-packages packages))
      ;; Run the provided expression.
      #$exp))
