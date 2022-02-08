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

(define-module (forge build git)
  #:use-module ((guix build git) #:prefix guix-build:)
  #:use-module (guix build utils)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (download-git-to-store
            latest-git-checkout))

;;;
;;; Commentary:
;;;
;;; This module provides build-side code to download a git repository
;;; to the store.
;;;

;;; Code:

(define* (download-git-to-store store name url commit #:key (git-command "git"))
  "Download COMMIT from git repository from URL to STORE under NAME
and return store path."
  (call-with-temporary-directory
   (lambda (directory)
     (unless (with-output-to-port (current-error-port)
               (lambda ()
                 (guix-build:git-fetch url commit directory
                                       #:git-command git-command)))
       (error "Error fetching git repository" url commit))
     (add-to-store store name #t "sha256" directory))))

(define latest-git-checkout
  (store-lift download-git-to-store))
