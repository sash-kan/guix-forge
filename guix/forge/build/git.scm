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
  #:use-module (rnrs exceptions)
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

(define (hline)
  "Print a horizontal line 50 '=' characters long."
  (display (make-string 50 #\=))
  (newline)
  (force-output))

(define* (download-git-to-store store name url #:key branch show-commit? deep-clone?)
  "Download BRANCH of git repository from URL to STORE under NAME and
return store path. If BRANCH is not specified, the default branch is
downloaded. git and certificates should be in the environment.

If DEEP-CLONE? is #t, the git repository is deep cloned and the .git
directory is included in the store. Else, the git repository is
shallow cloned and the .git directory is not included."
  (call-with-temporary-directory
   (lambda (directory)
     (with-directory-excursion directory
       (guard (condition ((invoke-error? condition)
                          (format (current-error-port)
                                  "'~a~{ ~a~}' failed with exit code ~a~%"
                                  (invoke-error-program condition)
                                  (invoke-error-arguments condition)
                                  (invoke-error-exit-status condition))
                          (exit #f)))
         (apply invoke
                "git" "clone" "--quiet"
                (append (if deep-clone?
                            (list)
                            (list "--depth" "1"))
                        ;; Append file:// to local repository path so
                        ;; that shallow clone works.
                        (list (if (string-prefix? "/" url)
                                  (string-append "file://" url)
                                  url))
                        (if branch
                            (list "--branch" branch)
                            (list))
                        (list "."))))
       (when show-commit?
         (hline)
         (invoke "git" "log" "HEAD~1..HEAD")
         (hline))
       (unless deep-clone?
         (delete-file-recursively ".git")))
     (add-to-store store name #t "sha256" directory))))

(define latest-git-checkout
  (store-lift download-git-to-store))
