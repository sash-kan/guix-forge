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

(use-modules (gnu packages autotools)
             ((gnu packages fonts) #:prefix guix:)
             (gnu packages gettext)
             (gnu packages guile)
             ((gnu packages skribilo) #:prefix guix:)
             (guix git-download)
             (guix packages)
             (guix utils))

;; Install web fonts in font-charter and font-fira-code. There is a
;; pending patchset https://issues.guix.gnu.org/54471 to do this in
;; upstream Guix. Use the upstream Guix packages once that patchset
;; lands in the upstream master branch.

(define font-charter
  (package
    (inherit guix:font-charter)
    (outputs '("out"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-web-fonts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (source (getcwd))
                    (fonts (string-append out "/share/fonts")))
               (for-each (lambda (file)
                           (install-file file (string-append fonts "/web")))
                         (find-files source "\\.(woff|woff2)$"))))))))))

(define font-fira-code
  (package
    (inherit guix:font-fira-code)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-web-fonts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (source (getcwd))
                    (fonts (string-append out "/share/fonts")))
               (for-each (lambda (file)
                           (install-file file (string-append fonts "/web")))
                         (find-files source "\\.(woff|woff2)$"))))))))))

;; Use a later unreleased version of skribilo since we need certain
;; improvements and bug fixes from it.
(define skribilo
  (let ((commit "76136f9e904e8eb17f494d20fa2969ef2d5eb1aa")
        (revision "1"))
    (package
      (inherit guix:skribilo)
      (name "skribilo")
      (version (git-version "0.9.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/skribilo.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "105jlpqs63fa724yldgs36bgnw3h4lq5addhmb9y3nla5a4vn2m2"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gettext" ,gnu-gettext)
         ,@(package-native-inputs guix:skribilo))))))

(packages->manifest
 (list font-charter font-fira-code guile-3.0 skribilo))
