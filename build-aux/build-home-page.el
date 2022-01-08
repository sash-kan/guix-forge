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

;;; Commentary:

;; This Emacs script generates the home page of the website from
;; README.org.

;;; Code:

(require 'ox-html)

(setq make-backup-files nil
      org-export-with-section-numbers nil
      org-export-with-sub-superscripts nil
      org-export-with-toc nil
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-postamble nil)

(defun build-website ()
  (with-current-buffer (find-file "README.org")
    (org-export-to-file 'html "website/index.html")))
