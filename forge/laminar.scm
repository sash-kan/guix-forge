;;; guix-forge --- Guix software forge meta-service
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (forge laminar)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages ci)
  #:use-module (guix records)
  #:export (guix-laminar-service-type
            guix-laminar-configuration
            guix-laminar-configuration-state-directory
            guix-laminar-configuration-jobs
            guix-laminar-job
            guix-laminar-job?
            guix-laminar-job-name
            guix-laminar-job-run
            guix-laminar-job-after
            guix-laminar-group
            guix-laminar-group?
            guix-laminar-group-name
            guix-laminar-group-regex))

(define-record-type* <guix-laminar-configuration>
  guix-laminar-configuration make-guix-laminar-configuration
  guix-laminar-configuration?
  (state-directory guix-laminar-configuration-state-directory
                   (default "/var/lib/laminar"))
  (jobs guix-laminar-configuration-jobs
        (default '()))
  (groups guix-laminar-configuration-groups
          (default '())))

(define-record-type* <guix-laminar-job>
  guix-laminar-job make-guix-laminar-job
  guix-laminar-job?
  (name guix-laminar-job-name)
  (run guix-laminar-job-run)
  (after guix-laminar-job-after
         (default #f)))

(define-record-type* <guix-laminar-group>
  guix-laminar-group make-guix-laminar-group
  guix-laminar-group?
  (name guix-laminar-group-name)
  (regex guix-laminar-group-regex))

(define (guix-laminar-activation config)
  (let* ((state-directory (guix-laminar-configuration-state-directory config))
         (groups-configuration (string-append state-directory "/cfg/groups.conf"))
         (jobs-directory (string-append state-directory "/cfg/jobs")))
    #~(begin
        (use-modules (srfi srfi-26))

        ;; Configure groups.
        (when (file-exists? #$groups-configuration)
          (delete-file #$groups-configuration))
        (symlink
         #$(plain-file "laminar-groups"
                       (string-join (map (lambda (group)
                                           (string-append (guix-laminar-group-name group)
                                                          "="
                                                          (guix-laminar-group-regex group)))
                                         (guix-laminar-configuration-groups config))
                                    "\n"))
         #$groups-configuration)
        ;; Create jobs directory and populate with job scripts.
        (mkdir-p #$(dirname jobs-directory))
        (when (file-exists? #$jobs-directory)
          (delete-file #$jobs-directory))
        (symlink
         #$(file-union "laminar-jobs"
                       (append-map (lambda (job)
                                     (let ((name (guix-laminar-job-name job))
                                           (run (guix-laminar-job-run job))
                                           (after (guix-laminar-job-after job)))
                                       (cons (list (string-append name ".run")
                                                   (program-file name run))
                                             (if after
                                                 (list (list (string-append name ".after")
                                                             (program-file name after)))
                                                 (list)))))
                                   (guix-laminar-configuration-jobs config)))
         #$jobs-directory)
        ;; Set permissions for laminar directory.
        (for-each (lambda (file)
                    (let ((user (getpw "laminar")))
                      (chown file (passwd:uid user) (passwd:gid user))))
                  (find-files #$state-directory
                              (lambda (file stat)
                                (memq (stat:type stat)
                                      '(regular directory)))
                              #:directories? #t)))))

(define guix-laminar-service-type
  (service-type
   (name 'guix-laminar)
   (description "Run guix-laminar.")
   (extensions (list (service-extension activation-service-type
                                        guix-laminar-activation)))
   (compose concatenate)
   (extend (lambda (config extended-values)
             (guix-laminar-configuration
              (inherit config)
              (jobs (append (guix-laminar-configuration-jobs config)
                            (filter guix-laminar-job? extended-values)))
              (groups (append (guix-laminar-configuration-groups config)
                              (filter guix-laminar-group? extended-values))))))
   (default-value (guix-laminar-configuration))))
