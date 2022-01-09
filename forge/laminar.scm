;;; guix-forge --- Guix software forge meta-service
;;; Copyright © 2021, 2022 Arun Isaac <arunisaac@systemreboot.net>
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
  #:export (forge-laminar-service-type
            forge-laminar-configuration
            forge-laminar-configuration-state-directory
            forge-laminar-configuration-jobs
            forge-laminar-job
            forge-laminar-job?
            forge-laminar-job-name
            forge-laminar-job-run
            forge-laminar-job-after
            forge-laminar-group
            forge-laminar-group?
            forge-laminar-group-name
            forge-laminar-group-regex))

(define-record-type* <forge-laminar-configuration>
  forge-laminar-configuration make-forge-laminar-configuration
  forge-laminar-configuration?
  (state-directory forge-laminar-configuration-state-directory
                   (default "/var/lib/laminar"))
  (jobs forge-laminar-configuration-jobs
        (default '()))
  (groups forge-laminar-configuration-groups
          (default '())))

(define-record-type* <forge-laminar-job>
  forge-laminar-job make-forge-laminar-job
  forge-laminar-job?
  (name forge-laminar-job-name)
  (run forge-laminar-job-run)
  (after forge-laminar-job-after
         (default #f)))

(define-record-type* <forge-laminar-group>
  forge-laminar-group make-forge-laminar-group
  forge-laminar-group?
  (name forge-laminar-group-name)
  (regex forge-laminar-group-regex))

(define (forge-laminar-activation config)
  (let* ((state-directory (forge-laminar-configuration-state-directory config))
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
                                           (string-append (forge-laminar-group-name group)
                                                          "="
                                                          (forge-laminar-group-regex group)))
                                         (forge-laminar-configuration-groups config))
                                    "\n"))
         #$groups-configuration)
        ;; Create jobs directory and populate with job scripts.
        (mkdir-p #$(dirname jobs-directory))
        (when (file-exists? #$jobs-directory)
          (delete-file #$jobs-directory))
        (symlink
         #$(file-union "laminar-jobs"
                       (append-map (lambda (job)
                                     (let ((name (forge-laminar-job-name job))
                                           (run (forge-laminar-job-run job))
                                           (after (forge-laminar-job-after job)))
                                       (cons (list (string-append name ".run")
                                                   (program-file name run))
                                             (if after
                                                 (list (list (string-append name ".after")
                                                             (program-file name after)))
                                                 (list)))))
                                   (forge-laminar-configuration-jobs config)))
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

(define forge-laminar-service-type
  (service-type
   (name 'forge-laminar)
   (description "Run forge-laminar.")
   (extensions (list (service-extension activation-service-type
                                        forge-laminar-activation)))
   (compose concatenate)
   (extend (lambda (config extended-values)
             (forge-laminar-configuration
              (inherit config)
              (jobs (append (forge-laminar-configuration-jobs config)
                            (filter forge-laminar-job? extended-values)))
              (groups (append (forge-laminar-configuration-groups config)
                              (filter forge-laminar-group? extended-values))))))
   (default-value (forge-laminar-configuration))))
