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

(define-module (forge forge)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu packages ci)
  #:use-module (gnu services mcron)
  #:use-module (guix records)
  #:use-module (forge laminar)
  #:export (forge-service-type
            forge-configuration
            forge-configuration-projects
            forge-project-configuration
            forge-project-configuration-user
            forge-project-configuration-repository
            forge-project-configuration-repository-branch
            forge-project-configuration-website-directory
            forge-project-configuration-ci-jobs))

(define-record-type* <forge-project-configuration>
  forge-project-configuration make-forge-project-configuration
  forge-project-configuration?
  (name forge-project-configuration-name)
  ;; The user field is optional because the repository may be remote
  ;; and not need to be owned by any user.
  (user forge-project-configuration-user
        (default #f))
  (repository forge-project-configuration-repository)
  (repository-branch forge-project-configuration-repository-branch
                     (default "main"))
  (description forge-project-configuration-description
               (default #f))
  (website-directory forge-project-configuration-website-directory
                     (default #f))
  (ci-jobs forge-project-configuration-ci-jobs
           (default '())))

(define-record-type* <forge-configuration>
  forge-configuration make-forge-configuration
  forge-configuration?
  (projects forge-configuration-projects
            (default '())))

(define (post-receive-hook project-name ci-jobs)
  "Return a git post-receive-hook that triggers CI-JOBS."
  (program-file (string-append project-name "-post-receive-hook")
                (with-imported-modules '((guix build utils))
                  #~(begin
                      (use-modules (guix build utils))
                      ;; TODO: Only trigger on updates to the main/master branch.
                      (display "Triggering continuous integration jobs..." (current-error-port))
                      (newline (current-error-port))
                      (apply invoke
                             #$(file-append laminar "/bin/laminarc")
                             "queue" '#$ci-jobs)))))

(define (forge-activation config)
  (let ((projects
         (map (lambda (project)
                (list (forge-project-configuration-user project)
                      (forge-project-configuration-repository project)
                      (forge-project-configuration-description project)
                      (forge-project-configuration-website-directory project)
                      (post-receive-hook
                       (forge-project-configuration-name project)
                       (map forge-laminar-job-name
                            (forge-project-configuration-ci-jobs project)))))
              (forge-configuration-projects config))))
    #~(begin
        (use-modules (rnrs io ports)
                     (srfi srfi-26)
                     (ice-9 match))
        
        (define (find-regular-files dir)
          (find-files dir (lambda (file stat)
                            (memq (stat:type stat)
                                  '(regular directory)))
                      #:directories? #t))
        
        (for-each (match-lambda
                    ((username repository description website-directory post-receive-hook)
                     ;; For local repositories only
                     (when (string-prefix? "/" repository)
                       ;; Set description.
                       (when description
                         (call-with-output-file (string-append repository "/description")
                           (cut put-string <> description)))
                       ;; Install post receive hook.
                       (let ((hook-link (string-append repository "/hooks/post-receive")))
                         (when (file-exists? hook-link)
                           (delete-file hook-link))
                         (symlink post-receive-hook hook-link))
                       ;; Set ownership of repository files.
                       (for-each (lambda (file)
                                   (let ((user (getpw username)))
                                     (chown file (passwd:uid user) (passwd:gid user))))
                                 (append (find-regular-files repository))))
                     ;; Set ownership of website directory.
                     (when website-directory
                       (let ((user (getpw "laminar")))
                         (chown website-directory (passwd:uid user) (passwd:gid user))))))
                  '#$projects))))

(define forge-service-type
  (service-type
   (name 'forge)
   (description "Run guix-forge.")
   (extensions (list (service-extension activation-service-type
                                        forge-activation)
                     (service-extension forge-laminar-service-type
                                        (lambda (config)
                                          (append
                                           ;; jobs
                                           (append-map forge-project-configuration-ci-jobs
                                                       (forge-configuration-projects config))
                                           ;; groups
                                           (filter-map (lambda (project)
                                                         (match (forge-project-configuration-ci-jobs project)
                                                           (() #f)
                                                           ((job) #f)
                                                           (jobs
                                                            (forge-laminar-group
                                                             (name (forge-project-configuration-name project))
                                                             (regex (string-join (map forge-laminar-job-name jobs)
                                                                                 "|"))))))
                                                       (forge-configuration-projects config)))))
                     ;; Set up cron jobs to trigger CI jobs for remote
                     ;; repositories.
                     ;; TODO: Run CI job only if there are new commits
                     ;; in the remote repository.
                     (service-extension mcron-service-type
                                        (lambda (config)
                                          (filter-map (lambda (project)
                                                        (and (not (string-prefix?
                                                                   "/" (forge-project-configuration-repository project)))
                                                             #~(job '(next-day)
                                                                    #$(post-receive-hook
                                                                       (forge-project-configuration-name project)
                                                                       (map forge-laminar-job-name
                                                                            (forge-project-configuration-ci-jobs project)))
                                                                    #:user "laminar")))
                                                      (forge-configuration-projects config))))))
   (default-value (forge-configuration))))
