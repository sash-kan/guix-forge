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
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (gnu packages ci)
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((gnu packages guile) #:select (guile-3.0 guile-zlib))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (gnu services mcron)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (forge laminar)
  #:use-module (forge webhook)
  #:export (forge-service-type
            forge-configuration
            forge-configuration-guix-daemon-uri
            forge-configuration-projects
            forge-project-configuration
            forge-project-configuration-user
            forge-project-configuration-repository
            forge-project-configuration-repository-branch
            forge-project-configuration-website-directory
            forge-project-configuration-ci-jobs
            forge-project-configuration-ci-jobs-trigger
            forge-derivation-job
            forge-derivation-job-name
            forge-derivation-job-run
            forge-derivation-job-after))

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
           (default '()))
  (ci-jobs-trigger forge-project-configuration-ci-jobs-trigger ; one of 'post-receive-hook, 'cron, 'webhook
                   (default (cond
                             ;; 'post-receive-hook for local repositories
                             ((string-prefix? "/" (forge-project-configuration-repository this-record))
                              'post-receive-hook)
                             ;; 'cron for remote repositories
                             (else 'cron)))
                   (thunked)))

(define-record-type* <forge-derivation-job>
  forge-derivation-job make-forge-derivation-job
  forge-derivation-job?
  (name forge-derivation-job-name)
  (run forge-derivation-job-run)
  (after forge-derivation-job-after
         (default #f)))

(define-record-type* <forge-configuration>
  forge-configuration make-forge-configuration
  forge-configuration?
  (guix-daemon-uri forge-configuration-guix-daemon-uri
                   (default (%daemon-socket-uri)))
  (projects forge-configuration-projects
            (default '())))

(define (forge-project-configuration-laminar-jobs project config)
  "Return CI jobs of PROJECT as a list of <forge-laminar-job>
objects. PROJECT is a <forge-project-configuration> object that is
part of CONFIG, a <forge-configuration> object. If PROJECT has jobs
described by <forge-derivation-job> objects, transform them to
<forge-laminar-job> objects."
  (map (lambda (job)
         (if (forge-derivation-job? job)
             (forge-laminar-job
              (name (forge-derivation-job-name job))
              (run (gexp-producer->job-script
                    (forge-project-configuration-repository project)
                    (forge-project-configuration-repository-branch project)
                    (forge-derivation-job-run job)
                    #:git-checkout-name (string-append (forge-project-configuration-name project)
                                                       "-checkout")
                    #:derivation-name (string-append (forge-derivation-job-name job)
                                                     "-derivation")
                    #:guix-daemon-uri (forge-configuration-guix-daemon-uri config)))
              (after (forge-derivation-job-after job)))
             job))
       (forge-project-configuration-ci-jobs project)))

(define* (ci-jobs-trigger-gexp ci-jobs #:key reason)
  "Return a G-expression that triggers CI-JOBS. CI-JOBS is a list of
<forge-laminar-job> objects."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        ;; TODO: Only trigger on updates to the main/master branch.
        (display "Triggering continuous integration jobs..." (current-error-port))
        (newline (current-error-port))
        (when reason
          (setenv "LAMINAR_REASON" reason))
        (apply invoke
               #$(file-append laminar "/bin/laminarc")
               "queue" '#$(map forge-laminar-job-name ci-jobs)))))

(define (forge-activation config)
  (let ((projects
         (map (lambda (project)
                (list (forge-project-configuration-user project)
                      (forge-project-configuration-repository project)
                      (forge-project-configuration-description project)
                      (forge-project-configuration-website-directory project)
                      (program-file
                       (forge-project-configuration-name project)
                       (ci-jobs-trigger-gexp
                        (forge-project-configuration-laminar-jobs project config)
                        #:reason "post-receive hook"))
                      (forge-project-configuration-ci-jobs-trigger project)))
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
                    ((username repository description website-directory ci-jobs-trigger ci-jobs-trigger-type)
                     ;; For local repositories only
                     (when (string-prefix? "/" repository)
                       ;; Set description.
                       (when description
                         (call-with-output-file (string-append repository "/description")
                           (cut put-string <> description)))
                       ;; Set ownership of repository files.
                       (for-each (lambda (file)
                                   (let ((user (getpw username)))
                                     (chown file (passwd:uid user) (passwd:gid user))))
                                 (append (find-regular-files repository))))
                     ;; Install post receive hook.
                     (when (eq? ci-jobs-trigger-type 'post-receive-hook)
                       (let ((hook-link (string-append repository "/hooks/post-receive")))
                         (when (file-exists? hook-link)
                           (delete-file hook-link))
                         (symlink ci-jobs-trigger hook-link)))
                     ;; Set ownership of website directory.
                     (when website-directory
                       (let ((user (getpw "laminar")))
                         (chown website-directory (passwd:uid user) (passwd:gid user))))))
                  '#$projects))))

(define (import-module? name)
  "Return #t if module NAME may be imported.  Else, return #f."
  (match name
    (('forge _ ...) #t)
    (name (guix-module-name? name))))

(define* (gexp-producer->job-script git-repository git-branch gexp-producer
                                    #:key git-checkout-name derivation-name guix-daemon-uri)
  "Return a G-expression describing a laminar job script.
GEXP-PRODUCER is a G-expression that expands to a lambda function.
The lambda function takes one argument---the latest git checkout of
GIT-REPOSITORY at GIT-BRANCH---and returns a G-expression describing a
derivation to run."
  (with-imported-modules (source-module-closure '((forge build git)
                                                  (guix gexp)
                                                  (guix profiles))
                                                #:select? import-module?)
    (with-extensions (list guile-gcrypt guile-zlib)
      #~(begin
          (use-modules (forge build git)
                       (guix derivations)
                       (guix gexp)
                       (guix monads)
                       (guix store)
                       (rnrs exceptions))

          (parameterize ((%daemon-socket-uri #$guix-daemon-uri))
            (with-store store
              (guard (condition ((store-protocol-error? condition)
                                 (exit #f)))
                (format (current-error-port)
                        "Built ~a successfully~%"
                        (run-with-store store
                          (mlet* %store-monad ((git-checkout (latest-git-checkout #$git-checkout-name
                                                                                  #$git-repository
                                                                                  #$git-branch
                                                                                  #:git-command #$(file-append git-minimal "/bin/git")))
                                               (tests-drv (gexp->derivation #$derivation-name
                                                            (#$gexp-producer git-checkout)
                                                            #:guile-for-build (read-derivation-from-file
                                                                               #$(raw-derivation-file
                                                                                  (with-store store
                                                                                    (package-derivation store guile-3.0))))
                                                            #:substitutable? #f)))
                            (mbegin %store-monad
                              (built-derivations (list tests-drv))
                              (return (derivation->output-path tests-drv)))))))))))))

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
                                           (append-map (cut forge-project-configuration-laminar-jobs <> config)
                                                       (forge-configuration-projects config))
                                           ;; groups
                                           (filter-map (lambda (project)
                                                         (match (forge-project-configuration-laminar-jobs project config)
                                                           (() #f)
                                                           ((job) #f)
                                                           (jobs
                                                            (forge-laminar-group
                                                             (name (forge-project-configuration-name project))
                                                             (regex (string-append "^(?:"
                                                                                   (string-join (map forge-laminar-job-name jobs)
                                                                                                "|")
                                                                                   ")$"))))))
                                                       (forge-configuration-projects config)))))
                     ;; Set up cron jobs to trigger CI jobs for remote
                     ;; repositories.
                     ;; TODO: Run CI job only if there are new commits
                     ;; in the remote repository.
                     (service-extension mcron-service-type
                                        (lambda (config)
                                          (filter-map (lambda (project)
                                                        (and (eq? (forge-project-configuration-ci-jobs-trigger project)
                                                                  'cron)
                                                             #~(job '(next-day)
                                                                    #$(program-file
                                                                       (forge-project-configuration-name project)
                                                                       (ci-jobs-trigger-gexp
                                                                        (forge-project-configuration-laminar-jobs project config)
                                                                        #:reason "Cron job"))
                                                                    #:user "laminar")))
                                                      (forge-configuration-projects config))))
                     (service-extension webhook-service-type
                                        (lambda (config)
                                          (filter-map (lambda (project)
                                                        (and (eq? (forge-project-configuration-ci-jobs-trigger project)
                                                                  'webhook)
                                                             (webhook-hook
                                                              (id (forge-project-configuration-name project))
                                                              (run (ci-jobs-trigger-gexp
                                                                    (forge-project-configuration-laminar-jobs project config)
                                                                    #:reason "Webhook")))))
                                                      (forge-configuration-projects config))))))
   (default-value (forge-configuration))))
