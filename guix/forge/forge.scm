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
  #:use-module ((gnu packages certs) #:select (nss-certs))
  #:use-module ((gnu packages ci) #:select (laminar))
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((gnu packages guile) #:select (guile-3.0 guile-zlib))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (gnu services mcron)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (forge laminar)
  #:use-module (forge utils)
  #:use-module (forge webhook)
  #:export (forge-service-type
            forge-configuration
            forge-configuration?
            forge-configuration-projects
            forge-project
            forge-project?
            this-forge-project
            forge-project-name
            forge-project-user
            forge-project-repository
            forge-project-repository-branch
            forge-project-website-directory
            forge-project-ci-jobs
            forge-project-ci-jobs-trigger
            derivation-job-gexp))

(define-record-type* <forge-project>
  forge-project make-forge-project
  forge-project?
  this-forge-project
  (name forge-project-name)
  ;; The user field is optional because the repository may be remote
  ;; and not need to be owned by any user.
  (user forge-project-user
        (default #f))
  (repository forge-project-repository)
  (repository-branch forge-project-repository-branch
                     (default "main"))
  (description forge-project-description
               (default #f))
  (website-directory forge-project-website-directory
                     (default #f))
  (ci-jobs forge-project-ci-jobs
           (default '()) (thunked))
  (ci-jobs-trigger forge-project-ci-jobs-trigger ; one of 'post-receive-hook, 'cron, 'webhook
                   (default (cond
                             ;; 'post-receive-hook for local repositories
                             ((string-prefix? "/" (forge-project-repository this-forge-project))
                              'post-receive-hook)
                             ;; 'cron for remote repositories
                             (else 'cron)))
                   (thunked)))

(define-record-type* <forge-configuration>
  forge-configuration make-forge-configuration
  forge-configuration?
  (projects forge-configuration-projects
            (default '())))

(define* (ci-jobs-trigger-gexp ci-jobs #:key reason)
  "Return a G-expression that triggers CI-JOBS. CI-JOBS is a list of
<forge-laminar-job> objects."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match))
        ;; TODO: Only trigger on updates to the main/master branch.

        ;; Trigger jobs if there are jobs that need to be
        ;; triggered.
        ;;
        ;; Even if there are none, we still need to manage the
        ;; post-receive-hook to ensure that it does not go
        ;; stale. Suppose that in one generation the user configures
        ;; CI jobs, but removes them in the next generation. If we did
        ;; not write to the post-receive-hook in the second
        ;; generation, it would still retain its previous contents and
        ;; trigger the jobs from the first generation.
        (match '#$(filter-map (lambda (job)
                                (and (forge-laminar-job-trigger? job)
                                     (forge-laminar-job-name job)))
                              ci-jobs)
          (() #t)
          (job-names
           (display "Triggering continuous integration jobs..." (current-error-port))
           (newline (current-error-port))
           (when #$reason
             (setenv "LAMINAR_REASON" #$reason))
           (apply invoke
                  #$(file-append laminar "/bin/laminarc")
                  "queue" job-names))))))

(define (forge-activation config)
  (let ((projects
         (map (lambda (project)
                (list (forge-project-user project)
                      (forge-project-repository project)
                      (forge-project-description project)
                      (forge-project-website-directory project)
                      (program-file
                       (string-append (forge-project-name project)
                                      "-post-receive-hook")
                       (ci-jobs-trigger-gexp
                        (forge-project-ci-jobs project)
                        #:reason "post-receive hook"))
                      (forge-project-ci-jobs-trigger project)))
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
                         (chown (dirname website-directory)
                                (passwd:uid user) (passwd:gid user))))))
                  '#$projects))))

(define (import-module? name)
  "Return #t if module NAME may be imported.  Else, return #f."
  (match name
    (('forge _ ...) #t)
    (name (guix-module-name? name))))

(define* (derivation-job-gexp project job gexp-producer
                              #:key (guix-daemon-uri (%daemon-socket-uri)) deep-clone?)
  "Return a G-expression that builds another G-expression as a
derivation and returns its output path. GEXP-PRODUCER is a
G-expression that expands to a lambda function. The lambda function
takes one argument---the latest git checkout of PROJECT, a
<forge-project> object---and returns a G-expression describing a
derivation to run. JOB is a <forge-laminar-job> object representing
the job that this derivation will be part of.

GUIX_DAEMON_URI is a file name or URI designating the Guix daemon
endpoint.

If DEEP-CLONE? is #t, the git checkout is a deep clone of the
repository that includes the .git directory. Else, it is a shallow
clone and does not include the .git directory."
  (with-imported-modules (source-module-closure '((forge build git)
                                                  (guix gexp)
                                                  (guix profiles))
                                                #:select? import-module?)
    (with-extensions (list guile-gcrypt guile-zlib)
      (with-packages (list git-minimal nss-certs)
        #~(begin
            ;; We pull out macros using module-ref and functions using
            ;; @@ instead of using use-modules because this gexp might
            ;; be substituted into other gexps and use-modules only
            ;; works at the top-level.
            (let-syntax ((guard (macro-transformer
                                 (module-ref (resolve-module '(rnrs exceptions))
                                             'guard)))
                         (mbegin (macro-transformer
                                  (module-ref (resolve-module '(guix monads))
                                              'mbegin)))
                         (mlet* (macro-transformer
                                 (module-ref (resolve-module '(guix monads))
                                             'mlet*)))
                         (with-store (macro-transformer
                                      (module-ref (resolve-module '(guix store))
                                                  'with-store)))
                         (return (identifier-syntax ((@@ (guix store) %store-monad)
                                                     %return))))
              (let* ((latest-git-checkout (@@ (forge build git) latest-git-checkout))
                     (built-derivations (@@ (guix derivations) built-derivations))
                     (derivation->output-path (@@ (guix derivations) derivation->output-path))
                     (read-derivation-from-file (@@ (guix derivations) read-derivation-from-file))
                     (gexp->derivation (@@ (guix gexp) gexp->derivation))
                     (%daemon-socket-uri (@@ (guix store) %daemon-socket-uri))
                     (%store-monad (@@ (guix store) %store-monad))
                     (store-protocol-error? (@@ (guix store) store-protocol-error?))
                     (run-with-store (@@ (guix store) run-with-store))
                     (derivation-output
                      (parameterize ((%daemon-socket-uri #$guix-daemon-uri))
                        (with-store store
                          (guard (condition ((store-protocol-error? condition)
                                             (exit #f)))
                            (run-with-store store
                              (mlet* %store-monad ((git-checkout (latest-git-checkout
                                                                  #$(string-append (forge-project-name project)
                                                                                   "-checkout")
                                                                  #$(forge-project-repository project)
                                                                  #:deep-clone? #$deep-clone?
                                                                  #:show-commit? #t))
                                                   (drv (gexp->derivation #$(string-append
                                                                             (forge-laminar-job-name job)
                                                                             "-derivation")
                                                          (#$gexp-producer git-checkout)
                                                          #:guile-for-build (read-derivation-from-file
                                                                             #$(raw-derivation-file
                                                                                (with-store store
                                                                                  (package-derivation store guile-3.0))))
                                                          #:substitutable? #f)))
                                (mbegin %store-monad
                                  (built-derivations (list drv))
                                  (return (derivation->output-path drv))))))))))
                (format (current-error-port) "Built ~a successfully~%" derivation-output)
                derivation-output)))))))

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
                                           (append-map forge-project-ci-jobs
                                                       (forge-configuration-projects config))
                                           ;; group jobs by project
                                           (filter-map (lambda (project)
                                                         (match (forge-project-ci-jobs project)
                                                           (() #f)
                                                           ((job) #f)
                                                           (jobs
                                                            (forge-laminar-group
                                                             (name (forge-project-name project))
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
                                                        (and (eq? (forge-project-ci-jobs-trigger project)
                                                                  'cron)
                                                             (any forge-laminar-job-trigger?
                                                                  (forge-project-ci-jobs project))
                                                             #~(job '(next-day)
                                                                    #$(program-file
                                                                       (string-append (forge-project-name project)
                                                                                      "-cron-job")
                                                                       (ci-jobs-trigger-gexp
                                                                        (forge-project-ci-jobs project)
                                                                        #:reason "Cron job"))
                                                                    #:user "laminar")))
                                                      (forge-configuration-projects config))))
                     (service-extension webhook-service-type
                                        (lambda (config)
                                          (filter-map (lambda (project)
                                                        (and (eq? (forge-project-ci-jobs-trigger project)
                                                                  'webhook)
                                                             (any forge-laminar-job-trigger?
                                                                  (forge-project-ci-jobs project))
                                                             (webhook-hook
                                                              (id (forge-project-name project))
                                                              (run (ci-jobs-trigger-gexp
                                                                    (forge-project-ci-jobs project)
                                                                    #:reason "Webhook")))))
                                                      (forge-configuration-projects config))))))
   (compose concatenate)
   (extend (lambda (config projects)
             (forge-configuration
              (inherit config)
              (projects (append (forge-configuration-projects config)
                                projects)))))
   (default-value (forge-configuration))))
