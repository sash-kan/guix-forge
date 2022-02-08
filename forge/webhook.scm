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

(define-module (forge webhook)
  #:use-module (srfi srfi-1)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module ((gnu packages guile) #:select (guile-json-4))
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (webhook-service-type
            webhook-configuration
            webhook-configuration?
            webhook-configuration-package
            webhook-configuration-port
            webhook-configuration-log-directory
            webhook-configuration-hooks
            webhook-hook
            webhook-hook?
            webhook-hook-id
            webhook-hook-run))

(define webhook
  (package
    (name "webhook")
    (version "2.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adnanh/webhook")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n03xkgwpzans0cymmzb0iiks8mi2c76xxdak780dk0jbv6qgp5i"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/adnanh/webhook"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/github.com/adnanh/webhook/webhook_test.go"
               (("/bin/echo")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/echo"))))))))
    (home-page "https://github.com/adnanh/webhook")
    (synopsis "Lightweight incoming webhook server")
    (description "webhook is a lightweight configurable tool written
in Go, that allows you to easily create HTTP endpoints (hooks) on your
server, which you can use to execute configured commands. You can also
pass data from the HTTP request (such as headers, payload or query
variables) to your commands. webhook also allows you to specify rules
which have to be satisfied in order for the hook to be triggered.

For example, if you're using Github or Bitbucket, you can use webhook
to set up a hook that runs a redeploy script for your project on your
staging server, whenever you push changes to the master branch of your
project.

If you use Mattermost or Slack, you can set up an \"Outgoing webhook
integration\" or \"Slash command\" to run various commands on your
server, which can then report back directly to you or your channels
using the \"Incoming webhook integrations\", or the appropriate
response body.

webhook aims to do nothing more than it should do, and that is:

@itemize
@item receive the request,
@item parse the headers, payload and query variables,
@item check if the specified rules for the hook are satisfied,
@item and finally, pass the specified arguments to the specified
command via command line arguments or via environment variables.
@end itemize

Everything else is the responsibility of the command's author.")
    (license license:expat)))

(define-record-type* <webhook-configuration>
  webhook-configuration make-webhook-configuration
  webhook-configuration?
  (package webhook-configuration-package
           (default webhook))
  (ip webhook-configuration-ip
      (default "127.0.0.1"))
  (port webhook-configuration-port
        (default 9000))
  (log-directory webhook-configuration-log-directory
                 (default "/var/log/webhook"))
  (hooks webhook-configuration-hooks
         (default '())))

(define-record-type* <webhook-hook>
  webhook-hook make-webhook-hook
  webhook-hook?
  (id webhook-hook-id)
  (run webhook-hook-run))

(define (webhook-activation config)
  ;; Create log directory.
  #~(mkdir-p #$(webhook-configuration-log-directory config)))

(define (hooks-json-gexp config)
  (with-extensions (list guile-json-4)
    #~(begin
        (use-modules (srfi srfi-26)
                     (json))
        
        (call-with-output-file #$output
          (cut scm->json
               ;; We convert from list to vector on the build-side
               ;; because a vector cannot be lowered correctly into a
               ;; G-expression.
               (list->vector
                ;; We build a true dotted association list in this
                ;; roundabout way because a true dotted association
                ;; list cannot be lowered correctly into a
                ;; G-expression.
                (map (cut map (cut apply cons <>) <>)
                     '#$(map (lambda (hook)
                               `(("id" ,(webhook-hook-id hook))
                                 ("execute-command" ,(program-file (webhook-hook-id hook)
                                                                   (webhook-hook-run hook)))))
                             (webhook-configuration-hooks config))))
               <>)))))

(define webhook-shepherd-service
  (lambda (config)
    (shepherd-service
     (documentation "Run webhook.")
     (provision '(webhook))
     (requirement '(networking))
     (modules '((gnu build shepherd)
                (gnu system file-systems)))
     (start (with-imported-modules (source-module-closure
                                    '((gnu build shepherd)
                                      (gnu system file-systems)))
              #~(make-forkexec-constructor/container
                 (list #$(file-append (webhook-configuration-package config)
                                      "/bin/webhook")
                       "-hooks" #$(computed-file "hooks.json"
                                                 (hooks-json-gexp config))
                       "-ip" #$(webhook-configuration-ip config)
                       "-port" #$(number->string (webhook-configuration-port config))
                       "-logfile" #$(string-append (webhook-configuration-log-directory config)
                                                   "/webhook.log"))
                 #:mappings (list (file-system-mapping
                                   (source #$(webhook-configuration-log-directory config))
                                   (target source)
                                   (writable? #t)))
                 #:log-file "/var/log/webhook.log")))
     (stop #~(make-kill-destructor)))))

(define webhook-service-type
  (service-type
   (name 'webhook)
   (description "Run webhook.")
   (extensions (list (service-extension activation-service-type
                                        webhook-activation)
                     (service-extension shepherd-root-service-type
                                        (compose list webhook-shepherd-service))))
   (compose concatenate)
   (extend (lambda (config hook-extensions)
             (webhook-configuration
              (inherit config)
              (hooks (append (webhook-configuration-hooks config)
                             hook-extensions)))))))
