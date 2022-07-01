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

(define-module (forge tissue)
  #:use-module (srfi srfi-1)
  #:use-module (forge socket)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module ((gnu packages autotools) #:select (autoconf automake))
  #:use-module ((gnu packages gettext) #:select (gnu-gettext))
  #:use-module ((gnu packages guile) #:select (guile-3.0 guile-git))
  #:use-module ((gnu packages guile-xyz) #:select (guile-filesystem guile-xapian))
  #:use-module ((gnu packages skribilo) #:select (skribilo) #:prefix guix:)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (tissue-service-type
            <tissue-configuration>
            tissue-configuration
            tissue-configuration?
            tissue-configuration-package
            tissue-configuration-socket
            tissue-configuration-state-directory
            tissue-configuration-hosts
            <tissue-host>
            tissue-host
            tissue-host?
            tissue-host-name
            tissue-host-user
            tissue-host-upstream-repository))

;; tissue requires an unreleased version of skribilo for its gemtext
;; reader.
(define-public skribilo-latest
  (let ((commit "621eb1945aec8f26f5aee4bdf896f2434e145182")
        (revision "1"))
    (package
      (inherit guix:skribilo)
      (name "skribilo")
      (version (git-version "0.9.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.systemreboot.net/skribilo")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16rdcvszl9x183y32hjdwns0lkrvkmwd2fsshymspb12k4cxj6i4"))))
      (native-inputs
       (modify-inputs (package-native-inputs guix:skribilo)
         (prepend autoconf)
         (prepend automake)
         (prepend gnu-gettext))))))

;; TODO: Contribute tissue package upstream to Guix after its first
;; release.
(define-public tissue
  (let ((commit "743f2eb0b2f107c8089bbd925dbd5052ff9fa9f9")
        (revision "0"))
    (package
      (name "tissue")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.systemreboot.net/tissue")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fcxwclhpy0p17m6qingrzmaq0py1ys92v4p2ygyn69hmq9i048f"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "prefix=" #$output))
             #:modules `(((guix build guile-build-system)
                          #:select (target-guile-effective-version))
                         ,@%gnu-build-system-modules)
             #:phases
             (with-imported-modules '((guix build guile-build-system))
               #~(modify-phases %standard-phases
                   (replace 'patch-source-shebangs
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "bin/tissue"
                         (("^exec guile")
                          (string-append "exec " (search-input-file inputs "/bin/guile"))))))
                   (delete 'configure)
                   (add-after 'install 'wrap
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out"))
                             (effective-version (target-guile-effective-version)))
                         (wrap-program (string-append out "/bin/tissue")
                           `("GUILE_LOAD_PATH" prefix
                             (,(string-append out "/share/guile/site/" effective-version)
                              ,(getenv "GUILE_LOAD_PATH")))
                           `("GUILE_LOAD_COMPILED_PATH" prefix
                             (,(string-append out "/lib/guile/" effective-version "/site-ccache")
                              ,(getenv "GUILE_LOAD_COMPILED_PATH")))))))))))
      (inputs (list guile-3.0 guile-filesystem guile-git guile-xapian))
      (propagated-inputs
       (list skribilo-latest))
      (home-page "https://tissue.systemreboot.net")
      (synopsis "Text based project information management system")
      (description
       "tissue is an issue tracker and project information management system
built on plain text files and git.  It features a static site
generator to build a project website and a powerful search interface
to search through project issues and documentation.  The search
interface is built on the Xapian search engine library, and is
available both as a command-line program and as a web server.")
      (license license:gpl3+))))

(define-record-type* <tissue-configuration>
  tissue-configuration make-tissue-configuration
  tissue-configuration?
  (package tissue-configuration-package
           (default tissue))
  (socket tissue-configuration-socket
          (default (forge-unix-socket
                    (path "/var/run/tissue/socket"))))
  (state-directory tissue-configuration-state-directory
                   (default "/var/lib/tissue"))
  (hosts tissue-configuration-hosts
         (default '())))

(define-record-type* <tissue-host>
  tissue-host make-tissue-host
  tissue-host?
  (name tissue-host-name)
  (user tissue-host-user
        (default "tissue"))
  (upstream-repository tissue-host-upstream-repository))

(define %tissue-accounts
  (list (user-account
         (name "tissue")
         (group "tissue")
         (system? #t)
         (comment "tissue user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "tissue")
         (system? #t))))

(define (tissue-conf-gexp config)
  (match-record config <tissue-configuration>
    (socket state-directory hosts)
    #~(begin
        (use-modules (ice-9 pretty-print))

        (call-with-output-file #$output
          (lambda (port)
            (pretty-print
             '((listen . #$(cond
                            ((forge-ip-socket? socket)
                             (match-record socket <forge-ip-socket>
                               (ip port)
                               (string-append (if (ipv4-address? ip)
                                                  ip
                                                  (string-append "[" ip "]"))
                                              ":"
                                              (number->string port))))
                            ((forge-unix-socket? socket)
                             (string-append "unix:" (forge-unix-socket-path socket)))
                            (else (raise (condition
                                          (make-message-condition
                                           "Socket must be a <forge-ip-socket> or <forge-unix-socket> record")
                                          (make-irritants-condition socket))))))
               (state-directory . #$state-directory)
               (hosts . #$(map (lambda (host)
                                 (match-record host <tissue-host>
                                   (name upstream-repository)
                                   `(,name (upstream-repository . ,upstream-repository))))
                               hosts)))
             port))))))

;; We cannot just pass the configuration file on the command-line
;; because we need future `tissue pull' invocations to find it. These
;; `tissue pull' invocations are beyond the scope of this service, and
;; will need to find the configuration at a standard location.
(define (tissue-etc-files config)
  `(("tissue.conf" ,(computed-file "tissue.conf"
                                   (tissue-conf-gexp config)))))

(define (tissue-activation config)
  (match-record config <tissue-configuration>
    (socket state-directory hosts)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (let ((user (getpw "tissue")))
            ;; Create socket directory.
            (when #$(forge-unix-socket? socket)
              (let ((socket-directory #$(dirname (forge-unix-socket-path socket))))
                (mkdir-p socket-directory)
                (chown socket-directory (passwd:uid user) (passwd:gid user))))
            ;; Create state directory.
            (mkdir-p #$state-directory)
            (chown #$state-directory (passwd:uid user) (passwd:gid user)))
          ;; Create host directories if they don't exist, and set
          ;; permissions.
          (for-each (match-lambda
                      ((hostname username)
                       (let ((host-directory (string-append #$state-directory "/" hostname))
                             (user (getpw username)))
                         (mkdir-p host-directory)
                         (for-each (lambda (file)
                                     (chown file (passwd:uid user) (passwd:gid user)))
                                   (find-files host-directory #:directories? #t)))))
                    '#$(map (lambda (host)
                              (match-record host <tissue-host>
                                (name user)
                                (list name user)))
                            hosts))))))

(define (tissue-shepherd-service config)
  (match-record config <tissue-configuration>
    (socket state-directory)
    (shepherd-service
     (documentation "Run tissue web server.")
     (provision '(tissue))
     (requirement '(networking))
     (modules '((gnu build shepherd)
                (gnu system file-systems)))
     (start
      (with-imported-modules (source-module-closure '((gnu build shepherd)
                                                      (gnu system file-systems)))
        #~(make-forkexec-constructor/container
           (list #$(file-append (tissue-configuration-package config)
                                "/bin/tissue")
                 "run-web"
                 (string-append "--config=" #$(computed-file "tissue.conf"
                                                             (tissue-conf-gexp config))))
           #:user "tissue"
           #:group "tissue"
           #:mappings (append (list (file-system-mapping
                                     (source #$state-directory)
                                     (target source))
                                    (file-system-mapping
                                     (source "/var/log/tissue.log")
                                     (target source)
                                     (writable? #t)))
                              (if #$(forge-unix-socket? socket)
                                  (list (file-system-mapping
                                         (source #$(dirname (forge-unix-socket-path socket)))
                                         (target source)
                                         (writable? #t)))
                                  (list)))
           #:log-file "/var/log/tissue.log")))
     (stop #~(make-kill-destructor)))))

(define tissue-service-type
  (service-type
   (name 'tissue)
   (description "Run tissue web server.")
   (extensions
    (list (service-extension account-service-type
                             (const %tissue-accounts))
          (service-extension etc-service-type
                             tissue-etc-files)
          (service-extension activation-service-type
                             tissue-activation)
          (service-extension shepherd-root-service-type
                             (compose list tissue-shepherd-service))))
   (compose concatenate)
   (extend (lambda (config host-extensions)
             (tissue-configuration
              (inherit config)
              (hosts (append (tissue-configuration-hosts config)
                             host-extensions)))))
   (default-value (tissue-configuration))))
