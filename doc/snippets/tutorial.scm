(use-modules (gnu)
             (gnu packages autotools)
             (gnu packages gawk)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages version-control)
             (gnu services ci)
             (forge forge)
             (forge laminar)
             (forge utils))

(define guile-json-tests
  (with-imported-modules '((guix build utils))
    (with-packages (list autoconf automake coreutils
                         gawk git-minimal gnu-make grep
                         guile-3.0 sed pkg-config)
      #~(begin
          (use-modules (guix build utils))
          (invoke "git" "clone" "/srv/git/guile-json" ".")
          (invoke "autoreconf" "--verbose" "--install" "--force")
          (invoke "./configure")
          (invoke "make")
          (invoke "make" "check")))))

(define guile-json-project
  (forge-project
   (name "guile-json")
   (user "vetri")
   (repository "/srv/git/guile-json")
   (description "JSON module for Guile")
   (ci-jobs (list (forge-laminar-job
                   (name "guile-json")
                   (run guile-json-tests))))))

(operating-system
  (host-name "tutorial")
  (timezone "UTC")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)))
  (file-systems %base-file-systems)
  (users (cons* (user-account
                 (name "vetri")
                 (group "users")
                 (home-directory "/home/vetri"))
                %base-user-accounts))
  (packages %base-packages)
  (services (cons* (service forge-service-type
                            (forge-configuration
                             (projects (list guile-json-project))))
                   (service laminar-service-type
                            (laminar-configuration
                             (bind-http "localhost:8080")))
                   %base-services)))
