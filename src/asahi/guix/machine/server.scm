(define-module (asahi guix machine server)
  #:use-module (asahi guix system server)
  #:use-module (gnu machine ssh)
  #:use-module (gnu machine)
  #:use-module (gnu)
  #:export (asahi-guix-server-machine))

(define asahi-guix-server-machine
  (machine
   (operating-system asahi-guix-server-system)
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
                   (host-name "ci.asahi-guix.org")
                   (system "aarch64-linux")
                   (user "root")
                   (identity "/home/roman/.ssh/id_rsa")
                   (port 22)))))

(list asahi-guix-server-machine)
