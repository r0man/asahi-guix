(define-module (asahi guix machine server)
  #:use-module (gnu)
  #:use-module (asahi guix system server)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:export (asahi-guix-server-machine))

(define asahi-guix-server-machine
  (machine
   (operating-system asahi-guix-server-system)
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
                   (host-name "asahi-guix")
                   (system "aarch64-linux")
                   (user "root")
                   (identity "~/.ssh/id_rsa")
                   (port 2222)))))

asahi-guix-server-machine
