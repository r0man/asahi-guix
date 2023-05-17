(define-module (asahi guix system desktop)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix system base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services sddm)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:export (asahi-desktop-operating-system))

(define* (asahi-desktop-operating-system . config)
  (let ((base (apply asahi-operating-system config)))
    (operating-system
      (inherit base)
      (services (append (list (service xfce-desktop-service-type)
                              (set-xorg-configuration
                               (xorg-configuration
                                (keyboard-layout (operating-system-keyboard-layout base)))
                               sddm-service-type))
                        %desktop-services)))))
