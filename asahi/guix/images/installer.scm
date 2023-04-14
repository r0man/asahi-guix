(define-module (asahi guix images installer)
  #:use-module (asahi guix system base)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages certs)
  #:use-module (guix platforms arm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (asahi-installer-image-type
            asahi-installer-os
            asahi-installer-raw-image))

(define asahi-installer-os
  (operating-system
    (inherit (asahi-operating-system))
    (file-systems
     (cons (file-system
             (device (file-system-label "my-root"))
             (mount-point "/")
             (type "ext4"))
           %base-file-systems))))

(define asahi-installer-image-type
  (image-type
   (name 'asahi-installer-raw)
   (constructor (lambda (os)
                  (image
                   (inherit (raw-with-offset-disk-image))
                   (operating-system os)
                   (platform aarch64-linux))))))

(define asahi-installer-raw-image
  (image
   (inherit
    (os+platform->image asahi-installer-os aarch64-linux
                        #:type asahi-installer-image-type))
   (name 'asahi-installer-raw-image)))

asahi-installer-raw-image
