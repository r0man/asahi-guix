(define-module (asahi installer)
  #:use-module (asahi firmware)
  #:use-module (asahi packages)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu system install)
  #:use-module (gnu system)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix gexp)
  #:use-module (guix)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define modules
  '(;; Asahi
    ;; For NVMe & SMC
    ;; "apple-mailbox"
    ;; For NVMe
    "nvme-apple"
    ;; For USB and HID
    "pinctrl-apple-gpio"
    ;; SMC core
    ;; "macsmc" "macsmc-rtkit"
    ;; For USB
    "apple-dart"
    "dwc3"
    "dwc3-of-simple"
    ;; "gpio_macsmc"
    "i2c-apple"
    "nvmem-apple-efuses"
    "pcie-apple"
    "phy-apple-atc"
    "tps6598x"
    "xhci-pci"
    ;; For HID
    "spi-apple" "spi-hid-apple" "spi-hid-apple-of"
    ;; For RTC
    "rtc-macsmc" "simple-mfd-spmi"
    ;; "spmi-apple-controller"
    "nvmem_spmi_mfd"
    ;; For MTP HID
    "apple-dockchannel" "dockchannel-hid"
    ;; "apple-rtkit-helper"
    ;; Guix
    "usb-storage"
    "uas"
    "usbhid"
    "hid-apple"
    "dm-crypt"
    "serpent_generic"
    "wp512"
    "nls_iso8859-1"
    "virtio_pci"
    "virtio_balloon"
    "virtio_blk"
    "virtio_net"
    "virtio-rng"))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel asahi-linux)
    (firmware (list linux-firmware))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/dev/sda"))))

    (initrd (lambda (file-systems . rest)
              ;; Create a standard initrd but set up networking
              ;; with the parameters QEMU expects by default.
              (apply raw-initrd file-systems
                     #:pre-mount (with-imported-modules (source-module-closure
                                                         '((asahi firmware)))
                                   #~(begin
                                       (display "PRE MOUNT\n")
                                       (use-modules (asahi firmware))
                                       (mount-efi-system-partition "/run/.system-efi")))
                     #:linux-modules modules
                     ;; #:qemu-networking? #t
                     rest)))

    (initrd-modules modules)

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("modprobe.blacklist=radeon"
                        "net.ifnames=0"
                        ;; "quiet"
                        ))

    (services
     (cons*
      ;; Include the channel file so that it can be used during installation
      (simple-service 'channel-file etc-service-type
                      (list `("channels.scm" ,(local-file "channels.scm"))))
      (operating-system-user-services installation-os)))

    ;; Add some extra packages useful for the installation process
    (packages
     (append (list git curl emacs-next)
             (operating-system-packages installation-os)))))

installation-os-nonfree
