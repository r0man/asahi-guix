(define-module (asahi guix installer)
  #:use-module (asahi guix packages)
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
  #:use-module (guix)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define m1-modules
  (list
   ;; For NVMe & SMC
   ;; "apple-mailbox"
   ;; For NVMe
   "nvme-apple"
   ;; For USB and HID
   "pinctrl-apple-gpio"
   ;; SMC core
   ;; "macsmc" "macsmc-rtkit"
   ;; For USB
   "i2c-apple" "tps6598x" "apple-dart" "dwc3" "dwc3-of-simple" "nvmem-apple-efuses"
   "phy-apple-atc" "xhci-pci" "pcie-apple"
   ;;"gpio_macsmc"
   ;; For HID
   "spi-apple" "spi-hid-apple" "spi-hid-apple-of"
   ;; For RTC
   "rtc-macsmc" "simple-mfd-spmi"
   ;; "spmi-apple-controller"
   "nvmem_spmi_mfd"
   ;; For MTP HID
   "apple-dockchannel" "dockchannel-hid"
   ;; "apple-rtkit-helper"
   ))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel asahi-linux)
    (firmware (list linux-firmware))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/dev/sda"))))
    (initrd-modules (append m1-modules
                            '("usb-storage"
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
                              "virtio-rng")))

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))

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
