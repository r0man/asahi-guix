(define-module (asahi guix initrd)
  #:use-module (asahi guix build firmware)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)
  #:use-module (gnu system linux-initrd)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (asahi-initrd-modules
            asahi-initrd-modules-edge))

(define initrd-modules
  (list "dm-crypt"
        "hid-apple"
        "nls_iso8859-1"
        "serpent_generic"
        "uas"
        "usb-storage"
        "usbhid"
        "wp512"
        "xhci-plat-hcd"))

(define asahi-initrd-modules
  (cons*
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
   "i2c-apple"
   "nvmem-apple-efuses"
   "pcie-apple"
   "phy-apple-atc"
   "tps6598x"
   "xhci-pci"
   ;; "gpio_macsmc"
   ;; For HID
   "spi-apple"
   "spi-hid-apple"
   "spi-hid-apple-of"
   ;; For RTC
   "rtc-macsmc"
   "simple-mfd-spmi"
   "spmi-apple-controller"
   "nvmem_spmi_mfd"
   ;; For MTP HID
   "apple-dockchannel"
   "dockchannel-hid"
   "apple-rtkit-helper"
   initrd-modules))

(define asahi-initrd-modules-edge
  (cons*
   ;; For NVMe & SMC
   ;; "apple-mailbox"
   ;; For NVMe
   "nvme-apple"
   ;; For USB and HID
   "pinctrl-apple-gpio"
   ;; SMC core
   "macsmc"
   "macsmc-rtkit"
   ;; For USB
   "apple-dart"
   "dwc3"
   "dwc3-of-simple"
   "gpio_macsmc"
   "i2c-apple"
   "nvmem-apple-efuses"
   "pcie-apple"
   "phy-apple-atc"
   "tps6598x"
   "xhci-pci"
   ;; For HID
   "spi-apple"
   "spi-hid-apple"
   "spi-hid-apple-of"
   ;; For RTC
   "nvmem_spmi_mfd"
   "rtc-macsmc"
   "simple-mfd-spmi"
   "spmi-apple-controller"
   ;; For MTP HID
   "apple-dockchannel"
   "apple-rtkit-helper"
   "dockchannel-hid"
   initrd-modules))
