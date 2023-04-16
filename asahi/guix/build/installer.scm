(define-module (asahi guix build installer)
  #:use-module (guix build json)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 pretty-print))

(define-record-type* <asahi-installer-os>
  asahi-installer-os make-asahi-installer-os
  asahi-installer-os?
  (boot-object asahi-installer-os-boot-object (default "m1n1.bin"))
  (default-os-name asahi-installer-os-default-os-name (default "Asahi Linux"))
  (name asahi-installer-os-name (default #f))
  (next-object asahi-installer-os-next-object (default "m1n1/boot.bin"))
  (package asahi-installer-os-package (default #f))
  (partitions asahi-installer-os-partitions (default #f))
  (supported-fw asahi-installer-os-supported-fw (default '("12.3" "12.3.1" "12.4"))))

(define-record-type* <asahi-installer-partition>
  asahi-installer-partition make-asahi-installer-partition
  asahi-installer-partition?
  (copy-firmware? asahi-installer-partition-copy-firmware? (default #f))
  (copy-installer-data? asahi-installer-partition-copy-installer-data? (default #f))
  (expand asahi-installer-partition-expand (default #f))
  (format asahi-installer-partition-format (default #f))
  (image asahi-installer-partition-image (default #f))
  (name asahi-installer-partition-name (default #f))
  (size asahi-installer-partition-size (default #f))
  (source asahi-installer-partition-source (default #f))
  (type asahi-installer-partition-type (default #f))
  (volume-id asahi-installer-partition-volume-id (default #f)))

(define esp-partition
  (asahi-installer-partition
   (name "EFI")
   (type "EFI")
   (size "500MB")
   (format "fat")
   (volume-id "0x2abf9f91")
   (copy-firmware? #t)
   (copy-installer-data? #t)
   (source "esp")))

(define root-partition
  (asahi-installer-partition
   (name "Root")
   (type "Linux")
   (size "8GB")
   (expand #t)
   (image "root.img")))

(define asahi-installer-os-minimal
  (asahi-installer-os
   (name "Asahi Guix Minimal")
   (package "asahi-base-20221122-4.zip")
   (partitions (list esp-partition root-partition))))

(define asahi-installer-os-desktop
  (asahi-installer-os
   (name "Asahi Guix Desktop")
   (package "asahi-desktop-20221122-4.zip")
   (partitions (list esp-partition
                     (asahi-installer-partition
                      (inherit root-partition)
                      (size "16GB"))))))

(define asahi-installer-oses
  (list asahi-installer-os-minimal
        asahi-installer-os-desktop))

(define (read-installer-data file)
  "Read the Asahi Guix installer data from FILE."
  (call-with-input-file file
    (lambda (in) (read-json in))))

(define (write-installer-data file data)
  "Write the Asahi Guix installer data to FILE."
  (mkdir-p (dirname file))
  (call-with-output-file file
    (lambda (out) (write-json data out))))

;; (pretty-print (read-installer-data "installer_data.json"))
;; (write-installer-data "test.json" (read-installer-data "installer_data.json"))
