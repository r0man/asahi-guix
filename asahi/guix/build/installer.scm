(define-module (asahi guix build installer)
  #:use-module (asahi guix build installer os)
  #:use-module (asahi guix build installer partition)
  #:use-module (asahi guix build utils)
  #:use-module (guix build json)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 pretty-print)
  #:export (%asahi-installer
            asahi-installer
            asahi-installer-build
            asahi-installer-os-list
            asahi-installer-read-data
            asahi-installer-write-data
            asahi-installer?
            make-asahi-installer))

(define-record-type* <asahi-installer>
  asahi-installer make-asahi-installer asahi-installer?
  (os-list asahi-installer-os-list (default '())))

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

(define %asahi-installer
  (asahi-installer
   (os-list
    (list
     (asahi-installer-os
      (name "Asahi Guix Minimal")
      (package "asahi-base-20221122-4.zip")
      (partitions (list esp-partition root-partition)))
     (asahi-installer-os
      (name "Asahi Guix Desktop")
      (package "asahi-desktop-20221122-4.zip")
      (partitions (list esp-partition
                        (asahi-installer-partition
                         (inherit root-partition)
                         (size "16GB")))))))))

(define (asahi-installer-from-json obj)
  (asahi-installer
   (os-list (map asahi-installer-os-from-json (assoc-ref obj "os_list")))))

(define (asahi-installer-to-json os)
  `(@ ("os_list" . ,(map asahi-installer-os-to-json
                         (asahi-installer-os-list os)))))

(define (asahi-installer-json-filename directory)
  (string-append directory "/installer_data.json"))

(define (asahi-installer-read-data file)
  "Read the Asahi Guix installer data from FILE."
  (asahi-installer-from-json (read-json-file file)))

(define (asahi-installer-write-data installer file)
  "Write the Asahi Guix installer data to FILE."
  (write-json-file file (asahi-installer-to-json installer)))

(define* (asahi-installer-build installer #:key (directory "/tmp/asahi-installer"))
  (let ((json-file (asahi-installer-json-filename directory)))
    (asahi-installer-write-data installer json-file)))
