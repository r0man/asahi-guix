(define-module (asahi guix build installer os)
  #:use-module (asahi guix build installer partition)
  #:use-module (guix records)
  #:export (asahi-installer-os
            asahi-installer-os-boot-object
            asahi-installer-os-default-os-name
            asahi-installer-os-desktop
            asahi-installer-os-from-json
            asahi-installer-os-minimal
            asahi-installer-os-name
            asahi-installer-os-next-object
            asahi-installer-os-package
            asahi-installer-os-partitions
            asahi-installer-os-supported-fw
            asahi-installer-os-to-json
            asahi-installer-os?
            make-asahi-installer-os))

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

(define (asahi-installer-os-from-json obj)
  (asahi-installer-os
   (boot-object (assoc-ref obj "boot_object"))
   (default-os-name (assoc-ref obj "default_os_name"))
   (name (assoc-ref obj "name"))
   (next-object (assoc-ref obj "next_object"))
   (package (assoc-ref obj "package"))
   (partitions (map asahi-installer-partition-from-json
                    (assoc-ref obj "partitions")))
   (supported-fw (assoc-ref obj "supported_fw"))))

(define (asahi-installer-os-to-json os)
  `(@ ("boot_object" . ,(asahi-installer-os-boot-object os))
      ("default_os_name" . ,(asahi-installer-os-default-os-name os))
      ("name" . ,(asahi-installer-os-name os))
      ("next_object" . ,(asahi-installer-os-next-object os))
      ("package" . ,(asahi-installer-os-package os))
      ("partitions" . ,(map asahi-installer-partition-to-json
                            (asahi-installer-os-partitions os)))
      ("supported_fw" . ,(asahi-installer-os-supported-fw os))))
