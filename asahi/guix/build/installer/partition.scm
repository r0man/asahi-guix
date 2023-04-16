(define-module (asahi guix build installer partition)
  #:use-module (guix records)
  #:export (asahi-installer-partition
            asahi-installer-partition-copy-firmware?
            asahi-installer-partition-copy-installer-data?
            asahi-installer-partition-expand
            asahi-installer-partition-format
            asahi-installer-partition-from-json
            asahi-installer-partition-image
            asahi-installer-partition-name
            asahi-installer-partition-size
            asahi-installer-partition-source
            asahi-installer-partition-to-json
            asahi-installer-partition-type
            asahi-installer-partition-volume-id
            asahi-installer-partition?
            make-asahi-installer-partition))

(define-record-type* <asahi-installer-partition>
  asahi-installer-partition make-asahi-installer-partition
  asahi-installer-partition?
  (copy-firmware? asahi-installer-partition-copy-firmware? (default #f))
  (copy-installer-data? asahi-installer-partition-copy-installer-data? (default #f))
  (expand asahi-installer-partition-expand (default #f))
  (format asahi-installer-partition-format (default #f))
  (image asahi-installer-partition-image (default #f))
  (name asahi-installer-partition-name)
  (size asahi-installer-partition-size)
  (source asahi-installer-partition-source (default #f))
  (type asahi-installer-partition-type)
  (volume-id asahi-installer-partition-volume-id (default #f)))

(define (asahi-installer-partition-from-json obj)
  (asahi-installer-partition
   (copy-firmware? (assoc-ref obj "copy_firmware"))
   (copy-installer-data? (assoc-ref obj "copy_installer_data"))
   (expand (assoc-ref obj "expand"))
   (format (assoc-ref obj "format"))
   (image (assoc-ref obj "image"))
   (name (assoc-ref obj "name"))
   (size (assoc-ref obj "size"))
   (source (assoc-ref obj "source"))
   (type (assoc-ref obj "type"))
   (volume-id (assoc-ref obj "volume_id"))))

(define (asahi-installer-partition-to-json partition)
  `(@ ("copy_firmware" . ,(asahi-installer-partition-copy-firmware? partition))
      ("copy_installer_data" . ,(asahi-installer-partition-copy-installer-data? partition))
      ("expand" . ,(asahi-installer-partition-expand partition))
      ("format" . ,(asahi-installer-partition-format partition))
      ("image" . ,(asahi-installer-partition-image partition))
      ("name" . ,(asahi-installer-partition-name partition))
      ("size" . ,(asahi-installer-partition-size partition))
      ("source" . ,(asahi-installer-partition-source partition))
      ("type" . ,(asahi-installer-partition-type partition))
      ("volume_id" . ,(asahi-installer-partition-volume-id partition))))
