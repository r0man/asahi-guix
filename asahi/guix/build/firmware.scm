(define-module (asahi guix build firmware)
  #:use-module (asahi guix build cpio)
  #:use-module (gnu build activation)
  #:use-module (gnu build file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (setup-firmware))

(define %esp-mount-point
  "/run/.system-efi")

(define %firmware-mount-point
  "/run/.system-firmware")

(define %efi-system-partition-uuid-path
  "/proc/device-tree/chosen/asahi,efi-system-partition")

(define (disk-partition-device partition)
  (format #f "/dev/~a" partition))

(define (disk-partition-devices)
  (map disk-partition-device (disk-partitions)))

(define (firmware-archive esp-mount-point)
  (string-append esp-mount-point "/vendorfw/firmware.cpio"))

(define (unmount-directory mount-point)
  (when (directory-exists? mount-point)
    (while (member mount-point (mount-points))
      (umount mount-point))))

(define (detect-firmware-devices devices)
  (filter (lambda (device)
            (let ((template "/tmp/detect-firmware-devices-XXXXXX"))
              (mkdir-p (dirname template))
              (let ((mount-point (mkdtemp template)))
                (unmount-directory mount-point)
                (mkdir-p mount-point)
                (catch 'system-error
                  (lambda ()
                    (mount device mount-point "vfat" MS_RDONLY)
                    (let ((archive? (file-exists? (firmware-archive mount-point))))
                      (unmount-directory mount-point)
                      (rmdir mount-point)
                      archive?))
                  (lambda (key . args)
                    (unmount-directory mount-point)
                    (rmdir mount-point)
                    #f)))))
          devices))

(define* (read-efi-system-partition-uuid
          #:key (path %efi-system-partition-uuid-path))
  (when (file-exists? path)
    (let ((content (call-with-input-file path get-string-all)))
      (uuid (string-trim-right content #\nul)))))

(define (efi-system-partition-device-chosen)
  (find-partition-by-uuid (read-efi-system-partition-uuid)))

(define (efi-system-partition-device-detected)
  (let ((devices (detect-firmware-devices (disk-partition-devices))))
    (when (not (null? devices))
      (first devices))))

(define (efi-system-partition-device)
  (or (efi-system-partition-device-chosen)
      (efi-system-partition-device-detected)))

(define (mount-efi-system-partition mount-point)
  (let ((esp-device (efi-system-partition-device)))
    (when esp-device
      (unmount-directory mount-point)
      (mkdir-p mount-point)
      ;; TODO: MS_RDONLY
      (mount esp-device mount-point "vfat")
      esp-device)))

(define (mount-firmware-directory mount-point)
  (unmount-directory mount-point)
  (mkdir-p mount-point)
  (mount "tmpfs" mount-point "tmpfs"))

(define (extract-firmware archive mount-point)
  (when (file-exists? archive)
    (extract-cpio-archive archive mount-point)))

(define* (setup-firmware #:key
                         (esp-mount-point %esp-mount-point)
                         (firmware-mount-point %firmware-mount-point))
  (display ":: Asahi Guix: Setting up firmware ...\n")
  (let ((esp-device (mount-efi-system-partition esp-mount-point)))
    (if esp-device
        (let ((firmware-archive (firmware-archive esp-mount-point)))
          (mount-firmware-directory firmware-mount-point)
          (extract-firmware firmware-archive firmware-mount-point)
          (unmount-directory esp-mount-point)
          (let ((activation-dir (string-append firmware-mount-point "/vendorfw")))
            (activate-firmware activation-dir)
            (format #t ":: Asahi Guix: Firmware from ~a activated at ~a.\n"
                    esp-device activation-dir)))
        (format #t ":: Asahi Guix: Firmware not activated.\n"))))

;; (setup-firmware)
;; (pretty-print (detect-firmware-devices (disk-partition-devices)))
