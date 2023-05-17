(define-module (asahi guix build firmware)
  #:use-module (asahi guix build block-devices)
  #:use-module (gnu build activation)
  #:use-module (gnu build file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs conditions)
  #:use-module (srfi srfi-1)
  #:export (firmware
            firmware-blkid
            firmware-device
            firmware-directory
            firmware-esp-uuid
            firmware-mount
            firmware?
            make-firmware
            setup-firmware))

(define %current-efi-system-partition-uuid-path
  "/proc/device-tree/chosen/asahi,efi-system-partition")

(define %sysfs-fimware-parameter-path
  "/sys/module/firmware_class/parameters/path")

(define-condition-type &firmware-error &condition
  make-firmware-error firmware-error?
  (firmware firmware-error-firmware))

(define-condition-type &firmware-esp-not-found &firmware-error
  make-firmware-esp-not-found firmware-esp-not-found?)

(define-condition-type &firmware-device-not-found &firmware-error
  make-firmware-device-not-found firmware-device-not-found?)

(define-record-type* <firmware>
  firmware make-firmware firmware?
  (directory
   firmware-directory
   (default "/run/asahi-guix/firmware"))
  (blkid
   firmware-blkid
   (default "blkid"))
  (cpio
   firmware-cpio
   (default "cpio"))
  (esp-uuid
   firmware-esp-uuid
   (default #f))
  (device
   firmware-device
   (default #f))
  (device-mount
   firmware-device-mount
   (default #f)))

(define (read-uuid path)
  (let ((content (call-with-input-file path get-string-all)))
    (uuid (string-trim-right content #\nul))))

(define (read-path path)
  (let ((content (call-with-input-file path get-string-all)))
    (string-trim-right content #\newline)))

(define (report message . args)
  (apply format #t (string-append ":: Asahi Guix: " message) args)
  (newline))

(define (firmware-read-esp-uuid)
  "Read the chosen EFI system partition UUID of FIRMWARE."
  (let ((path %current-efi-system-partition-uuid-path))
    (and (file-exists? path) (read-uuid path))))

(define (firmware-probe-device firmware)
  "Find the block device of the chosen ESP of FIRMWARE."
  (let ((uuid (firmware-read-esp-uuid)))
    (find (lambda (block-device)
            (equal? uuid (block-device-partuuid block-device)))
          (block-devices #:blkid (firmware-blkid firmware)))))

(define (firmware-probe-device-mount firmware)
  "Find the current mount of the chosen ESP of FIRMWARE."
  (let ((device (or (firmware-device firmware)
                    (firmware-probe-device firmware))))
    (when (block-device? device)
      (find (lambda (mount)
              (equal? (block-device-name device)
                      (mount-source mount)))
            (mounts)))))

(define (firmware-probe fw)
  (firmware
   (inherit fw)
   (device (firmware-probe-device fw))
   (device-mount (firmware-probe-device-mount fw))
   (esp-uuid (firmware-read-esp-uuid))))

(define (unmount-directory mount-point)
  (when (directory-exists? mount-point)
    (while (member mount-point (mount-points))
      (umount mount-point))))

(define (firmware-cpio-archive firmware)
  (let ((mount (firmware-device-mount firmware)))
    (when mount
      (string-append (mount-point mount) "/vendorfw/firmware.cpio"))))

(define (mount-firmware-directory firmware)
  (let ((mount-point (firmware-directory firmware)))
    (unmount-directory mount-point)
    (mkdir-p mount-point)
    (mount "tmpfs" mount-point "tmpfs")))

(define (firmware-extract-mounted-esp firmware)
  (let ((directory (firmware-directory firmware)))
    (invoke (firmware-cpio firmware)
            "--directory" directory
            "--extract"
            "--file" (firmware-cpio-archive firmware)
            "--make-directories"
            "--no-absolute-filenames"
            "--quiet")
    (let ((vendorfw (string-append directory "/vendorfw")))
      (copy-recursively vendorfw directory #:log #f)
      (delete-file-recursively vendorfw))))

(define (firmware-extract-unmounted-esp firmware)
  (let ((template "/run/asahi-guix/.efi-system-partition-XXXXXX"))
    (mkdir-p (dirname template))
    (let ((device (firmware-device firmware))
          (mount-point (mkdtemp template)))
      (unmount-directory mount-point)
      (mkdir-p mount-point)
      (mount (block-device-name device) mount-point "vfat") ;; TODO: MS_RDONLY
      (firmware-extract-mounted-esp (firmware-probe firmware))
      (unmount-directory mount-point)
      (rmdir mount-point))))

(define (copy-installer-firmware firmware)
  (if (firmware-device-mount firmware)
      (firmware-extract-mounted-esp firmware)
      (firmware-extract-unmounted-esp firmware)))

(define (copy-system-firmware firmware)
  (when (file-exists? %sysfs-fimware-parameter-path)
    (let ((source-dir (read-path %sysfs-fimware-parameter-path))
          (target-dir (firmware-directory firmware)))
      (when (directory-exists? source-dir)
        (copy-recursively source-dir target-dir
                          #:copy-file (lambda (source target)
                                        (when (file-exists? target)
                                          (delete-file target))
                                        (copy-file source target))
                          #:follow-symlinks? #t
                          #:log #f)))))

(define (check-requirements firmware)
  (unless (firmware-esp-uuid firmware)
    (raise-exception
     (condition
      (make-firmware-esp-not-found firmware))))
  (unless (firmware-device firmware)
    (raise-exception
     (condition
      (make-firmware-device-not-found firmware)))))

(define (setup-firmware firmware)
  (with-exception-handler
      (lambda (exception)
        (report "Failed to activate firmware:")
        (newline)
        (pretty-print exception)
        (newline)
        (if (firmware-error? exception)
            (values #f exception)
            (raise-exception exception)))
    (lambda ()
      (let ((firmware (firmware-probe firmware))
            (directory (firmware-directory firmware)))
        (check-requirements firmware)
        (mount-firmware-directory firmware)
        (copy-system-firmware firmware)
        (copy-installer-firmware firmware)
        (activate-firmware directory)
        (report "Firmware activated at ~a." directory)
        (values #t firmware)))
    #:unwind? #t))

;; (define my-firmware
;;   (firmware (directory "/tmp/my/firmware")))
;; (setup-firmware my-firmware)
