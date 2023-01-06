(define-module (asahi guix disk mbr)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (guix records)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42))

;; CHS Address

;; https://en.wikipedia.org/wiki/Cylinder-head-sector

(define %chs-address
  (bs:struct
   `((head ,uint8)
     (sector ,uint8)
     (cylinder ,uint8))))

(define-record-type* <chs-address>
  chs-address make-chs-address chs-address?
  (head chs-address-head)
  (sector chs-address-sector)
  (cylinder chs-address-cylinder))

(define (struct->chs-address struct)
  (chs-address
   (head (bytestructure-ref struct 'head))
   (sector (bytestructure-ref struct 'sector))
   (cylinder (bytestructure-ref struct 'cylinder))))

;; MBR Partition

;; https://wiki.osdev.org/MBR_(x86)

(define %partition-struct
  (bs:struct
   `((status ,uint8)
     (start ,%chs-address)
     (type ,uint8)
     (end ,%chs-address)
     (lba ,uint32)
     (sectors ,uint32))))

(define-record-type* <mbr-partition>
  mbr-partition make-mbr-partition mbr-partition?
  (status mbr-partition-status)
  (start mbr-partition-start)
  (type mbr-partition-type)
  (end mbr-partition-end)
  (lba mbr-partition-lba)
  (sectors mbr-partition-sectors))

(define (struct->mbr-partition struct)
  (mbr-partition
   (status (bytestructure-ref struct 'status))
   (start (struct->chs-address (bytestructure-ref struct 'start)))
   (type (bytestructure-ref struct 'type))
   (end (struct->chs-address (bytestructure-ref struct 'end)))
   (lba (bytestructure-ref struct 'lba))
   (sectors (bytestructure-ref struct 'sectors))))

;; MBR Table

(define %mbr-table-struct
  (bs:struct
   `((code ,(bs:vector 440 uint8))
     (disk-signature ,int)
     (reserved ,uint16)
     (partition-1 ,%partition-struct)
     (partition-2 ,%partition-struct)
     (partition-3 ,%partition-struct)
     (partition-4 ,%partition-struct)
     (boot-signature ,uint16))))

(define %mbr-table-struct-size
  (bytestructure-descriptor-size %mbr-table-struct))

(define-record-type* <mbr-table>
  mbr-table make-mbr-table mbr-table?
  (code mbr-table-code)
  (disk-signature mbr-table-disk-signature)
  (reserved mbr-table-reserved)
  (partitions mbr-table-partitions)
  (boot-signature mbr-table-boot-signature))

(define (struct->mbr-table struct)
  (mbr-table
   (code (list-ec (: offset 440) (bytestructure-ref struct 'code offset)))
   (disk-signature (bytestructure-ref struct 'disk-signature))
   (reserved (bytestructure-ref struct 'reserved))
   (partitions (map struct->mbr-partition
                    (list (bytestructure-ref struct 'partition-1)
                          (bytestructure-ref struct 'partition-2)
                          (bytestructure-ref struct 'partition-3)
                          (bytestructure-ref struct 'partition-4))))
   (boot-signature (bytestructure-ref struct 'boot-signature))))

(define (read-mbr device)
  (call-with-input-file device
    (lambda (port)
      (let ((bytes (get-bytevector-n port %mbr-table-struct-size)))
        (struct->mbr-table (make-bytestructure bytes 0 %mbr-table-struct))))
    #:binary #t))

(define (display-partititon partition)
  (format #t "Partition:\n")
  (format #t "  Status: ~a\n" (mbr-partition-status partition))
  (format #t "  Start: ~a\n" (mbr-partition-start partition))
  (format #t "  Type: ~a\n" (mbr-partition-type partition))
  (format #t "  End: ~a\n" (mbr-partition-end partition))
  (format #t "  LBA: ~a\n" (mbr-partition-lba partition))
  (format #t "  Sectors: ~a\n" (mbr-partition-sectors partition))
  (newline))

(define (display-mbr table)
  (format #t "Table:\n")
  (format #t "  Disk Signature: ~a\n" (mbr-table-disk-signature table))
  (format #t "  Boot Signature: ~a\n" (mbr-table-boot-signature table))
  (newline)
  (format #t "  Code:\n ~a\n\n" (mbr-table-code table))
  (map display-partititon (mbr-table-partitions table)))

(define my-mbr
  (read-mbr "/home/r0man/workspace/asahi-guix/test/dev/nvme0n1"))

(display-mbr my-mbr)
