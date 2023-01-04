(define-module (asahi blkid)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-42)
  #:use-module (system foreign-library)
  #:export (probe-device probe-devices))

(eval-when (expand load eval)
  (define dev_t int)
  (define mode_t int)
  (define uint int)
  (define uint64_t uint64))

;; Use #:search-path ?
;; (setenv "GUILE_EXTENSIONS_PATH" "/gnu/store/yg4l52c2hrxrnpfq23krjv8jgazhb9xd-util-linux-2.37.2-lib/lib")

(define libblkid
  (load-foreign-library "libblkid"))

(define new-probe-from-filename
  (foreign-library-function
   "libblkid" "blkid_new_probe_from_filename"
   #:return-type '* #:arg-types (list '*)))

(define free-probe
  (foreign-library-function
   "libblkid" "blkid_free_probe"
   #:arg-types (list '*)))

(define probe-get-partitions
  (foreign-library-function
   "libblkid" "blkid_probe_get_partitions"
   #:return-type '* #:arg-types (list '*)))

(define partlist-get-table
  (foreign-library-function
   "libblkid" "blkid_partlist_get_table"
   #:return-type '* #:arg-types (list '*)))

(define partlist-get-partition
  (foreign-library-function
   "libblkid" "blkid_partlist_get_partition"
   #:return-type '* #:arg-types (list '* ffi:int)))

(define partlist-numof-partitions
  (foreign-library-function
   "libblkid" "blkid_partlist_numof_partitions"
   #:return-type ffi:int #:arg-types (list '*)))

;; https://github.com/alisw/uuid/blob/master/libblkid/src/blkidP.h#L178

(eval-when (expand load eval)
  (define probe-struct
    (bs:struct
     `((fd ,int)
       (off ,uint64)
       (size ,uint64)
       (devno ,dev_t)
       (disk-devno ,dev_t)
       (blkssz ,uint)
       (mode ,mode_t)
       (zone-size ,uint64)
       (flags ,int)
       (prob-flags ,int)
       (wipe-off ,uint64)
       (wipe-size ,uint64)))))

(eval-when (expand load eval)
  (define partition-struct
    (bs:struct
     `((start ,uint64)
       (size ,uint64)
       (type ,int)
       (type-str ,(bs:string 37 'utf8))
       (flags ,long-long)
       (partno ,int)
       (uuid ,(bs:string 37 'utf8))
       (name ,(bs:string 128 'utf8))))))

(define-bytestructure-accessors probe-struct
  probe-struct-unwrap probe-struct-ref probe-struct-set!)

(define-bytestructure-accessors partition-struct
  partition-struct-unwrap partition-struct-ref partition-struct-set!)

(define (strip-string str)
  (define (helper str result)
    (cond
     ((string-null? str) result)
     ((char=? #\nul (string-ref str 0)) (helper (substring str 1) result))
     (else (helper (substring str 1) (string-append result (string (string-ref str 0)))))))
  (helper str ""))

(define (pointer->probe probe)
  (let* ((size (bytestructure-descriptor-size probe-struct))
         (bytes (ffi:pointer->bytevector probe size)))
    `((fd . ,(probe-struct-ref bytes fd))
      (off . ,(probe-struct-ref bytes off))
      (size . ,(probe-struct-ref bytes size))
      (devno . ,(probe-struct-ref bytes devno))
      (disk-devno . ,(probe-struct-ref bytes disk-devno))
      (blkssz . ,(probe-struct-ref bytes blkssz))
      (mode . ,(probe-struct-ref bytes mode))
      (zone-size . ,(probe-struct-ref bytes zone-size))
      (flags . ,(probe-struct-ref bytes flags))
      (prob-flags . ,(probe-struct-ref bytes prob-flags))
      (wipe-off . ,(probe-struct-ref bytes wipe-off))
      (wipe-size . ,(probe-struct-ref bytes wipe-size)))))

(define (pointer->partition partition)
  (let* ((size (bytestructure-descriptor-size partition-struct))
         (bytes (ffi:pointer->bytevector partition size)))
    `((start . ,(partition-struct-ref bytes start))
      (size . ,(partition-struct-ref bytes size))
      (type . ,(partition-struct-ref bytes type))
      (type-str . ,(strip-string (partition-struct-ref bytes type-str)))
      (flags . ,(partition-struct-ref bytes flags))
      (partno . ,(partition-struct-ref bytes partno))
      (uuid . ,(strip-string (partition-struct-ref bytes uuid)))
      (name . ,(strip-string (partition-struct-ref bytes name))))))

(define (probe-partitions probe)
  (let ((partition-list (probe-get-partitions probe)))
    (list-ec (: partition-num (partlist-numof-partitions partition-list))
             (pointer->partition (partlist-get-partition partition-list partition-num)))))

(define (probe-device device)
  (let* ((probe (new-probe-from-filename (ffi:string->pointer device)))
         (probe-data (pointer->probe probe))
         (partitions (probe-partitions probe)))
    (free-probe probe)
    `((probe . ,probe-data)
      (partitions . ,partitions))))

(define (probe-devices devices)
  (map (lambda (device)
         (cons device (probe-device device)))
       devices))

;; (pretty-print (probe-devices '("/dev/nvme0n1" "/dev/sda")))
