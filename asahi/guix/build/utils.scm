(define-module (asahi guix build utils)
  #:use-module (guix build json)
  #:use-module (guix build utils)
  #:export (read-json-file
            write-json-file))

(define (read-json-file file)
  "Read data in JSON format from FILE."
  (call-with-input-file file
    (lambda (port) (read-json port))))

(define (write-json-file file data)
  "Write DATA as JSON to FILE."
  (mkdir-p (dirname file))
  (call-with-output-file file
    (lambda (port) (write-json data port))))
