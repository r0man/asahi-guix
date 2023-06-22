(define-module (asahi guix packages bootloader)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public asahi-m1n1
  (let ((commit "88df8a63354f9987224ce507f7952abb3ec97258"))
    (package
      (name "asahi-m1n1")
      (version (git-version "1.2.9" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/m1n1")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zdrfyblfqwjac7j58vmgq28vkljkrik7vkbcbxbyrf4qil3giyc"))))
      (build-system gnu-build-system)
      (supported-systems (list "aarch64-linux"))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda _
                (setenv "RELEASE" "1")))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((dir (string-append (assoc-ref outputs "out") "/libexec/")))
                  (mkdir-p dir)
                  (copy-file "build/m1n1.bin" (string-append dir "m1n1.bin")))))
            ;; There are no tests
            (delete 'check))))
      (home-page "https://github.com/AsahiLinux/m1n1")
      (synopsis "Boot loader and experimentation playground for Apple Silicon")
      (description "m1n1 is the bootloader developed by the Asahi Linux project to bridge
the Apple (XNU) boot ecosystem to the Linux boot ecosystem.")
      (license license:expat))))

(define-public u-boot-apple-m1
  (let ((base (make-u-boot-package "apple_m1" "aarch64-linux-gnu"))
        (commit "471ea205bfda70428757f71a7463b5a03b3a52aa"))
    (package/inherit base
      (version (git-version "2023.04-2" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/u-boot")
               (commit commit)))
         (file-name (git-file-name (package-name base) version))
         (sha256
          (base32 "0k28ccc8z00hchaxxb40rgi5apz449pc9fp8xdhz81dkh9snzw6j"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'disable-tools-libcrypto)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend libressl))))))
