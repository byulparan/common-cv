(in-package #:cv)

(define-cfun ("cvConvertImage" convert-image) :void
  (src :pointer)
  (dst :pointer)
  (flags :convert-image-enum))

(define-cfun ("cvLoadImage" load-image) :pointer
  (file :string)
  (iscolor :ipl-load-image-enum))

(define-cfun ("cvLoadImageM" load-image-m) :pointer
  (filename :string)
  (iscolor :ipl-load-image-enum))
