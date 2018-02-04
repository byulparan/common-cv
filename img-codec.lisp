(in-package #:cv)

(define-cfun ("cvConvertImage" convert-image) :void
  (src :pointer)
  (dst :pointer)
  (flags :int 0))

(define-cfun ("cvLoadImage" load-image) :pointer
  (file :string)
  (iscolor :int +load-image-color+))

(define-cfun ("cvLoadImageM" load-image-m) :pointer
  (filename :string)
  (iscolor :int +load-image-color+))
