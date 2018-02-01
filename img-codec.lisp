(in-package #:cv)

(cffi:defcfun ("cvConvertImage" convert-image) :void
  (src :pointer)
  (dst :pointer)
  (flags :convert-image-enum))

(cffi:defcfun ("cvLoadImage" load-image) :pointer
  (file :string)
  (iscolor :ipl-load-image-enum))

(cffi:defcfun ("cvLoadImageM" load-image-m) :pointer
  (filename :string)
  (iscolor :ipl-load-image-enum))
