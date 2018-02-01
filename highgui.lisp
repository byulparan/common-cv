(in-package #:cv)

(cffi:defcfun ("cvNamedWindow" named-window) :void
  (name :string)
  (flags :named-window-enum))

(cffi:defcfun ("cvDestroyWindow" destroy-window) :void
  (name :string))

(cffi:defcfun ("cvShowImage" show-image) :void
  (name :string)
  (image :pointer))

(cffi:defcfun ("cvWaitKey" wait-key) :int
  (delay :int))

(cffi:defcfun ("cvSetMouseCallback" set-mouse-callback) :void
  (name :string)
  (callback :pointer)
  (param :pointer))


;;; @videoio
