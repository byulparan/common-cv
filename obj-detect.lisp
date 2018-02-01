(in-package #:cv)

(cffi:defcfun ("cvHaarDetectObjects" haar-detect-objects) :pointer
  (image :pointer)
  (cascade :pointer)
  (storage :pointer)
  (scale-factor :double)
  (min-neighbors :int)
  (flags :haar-detect-enum)
  (min-size (:struct size))
  (max-size (:struct size)))


