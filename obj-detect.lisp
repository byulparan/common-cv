(in-package #:cv)

(define-cfun ("cvHaarDetectObjects" haar-detect-objects) :pointer
  (image :pointer)
  (cascade :pointer)
  (storage :pointer)
  (scale-factor :double)
  (min-neighbors :int)
  (flags :haar-detect-enum)
  (min-size (:struct size))
  (max-size (:struct size)))

(define-cfun ("cvLoadHaarClassifierCascade" load-haar-classifier-cascade) :pointer
  (directory :string)
  (orig-window-size (:struct size)))

