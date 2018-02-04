(in-package #:cv)

(define-cfun ("cvHaarDetectObjects" haar-detect-objects) :pointer
  (image :pointer)
  (cascade :pointer)
  (storage :pointer)
  (scale-factor :double 1.1)
  (min-neighbors :int 3)
  (flags :int 0)
  (min-size (:struct size) (size 0 0))
  (max-size (:struct size) (size 0 0)))

(define-cfun ("cvLoadHaarClassifierCascade" load-haar-classifier-cascade) :pointer
  (directory :string)
  (orig-window-size (:struct size)))

