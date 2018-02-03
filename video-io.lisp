(in-package #:cv)

(define-cfun ("cvCreateFileCapture" create-file-capture) :pointer
  (filename :string))

(define-cfun ("cvCreateCameraCapture" create-camera-capture) :pointer
  (index :int))

(define-cfun ("cvGrabFrame" grab-frame) :int
  (capture :int))

(define-cfun ("cvRetrieveFrame" retrieve-frame) :pointer
  (capture :pointer)
  (stream-idx :int))

(define-cfun ("cvQueryFrame" query-frame) :pointer
  (capture :pointer))

(cffi:defcfun ("cvReleaseCapture" %release-capture) :void
  (capture :pointer))

(defun release-capture (capture)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) capture)
    (%release-capture ptr)))

(define-cfun ("cvGetCaptureProperty" get-capture-property) :double
  (capture :pointer)
  (property-id :cap-prop-enum))

(define-cfun ("cvSetCaptureProperty" set-capture-property) :int
  (capture :pointer)
  (property-id :cap-prop-enum)
  (value :double))


