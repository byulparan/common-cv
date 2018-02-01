(in-package #:cv)

(cffi:defcfun ("cvCreateFileCapture" create-file-capture) :pointer
  (filename :string))

(cffi:defcfun ("cvCreateCameraCapture" create-camera-capture) :pointer
  (index :int))

(cffi:defcfun ("cvGrabFrame" grab-frame) :int
  (capture :int))

(cffi:defcfun ("cvRetrieveFrame" retrieve-frame) :pointer
  (capture :pointer)
  (stream-idx :int))

(cffi:defcfun ("cvQueryFrame" query-frame) :pointer
  (capture :pointer))

(cffi:defcfun ("cvReleaseCapture" release-capture) :void
  (capture :pointer))

(cffi:defcfun ("cvGetCaptureProperty" get-capture-property) :double
  (capture :pointer)
  (property-id :cap-prop-enum))

(cffi:defcfun ("cvSetCaptureProperty" set-capture-property) :int
  (capture :pointer)
  (property-id :cap-prop-enum)
  (value :double))


