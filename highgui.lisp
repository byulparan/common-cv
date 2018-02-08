(in-package #:cv)

(define-cfun ("cvNamedWindow" named-window) :void
  (name :string)
  (flags :int +window-autosize+))

(define-cfun ("cvSetWindowProperty" set-window-property) :void
  (name :string)
  (prop-id :int)
  (prop-value :double))

(define-cfun ("cvGetWindowProperty" get-window-property) :double
  (name :string)
  (prop-id :int))

(define-cfun ("cvShowImage" show-image) :void
  (name :string)
  (image :pointer))

(define-cfun ("cvResizeWindow" resize-window) :void
  (nmae :string)
  (width :int)
  (height :int))

(define-cfun ("cvMoveWindow" move-window) :void
  (name :string)
  (x :int)
  (y :int))

(define-cfun ("cvDestroyWindow" destroy-window) :void
  (name :string))

(define-cfun ("cvDestroyAllWindows" destroy-all-windows) :void)

(define-cfun ("cvGetWindowHandle" get-window-handle) :pointer
  (name :string))

(define-cfun ("cvGetWindowName" get-window-name) :string
  (window-handle :pointer))

(cffi:defcfun ("cvCreateTrackbar" %create-trackbar) :int
  (trackbar-name :string)
  (window-name :string)
  (value :pointer)
  (count :int)
  (on-change :pointer))

(cffi:defcfun ("cvCreateTrackbar2" %create-trackbar2) :int
  (trackbar-name :string)
  (window-name :string)
  (value :pointer)
  (count :int)
  (on-change :pointer)
  (userdata :pointer))




(cffi:defcfun ("cvSetMouseCallback" %set-mouse-callback) :void
  (window-name :string)
  (on-mouse :pointer)
  (param :pointer))

(define-cfun ("cvWaitKey" wait-key) :int
  (delay :int))

(define-cfun ("cvSetMouseCallback" set-mouse-callback) :void
  (name :string)
  (callback :pointer)
  (param :pointer (cffi:null-pointer)))


;;; @videoio
