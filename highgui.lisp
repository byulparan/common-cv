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
(cffi:defcfun ("cvCreateCameraCapture" create-camera-capture) :pointer
  (index :int))

(cffi:defcfun ("cvCreateFileCapture" create-file-capture) :pointer
  (filename :string))

(cffi:defcfun ("cvReleaseCapture" release-capture) :void
  (capture :pointer))

(cffi:defcfun ("cvGetCaptureProperty" get-capture-property) :double
  (capture :pointer)
  (property-id :cap-prop-enum))

(cffi:defcfun ("cvSetCaptureProperty" set-capture-property) :int
  (capture :pointer)
  (property-id :cap-prop-enum)
  (value :double))

(cffi:defcfun ("cvQueryFrame" query-frame) :pointer
  (capture :pointer))


;;@imgcodecs
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

;; @core
(cffi:defcfun ("cvLoad" load) :pointer
  (filename :string)
  (memstorage :pointer)
  (name :string)
  (real-name :string))


;; @imgproc
(cffi:defcfun ("cvLine" line) :void
  (img :pointer)
  (pt1 (:struct point))
  (pt2 (:struct point))
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvRectangle" rectangle) :void
  (img :pointer)
  (pt1 (:struct point))
  (pt2 (:struct point))
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvCircle" circle) :void
  (img :pointer)
  (center (:struct point))
  (radius :int)
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvEllipse" ellipse) :void
  (img :pointer)
  (center (:struct point))
  (axes (:struct size))
  (angle :double)
  (start-angle :double)
  (end-angle :double)
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvEllipseBox" ellipse-box) :void
  (img :pointer)
  (box (:struct box2d))
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvFillConvexPoly" fill-convex-poly) :void
  (img :pointer)
  (pts :pointer)
  (npts :int)
  (color (:struct scalar))
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvFillPoly" fill-poly) :void
  (img :pointer)
  (ptr :pointer)
  (npts :pointer)
  (contours :int)
  (color (:struct scalar))
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvPolyLine" poly-line) :void
  (img :pointer)
  (ptr :pointer)
  (npts :pointer)
  (contours :int)
  (is-closed :int)
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))


(cffi:defcstruct font
  (name-font :string)
  (color (:struct scalar))
  (font-face :font-face-enum)
  (ascii :pointer)
  (greek :pointer)
  (cyrillic :pointer)
  (hscale :float)
  (vscale :float)
  (shear :float)
  (thickness :int)
  (dx :float)
  (line-type :int))

(cffi:defcfun ("cvPutText" :put-text) :void
  (img :pointer)
  (text :string)
  (org (:struct point))
  (font :pointer)
  (color (:struct scalar)))
