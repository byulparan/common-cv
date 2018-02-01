
(cffi:define-foreign-library opencv-core
  (:darwin "libopencv_core.dylib"))

(cffi:define-foreign-library opencv-imgproc
  (:darwin "libopencv_imgproc.dylib"))

(cffi:define-foreign-library opencv-objdetect
  (:darwin "libopencv_objdetect.dylib"))

(cffi:define-foreign-library opencv-highgui
  (:darwin "libopencv_highgui.dylib"))

(cffi:use-foreign-library opencv-core)
(cffi:use-foreign-library opencv-imgproc)
(cffi:use-foreign-library opencv-objdetect)
(cffi:use-foreign-library opencv-highgui)

