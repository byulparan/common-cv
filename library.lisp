
(cffi:define-foreign-library opencv-core
  (:darwin "libopencv_core.dylib")
  (:linux "libopencv_core.so"))

(cffi:define-foreign-library opencv-imgproc
  (:darwin "libopencv_imgproc.dylib")
  (:linux "libopencv_imgproc.so"))

(cffi:define-foreign-library opencv-objdetect
  (:darwin "libopencv_objdetect.dylib")
  (:linux "libopencv_objdetect.so"))

(cffi:define-foreign-library opencv-highgui
  (:darwin "libopencv_highgui.dylib")
  (:linux "libopencv_highgui.so"))

(cffi:use-foreign-library opencv-core)
(cffi:use-foreign-library opencv-imgproc)
(cffi:use-foreign-library opencv-objdetect)
(cffi:use-foreign-library opencv-highgui)

