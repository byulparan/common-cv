
(cffi:define-foreign-library opencv-core
  (:darwin "libopencv_core.dylib"))

(cffi:define-foreign-library opencv-imgproc
  (:darwin "libopencv_imgproc.dylib"))

(cffi:use-foreign-library opencv-core)
(cffi:use-foreign-library opencv-imgproc)

