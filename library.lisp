
(let* ((lib-dir (su:cat (su:full-pathname (asdf::system-source-directory :common-cv)) "C/"))
       (file (su:cat lib-dir "libopencv_wrap")))
  (unless (probe-file file)
    (su:run-program (su:cat "make -C " lib-dir) :output t :wait t))
  (cffi:load-foreign-library file))

