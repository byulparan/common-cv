
(asdf:defsystem #:common-cv
  :serial t
  :depends-on (#:cffi-libffi)
  :components ((:file "library")
	       (:file "package")
	       ;;(:file "macros")
	       (:file "constants")
	       (:file "cv-types")
	       (:file "filter")
	       (:file "img-proc")
	       (:file "obj-detect")
	       ;; (:file "highgui")
	       ))
