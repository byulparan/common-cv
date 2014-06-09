
(in-package #:cv)



(defmacro define-method (name args &body body)
  `(progn
     (defmethod ,name ,args
       ,@body)
     (export ',name :cv)))

(defmacro define-constant (name val)
  `(progn
     (defconstant ,name ,val)
     (export ',name :cv)))

(defmacro with-cv-scalar ((name cv-scalar) &body body)
  (alexandria:with-gensyms (value)
    `(with-slots ((,value val)) ,cv-scalar
       (cffi:with-foreign-object (,name '(:struct CvScalar))
	 (cffi:with-foreign-slots ((val) ,name (:struct CvScalar))
	   (dotimes (i 4)
	     (setf (cffi:mem-aref val :double i) (nth i ,value)))
	   ,@body)))))

(defmacro with-cv-point ((name cv-point) &body body)
  `(cffi:with-foreign-object (,name '(:struct CvPoint))
     (cffi:with-foreign-slots ((x y) ,name (:struct CvPoint))
       (setf x (x ,cv-point)
	     y (y ,cv-point))
	   ,@body)))

(defmacro with-cv-size ((name cv-size) &body body)
  `(cffi:with-foreign-object (,name '(:struct CvSize))
     (cffi:with-foreign-slots ((width height) ,name (:struct CvSize))
       (setf width (width ,cv-size)
	     height (height ,cv-size))
	   ,@body)))

(defmacro with-cv-box-2d ((name cv-box-2d) &body body)
  `(cffi:with-foreign-object (,name '(:struct CvBox2D))
     (cffi:with-foreign-slots ((center size angle) ,name (:struct CvBox2D))
       (cffi:with-foreign-slots ((x y) center (:struct CvPoint2D32f))
	 (setf x (x (box-center ,cv-box-2d))
	       y (y (box-center ,cv-box-2d))))
       (cffi:with-foreign-slots ((width height) size (:struct CvSize2D32f))
	 (setf width (width (box-size ,cv-box-2d))
	       height (height (box-size ,cv-box-2d)))))
     ,@body))



