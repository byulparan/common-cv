(in-package #:cv)

(defun named-window (name &optional (flags :cv-window-autosize))
  (cffi:foreign-funcall "cvNamedWindow" :string name
					:int (cffi:foreign-enum-value :named-window-enum flags)))

(defmacro with-named-window ((name &optional (flags :cv-window-autosize)) &body body)
  `(unwind-protect (progn (named-window ,name ,flags)
			  ,@body)
     (cffi:foreign-funcall "cvDestroyWindow" :string ,name)))

(defmethod show-image ((name string) (image ipl-image))
  (cffi:foreign-funcall "cvShowImage" :string name :pointer (ref image)))

(defun wait-key (delay)
  (cffi:foreign-funcall "cvWaitKey" :int delay :int))


;;; Event
(defvar *cv-callback* nil)
(cffi:defcallback mouse-callback :void ((event :int) (x :int) (y :int)
					(flags :int) (param :pointer))
  (declare (ignore param))
  (funcall *cv-callback* (cffi:foreign-enum-keyword :mouse-event-enum event) x y flags))

(defmethod set-mouse-callback ((name string) (callback function))
  (setf *cv-callback* callback)
  (cffi:foreign-funcall "cvSetMouseCallback" :string name :pointer (cffi:callback mouse-callback)
			:pointer (cffi-sys:null-pointer)))

(defun event-decode (&rest flags)
  (apply #'+ (mapcar #!(cffi:foreign-enum-value :mouse-event-flags-enum %1) flags)))






(defclass cv-capture ()
  ((ref :initarg :ref :accessor ref)))

(defun create-camera-capture (index)
  (let ((camera (cffi:foreign-funcall "cvCreateCameraCapture" :int index :pointer)))
    (assert (cl:not (cffi-sys:null-pointer-p camera)) nil "not found device index ~d" index)
    (make-instance 'cv-capture :ref camera)))

(defun create-file-capture (path)
  (let ((ref (cffi:foreign-funcall "cvCreateFileCapture" :string (su:full-pathname path) :pointer)))
    (assert (cl:not (cffi-sys:null-pointer-p ref)) nil "not load file ~s" path)
    (make-instance 'cv-capture :ref ref)))

(defmethod release-capture ((capture cv-capture))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) (ref capture))
    (cffi:foreign-funcall "cvReleaseCapture" :pointer ptr))
  (setf (ref capture) nil))

(defmethod get-capture-property ((cv-capture cv-capture) (property-id symbol))
  (cffi:foreign-funcall "cvGetCaptureProperty" :pointer (ref cv-capture)
			:int (cffi:foreign-enum-value :cap-prop-enum property-id) :double))

(defmethod set-capture-property ((cv-capture cv-capture) (property-id symbol) (value number))
  (cffi:foreign-funcall "cvSetCaptureProperty" :pointer (ref cv-capture)
			:int (cffi:foreign-enum-value :cap-prop-enum property-id)
			:double (* 1.0d0 value) :int))

(defmethod query-frame ((capture cv-capture))
  (let ((ref (cffi:foreign-funcall "cvQueryFrame" :pointer (ref capture) :pointer)))
    (when (cffi-sys:null-pointer-p ref)
      (set-capture-property capture :cv-cap-prop-pos-frame 1)
      (setf ref (cffi:foreign-funcall "cvQueryFrame" :pointer (ref capture) :pointer)))
    (reset-info-ipl-image (make-instance 'ipl-image :ref ref))))

(defmacro with-captured-camera ((name dev-index &key width height) &body body)
  `(let ((,name (create-camera-capture ,dev-index)))
     (when ,width
       (set-capture-property ,name :cv-cap-prop-frame-width ,width))
     (when ,height
       (set-capture-property ,name :cv-cap-prop-frame-height ,height))
     (unwind-protect (progn ,@body)
       (release-capture ,name))))

(defmacro with-captured-file ((name file-path &key width height) &body body)
  `(let ((,name (create-file-capture ,file-path)))
     (when ,width
       (set-capture-property ,name :cv-cap-prop-frame-width ,width))
     (when ,height
       (set-capture-property ,name :cv-cap-prop-frame-height ,height))
     (unwind-protect (progn ,@body)
       (release-capture ,name))))




(defmethod convert-image ((src cv-arr) (dst cv-arr) &optional (flags 0))
  (cffi:foreign-funcall "cvConvertImage" :pointer (ref src) :pointer (ref dst)
			:int (case flags
			       (0 0)
			       (otherwise (cffi:foreign-enum-value :convert-image-enum flags)))))

(defmethod load-image ((path string) (iscolor symbol))
  (cffi:with-foreign-string (file-path (su:full-pathname path))
    (let* ((native-ipl-image (cffi:foreign-funcall "cvLoadImage" :pointer file-path
								 :int (cffi:foreign-enum-value
								       :ipl-load-image-enum iscolor)
								 :pointer)))
      (assert (cl:not (cffi-sys:null-pointer-p native-ipl-image)) nil "can't load image ~s" path)
      (let ((ipl-image (make-instance 'ipl-image :ref native-ipl-image)))
	(reset-info-ipl-image ipl-image)))))


(defmethod load-file ((path string))
  (let ((file (cffi:with-foreign-string (c-path (su:full-pathname path))
		(cffi:foreign-funcall "cvLoad" :pointer c-path
					       :pointer (cffi-sys:null-pointer)
					       :pointer (cffi-sys:null-pointer)
					       :pointer (cffi-sys:null-pointer)
					       :pointer))))
    (assert (cl:not (cffi:null-pointer-p file)) nil "can't load file ~s" path)
    file))

(defmethod load-mat ((path string))
  (reset-info-mat (make-instance 'cv-mat :ref (load-file path))))

(defmethod load-haar-cascade ((path string))
  (make-instance 'cv-haar-classifier-cascade :ref (load-file path)))


(define-method line ((array cv-arr) (pt1 cv-point) (pt2 cv-point) (color cv-scalar)
			&optional (thickness 1) (line-type 8) (shift 0))
  (with-cv-scalar (native-color color)
    (with-cv-point (native-pt1 pt1)
      (with-cv-point (native-pt2 pt2)
	(cffi:foreign-funcall "cv_Line" :pointer (ref array) :pointer native-pt1 :pointer native-pt2
			      :pointer native-color :int (floor thickness) :int (floor line-type)
			      :int (floor shift))))))


(define-method rectangle ((array cv-arr) (pt1 cv-point) (pt2 cv-point) (color cv-scalar)
			     &optional (thickness 1) (line-type 8) (shift 0))
  (with-cv-scalar (native-color color)
    (with-cv-point (native-pt1 pt1)
      (with-cv-point (native-pt2 pt2)
	(cffi:foreign-funcall "cv_Rectangle" :pointer (ref array) :pointer native-pt1
			      :pointer native-pt2 :pointer  native-color
			      :int (floor thickness) :int (floor line-type) :int (floor shift))))))

(define-method cirlce ((array cv-arr) (center cv-point) (radius number) (color cv-scalar)
			  &optional (thickness 1) (line-type 8) (shift 0))
  (with-cv-point (native-center center)
    (with-cv-scalar (native-color color)
      (cffi:foreign-funcall "cv_Circle" :pointer (ref array) :pointer native-center
			    :int (floor radius) :pointer native-color
			    :int (floor thickness) :int (floor line-type) :int (floor shift)))))

(define-method ellipse ((img cv-arr) (center cv-point) (axes cv-size)
			   (angle number) (start-angle number) (end-angle number)
			   (color cv-scalar) &optional (thickness 1) (line-type 8)
			   (shift 0))
  (with-cv-point (native-center center)
    (with-cv-size (native-axes axes)
      (with-cv-scalar (native-color color)
	(cffi:foreign-funcall "cv_Ellipse" :pointer (ref img) :pointer native-center
			      :pointer native-axes :double (* 1.0d0 angle)
			      :double (* 1.0d0 start-angle) :double (* 1.0d0 end-angle)
			      :pointer native-color :int (floor thickness) :int (floor line-type)
			      :int (floor shift))))))


(define-method ellipse-box ((img cv-arr) (box cv-box-2d) (color cv-scalar)
			       &optional (thickness 1) (line-type 8) (shift 0))
  (with-cv-box-2d (native-box box)
    (with-cv-scalar (native-color color)
      (cffi:foreign-funcall "cv_EllipseBox" :pointer (ref img) :pointer native-box
			    :pointer native-color :int (floor thickness)
			    :int (floor line-type) :int (floor shift)))))



(define-method fill-poly ((img cv-arr) (pts list) (npts list) (contours integer)
			     (color cv-scalar) &optional (line-type 8) (shift 0))
  (cffi:with-foreign-object (npts-array :int contours)
    (dotimes (i contours)
      (setf (cffi:mem-aref npts-array :int i) (nth i npts)))
    (let ((point-objs (alexandria:flatten pts))
	  (count (apply #'+ npts)))
      (cffi:with-foreign-object (cv-point-arr '(:struct CvPoint) count)
	(dotimes (i count)
	  (cffi:with-foreign-slots ((x y) (cffi:mem-aptr cv-point-arr '(:struct CvPoint) i) (:struct CvPoint))
	    (setf x (x (nth i point-objs))
		  y (y (nth i point-objs)))))
	(cffi:with-foreign-object (point-array :pointer contours)
	  (labels ((up-count (lst n)
		     (if (zerop n) 0
			 (apply '+ (subseq lst 0 n)))))
	    (dotimes (i contours)
	      (setf (cffi:mem-aref point-array :pointer i) (cffi-sys:inc-pointer cv-point-arr
										 (* (cffi:foreign-type-size 'CvPoint)
										    (up-count npts i))))))
	  (with-cv-scalar (native-color color)
	    (cffi:foreign-funcall "cv_FillPoly" :pointer (ref img) :pointer point-array
				  :pointer npts-array :int contours :pointer native-color
				  :int (floor line-type) :int (floor shift))))))))


(define-method fill-convex-poly ((img cv-arr) (pts list) (npts integer)
				 (color cv-scalar) &optional (line-type 8) (shift 0))
  (cffi:with-foreign-object (pts-array '(:struct CvPoint) npts)
    (dotimes (i npts)
      (cffi:with-foreign-slots ((x y) (cffi:mem-aptr pts-array '(:struct CvPoint) i) (:struct CvPoint)) 
	(setf x (x (nth i pts))
	      y (y (nth i pts)))))
    (with-cv-scalar (native-color color)
       (cffi:foreign-funcall "cv_FillConvexPoly" :pointer (ref img) :pointer pts-array
						 :int npts :pointer native-color :int (floor line-type) :int (floor shift)))))





(define-method poly-line ((img cv-arr) (pts list) (npts list) (contours integer)
			     (is-closed integer) (color cv-scalar) &optional (thickness 1) (line-type 8) (shift 0))
  (cffi:with-foreign-object (npts-array :int contours)
    (dotimes (i contours)
      (setf (cffi:mem-aref npts-array :int i) (nth i npts)))
    (let ((point-objs (alexandria:flatten pts))
	  (count (apply #'+ npts)))
      (cffi:with-foreign-object (cv-point-arr '(:struct CvPoint) count)
	(dotimes (i count)
	  (cffi:with-foreign-slots ((x y) (cffi:mem-aptr cv-point-arr '(:struct CvPoint) i) (:struct CvPoint))
	    (setf x (x (nth i point-objs))
		  y (y (nth i point-objs)))))
	(cffi:with-foreign-object (point-array :pointer contours)
	  (labels ((up-count (lst n)
		     (if (zerop n) 0
			 (apply '+ (subseq lst 0 n)))))
	    (dotimes (i contours)
	      (setf (cffi:mem-aref point-array :pointer i) (cffi-sys:inc-pointer cv-point-arr
										 (* (cffi:foreign-type-size 'CvPoint)
										    (up-count npts i))))))
	  (with-cv-scalar (native-color color)
	    (cffi:foreign-funcall "cv_PolyLine" :pointer (ref img) :pointer point-array
				  :pointer npts-array :int contours :int is-closed
				  :pointer native-color :int (floor thickness) :int (floor line-type)
				  :int (floor shift))))))))

(defmethod put-text ((img cv-arr) (text string) (origin cv-point) (color cv-scalar)
			(font-face symbol) &optional (hscale 1) (vscale 1)
					      (shear 0) (thickness 1) (line-type 8))
  (with-cv-point (native-origin origin)
    (with-cv-scalar (native-color color)
      (cffi:foreign-funcall "cv_PutText" :pointer  (ref img) :string text
					 :pointer native-origin :pointer native-color
					 :int (cffi:foreign-enum-value :font-face-enum font-face)
					 :double (* 1.0d0 hscale) :double (* 1.0d0 vscale)
					 :double (* 1.0d0 shear) :int (floor thickness)
					 :int (floor line-type)))))





