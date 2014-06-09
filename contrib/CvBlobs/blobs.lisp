
(in-package #:cv)

(let* ((lib-dir (su:cat (su:full-pathname (asdf::system-source-directory :cvblob)) "C/"))
       (file (su:cat lib-dir "libcvblob_wrap")))
  (unless (probe-file file)
    (su:run-program (su:cat "make -C " lib-dir) :output t :wait t))
  #+linux (cffi:load-foreign-library "/usr/local/lib/libcvblob.so")
  (cffi:load-foreign-library file))

(cffi:defcenum :blob-render-color-enum
  (:cv-blob-render-color 1)
  (:cv-blob-render-centroid 2)
  (:cv-blob-render-bounding-box 4)
  (:cv-blob-render-angle 8)
  (:cv-blob-render-to-log 16)
  (:cv-blob-render-to-std 32))



(defclass cv-blob ()
  ((ref :initarg :ref :accessor ref)))

(defclass cv-blobs ()
  ((ref :initarg :ref :accessor ref)))


(defun make-cv-blobs ()
  (make-instance 'cv-blobs :ref (cffi:foreign-funcall "make_cv_blobs" :pointer)))

(defmethod destry-cv-blobs ((blobs cv-blobs))
  (cffi:foreign-funcall "destroy_cv_blobs" :pointer (ref blobs)))

(defmacro with-cv-blobs ((name) &body body)
  `(let ((,name (make-cv-blobs)))
     (unwind-protect (progn ,@body)
       (destry-cv-blobs ,name))))

(export 'with-cv-blobs :cv)

(define-method label ((img ipl-image) (img-out ipl-image) (blobs cv-blobs))
  (cffi:foreign-funcall "cv_Label" :pointer (ref img) :pointer (ref img-out)
				   :pointer (ref blobs) :unsigned-int))

(define-method filter-labels ((img-in ipl-image) (img-out ipl-image) (blobs cv-blobs))
  (cffi:foreign-funcall "cv_FilterLabels" :pointer (ref img-in) :pointer (ref img-out)
			:pointer (ref blobs)))

(define-method get-label ((img ipl-image) x y)
  (cffi:foreign-funcall "cv_GetLabel" :pointer (ref img)
				      :unsigned-int (floor x)
				      :unsigned-int (floor y)
				      :unsigned-int))

(define-method release-blobs ((blobs cv-blobs))
  (cffi:foreign-funcall "cv_ReleaseBlobs" :pointer (ref blobs)))

(define-method largest-blob ((blobs cv-blobs))
  (cffi:foreign-funcall "cv_LargestBlob" :pointer (ref blobs) :unsigned-int))

(define-method filter-by-area ((blobs cv-blobs) min-area max-area)
  (cffi:foreign-funcall "cv_FilterByArea" :pointer (ref blobs)
					  :unsigned-int (floor min-area)
					  :unsigned-int (floor max-area)))

(define-method filter-by-label ((blobs cv-blobs) label)
  (cffi:foreign-funcall "cv_FilterByLabel" :pointer (ref blobs)
					   :unsigned-int (floor label)))

(define-method centroid ((blob cv-blob))
  (cffi:with-foreign-object (point 'cvpoint2d64f)
    (cffi:foreign-funcall "cv_Centroid" :pointer (ref blob)
					:pointer point)
    (cffi:with-foreign-slots ((x y) point CvPoint2D64f)
      (point-2d-64f x y))))

(define-method angle ((blob cv-blob))
  (cffi:foreign-funcall "cvAngle" :pointer (ref blob) :double))

(define-method render-blob ((img-label ipl-image) (blob cv-blob) (img-src ipl-image)
			    (img-dst ipl-image) &optional (mode 15) (color (scalar 255 255 255))
			    (alpha 1.))
  (with-cv-scalar (native-scalar color)
    (cffi:foreign-funcall "cv_RenderBlob" :pointer (ref img-label)
					  :pointer (ref blob)
					  :pointer (ref img-src)
					  :pointer (ref img-dst)
					  :unsigned-short (if (numberp mode) (floor mode) (cffi:foreign-enum-value :blob-render-color-enum mode))
					  :pointer native-scalar
					  :double (* 1.0d0 alpha))))

(define-method render-blobs ((img-label ipl-image) (blobs cv-blobs) (img-src ipl-image)
			     (img-dst ipl-image) &optional (mode 15 )  (alpha 1.))
  (cffi:foreign-funcall "cv_RenderBlobs" :pointer (ref img-label)
					 :pointer (ref blobs)
					 :pointer (ref img-src)
					 :pointer (ref img-dst)
					 :unsigned-short (if (numberp mode) (floor mode) (cffi:foreign-enum-value :blob-render-color-enum mode))
					 :double (* 1.0d0 alpha)))

(define-method set-image-roi-to-blob ((img ipl-image) (blob cv-blob))
  (cffi:foreign-funcall "cv_SetImageROItoBlob" :pointer (ref img)
					       :pointer (ref blob)))

(define-method blob-mean-color ((blob cv-blob) (img-label ipl-image) (img ipl-image))
  (let ((color nil))
    (cffi:with-foreign-object (native-scalar 'CvScalar)
      (cffi:foreign-funcall "cv_BlobMeanColor" :pointer (ref blob)
					       :pointer (ref img-label)
					       :pointer (ref img)
					       :pointer native-scalar)
      (cffi:with-foreign-slots ((val) native-scalar CvScalar)
	(dotimes (i 4)
	  (push (cffi:mem-aref val :double i) color)))
      (apply #'scalar (nreverse color)))))

(define-method dot-product-points ((a cv-point) (b cv-point) (c cv-point))
  (with-cv-point (native-a a)
    (with-cv-point (native-b b)
      (with-cv-point (native-c c)
	(cffi:foreign-funcall "cv_DotProductPoints" :pointer native-a
						    :pointer native-b
						    :pointer native-c
						    :double)))))

(define-method cross-product-points ((a cv-point) (b cv-point) (c cv-point))
  (with-cv-point (native-a a)
    (with-cv-point (native-b b)
      (with-cv-point (native-c c)
	(cffi:foreign-funcall "cv_CrossProductPoints" :pointer native-a
						      :pointer native-b
						      :pointer native-c
						      :double)))))

(define-method distance-point-point ((a cv-point) (b cv-point))
  (with-cv-point (native-a a)
    (with-cv-point (native-b b)
      (cffi:foreign-funcall "cv_DistancePointPoint" :pointer native-a
						    :pointer native-b
						    :double))))


(define-method distance-line-point ((a cv-point) (b cv-point) (c cv-point) &optional (is-segment t))
  (with-cv-point (native-a a)
    (with-cv-point (native-b b)
      (with-cv-point (native-c c)
	(cffi:foreign-funcall "cv_DistanceLinePoint" :pointer native-a
						     :pointer native-b
						     :pointer native-c
						     :int (if is-segment 1 0)
						     :double)))))


(defmethod do-cv-blobs-first ((blobs cv-blobs))
  (let ((blob (cffi:foreign-funcall "do_cv_blobs_first" :pointer (ref blobs)
							:pointer)))
    (if blob (make-instance 'cv-blob :ref blob)
	nil)))

(defmethod do-cv-blobs-next ()
  (let ((blob (cffi:foreign-funcall "do_cv_blobs_next" :pointer)))
    (if blob (make-instance 'cv-blob :ref blob) nil)))

(defmacro do-blobs ((name blobs) &body body)
  `(do ((,name (do-cv-blobs-first ,blobs) (do-cv-blobs-next)))
       ((not ,name) nil)
     ,@body))

(export 'do-blobs :cv)





;;;

(cffi:defcenum :track-render-enum
  (:CV-TRACK-RENDER-ID            #x0001)
  (:CV-TRACK-RENDER-BOUNDING-BOX  #x0002)
  (:CV-TRACK-RENDER-TO-LOG        #x0010)
  (:CV-TRACK-RENDER-TO-STD        #x0020))


(defclass cv-tracks ()
  ((ref :initarg :ref :accessor ref)))

(defun make-cv-tracks ()
  (make-instance 'cv-tracks :ref (cffi:foreign-funcall "make_cv_tracks" :pointer)))

(defmethod destroy-cv-tracks ((tracks cv-tracks))
  (cffi:foreign-funcall "destroy_cv_tracks" :pointer (ref tracks)))

(defmacro with-cv-tracks ((name) &body body)
  `(let ((,name (make-cv-tracks)))
     (unwind-protect (progn ,@body)
       (destroy-cv-tracks ,name))))

(export 'with-cv-tracks :cv)

(define-method release-tracks ((tracks cv-tracks))
  (cffi:foreign-funcall "cv_ReleaseTracks" :pointer (ref tracks)))

(define-method update-tracks ((blobs cv-blobs) (tracks cv-tracks) th-distance th-inactive
			      &optional  (th-active 0))
  (cffi:foreign-funcall "cv_UpdateTracks" :pointer (ref blobs)
					  :pointer (ref tracks)
					  :double (* 1.0d0 th-distance)
					  :unsigned-int (floor th-inactive)
					  :unsigned-int (floor th-active)))

(define-method render-tracks ((tracks cv-tracks) (img-src ipl-image) (img-dst ipl-image) &optional (mode 15))
  (cffi:foreign-funcall "cv_RenderTracks" :pointer (ref tracks)
					  :pointer (ref img-src)
					  :pointer (ref img-dst)
					  :unsigned-short
					  (apply #'logior (mapcar #'(lambda (md)
								      (if (numberp md) md
									  (cffi:foreign-enum-value :track-render-enum md))) (su:mklist mode)))
					  :pointer  (cffi-sys:null-pointer)))
