
(in-package #:cv)

(cffi:defcstruct CvPoint
  (x :int)
  (y :int))

(defclass cv-point ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defun point (x y)
  (make-instance 'cv-point :x (floor x)
			   :y (floor y)))


(cffi:defcstruct CvPoint2D32f
  (x :float)
  (y :float))

(defclass cv-point-2d-32f ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defun point-2d-32f (x y)
  (make-instance 'cv-point-2d-32f :x (coerce x 'single-float)
				  :y (coerce y 'single-float)))

(cffi:defcstruct CvPoint2D64f
  (x :double)
  (y :double))

(defclass cv-point-2d-64f ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defun point-2d-64f (x y)
  (make-instance 'cv-point-2d-64f :x (* 1.0d0 x)
				  :y (* 1.0d0 y)))


;;; Size
(cffi:defcstruct CvSize
  (width :int)
  (height :int))

(defclass cv-size ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defun size (width height)
  (make-instance 'cv-size :width (floor width)
			  :height (floor height)))


(cffi:defcstruct CvSize2D32f
  (width :float)
  (height :float))

(defclass cv-size-2d-32f ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))


(defun size-2d-32f (width height)
  (make-instance 'cv-size :width (coerce width 'single-float)
			  :height (coerce height 'single-float)))

;;; CvBox2D
(cffi:defcstruct CvBox2D
  (center (:struct CvPoint2D32f))
  (size (:struct CvSize2D32f))
  (angle :float))

(defclass cv-box-2d ()
  ((box-center :initarg :box-center :accessor box-center)
   (box-size :initarg :box-size :accessor box-size)
   (box-angle :initarg :box-angle :accessor box-angle)))


(defun box-2d (x y width height angle)
  (make-instance 'cv-box-2d :box-center (point-2d-32f x y)
			    :box-size (size-2d-32f width height)
			    :box-angle (float angle)))




;;; cv-rect
(cffi:defcstruct CvRect
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defclass cv-rect ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defun rect (x y width height)
  (make-instance 'cv-rect :x (floor x)
			  :y (floor y)
			  :width (floor width)
			  :height (floor height)))



;;; cv-scalar
(cffi:defcstruct CvScalar
  (val :double :count 4))

(defclass cv-scalar ()
  ((val :initarg :val :accessor val)))

(defmethod scalar ((val0 number) &optional (val1 0) (val2 0) (val3 0))
  (make-instance 'cv-scalar :val (mapcar #!(* 1.0d0 %1) (list val0 val1 val2 val3))))

(defmethod scalar-all ((val0123 number))
  (let ((val0123 (* 1.0d0 val0123)))
    (make-instance 'cv-scalar :val (list val0123
					 val0123
					 val0123
					 val0123))))

(defmethod real-scalar ((val0 number))
  (make-instance 'cv-scalar :val (list (* 1.0d0 val0) 0 0 0)))






;;;
(defclass cv-arr ()
  ())

(defclass cv-mat (cv-arr)
  ((type :initarg :type :accessor type)
   (steps :initarg :steps :accessor steps)
   (data :initarg :data :accessor data)
   (rows :initarg :rows :accessor rows)
   (cols :initarg :cols :accessor cols)
   (ref :initarg :ref :accessor ref)))


(defmethod reset-info-mat ((cv-mat cv-mat))
  (with-slots (type steps data rows cols ref) cv-mat
    (setf type (cffi:foreign-enum-keyword :mat-type-enum (cffi:foreign-funcall "cvGetElemType" :pointer  ref :int))
	  steps (cffi:foreign-funcall "get_mat_step" :pointer ref :int)
	  rows (cffi:foreign-funcall "get_mat_rows" :pointer ref :int)
	  cols (cffi:foreign-funcall "get_mat_cols" :pointer ref :int)
	  data (cffi:foreign-funcall "get_mat_data" :pointer ref :pointer)))
  cv-mat)

(defmethod (setf data) ((data-list list) (mat cv-mat))
  (let* ((type (if (eql (type mat) :cv-32fc1) :float :double))
	 (number-to-ptr (if (eql type :float) 'single-float 'double-float))
	 (mem (data mat)))
    (dotimes (i (length data-list))
      (setf (cffi:mem-aref mem type i) (coerce (nth i data-list) number-to-ptr)))))

(defmethod create-mat ((rows integer) (cols integer) (type symbol))
  (let* ((native-mat
	   (cffi:foreign-funcall "cvCreateMat" :int rows
					       :int cols
					       :int (cffi:foreign-enum-value :mat-type-enum type)
					       :pointer))
	 (mat (make-instance 'cv-mat :ref native-mat :type type)))
    (reset-info-mat mat)
    mat))

(defmethod release-mat ((mat cv-mat))
  (cffi:with-foreign-object (data :pointer)
    (setf (cffi:mem-ref data :pointer) (ref mat))
    (cffi:foreign-funcall "cvReleaseMat" :pointer data)))


;;;

(defclass ipl-image (cv-arr)
  ((n-size :initarg :n-size :initform nil :accessor n-size)
   (id :initarg :id :initform nil :accessor id)
   (n-channels :initarg :n-channels :initform nil :accessor n-channels)
   (alpha-channel :initarg :alpha-channel :initform nil :accessor alpha-channel)
   (depth :initarg :depth :initform nil :accessor depth)
   (data-order :initarg :data-order :initform nil :accessor data-order)
   (origin :initarg :origin :initform nil :accessor origin)
   (align :initarg :align :initform nil :accessor align)
   (width :initarg :width :initform nil :accessor width)
   (height :initarg :height :initform nil :accessor height)
   (image-size :initarg :image-size :initform nil :accessor image-size)
   (image-data :initarg :image-data :initform nil :accessor image-data)
   (width-step :initarg :width-step :initform nil :accessor width-step)
   (ref :initarg :ref :initform nil :accessor ref)))

(defmethod reset-info-ipl-image ((ipl-image ipl-image))
  (with-slots (n-size id n-channels alpha-channel depth data-order
		       origin align width height image-size image-data width-step ref) ipl-image
    (setf n-size (cffi:foreign-funcall "get_nSize" :pointer ref :int)
	  id (cffi:foreign-funcall "get_ID" :pointer ref :int)
	  n-channels (cffi:foreign-funcall "get_nChannels" :pointer ref :int)
	  alpha-channel (cffi:foreign-funcall "get_alphaChannel" :pointer ref :int)
	  depth (cffi:foreign-enum-keyword :ipl-depth-enum
					   (cffi:foreign-funcall "get_depth" :pointer ref :int))
	  data-order (cffi:foreign-enum-keyword :ipl-data-order-enum
						(cffi:foreign-funcall "get_dataOrder"
								      :pointer ref :int))
	  origin (cffi:foreign-enum-keyword :ipl-origin-enum
					    (cffi:foreign-funcall "get_origin" :pointer ref :int))
	  align (cffi:foreign-enum-keyword :ipl-align-enum
					   (cffi:foreign-funcall "get_align" :pointer ref :int))
	  width (cffi:foreign-funcall "get_width" :pointer ref :int)
	  height (cffi:foreign-funcall "get_height" :pointer ref :int)
	  image-size (cffi:foreign-funcall "get_imageSize" :pointer ref :int)
	  image-data (cffi:foreign-funcall "get_imageData" :pointer ref :pointer)
	  width-step (cffi:foreign-funcall "get_widthStep" :pointer ref :int)))
  ipl-image)


(defmethod create-image ((size cv-size) (depth symbol) (channels integer))
  (cffi:with-foreign-object (native-cv-size '(:struct CvSize))
    (cffi:with-foreign-slots ((width height) native-cv-size (:struct cvsize))
      (setf width (width size)
	    height (height size)) 
      (let* ((native-ipl-image
	       (cffi:foreign-funcall "cv_CreateImage" :pointer native-cv-size
						      :int (cffi:foreign-enum-value :ipl-depth-enum depth)
						      :int channels :pointer))
	     (ipl-image (make-instance 'ipl-image :ref native-ipl-image)))
	  (reset-info-ipl-image ipl-image)))))


(defmacro with-ipl-image (bind &body body)
  `(let* ,(mapcar #!(cons (car %1) (list (cons 'create-image (cdr %1)))) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(release-image %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmethod create-image-header ((size cv-size) (depth symbol) (channels integer))
  (cffi:with-foreign-object (native-cv-size '(:struct cvsize))
    (cffi:with-foreign-slots ((width height) native-cv-size (:struct cvsize))
      (setf width (width size)
	    height (height size))
      (let* ((native-ipl-image
	       (cffi:foreign-funcall "cv_CreateImageHeader" :pointer native-cv-size
							    :int (cffi:foreign-enum-value :ipl-depth-enum depth)
							    :int channels :pointer)))
	(make-instance 'ipl-image :width (width size)
				  :height (height size)
				  :n-size size
				  :n-channels channels
				  :depth depth
				  :ref native-ipl-image)))))





(defmethod release-image ((ipl-image ipl-image))
  (cffi:with-foreign-object (ipl-ref :pointer)
    (setf (cffi:mem-ref ipl-ref :pointer) (ref ipl-image))
    (cffi:foreign-funcall "cvReleaseImage" :pointer ipl-ref)
    (setf (image-data ipl-image) (cffi-sys:null-pointer))
    (setf (ref ipl-image) nil)))

(defmethod set-image-roi ((ipl-image ipl-image) (rect cv-rect))
  (cffi:with-foreign-object (r '(:struct cvrect))
    (cffi:with-foreign-slots ((x y width height) r (:struct cvrect))
      (setf x (x rect)
	    y (y rect)
	    width (width rect)
	    height (height rect))
      (cffi:foreign-funcall "cv_SetImageROI" :pointer (ref ipl-image) :pointer r))))

(defmethod reset-image-roi ((ipl-image ipl-image))
  (cffi:foreign-funcall "cvResetImageROI" :pointer (ref ipl-image)))

(defmacro with-image-roi ((ipl-image rect) &body body)
  `(unwind-protect (progn
		     (set-image-roi ,ipl-image ,rect)
		     ,@body)
     (reset-image-roi ,ipl-image)))

(defmethod (setf origin) ((origin symbol) (ipl-image ipl-image))
  (with-slots ((origin-src origin) ref) ipl-image
    (cffi:foreign-funcall "set_origin" :pointer ref
				       :int (cffi:foreign-enum-value :ipl-origin-enum origin))
    (setf origin-src origin)))

(defmethod (setf width-step) ((width-step integer) (ipl-image ipl-image))
  (with-slots ((width-step-src width-step) ref) ipl-image
    (cffi:foreign-funcall "set_widthStep" :pointer ref :int width-step)
    (setf width-step-src width-step)))

(defmethod (setf image-data) ((image-data #+ccl ccl::macptr
					  #+sbcl sb-sys:system-area-pointer
					  #+ecl si:foreign-data)
			      (ipl-image ipl-image))
  (with-slots ((image-data-src image-data) ref) ipl-image
    (cffi:foreign-funcall "set_imageData" :pointer ref :pointer image-data)
    (setf image-data-src image-data)))


(defmethod (setf depth) ((depth symbol) (ipl-image ipl-image))
  (let ((depth-num (cffi:foreign-enum-value :ipl-depth-enum depth)))
    (with-slots ((depth-src depth) ref) ipl-image
      (cffi:foreign-funcall "set_depth" :pointer ref :int depth-num)
      (setf depth-src depth))))

(defmethod (setf width) ((width integer) (ipl-image ipl-image))
  (with-slots ((width-src width) ref) ipl-image
    (cffi:foreign-funcall "set_width" :pointer ref :int width)
    (setf width-src width)))

(defmethod (setf height) ((height integer) (ipl-image ipl-image))
  (with-slots ((height-src height) ref) ipl-image
    (cffi:foreign-funcall "set_height" :pointer ref :int height)
    (setf height-src height)))

(defmethod (setf n-channels) ((n-channels integer) (ipl-image ipl-image))
  (with-slots ((chan-src n-channels) ref) ipl-image
    (cffi:foreign-funcall "set_nChannels" :pointer ref :int n-channels)
    (setf chan-src n-channels)))

(defmethod (setf image-size) ((img-size integer) (ipl-image ipl-image))
  (with-slots (image-size ref) ipl-image
    (cffi:foreign-funcall "set_imageSize" :pointer ref :int img-size)))


;;;
(define-method get1d ((arr cv-arr) idx0)
  (let ((scalar-value nil))
    (cffi:with-foreign-object (native-scalar '(:struct CvScalar))
      (cffi:foreign-funcall "cv_Get1D" :pointer (ref arr)
				       :int (floor idx0)
				       :pointer native-scalar)
      (cffi:with-foreign-slots ((val) native-scalar (:struct CvScalar))
	(dotimes (i 4)
	  (push (cffi:mem-aref val :double i) scalar-value))))
    (apply #'scalar (nreverse scalar-value))))

(define-method get2d ((arr cv-arr) idx0 idx1)
  (let ((scalar-value nil))
    (cffi:with-foreign-object (native-scalar '(:struct CvScalar))
      (cffi:foreign-funcall "cv_Get2D" :pointer (ref arr)
				       :int (floor idx0)
				       :int (floor idx1)
				       :pointer native-scalar)
      (cffi:with-foreign-slots ((val) native-scalar (:struct CvScalar))
	(dotimes (i 4)
	  (push (cffi:mem-aref val :double i) scalar-value))))
    (apply #'scalar (nreverse scalar-value))))

(define-method get3d ((arr cv-arr) idx0 idx1 idx2)
  (let ((scalar-value nil))
    (cffi:with-foreign-object (native-scalar '(:struct CvScalar))
      (cffi:foreign-funcall "cv_Get3D" :pointer (ref arr)
				       :int (floor idx0)
				       :int (floor idx1)
				       :int (floor idx2)
				       :pointer native-scalar)
      (cffi:with-foreign-slots ((val) native-scalar (:struct CvScalar))
	(dotimes (i 4)
	  (push (cffi:mem-aref val :double i) scalar-value))))
    (apply #'scalar (nreverse scalar-value))))

(define-method set1d ((arr cv-arr) idx0 (scalar cv-scalar))
  (with-cv-scalar (native-scalar scalar)
    (cffi:foreign-funcall "cv_Set1D" :pointer (ref arr)
				     :int (floor idx0)
				     :pointer native-scalar)))

(define-method set2d ((arr cv-arr) idx0 idx1 (scalar cv-scalar))
  (with-cv-scalar (native-scalar scalar)
    (cffi:foreign-funcall "cv_Set2D" :pointer (ref arr)
				     :int (floor idx0)
				     :int (floor idx1)
				     :pointer native-scalar)))

(define-method set3d ((arr cv-arr) idx0 idx1 idx2 (scalar cv-scalar))
  (with-cv-scalar (native-scalar scalar)
    (cffi:foreign-funcall "cv_Set3D" :pointer (ref arr)
				     :int (floor idx0)
				     :int (floor idx1)
				     :int (floor idx2)
				     :pointer native-scalar)))

(define-method cvmGet ((mat cv-mat) row col)
  (cffi:foreign-funcall "cv_mGet" :pointer (ref mat) :int (floor row) :int (floor col)
			:double))

(define-method cvmset ((mat cv-mat) row col value)
  (cffi:foreign-funcall "cv_mSet" :pointer (ref mat) :int (floor row) :int (floor col)
			:double (* 1.0d0 value)))

;;; 






;;; IPL-CONV-KERNEL
(defclass ipl-conv-kernel ()
  ((cols :initarg :cols :accessor cols)
   (rows :initarg :rows :accessor rows)
   (anchor-x :initarg :anchor-x :accessor anchor-x)
   (anchor-y :initarg :anchor-y :accessor anchor-y)
   (shape :initarg :shape :accessor shape)
   (valuess :initarg :valuess :initform nil :accessor valuess)
   (ref :initarg :ref :accessor ref)))

(define-method create-structuring-element-ex (cols rows anchor-x anchor-y shape &optional (values nil))
  (let ((ptr
	  (if (cl:and values (eql shape :cv-shape-custom))
	      (cffi:with-foreign-object (value-array :int (length values))
		(dotimes (i (length values))
		  (setf (cffi:mem-aref value-array :int) (nth i values)))
		(cffi:foreign-funcall "cvCreateStructuringElementEx" :int (floor cols) :int (floor rows) :int (floor anchor-x) :int (floor anchor-y)
				      :int(cffi:foreign-enum-value :conv-kernel-shape-enum shape)
				      :pointer value-array :pointer))
	      (cffi:foreign-funcall "cvCreateStructuringElementEx" :int (floor cols)
				    :int (floor rows)
				    :int  (floor anchor-x)
				    :int (floor anchor-y)
				    :int  (cffi:foreign-enum-value :conv-kernel-shape-enum shape)
				    :pointer (cffi-sys:null-pointer) :pointer))))
    (make-instance 'ipl-conv-kernel :cols (floor cols)
				    :rows (floor rows)
				    :anchor-x (floor anchor-x)
				    :anchor-y (floor anchor-y)
				    :shape shape
				    :valuess values
				    :ref ptr)))

(define-method release-structuring-element ((element ipl-conv-kernel))
  (cffi:with-foreign-object (ele :pointer)
    (setf (cffi:mem-ref ele :pointer) (ref element))
    (cffi:foreign-funcall "cvReleaseStructuringElement" :pointer ele)
    (setf (ref element) nil)))


;;; Histgram

(defclass cv-histogram ()
  ((dims :initarg :dims :accessor dims)
   (ref :initarg :ref :accessor ref)))

(define-method create-hist (dims sizes type &optional ranges (uniform 1))
  (cffi:with-foreign-object (native-size :int (length sizes))
    (dotimes (i (length sizes))
      (setf (cffi:mem-aref native-size :int i) (nth i sizes)))
    (when ranges
      (cffi:with-foreign-object (ranges-pointer :pointer (length ranges))
	(dotimes (i (length ranges))
	  (let ((elements (cffi:foreign-alloc :float :count (length (nth i ranges)))))
	    (dotimes (k (length (nth i ranges)))
	      (setf (cffi:mem-aref elements :float k) (coerce (nth k (nth i ranges)) 'single-float)))
	    (setf (cffi:mem-aref ranges-pointer :pointer i) elements)))
	(let ((ref (cffi:foreign-funcall "cvCreateHist"
					 :int (floor dims)
					 :pointer native-size
					 :int (cffi:foreign-enum-value :hist-enum type)
					 :pointer (if ranges ranges-pointer
						      (cffi-sys:null-pointer))
					 :int (floor uniform)
					 :pointer)))
	  (prog1
	      (make-instance 'cv-histogram :ref ref :dims dims)
	    (when ranges
	      (dotimes (i (length ranges))
		(cffi-sys:foreign-free (cffi:mem-aref ranges-pointer :pointer i))))))))))


(define-method clear-hist ((hist cv-histogram))
  (cffi:foreign-funcall "cvClearHist" :pointer (ref hist)))

(define-method release-hist ((hist cv-histogram))
  (cffi:with-foreign-object (object :pointer)
    (setf (cffi:mem-ref object :pointer) (ref hist))
    (cffi:foreign-funcall "cvReleaseHist" :pointer object)))







;;; CvMemStorage
(defclass cv-mem-storage ()
  ((ref :initarg :ref :accessor ref)))

(define-method create-mem-storage (&optional (block-size 0))
  (make-instance 'cv-mem-storage :ref
		 (cffi:foreign-funcall "cvCreateMemStorage" :int block-size :pointer)))

(define-method release-mem-storage ((storage cv-mem-storage))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) (ref storage))
    (cffi:foreign-funcall "cvReleaseMemStorage" :pointer ptr)
    (setf (ref storage) nil)))

(define-method clear-mem-storage ((storage cv-mem-storage))
  (cffi:foreign-funcall "cvClearMemStorage" :pointer (ref storage)))




;;; CvSeq
(defclass cv-seq ()
  ((ref :initarg :ref :accessor ref)))

(define-method create-seq (seq-flags header-size elem-size (storage cv-mem-storage))
  (labels ((flags ()
  	     (apply 'logior (mapcar #!(cffi:foreign-enum-value :cv-seq-type-enum %1) (su:mklist seq-flags)))))
    (make-instance 'cv-seq :ref
		   (cffi:foreign-funcall "cvCreateSeq" :int (flags)
						       :int (floor header-size)
						       :int (floor elem-size)
						       :pointer (ref storage)
						       :pointer))))

(define-method clear-seq ((seq cv-seq))
  (cffi:foreign-funcall "cvClearSeq" :pointer (ref seq)))



(define-method seq-push ((seq cv-seq) element)
  (cffi:foreign-funcall "cvSeqPush" :pointer (ref seq) :pointer element
			:pointer))

(define-method seq-push-front ((seq cv-seq) element)
  (cffi:foreign-funcall "cvSeqPushFront" :pointer (ref seq) :pointer element
			:pointer))

(define-method seq-pop ((seq cv-seq) element)
  (cffi:foreign-funcall "cvSeqPop" :pointer (ref seq) :pointer element))

(define-method seq-pop-front ((seq cv-seq) element)
  (cffi:foreign-funcall "cvSeqPopFront" :pointer (ref seq) :pointer element))

(define-method seq-push-multi ((seq cv-seq) elements count &optional (in-front 0))
  (cffi:foreign-funcall "cvSeqPushMulti" :pointer (ref seq) :pointer elements
			:int (floor count) :int (floor in-front)))

(define-method seq-pop-multi ((seq cv-seq) elements count &optional (in-front 0))
  (cffi:foreign-funcall "cvSeqPopMulti" :pointer (ref seq) :pointer elements
					:int (floor count) :int (floor in-front)))

(define-method seq-insert ((seq cv-seq) before-index element)
  (cffi:foreign-funcall "cvSeqInsert" :pointer (ref seq) :int (floor before-index)
			:pointer element :pointer))

(define-method seq-remove ((seq cv-seq) index)
  (cffi:foreign-funcall "cvSeqRemove" :pointer (ref seq) :int (floor index)))



(define-method get-seq-elem ((seq cv-seq) index)
  (cffi:foreign-funcall "cvGetSeqElem" :pointer (ref seq)
				       :int (floor index)
				       :pointer))

(define-method get-seq-elem-rect ((seq cv-seq) index)
  (let ((native-rect (get-seq-elem seq index)))
    (cffi:with-foreign-slots ((x y width height) native-rect (:struct CvRect))
      (rect x y width height))))

(define-method get-seq-elem-point ((seq cv-seq) index)
  (let ((native-point (get-seq-elem seq index)))
    (cffi:with-foreign-slots ((x y) native-point (:struct CvPoint))
      (point x y))))

(define-method pop-seq-point-2d-32f ((seq cv-seq))
  (cffi:with-foreign-object (native-point '(:struct CvPoint2D32f))
    (unless (zerop (total seq))
      (seq-pop-front seq native-point))
    (cffi:with-foreign-slots ((x y) native-point (:struct CvPoint2D32f))
      (point-2d-32f x y))))

(define-method seq-elem-idx ((seq cv-seq) element)
  (cffi:foreign-funcall "cvSeqElemIdx" :pointer (ref seq)
				       :pointer (ref element)
				       :pointer (cffi-sys:null-pointer)
				       :int))

(define-method clone-seq ((seq cv-seq) &optional storage)
  (let ((result (cffi:foreign-funcall "cvCloneSeq" :pointer (ref seq)
						   :pointer (if storage (ref storage) (cffi-sys:null-pointer))
						   :pointer)))
    (make-instance 'cv-seq :ref result)))



(define-method total ((seq cv-seq))
  (cffi:foreign-funcall "get_seq_total" :pointer (ref seq) :int))

(define-method next ((seq cv-seq))
  (let ((next (cffi:foreign-funcall "get_next" :pointer (ref seq)
					       :pointer)))
    (unless (cffi-sys:null-pointer-p next)
      (make-instance 'cv-seq :ref next))))


;;; 
(defclass cv-seq-writer ()
  ((ref :initarg :ref :accessor ref)))

(define-method start-write-seq (seq-flags header-size elem-size (storage cv-mem-storage))
  (make-instance 'cv-seq-writer :ref
		 (cffi:foreign-funcall "cv_StartWriteSeq" :int (floor seq-flags)
							  :int (floor header-size)
							  :int (floor elem-size)
							  :pointer (ref storage)
							  :pointer)))

(define-method start-append-to-seq ((seq cv-seq) (writer cv-seq-writer))
  (cffi:foreign-funcall "cvStartAppendToSeq" :pointer (ref seq) :pointer (ref writer)))

(define-method end-write-seq ((writer cv-seq-writer))
  (make-instance 'cv-seq :ref
		 (cffi:foreign-funcall "cvEndWriteSeq" :pointer (ref writer) :pointer)))

(define-method flush-seq-writer ((writer cv-seq-writer))
  (cffi:foreign-funcall "cvFlushSeqWriter" :pointer (ref writer)))

(define-method write-seq-elem-point ((point cv-point) (writer cv-seq-writer))
  (with-cv-point (native-point point)
    (cffi:foreign-funcall "cv_write_seq_elem_point" :pointer native-point
					      :pointer (ref writer))))

(define-method write-seq-elem-rect ((rect cv-rect) (writer cv-seq-writer))
  (cffi:with-foreign-object (native-rect '(:struct CvRect))
    (cffi:with-foreign-slots ((x y width height) native-rect (:struct CvRect))
      (setf x (x rect) y (y rect) width (width rect) height (height rect)))
    (cffi:foreign-funcall "cv_write_seq_elem_rect" :pointer native-rect
					      :pointer (ref writer))))


(defclass cv-seq-reader ()
  ((ref :initarg :ref :accessor ref)))

(define-method start-read-seq ((seq cv-seq) &optional (reverse 0))
  (make-instance 'cv-seq-reader :ref
		 (cffi:foreign-funcall "cv_StartReadSeq" :pointer (ref seq)
							 :int (floor reverse)
							 :pointer)))

(define-method get-seq-reader-pos ((reader cv-seq-reader))
  (cffi:foreign-funcall "cvGetSeqReaderPos" :pointer (ref reader) :int))

(define-method set-seq-reader-pos ((reader cv-seq-reader) index &optional (is-relative 0))
  (cffi:foreign-funcall "cvSetSeqReaderPos" :pointer (ref reader)
					    :int (floor index)
					    :int (floor is-relative)))

(define-method read-seq-elem-point ((reader cv-seq-reader))
  (cffi:with-foreign-object (native-point '(:struct CvPoint))
    (cffi:foreign-funcall "cv_read_seq_elem_point" :pointer native-point :pointer (ref reader))
    (cffi:with-foreign-slots ((x y) native-point (:struct CvPoint))
      (point x y))))

(define-method read-seq-elem-rect ((reader cv-seq-reader))
  (cffi:with-foreign-object (native-rect '(:struct CvRect))
    (cffi:foreign-funcall "cv_read_seq_elem_rect" :pointer native-rect :pointer (ref reader))
    (cffi:with-foreign-slots ((x y width height) native-rect (:struct CvRect))
      (rect x y width height))))

;;; 

(defclass cv-slice ()
  ((start-index :initarg :start-index :accessor start-index)
   (end-index :initarg :end-index :accessor end-index)))

(cffi:defcstruct CvSlice
  (start-index :int)
  (end-index :int))

(define-method slice (start-index end-index)
  (make-instance 'cv-slice :start-index start-index :end-index end-index))

(defclass cv-contour-scanner ()
  ((ref :initarg :ref :accessor ref)))

(defclass cv-chain ()
  ((ref :initarg :ref :accessor ref)))

(defclass cv-chain-pt-reader ()
  ((ref :initarg :ref :accessor ref)))




(define-method seq-slice ((seq cv-seq) (slice cv-slice) storage copy-data)
  (cffi:with-foreign-object (native-slice '(:struct CvSlice))
    (cffi:with-foreign-slots ((start-index end-index) native-slice (:struct CvSlice))
      (setf start-index (start-index slice)
	    end-index (end-index slice)))
    (make-instance 'cv-seq :ref
		   (cffi:foreign-funcall "cv_SeqSlice" :pointer (ref seq)
						       :pointer native-slice
						       :pointer (if storage (ref storage) (cffi-sys:null-pointer))
						       :int (if copy-data 1 0)
						       :pointer))))

(define-method seq-remove-slice ((seq cv-seq) (slice cv-slice))
  (cffi:with-foreign-object (native-slice '(:struct CvSlice))
    (cffi:with-foreign-slots ((start-index end-index) native-slice (:struct CvSlice))
      (setf start-index (start-index slice)
	    end-index (end-index slice)))
    (cffi:foreign-funcall "cv_SeqRemoveSlice" :pointer (ref seq)
					      :pointer native-slice)))


(define-method seq-insert-slice ((seq cv-seq) before-index (from-arr cv-arr))
  (cffi:foreign-funcall "cvSeqInsertSlice" :pointer (ref seq)
					    :int (floor before-index)
					    :pointer (ref from-arr)))

;;; CvMoments

(defclass cv-moments ()
  ((ref :initarg :ref :accessor ref)))


(defclass cv-contour-tree ()
  ((ref :initarg :ref :accessor ref)))

(defclass cv-term-criteria ()
  ((ref :initarg :ref :accessor ref)))



;;; HaarClassifierCascade

(defclass cv-haar-classifier-cascade ()
  ((ref :initarg :ref :accessor ref)))

