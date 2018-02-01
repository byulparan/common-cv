(in-package #:cv)

;;; CvPoint
(cffi:defcstruct (Point :class %CvPoint)
  (x :int)
  (y :int))

(defstruct (point
	    (:constructor point (x y)))
  x y)

(defmethod cffi:translate-from-foreign (p (type %CvPoint))
  (cffi:with-foreign-slots ((x y) p (:struct Point))
    (point x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %CvPoint) p)
  (cffi:with-foreign-slots ((x y) p (:struct Point))
    (setf x (point-x point)
	  y (point-y point))))


;;; CvPoint2D32f
(cffi:defcstruct (Point2D-32f :class %CvPoint2D32f)
  (x :float)
  (y :float))

(defstruct (point2d-32f
	    (:constructor point2d-32f (x y)))
  x y)

(defmethod cffi:translate-from-foreign (p (type %CvPoint2D32f))
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-32f))
    (point2d-32f x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %CvPoint2D32f) p)
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-32f))
    (setf x (point2d-32f-x point)
	  y (point2d-32f-y point))))


;;; CvPoint2D64f
(cffi:defcstruct (Point2D-64f :class %CvPoint2D64f)
  (x :double)
  (y :double))

(defstruct (point2d-64f
	    (:constructor point2d-64f (x y)))
  x y)

(defmethod cffi:translate-from-foreign (p (type %CvPoint2D64f))
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-64f))
    (point2d-64f x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %CvPoint2D64f) p)
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-64f))
    (setf x (point2d-64f-x point)
	  y (point2d-64f-y point))))


;;; CvSize
(cffi:defcstruct (Size :class %CvSize)
  (width :int)
  (height :int))

(defstruct (size
	    (:constructor size (width height)))
  width height)

(defmethod cffi:translate-from-foreign (p (type %CvSize))
  (cffi:with-foreign-slots ((width height) p (:struct Size))
    (size width height)))

(defmethod cffi:translate-into-foreign-memory (size (type %CvSize) p)
  (cffi:with-foreign-slots ((width height) p (:struct Size))
    (setf width  (size-width size)
	  height (size-height size))))


;;; CvSize2D32f 
(cffi:defcstruct (Size2D-32f :class %CvSize2D32f)
  (width :float)
  (height :float))

(defstruct (size2d-32f
	    (:constructor size2d-32f (width height)))
  width height)

(defmethod cffi:translate-from-foreign (p (type %CvSize2D32f))
  (cffi:with-foreign-slots ((width height) p (:struct Size2D-32f))
    (size2d-32f width height)))

(defmethod cffi:translate-into-foreign-memory (size (type %CvSize2D32f) p)
  (cffi:with-foreign-slots ((width height) p (:struct Size2D-32f))
    (setf width  (size2d-32f-width size)
	  height (size2d-32f-height size))))


;;; CvBox2D
(cffi:defcstruct (Box2D :class %CvBox2D)
  (center (:struct Point2D-32f))
  (size (:struct  Size2D-32f))
  (angle :float))

(defstruct (box2d
	    (:constructor box2d (center size angle)))
  center size angle)

(defmethod cffi:translate-from-foreign (p (type %CvBox2D))
  (cffi:with-foreign-slots ((center size angle) p (:struct Box2D))
    (box2d center size angle)))

(defmethod cffi:translate-into-foreign-memory (box2d (type %CvBox2D) p)
  (cffi:with-foreign-slots ((center size angle) p (:struct Box2D))
    (setf center  (box2d-center box2d)
	  size (box2d-size box2d)
	  angle (box2d-angle box2d))))

;;; CvRect
(cffi:defcstruct (Rect :class %CvRect)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defstruct (rect
	    (:constructor rect (x y width height)))
  x y width height)


(defmethod cffi:translate-from-foreign (p (type %CvRect))
  (cffi:with-foreign-slots ((x y widht height) p (:struct Rect))
    (rect x y width height)))

(defmethod cffi:translate-into-foreign-memory (rect (type %CvRect) p)
  (cffi:with-foreign-slots ((x y width height) p (:struct Rect))
    (setf x (rect-x rect)
	  y (rect-y rect)
	  width (rect-width rect)
	  height (rect-height rect))))

;;; CvScalar
(cffi:defcstruct (Scalar :class %CvScalar)
  (val :double :count 4))

(defstruct (scalar
	    (:constructor scalar (val0 val1 val2 val3)))
  val0 val1 val2 val3)

(defmethod cffi:translate-from-foreign (p (type %CvScalar))
  (cffi:with-foreign-slots ((val) p (:struct Scalar))
    (scalar (cffi:mem-aref val :double 0)
	    (cffi:mem-aref val :double 1)
	    (cffi:mem-aref val :double 2)
	    (cffi:mem-aref val :double 3))))

(defmethod cffi:translate-into-foreign-memory (scalar (type %CvScalar) p)
  (cffi:with-foreign-slots ((val) p (:struct Scalar))
    (setf (cffi:mem-aref val :double 0) (coerce (scalar-val0 scalar) 'double-float)
	  (cffi:mem-aref val :double 1) (coerce (scalar-val1 scalar) 'double-float)
	  (cffi:mem-aref val :double 2) (coerce (scalar-val2 scalar) 'double-float)
	  (cffi:mem-aref val :double 3) (coerce (scalar-val3 scalar) 'double-float))))

(defun scalar-all (number)
  (let ((number (coerce number 'double-float)))
    (scalar number number number number)))

(defun real-scalar (number)
  (scalar (coerce number 'double-float) 0.0d0 0.0d0 0.0d0))


;;; 
;;;
(cffi:defcstruct mat
  (height :int)
  (width :int)
  (data :pointer)
  (hdr_refcount :int)
  (refcount :pointer)
  (step :int)
  (type :int))

(cffi:defcfun ("cvCreateMat" create-mat) (:pointer (:struct mat))
  (rows :int)
  (cols :int)
  (type :mat-type-enum))

(cffi:defcfun ("cvReleaseMat" release-mat) :void
  (ptr :pointer))

(defun release-mat* (mat-ref)
  (cffi:with-foreign-objects ((ptr :pointer))
    (setf (cffi:mem-ref ptr :pointer) mat-ref)
    (release-mat ptr)))

;;;
(cffi:defcstruct ipl-image
  (align :int)
  (alpha-channel :int)
  (border-const :int :count 4)
  (border-mode :int :count 4)
  (channel-seq :char :count 4)
  (color-model :char :count 4)
  (data-order :int)
  (depth :int)
  (height :int)
  (id :int)
  (image-data :pointer)
  (image-data-origin :pointer)
  (image-id :pointer)
  (image-size :int)
  (mask-roi :pointer)
  (n-channels :int)
  (n-size :int)
  (origin :int)
  (roi :pointer)
  (tile-info :pointer)
  (width :int)
  (width-step :int))

(cffi:defcfun ("cvCreateImage" create-image) :pointer
  (size (:struct size))
  (depth :ipl-depth-enum)
  (channels :int))

(cffi:defcfun ("cvCreateImageHeader" create-image-header) :pointer
  (size (:struct size))
  (depth :ipl-depth-enum)
  (channels :int))

(cffi:defcfun ("cvReleaseImage" release-image) :void
  (ptr :pointer))

(defun release-image* (ipl-ref)
  (cffi:with-foreign-objects ((ptr :pointer))
    (setf (cffi:mem-ref ptr :pointer) ipl-ref)
    (release-image ptr)))

(cffi:defcfun ("cvSetImageROI" set-image-roi) :void
  (ipl-ref :pointer)
  (rect (:struct rect)))

(cffi:defcfun ("cvResetImageROI" reset-image-roi) :void
  (ipl-ref :pointer))


;;;
;;; 
(cffi:defcfun ("cvGet1D" get-1d) (:struct scalar)
  (arr-ref :pointer)
  (idx0 :int))

(cffi:defcfun ("cvGet2D" get-2d) (:struct scalar)
  (arr-ref :pointer)
  (idx0 :int)
  (idx1 :int))

(cffi:defcfun ("cvGet3D" get-3d) (:struct scalar)
  (arr-ref :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int))

(cffi:defcfun ("cvSet1D" set-1d) :void
  (arr-ref :pointer)
  (idx0 :int)
  (value (:struct scalar)))

(cffi:defcfun ("cvSet2D" set-2d) :void
  (arr-ref :pointer)
  (idx0 :int)
  (idx1 :int)
  (value (:struct scalar)))

(cffi:defcfun ("cvSet3D" set-3d) :void
  (arr-ref :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int)
  (value (:struct scalar)))

(cffi:defcfun ("cvmGet" m-get) :double
  (mat-ref (:pointer (:struct mat)))
  (row :int)
  (col :int))

(cffi:defcfun ("cvmSet" m-set) :void
  (mat-ref (:pointer (:struct mat)))
  (row :int)
  (col :int)
  (value :double))

;; 

;; ;;; IPL-CONV-KERNEL
;; (defclass ipl-conv-kernel ()
;;   ((cols :initarg :cols :accessor cols)
;;    (rows :initarg :rows :accessor rows)
;;    (anchor-x :initarg :anchor-x :accessor anchor-x)
;;    (anchor-y :initarg :anchor-y :accessor anchor-y)
;;    (shape :initarg :shape :accessor shape)
;;    (valuess :initarg :valuess :initform nil :accessor valuess)
;;    (ref :initarg :ref :accessor ref)))

;; (define-method create-structuring-element-ex (cols rows anchor-x anchor-y shape &optional (values nil))
;;   (let ((ptr
;; 	  (if (cl:and values (eql shape :cv-shape-custom))
;; 	      (cffi:with-foreign-object (value-array :int (length values))
;; 		(dotimes (i (length values))
;; 		  (setf (cffi:mem-aref value-array :int) (nth i values)))
;; 		(cffi:foreign-funcall "cvCreateStructuringElementEx" :int (floor cols) :int (floor rows) :int (floor anchor-x) :int (floor anchor-y)
;; 				      :int(cffi:foreign-enum-value :conv-kernel-shape-enum shape)
;; 				      :pointer value-array :pointer))
;; 	      (cffi:foreign-funcall "cvCreateStructuringElementEx" :int (floor cols)
;; 				    :int (floor rows)
;; 				    :int  (floor anchor-x)
;; 				    :int (floor anchor-y)
;; 				    :int  (cffi:foreign-enum-value :conv-kernel-shape-enum shape)
;; 				    :pointer (cffi-sys:null-pointer) :pointer))))
;;     (make-instance 'ipl-conv-kernel :cols (floor cols)
;; 				    :rows (floor rows)
;; 				    :anchor-x (floor anchor-x)
;; 				    :anchor-y (floor anchor-y)
;; 				    :shape shape
;; 				    :valuess values
;; 				    :ref ptr)))

;; (define-method release-structuring-element ((element ipl-conv-kernel))
;;   (cffi:with-foreign-object (ele :pointer)
;;     (setf (cffi:mem-ref ele :pointer) (ref element))
;;     (cffi:foreign-funcall "cvReleaseStructuringElement" :pointer ele)
;;     (setf (ref element) nil)))


;; ;;; Histgram

;; (defclass cv-histogram ()
;;   ((dims :initarg :dims :accessor dims)
;;    (ref :initarg :ref :accessor ref)))

;; (define-method create-hist (dims sizes type &optional ranges (uniform 1))
;;   (cffi:with-foreign-object (native-size :int (length sizes))
;;     (dotimes (i (length sizes))
;;       (setf (cffi:mem-aref native-size :int i) (nth i sizes)))
;;     (when ranges
;;       (cffi:with-foreign-object (ranges-pointer :pointer (length ranges))
;; 	(dotimes (i (length ranges))
;; 	  (let ((elements (cffi:foreign-alloc :float :count (length (nth i ranges)))))
;; 	    (dotimes (k (length (nth i ranges)))
;; 	      (setf (cffi:mem-aref elements :float k) (coerce (nth k (nth i ranges)) 'single-float)))
;; 	    (setf (cffi:mem-aref ranges-pointer :pointer i) elements)))
;; 	(let ((ref (cffi:foreign-funcall "cvCreateHist"
;; 					 :int (floor dims)
;; 					 :pointer native-size
;; 					 :int (cffi:foreign-enum-value :hist-enum type)
;; 					 :pointer (if ranges ranges-pointer
;; 						      (cffi-sys:null-pointer))
;; 					 :int (floor uniform)
;; 					 :pointer)))
;; 	  (prog1
;; 	      (make-instance 'cv-histogram :ref ref :dims dims)
;; 	    (when ranges
;; 	      (dotimes (i (length ranges))
;; 		(cffi-sys:foreign-free (cffi:mem-aref ranges-pointer :pointer i))))))))))


;; (define-method clear-hist ((hist cv-histogram))
;;   (cffi:foreign-funcall "cvClearHist" :pointer (ref hist)))

;; (define-method release-hist ((hist cv-histogram))
;;   (cffi:with-foreign-object (object :pointer)
;;     (setf (cffi:mem-ref object :pointer) (ref hist))
;;     (cffi:foreign-funcall "cvReleaseHist" :pointer object)))







;; ;;; CvMemStorage
;; (defclass cv-mem-storage ()
;;   ((ref :initarg :ref :accessor ref)))

;; (define-method create-mem-storage (&optional (block-size 0))
;;   (make-instance 'cv-mem-storage :ref
;; 		 (cffi:foreign-funcall "cvCreateMemStorage" :int block-size :pointer)))

;; (define-method release-mem-storage ((storage cv-mem-storage))
;;   (cffi:with-foreign-object (ptr :pointer)
;;     (setf (cffi:mem-ref ptr :pointer) (ref storage))
;;     (cffi:foreign-funcall "cvReleaseMemStorage" :pointer ptr)
;;     (setf (ref storage) nil)))

;; (define-method clear-mem-storage ((storage cv-mem-storage))
;;   (cffi:foreign-funcall "cvClearMemStorage" :pointer (ref storage)))




;; ;;; CvSeq
;; (defclass cv-seq ()
;;   ((ref :initarg :ref :accessor ref)))

;; (define-method create-seq (seq-flags header-size elem-size (storage cv-mem-storage))
;;   (labels ((flags ()
;;   	     (apply 'logior (mapcar #!(cffi:foreign-enum-value :cv-seq-type-enum %1) (su:mklist seq-flags)))))
;;     (make-instance 'cv-seq :ref
;; 		   (cffi:foreign-funcall "cvCreateSeq" :int (flags)
;; 						       :int (floor header-size)
;; 						       :int (floor elem-size)
;; 						       :pointer (ref storage)
;; 						       :pointer))))

;; (define-method clear-seq ((seq cv-seq))
;;   (cffi:foreign-funcall "cvClearSeq" :pointer (ref seq)))



;; (define-method seq-push ((seq cv-seq) element)
;;   (cffi:foreign-funcall "cvSeqPush" :pointer (ref seq) :pointer element
;; 			:pointer))

;; (define-method seq-push-front ((seq cv-seq) element)
;;   (cffi:foreign-funcall "cvSeqPushFront" :pointer (ref seq) :pointer element
;; 			:pointer))

;; (define-method seq-pop ((seq cv-seq) element)
;;   (cffi:foreign-funcall "cvSeqPop" :pointer (ref seq) :pointer element))

;; (define-method seq-pop-front ((seq cv-seq) element)
;;   (cffi:foreign-funcall "cvSeqPopFront" :pointer (ref seq) :pointer element))

;; (define-method seq-push-multi ((seq cv-seq) elements count &optional (in-front 0))
;;   (cffi:foreign-funcall "cvSeqPushMulti" :pointer (ref seq) :pointer elements
;; 			:int (floor count) :int (floor in-front)))

;; (define-method seq-pop-multi ((seq cv-seq) elements count &optional (in-front 0))
;;   (cffi:foreign-funcall "cvSeqPopMulti" :pointer (ref seq) :pointer elements
;; 					:int (floor count) :int (floor in-front)))

;; (define-method seq-insert ((seq cv-seq) before-index element)
;;   (cffi:foreign-funcall "cvSeqInsert" :pointer (ref seq) :int (floor before-index)
;; 			:pointer element :pointer))

;; (define-method seq-remove ((seq cv-seq) index)
;;   (cffi:foreign-funcall "cvSeqRemove" :pointer (ref seq) :int (floor index)))



;; (define-method get-seq-elem ((seq cv-seq) index)
;;   (cffi:foreign-funcall "cvGetSeqElem" :pointer (ref seq)
;; 				       :int (floor index)
;; 				       :pointer))

;; (define-method get-seq-elem-rect ((seq cv-seq) index)
;;   (let ((native-rect (get-seq-elem seq index)))
;;     (cffi:with-foreign-slots ((x y width height) native-rect (:struct CvRect))
;;       (rect x y width height))))

;; (define-method get-seq-elem-point ((seq cv-seq) index)
;;   (let ((native-point (get-seq-elem seq index)))
;;     (cffi:with-foreign-slots ((x y) native-point (:struct CvPoint))
;;       (point x y))))


;; (define-method seq-elem-idx ((seq cv-seq) element)
;;   (cffi:foreign-funcall "cvSeqElemIdx" :pointer (ref seq)
;; 				       :pointer (ref element)
;; 				       :pointer (cffi-sys:null-pointer)
;; 				       :int))

;; (define-method clone-seq ((seq cv-seq) &optional storage)
;;   (let ((result (cffi:foreign-funcall "cvCloneSeq" :pointer (ref seq)
;; 						   :pointer (if storage (ref storage) (cffi-sys:null-pointer))
;; 						   :pointer)))
;;     (make-instance 'cv-seq :ref result)))



;; (define-method total ((seq cv-seq))
;;   (cffi:foreign-funcall "get_seq_total" :pointer (ref seq) :int))

;; (define-method next ((seq cv-seq))
;;   (let ((next (cffi:foreign-funcall "get_next" :pointer (ref seq)
;; 					       :pointer)))
;;     (unless (cffi-sys:null-pointer-p next)
;;       (make-instance 'cv-seq :ref next))))


;; ;;; 
;; (defclass cv-seq-writer ()
;;   ((ref :initarg :ref :accessor ref)))

;; (define-method start-write-seq (seq-flags header-size elem-size (storage cv-mem-storage))
;;   (make-instance 'cv-seq-writer :ref
;; 		 (cffi:foreign-funcall "cv_StartWriteSeq" :int (floor seq-flags)
;; 							  :int (floor header-size)
;; 							  :int (floor elem-size)
;; 							  :pointer (ref storage)
;; 							  :pointer)))

;; (define-method start-append-to-seq ((seq cv-seq) (writer cv-seq-writer))
;;   (cffi:foreign-funcall "cvStartAppendToSeq" :pointer (ref seq) :pointer (ref writer)))

;; (define-method end-write-seq ((writer cv-seq-writer))
;;   (make-instance 'cv-seq :ref
;; 		 (cffi:foreign-funcall "cvEndWriteSeq" :pointer (ref writer) :pointer)))

;; (define-method flush-seq-writer ((writer cv-seq-writer))
;;   (cffi:foreign-funcall "cvFlushSeqWriter" :pointer (ref writer)))

;; (define-method write-seq-elem-point ((point cv-point) (writer cv-seq-writer))
;;   (with-cv-point (native-point point)
;;     (cffi:foreign-funcall "cv_write_seq_elem_point" :pointer native-point
;; 					      :pointer (ref writer))))

;; (define-method write-seq-elem-rect ((rect cv-rect) (writer cv-seq-writer))
;;   (cffi:with-foreign-object (native-rect '(:struct CvRect))
;;     (cffi:with-foreign-slots ((x y width height) native-rect (:struct CvRect))
;;       (setf x (x rect) y (y rect) width (width rect) height (height rect)))
;;     (cffi:foreign-funcall "cv_write_seq_elem_rect" :pointer native-rect
;; 					      :pointer (ref writer))))


;; (defclass cv-seq-reader ()
;;   ((ref :initarg :ref :accessor ref)))

;; (define-method start-read-seq ((seq cv-seq) &optional (reverse 0))
;;   (make-instance 'cv-seq-reader :ref
;; 		 (cffi:foreign-funcall "cv_StartReadSeq" :pointer (ref seq)
;; 							 :int (floor reverse)
;; 							 :pointer)))

;; (define-method get-seq-reader-pos ((reader cv-seq-reader))
;;   (cffi:foreign-funcall "cvGetSeqReaderPos" :pointer (ref reader) :int))

;; (define-method set-seq-reader-pos ((reader cv-seq-reader) index &optional (is-relative 0))
;;   (cffi:foreign-funcall "cvSetSeqReaderPos" :pointer (ref reader)
;; 					    :int (floor index)
;; 					    :int (floor is-relative)))

;; (define-method read-seq-elem-point ((reader cv-seq-reader))
;;   (cffi:with-foreign-object (native-point '(:struct CvPoint))
;;     (cffi:foreign-funcall "cv_read_seq_elem_point" :pointer native-point :pointer (ref reader))
;;     (cffi:with-foreign-slots ((x y) native-point (:struct CvPoint))
;;       (point x y))))

;; (define-method read-seq-elem-rect ((reader cv-seq-reader))
;;   (cffi:with-foreign-object (native-rect '(:struct CvRect))
;;     (cffi:foreign-funcall "cv_read_seq_elem_rect" :pointer native-rect :pointer (ref reader))
;;     (cffi:with-foreign-slots ((x y width height) native-rect (:struct CvRect))
;;       (rect x y width height))))

;; ;;; 

;; (defclass cv-slice ()
;;   ((start-index :initarg :start-index :accessor start-index)
;;    (end-index :initarg :end-index :accessor end-index)))

;; (cffi:defcstruct CvSlice
;;   (start-index :int)
;;   (end-index :int))

;; (define-method slice (start-index end-index)
;;   (make-instance 'cv-slice :start-index start-index :end-index end-index))

;; (defclass cv-contour-scanner ()
;;   ((ref :initarg :ref :accessor ref)))

;; (defclass cv-chain ()
;;   ((ref :initarg :ref :accessor ref)))

;; (defclass cv-chain-pt-reader ()
;;   ((ref :initarg :ref :accessor ref)))




;; (define-method seq-slice ((seq cv-seq) (slice cv-slice) storage copy-data)
;;   (cffi:with-foreign-object (native-slice '(:struct CvSlice))
;;     (cffi:with-foreign-slots ((start-index end-index) native-slice (:struct CvSlice))
;;       (setf start-index (start-index slice)
;; 	    end-index (end-index slice)))
;;     (make-instance 'cv-seq :ref
;; 		   (cffi:foreign-funcall "cv_SeqSlice" :pointer (ref seq)
;; 						       :pointer native-slice
;; 						       :pointer (if storage (ref storage) (cffi-sys:null-pointer))
;; 						       :int (if copy-data 1 0)
;; 						       :pointer))))

;; (define-method seq-remove-slice ((seq cv-seq) (slice cv-slice))
;;   (cffi:with-foreign-object (native-slice '(:struct CvSlice))
;;     (cffi:with-foreign-slots ((start-index end-index) native-slice (:struct CvSlice))
;;       (setf start-index (start-index slice)
;; 	    end-index (end-index slice)))
;;     (cffi:foreign-funcall "cv_SeqRemoveSlice" :pointer (ref seq)
;; 					      :pointer native-slice)))


;; (define-method seq-insert-slice ((seq cv-seq) before-index (from-arr cv-arr))
;;   (cffi:foreign-funcall "cvSeqInsertSlice" :pointer (ref seq)
;; 					    :int (floor before-index)
;; 					    :pointer (ref from-arr)))

;; ;;; CvMoments

;; (defclass cv-moments ()
;;   ((ref :initarg :ref :accessor ref)))


;; (defclass cv-contour-tree ()
;;   ((ref :initarg :ref :accessor ref)))

;; (defclass cv-term-criteria ()
;;   ((ref :initarg :ref :accessor ref)))



;; ;;; HaarClassifierCascade

;; (defclass cv-haar-classifier-cascade ()
;;   ((ref :initarg :ref :accessor ref)))

