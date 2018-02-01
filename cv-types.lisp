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
;;; CvMat
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

(defun release-mat* (mat)
  (cffi:with-foreign-objects ((ptr :pointer))
    (setf (cffi:mem ptr :pointer) mat)
    (release-mat ptr)))

;;; IplImage
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

(defun release-image* (ipl)
  (cffi:with-foreign-objects ((ptr :pointer))
    (setf (cffi:mem ptr :pointer) ipl)
    (release-image ptr)))

(cffi:defcfun ("cvSetImageROI" set-image-roi) :void
  (ipl :pointer)
  (rect (:struct rect)))

(cffi:defcfun ("cvResetImageROI" reset-image-roi) :void
  (ipl :pointer))


;;;
;;; 
(cffi:defcfun ("cvGet1D" get-1d) (:struct scalar)
  (arr :pointer)
  (idx0 :int))

(cffi:defcfun ("cvGet2D" get-2d) (:struct scalar)
  (arr :pointer)
  (idx0 :int)
  (idx1 :int))

(cffi:defcfun ("cvGet3D" get-3d) (:struct scalar)
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int))

(cffi:defcfun ("cvSet1D" set-1d) :void
  (arr :pointer)
  (idx0 :int)
  (value (:struct scalar)))

(cffi:defcfun ("cvSet2D" set-2d) :void
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (value (:struct scalar)))

(cffi:defcfun ("cvSet3D" set-3d) :void
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int)
  (value (:struct scalar)))

(cffi:defcfun ("cvmGet" m-get) :double
  (mat (:pointer (:struct mat)))
  (row :int)
  (col :int))

(cffi:defcfun ("cvmSet" m-set) :void
  (mat (:pointer (:struct mat)))
  (row :int)
  (col :int)
  (value :double))



;; 
;; IPL-CONV-KERNEL
(cffi:defcstruct ipl-conv-kernel
  (anchor-x :int)
  (anchor-y :int)
  (n-cols :int)
  (n-rows :int)
  (n-shift-r :int)
  (values :pointer))

(cffi:defcfun ("cvCreateStructuringElementEx" create-structuring-element-ex)
    (:pointer (:struct ipl-conv-kernel))
  (cols :int)
  (rows :int)
  (anchor-x :int)
  (anchor-y :int)
  (shape :int)
  (values :pointer))

(cffi:defcfun ("cvReleaseStructuringElement" release-structuring-element) :void
  (ptr :pointer))

(defun release-structuring-element* (ipl-conv-kernel)
  (cffi:with-foreign-objects ((ptr :pointer))
    (setf (cffi:mem ptr :pointer) ipl-conv-kernel)
    (release-structuring-element ptr)))


;;; Histogram
(cffi:defcfun ("cvCreateHist" create-hist) :pointer
  (dims :int)
  (sizes :pointer)
  (type :hist-enum)
  (ranges :pointer)
  (uniform :int))

(defun create-hist* (dims sizes type &optional ranges (uniform 1))
  (let* ((ref nil))
    (cffi:with-foreign-objects ((native-size :int (length sizes)))
      (dotimes (i (length sies))
	(setf (cffi:mem-aref native-size :int i) (nth i sizes)))
      (if ranges (cffi:with-foreign-object (ranges-pointer :pointer (length ranges))
		   (dotimes (i (length ranges))
		     (let ((elements (cffi:foreign-alloc :float :count (length (nth i ranges)))))
		       (dotimes (k (length (nth i ranges)))
			 (setf (cffi:mem-aref elements :float k) (coerce (nth k (nth i ranges)) 'single-float)))
		       (setf (cffi:mem-aref ranges-pointer :pointer i) elements)))
		   (setf ref (cffi:foreign-funcall "cvCreateHist"
						   :int (floor dims)
						   :pointer native-size
						   :int (cffi:foreign-enum-value :hist-enum type)
						   :pointer ranges-pointer
						   :int (floor uniform)
						   :pointer))
		   (dotimes (i (length ranges))
		     (cffi-sys:foreign-free (cffi:mem-aref ranges-pointer :pointer i))))
	(setf ref (cffi:foreign-funcall "cvCreateHist"
					:int (floor dims)
					:pointer native-size
					:int (cffi:foreign-enum-value :hist-enum type)
					:pointer (cffi:null-pointer)
					:int (floor uniform)
					:pointer))))
    ref))

(cffi:defcfun ("cvClearHist" clear-hist) :void
  (hist :pointer))

(cffi:defcfun ("cvReleaseHist" release-hist) :void
  (ptr :pointer))

(defun release-hist* (hist)
  (cffi:with-foreign-objects ((ptr :pointer))
    (setf (cffi:mem ptr :pointer) hist)
    (release-hist ptr)))

;;; CvMemStorage
(cffi:defcfun ("cvCreateMemStorage" create-mem-storage) :pointer
  (block-size :int))

(cffi:defcfun ("cvReleaseMemStorage" release-mem-storage) :void
  (ptr :pointer))

(defun release-mem-storage* (mem-storage)
  (cffi:with-foreign-objects ((ptr :pointer))
    (setf (cffi:mem ptt :pointer) mem-storage)
    (release-mem-storage ptr)))

(cffi:defcfun ("cvClearMemStorage" clear-mem-storage) :void
  (mem-storage :pointer))


;;; CvSeq
(cffi:defcstruct seq
  (flags :int)
  (header-size :int)
  (h-next :pointer)
  (h-prev :pointer)
  (v-next :pointer)
  (v-pref :pointer)
  (total :int)
  (elem-size :int)
  (block-max :pointer)
  (ptr :pointer)
  (delta-elems :int)
  (storage :pointer)
  (free-blocks :pointer)
  (first :pointer))

(cffi:defcfun ("cvCreateSeq" create-seq) :pointer
  (seq-flags :int)
  (header-size :unsigned-long)
  (elem-size :unsigned-long)
  (storage :pointer))

(cffi:defcfun ("cvClearSeq" clear-seq) :void
  (seq :pointer))

(cffi:defcfun ("cvSeqPush" seq-push) :pointer
  (seq :pointer)
  (element :pointer))

(cffi:defcfun ("cvSeqPushFront" seq-push-front) :pointer
  (seq :pointer)
  (element :pointer))

(cffi:defcfun ("cvSeqPop" seq-pop) :void
  (seq :pointer)
  (element :pointer))

(cffi:defcfun ("cvSeqPopFront" seq-pop-front) :void
  (seq :pointer)
  (element :pointer))

(cffi:defcfun ("cvSeqPushMulti" seq-push-multi) :void
  (seq :pointer)
  (elements :pointer)
  (count :int)
  (in-front :int))

(cffi:defcfun ("cvSeqPopMulti" seq-pop-multi) :void
  (seq :pointer)
  (elements :pointer)
  (count :int)
  (in-front :int))

(cffi:defcfun ("cvSeqInsert" seq-insert) :pointer
  (seq :pointer)
  (before-index :int)
  (element :pointer))

(cffi:defcfun ("cvSeqRemove" seq-remove) :void
  (seq :pointer)
  (index :int))

(cffi:defcfun ("cvGetSeqElem" get-seq-elem) :pointer
  (seq :pointer)
  (index :int))

(defun get-seq-elem-rect (seq index)
  (let* ((rect (get-seq-elem seq index)))
    (cffi:with-foreign-slots ((x y width height) rect (:struct rect))
      (rect x y width height))))

(defun get-seq-elem-point (seq index)
  (let* ((point (get-seq-elem seq index)))
    (cffi:with-foreign-slots ((x y) point (:struct point))
      (point x y))))

(cffi:defcfun ("cvSeqElemIdx" seq-elem-idx) :int
  (seq :pointer)
  (element :pointer)
  (block :pointer))

(cffi:defcfun ("cvCloneSeq" clone-seq) :pointer
  (seq :pointer)
  (storage :pointer))


;;; CvSeqWriter
(cffi:defcstruct seq-writer
  (header-size :int)
  (seq :pointer)
  (block :pointer)
  (ptr :pointer)
  (block-min :pointer)
  (block-max :pointer))

(cffi:defcfun ("cvStartWriteSeq" start-write-seq)
  (seq-flags :int)
  (header-size :int)
  (elem-size :int)
  (storage :pointer)
  (writer :pointer))

(cffi:defcfun ("cvStartAppendToSeq" start-append-to-seq) :void
  (seq :pointer)
  (seq-writer :pointer))

(cffi:defcfun ("cvEndWriteSeq" end-write-seq) :pointer
  (seq-writer :pointer))

(cffi:defcfun ("cvFlushSeqWriter" flush-seq-writer) :void
  (seq-writer :pointer))


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


;;; CvSeqReader
(cffi:defcstruct seq-reader
  (header-size :int)
  (seq :pointer)
  (block :pointer)
  (ptr :pointer)
  (block-min :pointer)
  (block-max :pointer)
  (delta-index :int)
  (prev-elem :pointer))

(cffi:defcfun ("cvStartReadSeq" start-read-seq) :pointer
  (seq :pointer)
  (seq-reader :pointer)
  (reverse :int))

(cffi:defcfun ("cvGetSeqReaderPos" get-seq-reader-pos) :int
  (seq-reader :pointer))

(cffi:defcfun ("cvSetSeqReaderPos" set-seq-reader-pos) :void
  (seq-reader :pointer)
  (index :int)
  (is-relative :int))

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


;;; CvSlice
(cffi:defcstruct (Slice :class %CvSlice)
  (start-index :int)
  (end-index :int))

(defstruct (slice
	    (:constructor slice (start-index end-index)))
  start-index end-index)

(defmethod cffi:translate-from-foreign (p (type %CvSlice))
  (cffi:with-foreign-slots ((start-index end-index) p (:struct Slice))
    (slice start-index end-index)))

(defmethod cffi:translate-into-foreign-memory (slice (type %CvSlice) p)
  (cffi:with-foreign-slots ((start-index end-index) p (:struct slice))
    (setf start-index (slice-start-index slice)
	  end-index (slice-end-index slice))))


(cffi:defcfun ("cvSeqSlice" seq-slice) :pointer
  (seq :pointer)
  (slice (:struct slice))
  (storage :pointer)
  (copy-data :int))

(cffi:defcfun ("cvSeqRemoveSlice" seq-remove-slice) :void
  (seq :pointer)
  (slice (:struct slice)))

(cffi:defcfun ("cvSeqInsertSlice" seq-insert-slice) :void
  (seq :pointer)
  (before-index :int)
  (from-arr :pointer))

(cffi:defcfun ("cvSeqInvert" seq-invert) :void
  (seq :pointer))



