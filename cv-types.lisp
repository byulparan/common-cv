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



;;;
;;; 

;; 
;; IPL-CONV-KERNEL
(cffi:defcstruct ipl-conv-kernel
  (anchor-x :int)
  (anchor-y :int)
  (n-cols :int)
  (n-rows :int)
  (n-shift-r :int)
  (values :pointer))




;;; Histogram






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


;; (cffi:defcfun ("cvCloneSeq" clone-seq) :pointer
;;   (seq :pointer)
;;   (storage :pointer))


;;; CvSeqWriter
(cffi:defcstruct seq-writer
  (header-size :int)
  (seq :pointer)
  (block :pointer)
  (ptr :pointer)
  (block-min :pointer)
  (block-max :pointer))



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





