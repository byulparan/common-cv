(in-package #:cv)

;;; CvPoint
(cffi:defcstruct (Point :class %CvPoint)
  (x :int)
  (y :int))

(defstruct point x y)
(defun point (x y)
  (make-point :x (floor x)
	      :y (floor y)))

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

(defstruct point2d-32f x y)
(defun point2d-32f (x y)
  (make-point2d-32f :x (coerce x 'single-float)
		    :y (coerce y 'single-float)))

(defmethod cffi:translate-from-foreign (p (type %CvPoint2D32f))
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-32f))
    (point2d-32f x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %CvPoint2D32f) p)
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-32f))
    (setf x (coerce (point2d-32f-x point) 'single-float)
	  y (coerce (point2d-32f-y point) 'single-float))))


;;; CvPoint2D64f
(cffi:defcstruct (Point2D-64f :class %CvPoint2D64f)
  (x :double)
  (y :double))

(defstruct point2d-64f x y)
(defun point2d-64f (x y)
  (make-point2d-64f :x (coerce x 'double-float)
		    :y (coerce y 'double-float)))

(defmethod cffi:translate-from-foreign (p (type %CvPoint2D64f))
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-64f))
    (point2d-64f x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %CvPoint2D64f) p)
  (cffi:with-foreign-slots ((x y) p (:struct Point2D-64f))
    (setf x (point2d-64f-x  point)
	  y (point2d-64f-y point))))


;;; CvSize
(cffi:defcstruct (Size :class %CvSize)
  (width :int)
  (height :int))

(defstruct size width height)
(defun size (width height)
  (make-size :width (floor width)
	     :height (floor height)))

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

(defstruct size2d-32f width height)
(defun size2d-32f (width height)
  (make-size2d-32f :width (coerce width 'single-float)
		   :height (coerce height 'single-float)))

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

(defstruct box2d center size angle)
(defun box2d (center size angle)
  (make-box2d :center center
	      :size size
	      :angle (coerce angle 'single-float)))

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

(defstruct rect x y width height)
(defun rect (x y width height)
  (make-rect :x (floor x)
	      :y (floor y)
	      :width (floor width)
	      :height (floor height)))


(defmethod cffi:translate-from-foreign (p (type %CvRect))
  (cffi:with-foreign-slots ((x y width height) p (:struct Rect))
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

(defstruct scalar
  d0 d1 d2 d3)

(defun scalar (d0 &optional (d1 0.0) (d2 0.0) (d3 0.0))
  (make-scalar :d0 (coerce d0 'double-float)
	       :d1 (coerce d1 'double-float)
	       :d2 (coerce d2 'double-float)
	       :d3 (coerce d3 'double-float)))


(defmethod cffi:translate-from-foreign (p (type %CvScalar))
  (cffi:with-foreign-slots ((val) p (:struct Scalar))
    (scalar (cffi:mem-aref val :double 0)
	    (cffi:mem-aref val :double 1)
	    (cffi:mem-aref val :double 2)
	    (cffi:mem-aref val :double 3))))

(defmethod cffi:translate-into-foreign-memory (scalar (type %CvScalar) p)
  (cffi:with-foreign-slots ((val) p (:struct Scalar))
    (setf (cffi:mem-aref val :double 0) (coerce (scalar-d0 scalar) 'double-float)
	  (cffi:mem-aref val :double 1) (coerce (scalar-d1 scalar) 'double-float)
	  (cffi:mem-aref val :double 2) (coerce (scalar-d2 scalar) 'double-float)
	  (cffi:mem-aref val :double 3) (coerce (scalar-d3 scalar) 'double-float))))

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



;; 
;; IPL-CONV-KERNEL
(cffi:defcstruct ipl-conv-kernel
  (anchor-x :int)
  (anchor-y :int)
  (n-cols :int)
  (n-rows :int)
  (n-shift-r :int)
  (values :pointer))


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


;;; CvSeqWriter
(cffi:defcstruct seq-writer
  (header-size :int)
  (seq :pointer)
  (block :pointer)
  (ptr :pointer)
  (block-min :pointer)
  (block-max :pointer))


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


;;; CvSlice
(cffi:defcstruct (Slice :class %CvSlice)
  (start-index :int)
  (end-index :int))

(defstruct slice start-index end-index)
(defun slice (start-index end-index)
  (make-slice :start-index (floor start-index)
	      :end-index (floor end-index)))

(defmethod cffi:translate-from-foreign (p (type %CvSlice))
  (cffi:with-foreign-slots ((start-index end-index) p (:struct Slice))
    (slice start-index end-index)))

(defmethod cffi:translate-into-foreign-memory (slice (type %CvSlice) p)
  (cffi:with-foreign-slots ((start-index end-index) p (:struct slice))
    (setf start-index (slice-start-index slice)
	  end-index (slice-end-index slice))))


