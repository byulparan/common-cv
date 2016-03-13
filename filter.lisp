
(in-package #:cv)

(define-method abs-diff ((src1 cv-arr) (src2 cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvAbsDiff" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)))

(define-method abs-diff-s ((src cv-arr) (dst cv-arr) (value cv-scalar))
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_AbsDiffS" :pointer (ref src) :pointer (ref dst) :pointer native-cv-scalar)))

(define-method abs ((src cv-arr) (dst ipl-image))
  (abs-diff-s src dst (scalar-all 0)))

(define-method add ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional mask)
  (cffi:foreign-funcall "cvAdd" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)
			:pointer (if mask (ref mask) (cffi-sys:null-pointer))))

(define-method add-s ((src cv-arr) (value cv-scalar) (dst cv-arr) &optional mask)
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_AddS" :pointer (ref src) :pointer native-cv-scalar :pointer (ref dst)
				    :pointer (if mask (ref mask) (cffi-sys:null-pointer)))))

(define-method add-weighted ((src1 cv-arr) (alpha number) (src2 cv-arr) (beta number)
			    (gamma number) (dst cv-arr))
  (cffi:foreign-funcall "cvAddWeighted" :pointer  (ref src1) :double  (* 1.0d0 alpha)
			:pointer (ref src2) :double (* 1.0d0 beta) :double (* 1.0d0 gamma) :pointer (ref dst)))

(define-method set-zero ((arr cv-arr))
  (cffi:foreign-funcall "cvSetZero" :pointer (ref arr)))

(define-method zero ((arr cv-arr))
  (set-zero arr))

(define-method and ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional mask)
  (cffi:foreign-funcall "cvAnd" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)
			:pointer (if mask (ref mask) (cffi-sys:null-pointer))))

(define-method and-s ((src cv-arr) (value cv-scalar) (dst cv-arr) &optional mask)
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_AndS" :pointer (ref src) :pointer native-cv-scalar :pointer (ref dst)
			  :pointer (if mask (ref mask) (cffi-sys:null-pointer)))))

(define-method avg ((src cv-arr) &optional mask)
  (cffi:with-foreign-object (native-cv-scalar '(:struct CvScalar))
    (cffi:foreign-funcall "cv_Avg" :pointer (ref src) :pointer (if mask (ref mask) (cffi-sys:null-pointer))
			  :pointer native-cv-scalar)
    (cffi:with-foreign-slots ((val) native-cv-scalar (:struct CvScalar))
      (let ((l (make-list 4)))
	(dotimes (i 4)
	  (setf (nth i l) (cffi:mem-aref val :double i)))
	(apply 'scalar l)))))

(define-method avg-sdv ((arr cv-arr) (mean cv-scalar) (std-dev cv-scalar) &optional mask)
  (with-cv-scalar (mean-native-scalar mean)
    (with-cv-scalar (std-dev-native-scalar std-dev)
      (cffi:foreign-funcall "cvAvgSdv" :pointer (ref arr)  :pointer mean-native-scalar :pointer std-dev-native-scalar
			    :pointer (if mask (ref mask) (cffi-sys:null-pointer))))))


(define-method calc-covar-matrix ((vects list) (count number) (cov-mat cv-arr)
				 (avg cv-arr) (flags symbol))
  (cffi:with-foreign-object (vec-array :pointer count)
    (dotimes (i count)
      (setf (cffi:mem-aref vec-array :pointer i) (ref (nth i vects))))
    (cffi:foreign-funcall "cvCalcCovarMatrix" :pointer vec-array
			  :int (floor count) :pointer (ref cov-mat) :pointer (ref avg)
			  :int (cffi:foreign-enum-value :covar-matrix-enum flags))))

(define-method cmp ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) (cmp-op symbol))
  (cffi:foreign-funcall "cvCmp" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)
			:int (cffi:foreign-enum-value :cmp-enum cmp-op)))

(define-method cmp-s ((src cv-arr) (value number) (dst cv-arr) (cmp-op symbol))
  (cffi:foreign-funcall "cvCmpS" :pointer (ref src) :double  (* 1.0d0 value) :pointer (ref dst)
			:int (cffi:foreign-enum-value :cmp-enum cmp-op)))


;;; convert scale
(define-method convert-scale ((src cv-arr) (dst cv-arr) &optional (scale 1.0d0) (shift 0.0d0))
  (cffi:foreign-funcall "cvConvertScale" :pointer (ref src) :pointer (ref dst) :double (* 1.0d0 scale)
			:double (* 1.0d0 shift)))

(define-method scale ((src cv-arr) (dst cv-arr) &optional (scale 1.0) (shift 0.0))
  (convert-scale src dst scale shift))

(define-method convert ((src cv-arr) (dst cv-arr))
  (convert-scale src dst 1 0))

(define-method cvt-scale ((src cv-arr) (dst cv-arr) &optional (scale 1.0) (shift 0.0))
  (convert-scale src dst scale shift))

(define-method convert-scale-abs ((src cv-arr) (dst cv-arr) &optional (scale 1.0) (shift 0.0))
  (cffi:foreign-funcall "cvConvertScaleAbs" :pointer (ref src) :pointer (ref dst) :double (* 1.0d0 scale)
			:double (* 1.0d0 shift)))

(define-method copy ((src cv-arr) (dst cv-arr) &optional mask)
  (cffi:foreign-funcall "cvCopy" :pointer (ref src) :pointer (ref dst) :pointer (if mask (ref mask) (cffi-sys:null-pointer))))


(define-method count-non-zero ((arr cv-arr))
  (cffi:foreign-funcall "cvCountNonZero" :pointer (ref arr) :int))

(define-method cross-product ((src1 cv-arr) (src2 cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvCrossProduct" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)))

(define-method round (value)
  (cffi:foreign-funcall "cv_Round" :double (* 1.0d0 value)
				  :int))


;;; convert color
(define-method cvt-color ((src cv-arr) (dst cv-arr) (code symbol))
  (cffi:foreign-funcall "cvCvtColor" :pointer (ref src) :pointer (ref dst) :int (cffi:foreign-enum-value :cvt-color-enum code)))



(define-method det ((mat cv-arr))
  (cffi:foreign-funcall "cvDet" :pointer (ref mat) :double))

(define-method div ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional (scale 1))
  (cffi:foreign-funcall "cvDiv" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst) :double (* 1.0d0 scale)))


(define-method dot-product ((src1 cv-arr) (src2 cv-arr))
  (cffi:foreign-funcall "cvDotProduct" :pointer (ref src1) :pointer (ref src2) :double))

(define-method eigen-vv ((mat cv-arr) (evects cv-arr) (evals cv-arr)
			&optional (eps 0) (lowindex -1) (highindex -1))
  (cffi:foreign-funcall "cvEigenVV" :pointer (ref mat) :pointer (ref evects) :pointer (ref evals)
			:double (* eps 1.0d0) :int (floor lowindex) :int (floor highindex)))

(define-method flip ((src cv-arr) &optional dst (flip-mode 0))
  (cffi:foreign-funcall "cvFlip" :pointer (ref src) :pointer (if dst (ref dst) (cffi-sys:null-pointer))
			:int (floor flip-mode)))

(define-method gemm ((src1 cv-arr) (src2 cv-arr) (alpha number) (src3 cv-arr) (beta number)
		    (dst cv-arr) &optional (tabc 0))
  (cffi:foreign-funcall "cvGEMM" :pointer (ref src1) :pointer (ref src2) :double (* 1.0d0 alpha)
			:pointer (ref src3) :double (* 1.0d0 beta) :pointer (ref dst) :int (floor  tabc)))


(define-method get-col ((arr ipl-image) (submat cv-mat) (col integer))
  (get-cols arr submat col (+ col 1)))

(define-method get-cols ((arr cv-arr) (submat cv-mat) (start-col number) (end-col number))
  (let ((mat
	  (cffi:foreign-funcall "cvGetCols" :pointer (ref arr) :pointer (ref submat) :int (floor start-col)
				:int (floor end-col) :pointer)))
    (reset-info-mat submat)
    (reset-info-mat (make-instance 'cv-mat :ref mat))))

(define-method get-diag ((arr cv-arr) (submat cv-mat) &optional (diag 0))
  (let ((mat (cffi:foreign-funcall "cvGetDiag" :pointer (ref arr) :pointer (ref submat) :int (floor diag)
				   :pointer)))
    (reset-info-mat submat)
    (reset-info-mat (make-instance 'cv-mat :ref mat))))

(define-method get-dims ((arr cv-arr) &optional sizes)
  (cffi:foreign-funcall "cvGetDims" :pointer (ref arr) :pointer (if sizes sizes (cffi-sys:null-pointer))
			:int))


(define-method get-dim-size ((arr cv-arr) (index number))
  (cffi:foreign-funcall "cvGetDimSize" :pointer (ref arr) :int (floor index) :int))


(define-method get-rows ((arr cv-arr) (submat cv-mat) (start-row number) (end-row number) &optional (delta-row 1))
  (let ((mat (cffi:foreign-funcall "cvGetRows" :pointer (ref arr) :pointer (ref submat) :int (floor start-row)
				   :int (floor end-row) :int (floor delta-row) :pointer)))
    (reset-info-mat submat)
    (reset-info-mat (make-instance 'cv-mat :ref mat))))

(define-method get-row ((arr cv-arr) (submat cv-mat) (row number))
  (get-rows arr submat row (+ row 1) 1))

(define-method get-size ((arr cv-arr))
  (cffi:with-foreign-object (native-size '(:struct CvSize))
    (cffi:foreign-funcall "cv_GetSize" :pointer (ref arr) :pointer native-size)
    (cffi:with-foreign-slots ((width height) native-size (:struct CvSize))
      (size width height))))


(define-method get-sub-rect ((arr cv-arr) (submat cv-mat) (rect cv-rect))
  (cffi:with-foreign-object (native-cv-rect '(:struct CvRect))
    (cffi:with-foreign-slots ((x y width height) native-cv-rect (:struct CvRect))
      (setf x (x rect))
      (setf y (y rect))
      (setf width (width rect))
      (setf height (height rect))
      (let ((mat (cffi:foreign-funcall "cv_GetSubRect" :pointer (ref arr) :pointer (ref submat)
				       :pointer native-cv-rect :pointer)))
	(reset-info-mat submat)
	(reset-info-mat (make-instance 'cv-mat :ref mat))))))


(define-method in-range ((src cv-arr) (lower cv-arr) (upper cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvInRange" :pointer (ref src) :pointer (ref lower) :pointer (ref upper)
			:pointer (ref dst)))

(define-method in-range-s ((src cv-arr) (lower cv-scalar) (upper cv-scalar) (dst cv-arr))
  (with-cv-scalar (native-lower-scalar lower)
    (with-cv-scalar (native-upper-scalar upper)
      (cffi:foreign-funcall "cv_InRangeS" :pointer (ref src) :pointer native-lower-scalar
			    :pointer native-upper-scalar :pointer (ref dst)))))



(define-method invert ((src cv-arr) (dst cv-arr) &optional (method :cv-lu))
  (cffi:foreign-funcall "cvInvert" :pointer (ref src) :pointer (ref dst) :int (cffi:foreign-enum-value :invert-enum method) :double))

(define-method inv ((src cv-arr) (dst cv-arr) &optional (method :cv-lu))
  (invert src dst method))

(define-method mahalonobis ((vec1 cv-arr) (vec2 cv-arr) (mat cv-arr))
  (cffi:foreign-funcall "cvMahalonobis" :pointer (ref vec1) :pointer (ref vec2) :pointer (ref mat) :double))


(define-method max ((src1 cv-arr) (src2 cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvMax" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)))

(define-method max-s ((src cv-arr) (value number) (dst cv-arr))
  (cffi:foreign-funcall "cvMaxS" :pointer (ref src) :double (* 1.0d0 value) :pointer (ref dst)))

(define-method merge ((src0 cv-arr) (src1 cv-arr) (src2 cv-arr) (src3 cv-arr)
		     (dst cv-arr))
  (cffi:foreign-funcall "cvMerge" :pointer (ref src0) :pointer (ref src1) :pointer (ref src2)
			:pointer (ref src3) :pointer (ref dst)))

(define-method min ((src1 cv-arr) (src2 cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvMin" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)))

(define-method min-s ((src cv-arr) (value number) (dst cv-arr))
  (cffi:foreign-funcall "cvMinS" :pointer (ref src) :double  (* 1.0d0 value) :pointer (ref dst)))


(define-method min-max-loc ((arr cv-arr)
			    (min-val #+ccl ccl::macptr #+sbcl sb-sys::system-area-pointer #+ecl si:foreign-data)
			    (max-val #+ccl ccl::macptr #+sbcl sb-sys:system-area-pointer #+ecl si:foreign-data)
			    (min-loc cv-point)
			    (max-loc cv-point) &optional mask)
  (cffi:with-foreign-objects ((min-loc-pointer '(:struct CvPoint))
			      (max-loc-pointer '(:struct CvPoint)))
    (cffi:foreign-funcall "cvMinMaxLoc" :pointer (ref arr) :pointer min-val :pointer  max-val
					:pointer min-loc-pointer :pointer  max-loc-pointer
					:pointer (if mask (ref mask) (cffi-sys:null-pointer)))
    (cffi:with-foreign-slots ((x y) min-loc-pointer (:struct CvPoint))
      (setf (x min-loc) x
	    (y min-loc) y))
    (cffi:with-foreign-slots ((x y) max-loc-pointer (:struct CvPoint))
      (setf (x max-loc) x
	    (y max-loc) y)))
  (values))


(define-method mul ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional (scale 1))
  (cffi:foreign-funcall "cvMul" :pointer (ref src1) :pointer (ref src2)  :pointer (ref dst) :double (* 1.0d0 scale)))

(define-method not ((src cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvNot" :pointer (ref src) :pointer (ref dst)))



(define-method norm ((arr1 cv-arr) &optional arr2 (norm-type :cv-l2) mask)
  (cffi:foreign-funcall "cvNorm" :pointer (ref arr1) :pointer (if arr2 (ref arr2) (cffi-sys:null-pointer))
			:int (cffi:foreign-enum-value :norm-enum norm-type)
			:pointer (if mask (ref mask) (cffi-sys:null-pointer)) :double))

(define-method normalize ((src1 cv-arr) (dst cv-arr) &optional (a 1.0) (b 1.0) (norm-type :cv-l2) mask)
  (cffi:foreign-funcall "cvNormalize" :pointer (ref src1) :pointer (ref dst) :double (* 1.0d0 a)
			:double (* 1.0d0 b) :int (cffi:foreign-enum-value :norm-enum norm-type)
			:pointer (if mask (ref mask) (cffi-sys:null-pointer))))




(define-method or ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional mask)
  (cffi:foreign-funcall "cvOr" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)
			:pointer (if mask (ref mask) (cffi-sys:null-pointer))))

(define-method or-s ((src cv-arr) (value cv-scalar) (dst cv-arr) &optional mask)
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_OrS" :pointer (ref src) :pointer native-cv-scalar :pointer (ref dst)
			  :pointer (if mask (ref mask) (cffi-sys:null-pointer)))))


(define-method reduce ((src cv-arr) (dst cv-arr) &optional (dim -1) (op :cv-reduce-sum))
  (cffi:foreign-funcall "cvReduce" :pointer (ref src) :pointer (ref dst) :int (floor dim)
			:int (cffi:foreign-enum-value :reduce-enum op)))

(define-method repeat ((src cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvRepeat" :pointer (ref src) :pointer (ref dst)))


(define-method set ((src cv-arr) (value cv-scalar) &optional mask)
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_Set" :pointer (ref src) :pointer native-cv-scalar
			  :pointer (if mask (ref mask) (cffi-sys:null-pointer)))))


(define-method solve ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional (method :cv-lu))
  (cffi:foreign-funcall "cvSolve" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst) :int (floor method)
			:int))

(define-method split ((src cv-arr) dst0 dst1 dst2 dst3)
  (cffi:foreign-funcall "cvSplit" :pointer (ref src)
				  :pointer (if dst0 (ref dst0) (cffi-sys:null-pointer))
				  :pointer (if dst1 (ref dst1) (cffi-sys:null-pointer))
				  :pointer (if dst2 (ref dst2) (cffi-sys:null-pointer))
				  :pointer (if dst3 (ref dst3) (cffi-sys:null-pointer))))


(define-method cvt-pix-to-plane ((src cv-arr) dst0 dst1 dst2 dst3)
  (cffi:foreign-funcall "cv_CvtPixToPlane" :pointer (ref src)
					   :pointer (if dst0 (ref dst0) (cffi-sys:null-pointer))
					   :pointer (if dst1 (ref dst1) (cffi-sys:null-pointer))
					   :pointer (if dst2 (ref dst2) (cffi-sys:null-pointer))
					   :pointer (if dst3 (ref dst3) (cffi-sys:null-pointer))))



(define-method sub ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional mask)
  (cffi:foreign-funcall "cvSub" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)
			:pointer (if mask (ref mask) (cffi-sys:null-pointer))))


(define-method sub-s ((src cv-arr) (value cv-scalar) (dst cv-arr) &optional mask)
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_SubS" :pointer (ref src) :pointer native-cv-scalar :pointer (ref dst)
			  :pointer (if mask (ref mask) (cffi-sys:null-pointer)))))

(define-method sub-r-s ((src cv-arr) (value cv-scalar) (dst cv-arr) &optional mask)
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_SubRS" :pointer (ref src) :pointer native-cv-scalar
			  :pointer (ref dst) :pointer (if mask (ref mask) (cffi-sys:null-pointer)))))

(define-method svd ((a cv-arr) (w cv-arr) &optional u v flags)
  (cffi:foreign-funcall "cvSVD" :pointer (ref a) :pointer (ref w) :pointer (if u (ref u) (cffi-sys:null-pointer))
				:pointer (if v (ref v) (cffi-sys:null-pointer))
				:int (case flags
				       (0 0)
				       (otherwise (cffi:foreign-enum-value :svd-enum flags)))))

(define-method svbksb ((w cv-arr) (u cv-arr) (v cv-arr) (b cv-arr) (x cv-arr) &optional (flags 0))
  (cffi:foreign-funcall "cvSVBkSb" :pointer (ref w) :pointer (ref u) :pointer (ref v) :pointer (ref b)
			:pointer (ref x) :int (case flags
						(0 0)
						(otherwise (cffi:foreign-enum-value :svd-enum flags)))))


(define-method transpose ((src cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvTranspose" :pointer (ref src) :pointer (ref dst)))

(define-method xor ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) &optional mask)
  (cffi:foreign-funcall "cvXor" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)
			:pointer (if mask (ref mask) (cffi-sys:null-pointer))))

(define-method xor-s ((src cv-arr) (value cv-scalar) (dst cv-arr) &optional mask)
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_XorS" :pointer (ref src) :pointer native-cv-scalar :pointer (ref dst)
			  :pointer (if mask (ref mask) (cffi-sys:null-pointer)))))

(define-method trace ((mat cv-arr))
  (let ((cv-scalar (scalar 0)))
    (cffi:with-foreign-object (native-cv-scalar '(:struct CvScalar))
      (cffi:foreign-funcall "cv_Trace" :pointer (ref mat) :pointer native-cv-scalar)
      (cffi:with-foreign-slots ((val) native-cv-scalar (:struct CvScalar))
	(dotimes (i 4)
	  (setf (nth i (val cv-scalar)) (cffi:mem-aref val :double i)))))
    cv-scalar))

(define-method set-identity ((mat cv-arr) &optional (value 1))
  (with-cv-scalar (native-cv-scalar value)
    (cffi:foreign-funcall "cv_SetIdentity" :pointer (ref mat) :pointer native-cv-scalar)))

(define-method sum ((arr cv-arr))
  (let ((cv-scalar (scalar 0)))
    (cffi:with-foreign-object (native-cv-scalar '(:struct CvScalar))
      (cffi:foreign-funcall "cv_Sum" :pointer (ref arr) :pointer native-cv-scalar)
      (cffi:with-foreign-slots ((val) native-cv-scalar (:struct CvScalar))
	(dotimes (i 4)
	  (setf (nth i (val cv-scalar)) (cffi:mem-aref val :double i)))))
    cv-scalar))






;;; 스무딩
(define-method smooth ((src cv-arr) (dst cv-arr) &optional (smooth-type :cv-gaussian)
		       (size1 3) (size2 0) (size3 0) (size4 0))
  (cffi:foreign-funcall "cvSmooth" :pointer (ref src) :pointer (ref dst)
			:int (cffi:foreign-enum-value :smooth-method-enum smooth-type)
			:int (floor size1) :int (floor size2) :double (* 1.0d0 size3) :double (* 1.0d0 size4)))

;;; 팽창과 침식

(define-method erode ((src ipl-image) (dst ipl-image) &optional b (iterations 1))
  (cffi:foreign-funcall "cvErode" :pointer (ref src) :pointer (ref dst)
			:pointer (if b (ref b) (cffi-sys:null-pointer)) :int (floor iterations)))

(define-method dilate ((src ipl-image) (dst ipl-image) &optional b (iterations 1))
  (cffi:foreign-funcall "cvDilate" :pointer (ref src) :pointer (ref dst) :pointer (if b (ref b) (cffi-sys:null-pointer)) :int (floor iterations)))

(define-method morphology-ex ((src cv-arr) (dst cv-arr) temp (element ipl-conv-kernel)
			      (operation symbol) &optional (iterations 1))
  (cffi:foreign-funcall "cvMorphologyEx" :pointer (ref src)
					 :pointer (ref dst)
					 :pointer (if temp (ref temp) (cffi-sys:null-pointer))
					 :pointer (ref element)
					 :int (cffi:foreign-enum-value :morphology-enum operation)
					 :int (floor iterations)))


(defun encode-flood-flags (val)
  (if (listp val) (apply 'logior (mapcar #!(if (numberp %1) %1 (cffi:foreign-enum-value :flood-fill-enum %1)) val))
      val))

(define-method flood-fill ((img ipl-image) (seed-point cv-point) (new-val cv-scalar)
			   &optional (lo-diff (scalar-all 0)) (up-diff (scalar-all 0))
			   (comp (cffi-sys:null-pointer)) (flags 4) mask)
  (with-cv-point (native-seed-point seed-point)
    (with-cv-scalar (native-new-val new-val)
      (with-cv-scalar (native-lo-diff lo-diff)
	(with-cv-scalar (native-up-diff up-diff)
	  (cffi:foreign-funcall "cv_FloodFill" :pointer (ref img) :pointer native-seed-point
				:pointer native-new-val
				:pointer native-lo-diff
				:pointer native-up-diff
				:pointer comp
				:int (encode-flood-flags flags)
				:pointer (if mask (ref mask) (cffi-sys:null-pointer))))))))

(define-method resize ((src cv-arr) (dst cv-arr) &optional (interpolation :cv-inter-linear))
  (cffi:foreign-funcall "cvResize" :pointer (ref src) :pointer (ref dst) :int (cffi:foreign-enum-value :resize-enum interpolation)))

(define-method pyr-down ((src ipl-image) (dst ipl-image) &optional (filter :ipl-gaussian-5x5))
  (cffi:foreign-funcall "cvPyrDown" :pointer (ref src) :pointer (ref dst) :int (cffi:foreign-enum-value :pyramid-enum filter)))

(define-method pyr-up ((src ipl-image) (dst ipl-image) &optional (filter :ipl-gaussian-5x5))
  (cffi:foreign-funcall "cvPyrUp" :pointer (ref src) :pointer (ref dst) :int (cffi:foreign-enum-value :pyramid-enum filter)))



(define-method pyr-segmentation ((src ipl-image) (dst ipl-image) (storage cv-mem-storage)
				 (comp cv-seq) (level number) (threshold1 number) (threshold2 number))
  (cffi:foreign-funcall "cvPyrSegmentation" :pointer (ref src) :pointer (ref dst) :pointer (ref storage)
			:pointer (ref comp) :int (floor level) :double (* 1.0d0 threshold1) :double (* 1.0d0 threshold2)))


(define-method threshold ((src ipl-image) (dst ipl-image) (threshold number) (max-value number) (threshold-type symbol))
  (cffi:foreign-funcall "cvThreshold" :pointer (ref src) :pointer (ref dst) :double (* 1.0d0 threshold)
			:double (* 1.0d0 max-value) :int  (cffi:foreign-enum-value :threshold-enum threshold-type)))


(define-method adaptive-threshold ((src cv-arr) (dst cv-arr) (max-val number) &optional (adaptive-method :cv-adaptive-thresh-mean-c)
				   (threshold-type :cv-thresh-binary) (block-size 3) (param1 5))
  (cffi:foreign-funcall "cvAdaptiveThreshold" :pointer (ref src) :pointer (ref dst) :double (* 1.0d0 max-val)
			:int (cffi:foreign-enum-value :adaptive-method-enum adaptive-method)
			:int (cffi:foreign-enum-value :threshold-enum threshold-type)
			:int (floor block-size) :double (* 1.0d0 param1)))


(define-method filter-2d ((src cv-arr) (dst cv-arr) (kernel cv-mat) &optional (anchor (point -1 -1)))
  (with-cv-point (native-cv-point anchor)
    (cffi:foreign-funcall "cv_Filter2D" :pointer (ref src) :pointer (ref dst) :pointer (ref kernel)
			  :pointer native-cv-point)))

(define-method copy-make-border ((src cv-arr) (dst cv-arr) (offset cv-point) (bordertype symbol)
				 &optional (value (scalar-all 0)))
  (with-cv-point (native-cv-offset offset)
    (with-cv-scalar (native-cv-value value)
      (cffi:foreign-funcall "cv_CopyMakeBorder" :pointer (ref src) :pointer (ref dst)
			    :pointer native-cv-offset
			    :int (cffi:foreign-enum-value :make-border-enum  bordertype)
			    :pointer native-cv-value))))

(define-method sobel ((src cv-arr) (dst cv-arr) (x-order number) (y-order number)
		      &optional (aperture-size 3))
  (cffi:foreign-funcall "cvSobel" :pointer (ref src) :pointer (ref dst) :int (floor x-order) :int (floor y-order)
			:int (floor (if (eql aperture-size :cv-scharr) -1 aperture-size))))

(define-method laplace ((src cv-arr) (dst cv-arr) &optional (aperture-size 3))
  (cffi:foreign-funcall "cvLaplace" :pointer (ref src) :pointer (ref dst) :int (floor aperture-size)))

(define-method canny ((image cv-arr) (edges cv-arr) (threshold1 number) (threshold2 number)
		      &optional (aperture-size 3))
  (cffi:foreign-funcall "cvCanny" :pointer (ref image) :pointer (ref edges) :double (* 1.0d0 threshold1)
			:double (* 1.0d0 threshold2) :int (floor aperture-size)))



(defun remap-warp-flags (op flags)
  (if (= 2 (length flags))
      (funcall op (cffi:foreign-enum-value :remap-enum (first flags))
	       (cffi:foreign-enum-value :warp-affine-enum (second flags)))
      (cffi:foreign-enum-value :remap-enum (first flags))))

(define-method remap ((src cv-arr) (dst cv-arr) (map-x cv-arr) (map-y cv-arr)
		      &optional (flags '(:cv-inter-linear :cv-warp-fill-outliers))
		      (fillval (scalar-all 0)))
  (with-cv-scalar (native-cv-fillval fillval)
    (cffi:foreign-funcall "cv_Remap" :pointer (ref src) :pointer (ref dst) :pointer (ref map-y)
			  :pointer (ref map-y) :int (remap-warp-flags 'logior flags)
			  :pointer native-cv-fillval)))

(define-method warp-affine ((src cv-arr) (dst cv-arr) (map-matrix cv-mat)
			    &optional (flags '(:cv-inter-linear :cv-warp-fill-outliers))
			    (fillval (scalar-all 0)))
  (with-cv-scalar (native-cv-fillval fillval)
    (cffi:foreign-funcall "cv_WarpAffine" :pointer (ref src) :pointer (ref dst)
			  :pointer (ref map-matrix) :int (remap-warp-flags '+ flags)
			  :pointer native-cv-fillval)))

(define-method get-quadrangle-sub-pix ((src cv-arr) (dst cv-arr) (map-matrix cv-mat))
  (cffi:foreign-funcall "cvGetQuadrangleSubPix" :pointer (ref src) :pointer (ref dst) :pointer (ref map-matrix)))


(define-method get-affine-transform ((pts-src list) (pts-dst list) (map-matrix cv-mat))
  (cffi:with-foreign-object (native-pts-src '(:struct CvPoint2D32f) (length pts-src))
    (dotimes (i (length pts-src))
      (cffi:with-foreign-slots ((x y) (cffi:mem-aptr native-pts-src '(:struct CvPoint2D32f) i) (:struct CvPoint2D32f))
	(setf x (x (nth i pts-src))
	      y (y (nth i pts-src)))))
    (cffi:with-foreign-object (native-pts-dst '(:struct CvPoint2D32f) (length pts-dst))
      (dotimes (i (length pts-dst))
	(cffi:with-foreign-slots ((x y) (cffi:mem-aptr native-pts-dst '(:struct CvPoint2D32f) i) (:struct CvPoint2D32f))
	  (setf x (x (nth i pts-dst))
		y (y (nth i pts-dst)))))
      (cffi:foreign-funcall "cvGetAffineTransform" :pointer native-pts-src :pointer  native-pts-dst
			    :pointer (ref map-matrix)))))

(define-method 2d-rotation-matrix ((center cv-point-2d-32f) angle scale (map-matrix cv-mat))
  (cffi:with-foreign-object (native-cv-center '(:struct CvPoint2D32f))
    (cffi:with-foreign-slots ((x y) native-cv-center (:struct CvPoint2D32f))
      (setf x (x center)
	    y (y center))
      (cffi:foreign-funcall "cv_2DRotationMatrix" :pointer native-cv-center
			    :double (* 1.0d0 angle) :double (* 1.0d0 scale)  :pointer (ref map-matrix)))))


(define-method transform ((src cv-arr) (dst cv-arr) (trans-mat cv-mat) &optional (shift-vec nil))
  (cffi:foreign-funcall "cvTransform" :pointer (ref src) :pointer (ref dst) :pointer (ref trans-mat)
			:pointer (if shift-vec (ref shift-vec) (cffi-sys:null-pointer))))


(define-method warp-perspective ((src cv-arr) (dst cv-arr) (map-matrix cv-mat)
				 &optional (flags '(:cv-inter-linear :cv-warp-fill-outliers))
				 (fillval (scalar-all 0)))
  (with-cv-scalar (native-fill-val fillval)
    (cffi:foreign-funcall "cv_WarpPerspective" :pointer (ref src) :pointer (ref dst)
			  :pointer (ref map-matrix) :int (remap-warp-flags '+ flags) :pointer native-fill-val)))


(define-method get-perspective-transform ((pts-src list) (pts-dst list) (map-matrix cv-mat))
  (cffi:with-foreign-object (native-pts-src '(:struct CvPoint2D32f) (length pts-src))
    (dotimes (i (length pts-src))
      (cffi:with-foreign-slots ((x y) (cffi:mem-aptr native-pts-src '(:struct CvPoint2D32f) i)  (:struct CvPoint2D32f))
	(setf x (x (nth i pts-src))
	      y (y (nth i pts-src)))))
    (cffi:with-foreign-object (native-pts-dst '(:struct CvPoint2D32f) (length pts-dst))
      (dotimes (i (length pts-dst))
	(cffi:with-foreign-slots ((x y) (cffi:mem-aptr native-pts-dst '(:struct CvPoint2D32f) i) (:struct CvPoint2D32f))
	  (setf x (x (nth i pts-dst))
		y (y (nth i pts-dst)))))
      (cffi:foreign-funcall "cvGetPerspectiveTransform" :pointer native-pts-src
			    :pointer native-pts-dst :pointer (ref map-matrix)))))

(define-method perspective-transform ((src cv-arr) (dst cv-arr) (mat cv-mat))
  (cffi:foreign-funcall "cvPerspectiveTransform" :pointer (ref src) :pointer (ref dst) :pointer (ref mat)))


(define-method cart-to-polar ((x cv-arr) (y cv-arr) (magnitude cv-arr)
			      &optional angle (angle-in-degrees 0))
  (cffi:foreign-funcall "cvCartToPolar" :pointer (ref x) :pointer (ref y) :pointer (ref magnitude)
			:pointer (if angle (ref angle) (cffi-sys:null-pointer))
			:int (floor angle-in-degrees)))

(define-method polar-to-cart ((magnitude cv-arr) (angle cv-arr) (x cv-arr)
			      (y cv-arr) &optional (angle-in-degrees 0))
  (cffi:foreign-funcall "cvPolarToCart" :pointer (ref magnitude) :pointer (ref angle)
			:pointer (ref x) :pointer  (ref y) :int	(floor angle-in-degrees)))


(define-method log-polar ((src cv-arr) (dst cv-arr) (center cv-point-2d-32f) m
			  &optional (flags '(:cv-inter-linear :cv-warp-fill-outliers)))
  (cffi:with-foreign-object (native-center '(:struct CvPoint2D32f))
    (cffi:with-foreign-slots ((x y) native-center (:struct CvPoint2D32f))
      (setf x (x center)
	    y (y center)))
    (cffi:foreign-funcall "cv_LogPolar" :pointer (ref src) :pointer (ref dst) :pointer native-center
			  :double (* 1.0d0 m) :int (remap-warp-flags '+ flags))))

(define-method dft ((src cv-arr) (dst cv-arr) (flags symbol) &optional (nonzero-rows 0))
  (cffi:foreign-funcall "cvDFT" :pointer (ref src) :pointer (ref dst) :int (cffi:foreign-enum-value :dxt-enum flags)
			:int (floor nonzero-rows)))

(define-method get-optimal-dft-size ((size0 number))
  (cffi:foreign-funcall "cvGetOptimalDFTSize" :int (floor size0) :int))

(define-method mul-spectrums ((src1 cv-arr) (src2 cv-arr) (dst cv-arr) flags)
  (labels ((flags ()
	     (apply '+ (mapcar #!(cffi:foreign-enum-value :dxt-enum %1) (su:mklist flags)))))
    (cffi:foreign-funcall "cvMulSpectrums" :pointer (ref src1) :pointer (ref src2) :pointer (ref dst)
			  :int (flags))))

(define-method dct ((src cv-arr) (dst cv-arr) flags)
  (labels ((flags ()
	     (apply '+ (mapcar #!(cffi:foreign-enum-value :dxt-enum %1) (su:mklist flags)))))
    (cffi:foreign-funcall "cvDCT" :pointer (ref src) :pointer (ref dst) :int (flags))))

(define-method integral ((image cv-arr) (sum cv-arr) &optional sqsum tilted-sum)
  (cffi:foreign-funcall "cvIntegral" :pointer (ref image) :pointer (ref sum)
				     :pointer (if sqsum (ref sqsum) (cffi-sys:null-pointer))
				     :pointer (if tilted-sum (ref tilted-sum) (cffi-sys:null-pointer))))


(define-method dist-transform ((src cv-arr) (dst cv-arr) &optional (distance-type :cv-dist-l2)
			       (mask-size 3) kernel labels)
  (let (native-kernel)
    (when kernel
      (setf native-kernel (cffi:foreign-alloc :float :count (length kernel)))
      (dotimes (i (length kernel))
	(setf (cffi:mem-aref native-kernel :float i) (coerce (nth i kernel) 'single-float))))
    (unwind-protect
	 (cffi:foreign-funcall "cvDistTransform" :pointer (ref src) :pointer (ref dst) :int  (cffi:foreign-enum-value :dist-enum distance-type) :int (floor mask-size) :pointer (if kernel native-kernel (cffi-sys:null-pointer))
			       :pointer (if labels (ref labels) (cffi-sys:null-pointer)))
      (when kernel
	(cffi:foreign-string-free native-kernel)))))

(define-method equalize-hist ((src cv-arr) (dst cv-arr))
  (cffi:foreign-funcall "cvEqualizeHist" :pointer (ref src) :pointer (ref dst)))


(define-method hough-lines ((image cv-arr) (lines cv-mem-storage) (rho number) (theta number) (threshold number) &key (method :cv-hough-standard) (param1 0.0d0) (param2 0.0d0))
  (make-instance 'cv-seq
		 :ref (cffi:foreign-funcall "cvHoughLines2" :pointer (ref image) :pointer (ref lines)
					    :int (cffi:foreign-enum-value :hough-enum method)
					    :double (* 1.0d0 rho) :double (* 1.0d0 theta) :int (floor threshold)
					    :double (* 1.0d0 param1) :double (* 1.0d0 param2)
					    :pointer)))

(define-method hough-circles ((image cv-arr) (circles cv-mem-storage) (dp number) (min-dist number) &key (method :cv-hough-gradient) (param1 100.0d0) (param2 100.0d0) (min-radius 0) (max-radius 0))
  (make-instance 'cv-seq
		 :ref (cffi:foreign-funcall "cvHoughCircles" :pointer (ref image) :pointer (ref circles)
					    :int (cffi:foreign-enum-value :hough-enum method)
					    :double (* 1.0d0 dp) :double (* 1.0d0 min-dist)
					    :double (* 1.0d0 param1) :double (* 1.0d0 param2)
					    :int (floor min-radius) :int (floor max-radius)
					    :pointer)))
