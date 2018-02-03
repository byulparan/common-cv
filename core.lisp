(in-package :cv)

;; ****************************************************************************************\
;;          Array allocation, deallocation, initialization and access to elements         *
;; ***************************************************************************************/

(define-cfun ("cvCreateImageHeader" create-image-header) :pointer
  (size (:struct size))
  (depth :ipl-depth-enum)
  (channels :int))

(define-cfun ("cvCreateImage" create-image) :pointer
  (size (:struct size))
  (depth :ipl-depth-enum)
  (channels :int))

(cffi:defcfun ("cvReleaseImage" %release-image) :void
  (ptr :pointer))

(defun release-image (ipl-image)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) ipl-image)
    (%release-image ptr)))

(cffi:defcfun ("cvReleaseImageHeader" %release-image-header) :void
  (ptr :pointer))

(defun release-image-header (ipl-image)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) ipl-image)
    (%release-image-header ptr)))

(define-cfun ("cvCloneImage" clone-image) :pointer
  (ipl-image :pointer))

(define-cfun ("cvSetImageROI" set-image-roi) :void
  (ipl :pointer)
  (rect (:struct rect)))

(define-cfun ("cvResetImageROI" reset-image-roi) :void
  (ipl :pointer))



(define-cfun ("cvCreateMat" create-mat) (:pointer (:struct mat))
  (rows :int)
  (cols :int)
  (type :mat-type-enum))

(cffi:defcfun ("cvReleaseMat" %release-mat) :void
  (ptr :pointer))

(defun release-mat (mat)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) mat)
    (%release-mat ptr)))

(define-cfun ("cvGetSubRect" get-sub-rect) :pointer
  (arr :pointer)
  (submat :pointer)
  (rect (:struct rect)))



(defun get-row (arr submat row)
  (get-rows arr submat row (+ row 1) 1))

(define-cfun ("cvGetRows" get-rows) :pointer
  (arr :pointer)
  (submat :pointer)
  (start-row :int)
  (end-row :int)
  (delta-row :int))

(defun get-col (arr submat col)
  (get-cols arr submat col (+ col 1)))

(define-cfun ("cvGetCols" get-cols) :pointer
  (arr :pointer)
  (submat :pointer)
  (start-col :int)
  (end-col :int))

(define-cfun ("cvGetDiag" get-diag) :pointer
  (arr :pointer)
  (submat :pointer)
  (diag :int))

(define-cfun ("cvGetDims" get-dims) :int
  (arr :pointer)
  (sizes :pointer))

(define-cfun ("cvGetDimSize" get-dim-size) :int
  (arr :pointer)
  (index :int))


(define-cfun ("cvGet1D" get-1d) (:struct scalar)
  (arr :pointer)
  (idx0 :int))

(define-cfun ("cvGet2D" get-2d) (:struct scalar)
  (arr :pointer)
  (idx0 :int)
  (idx1 :int))

(define-cfun ("cvGet3D" get-3d) (:struct scalar)
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int))

(define-cfun ("cvSet1D" set-1d) :void
  (arr :pointer)
  (idx0 :int)
  (value (:struct scalar)))

(define-cfun ("cvSet2D" set-2d) :void
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (value (:struct scalar)))

(define-cfun ("cvSet3D" set-3d) :void
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int)
  (value (:struct scalar)))

(define-cfun ("cvRepeat" repeat) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvGetSize" get-size) (:struct size)
  (arr :pointer))

(define-cfun ("cvCopy" copy) :void
  (src :pointer)
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvSet" set) :void
  (src :pointer)
  (value (:struct scalar))
  (mask :pointer))

(define-cfun ("cvSetZero" set-zero) :void
  (arr :pointer))

(define-cfun ("cvSplit" split) :void
  (src :pointer)
  (dst0 :pointer)
  (dst1 :pointer)
  (dst2 :pointer)
  (dst3 :pointer))

(define-cfun ("cvMerge" merge) :void
  (src0 :pointer)
  (src1 :pointer)
  (src2 :pointer)
  (src3 :pointer)
  (dst :pointer))

(define-cfun ("cvConvertScale" convert-scale) :void
  (src :pointer)
  (dst :pointer)
  (scale :double)
  (shift :double))

(defun scale (src dst &optional (scale 1.0d0) (shift 0.0d0))
  (convert-scale src dst scale shift))

(defun convert (src dst)
  (convert-scale src dst 1.0d0 0.0d0))

(defun cvt-scale (src dst &optional (scale 1.0d0) (shift 0.0d0))
  (convert-scale src dst scale shift))

(define-cfun ("cvConvertScaleAbs" convert-scale-abs) :void
  (src :pointer)
  (dst :pointer)
  (scale :double)
  (shift :double))


;; ****************************************************************************************\
;;                   Arithmetic, logic and comparison operations                          *
;;  ***************************************************************************************/

(define-cfun ("cvAdd" add) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvAddS" add-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvSub" sub) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvSubS" sub-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvSubRS" sub-r-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvMul" mul) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double))

(define-cfun ("cvDiv" div) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double))


(define-cfun ("cvAddWeighted" add-weighted) :void
  (src :pointer)
  (alpha :double)
  (src2 :pointer)
  (beta :double)
  (gamma :double)
  (dst :pointer))

(define-cfun ("cvDotProduct" dot-product) :double
  (src1 :pointer)
  (src2 :pointer))


(define-cfun ("cvAnd" and) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvAndS" and-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvOr" or) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvOrS" or-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvXor" xor) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvXorS" xor-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(define-cfun ("cvNot" not) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvInRange" in-range) :void
  (src :pointer)
  (lower :pointer)
  (upper :pointer)
  (dst :pointer))

(define-cfun ("cvInRangeS" in-range-s) :void
  (src :pointer)
  (lower (:struct scalar))
  (upper (:struct scalar))
  (dst :pointer))

(define-cfun ("cvCmp" cmp) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (cmp-op :cmp-enum))

(define-cfun ("cvCmpS" cmp-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer)
  (cmp-op :cmp-enum))

(define-cfun ("cvMin" min) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(define-cfun ("cvMax" max) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(define-cfun ("cvMinS" min-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer))

(define-cfun ("cvMaxS" max-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer))

(define-cfun ("cvAbsDiff" abs-diff) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(define-cfun ("cvAbsDiffS" abs-diff-s) :void
  (src :pointer)
  (dst :pointer)
  (value (:struct scalar)))

(defun abs (src dst)
  (abs-diff-s src dst (scalar-all 0.0d0)))


;; ****************************************************************************************\
;;                                Math operations                                         *
;; ***************************************************************************************/

(define-cfun ("cvCartToPolar" cart-to-polar) :void
  (x :pointer)
  (y :pointer)
  (magintude :pointer)
  (angle :pointer)
  (angle-in-degrees :int))

(define-cfun ("cvPolarToCart" polar-to-cart) :void
  (magnitude :pointer)
  (angle :pointer)
  (x :pointer)
  (y :pointer)
  (angle-in-degrees :int))



;; ****************************************************************************************\
;;                                Matrix operations                                       *
;  ***************************************************************************************/

(define-cfun ("cvCrossProduct" cross-product) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(define-cfun ("cvGEMM" gemm) :void
  (src1 :pointer)
  (src2 :pointer)
  (alpha :double)
  (src3 :pointer)
  (beta :double)
  (dst :pointer)
  (t-abc :int))

(define-cfun ("cvTransform" transform) :void
  (src :pointer)
  (dst :pointer)
  (trans-mat :pointer)
  (shift-vec :pointer))

(define-cfun ("cvPerspectiveTransform" perspective-transform) :void
  (src :pointer)
  (dst :pointer)
  (mat :pointer))

(define-cfun ("cvTranspose" transpose) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvFlip" flip) :void
  (src :pointer)
  (dst :pointer)
  (flip-mode :int))

(define-cfun ("cvSVD" svd) :void
  (a :pointer)
  (w :pointer)
  (u :pointer)
  (v :pointer)
  (flags :svd-enum))

(define-cfun ("cvSVBkSb" svbksb) :void
  (w :pointer)
  (u :pointer)
  (v :pointer)
  (b :pointer)
  (x :pointer)
  (flags :svd-enum))

(define-cfun ("cvInvert" invert) :double
  (src :pointer)
  (dst :pointer)
  (method :invert-enum))

(defun inv (src dst &optional (method :cv-lu))
  (invert src dst method))

(define-cfun ("cvSolve" solve) :int
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (method :int))

(define-cfun ("cvDet" det) :double
  (mat :pointer))

(define-cfun ("cvTrace" trace) (:struct scalar)
  (mat :pointer))

(define-cfun ("cvEigenVV" eigen-vv) :void
  (mat :pointer)
  (evects :pointer)
  (evals :pointer)
  (eps :double)
  (low-index :int)
  (high-index :int))

(define-cfun ("cvSetIdentity" set-identity) :void
  (mat :pointer)
  (value (:struct scalar)))


(define-cfun ("cvCalcCovarMatrix" calc-covar-matrix) :void
  (vects :pointer)
  (count :int)
  (cov-mat :pointer)
  (avg :pointer)
  (flags :covar-matrix-enum))


;; ****************************************************************************************\
;;                                    Array Statistics                                    *
;; ***************************************************************************************/

(define-cfun ("cvSum" sum) (:struct scalar)
  (arr :pointer))

(define-cfun ("cvCountNonZero" count-non-zero) :int
  (arr :pointer))

(define-cfun ("cvAvg" avg) (:struct scalar)
  (arr :pointer)
  (mask :pointer))

(define-cfun ("cvAvgSdv" avg-sdv) :void
  (arr :pointer)
  (mean :pointer)
  (std-dev :pointer)
  (mask :pointer))

(define-cfun ("cvMinMaxLoc" min-max-loc) :void
  (arr :pointer)
  (min-val :pointer)
  (max-val :pointer)
  (min-loc :pointer)
  (max-loc :pointer)
  (mask :pointer))

(define-cfun ("cvNorm" norm) :double
  (arr1 :pointer)
  (arr2 :pointer)
  (norm-type :norm-enum)
  (mask :pointer))

(define-cfun ("cvNormalize" normalize) :void
  (src1 :pointer)
  (dst :pointer)
  (a :double)
  (b :double)
  (norm-type :norm-enum)
  (mask :pointer))

(define-cfun ("cvReduce" reduce) :void
  (src :pointer)
  (dst :pointer)
  (dim :int)
  (op :reduce-enum))

;; ****************************************************************************************\
;;                      Discrete Linear Transforms and Related Functions                  *
;; ***************************************************************************************/

(define-cfun ("cvDFT" dft) :void
  (src :pointer)
  (dst :pointer)
  (flags :dxt-enum)
  (nonzero-rows :int))

(define-cfun ("cvMulSpectrums" mul-spectrums) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (flags :dxt-enum))

(define-cfun ("cvGetOptimalDFTSize" get-optimal-dft-size) :int
  (size0 :int))

(define-cfun ("cvDCT" dct) :void
  (src :pointer)
  (dst :pointer)
  (flags :dxt-enum))


;;**************************************************************************************
;;                              Dynamic data structures                                 
;;**************************************************************************************

;;; CvMemStorage
(define-cfun ("cvCreateMemStorage" create-mem-storage) :pointer
  (block-size :int))

(cffi:defcfun ("cvReleaseMemStorage" %release-mem-storage) :void
  (ptr :pointer))

(defun release-mem-storage (storage)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) storage)
    (%release-mem-storage ptr)))

(define-cfun ("cvClearMemStorage" clear-mem-storage) :void
  (mem-storage :pointer))

(define-cfun ("cvCreateSeq" create-seq) :pointer
  (seq-flags :int)
  (header-size :unsigned-long)
  (elem-size :unsigned-long)
  (storage :pointer))

(define-cfun ("cvClearSeq" clear-seq) :void
  (seq :pointer))

(define-cfun ("cvSeqPush" seq-push) :pointer
  (seq :pointer)
  (element :pointer))

(define-cfun ("cvSeqPushFront" seq-push-front) :pointer
  (seq :pointer)
  (element :pointer))

(define-cfun ("cvSeqPop" seq-pop) :void
  (seq :pointer)
  (element :pointer))

(define-cfun ("cvSeqPopFront" seq-pop-front) :void
  (seq :pointer)
  (element :pointer))

(define-cfun ("cvSeqPushMulti" seq-push-multi) :void
  (seq :pointer)
  (elements :pointer)
  (count :int)
  (in-front :int))

(define-cfun ("cvSeqPopMulti" seq-pop-multi) :void
  (seq :pointer)
  (elements :pointer)
  (count :int)
  (in-front :int))

(define-cfun ("cvSeqInsert" seq-insert) :pointer
  (seq :pointer)
  (before-index :int)
  (element :pointer))

(define-cfun ("cvSeqRemove" seq-remove) :void
  (seq :pointer)
  (index :int))

(define-cfun ("cvGetSeqElem" get-seq-elem) :pointer
  (seq :pointer)
  (index :int))

(define-cfun ("cvSeqElemIdx" seq-elem-idx) :int
  (seq :pointer)
  (element :pointer)
  (block :pointer))

(define-cfun ("cvStartWriteSeq" start-write-seq) :void
  (seq-flags :int)
  (header-size :int)
  (elem-size :int)
  (storage :pointer)
  (writer :pointer))

(define-cfun ("cvStartAppendToSeq" start-append-to-seq) :void
  (seq :pointer)
  (seq-writer :pointer))

(define-cfun ("cvEndWriteSeq" end-write-seq) :pointer
  (seq-writer :pointer))

(define-cfun ("cvFlushSeqWriter" flush-seq-writer) :void
  (seq-writer :pointer))

(define-cfun ("cvStartReadSeq" start-read-seq) :pointer
  (seq :pointer)
  (seq-reader :pointer)
  (reverse :int))

(define-cfun ("cvGetSeqReaderPos" get-seq-reader-pos) :int
  (seq-reader :pointer))

(define-cfun ("cvSetSeqReaderPos" set-seq-reader-pos) :void
  (seq-reader :pointer)
  (index :int)
  (is-relative :int))

(define-cfun ("cvSeqSlice" seq-slice) :pointer
  (seq :pointer)
  (slice (:struct slice))
  (storage :pointer)
  (copy-data :int))

(define-cfun ("cvSeqRemoveSlice" seq-remove-slice) :void
  (seq :pointer)
  (slice (:struct slice)))

(define-cfun ("cvSeqInsertSlice" seq-insert-slice) :void
  (seq :pointer)
  (before-index :int)
  (from-arr :pointer))

(define-cfun ("cvSeqInvert" seq-invert) :void
  (seq :pointer))



;; ****************************************************************************************\
;;                                    Data Persistence                                    *
;; ****************************************************************************************/

(define-cfun ("cvLoad" load) :pointer
  (filename :string)
  (memstorage :pointer)
  (name :string)
  (real-name :string))


