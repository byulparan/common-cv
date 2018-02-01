(in-package :cv)

;; ****************************************************************************************\
;;          Array allocation, deallocation, initialization and access to elements         *
;; ***************************************************************************************/

(cffi:defcfun ("cvCreateImageHeader" create-image-header) :pointer
  (size (:struct size))
  (depth :ipl-depth-enum)
  (channels :int))

(cffi:defcfun ("cvCreateImage" create-image) :pointer
  (size (:struct size))
  (depth :ipl-depth-enum)
  (channels :int))

(cffi:defcfun ("cvReleaseImage" release-image) :void
  (ptr :pointer))

(cffi:defcfun ("cvReleaseImageHeader" release-image-header) :void
  (ptr :pointer))

(cffi:defcfun ("cvCloneImage" clone-image) :pointer
  (ipl-image :pointer))

(cffi:defcfun ("cvSetImageROI" set-image-roi) :void
  (ipl :pointer)
  (rect (:struct rect)))

(cffi:defcfun ("cvResetImageROI" reset-image-roi) :void
  (ipl :pointer))



(cffi:defcfun ("cvCreateMat" create-mat) (:pointer (:struct mat))
  (rows :int)
  (cols :int)
  (type :mat-type-enum))

(cffi:defcfun ("cvReleaseMat" release-mat) :void
  (ptr :pointer))


(cffi:defcfun ("cvGetSubRect" get-sub-rect) :pointer
  (arr :pointer)
  (submat :pointer)
  (rect (:struct rect)))



(defun get-row (arr submat row)
  (get-rows arr submat row (+ row 1) 1))

(cffi:defcfun ("cvGetRows" get-rows) :pointer
  (arr :pointer)
  (submat :pointer)
  (start-row :int)
  (end-row :int)
  (delta-row :int))

(defun get-col (arr submat col)
  (get-cols arr submat col (+ col 1)))

(cffi:defcfun ("cvGetCols" get-cols) :pointer
  (arr :pointer)
  (submat :pointer)
  (start-col :int)
  (end-col :int))

(cffi:defcfun ("cvGetDiag" get-diag) :pointer
  (arr :pointer)
  (submat :pointer)
  (diag :int))

(cffi:defcfun ("cvGetDims" get-dims) :int
  (arr :pointer)
  (sizes :pointer))

(cffi:defcfun ("cvGetDimSize" get-dim-size) :int
  (arr :pointer)
  (index :int))


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

(cffi:defcfun ("cvRepeat" repeat) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("cvGetSize" get-size) (:struct size)
  (arr :pointer))

(cffi:defcfun ("cvCopy" copy) :void
  (src :pointer)
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvSet" set) :void
  (src :pointer)
  (value (:struct scalar))
  (mask :pointer))

(cffi:defcfun ("cvSetZero" set-zero) :void
  (arr :pointer))

(cffi:defcfun ("cvSplit" split) :void
  (src :pointer)
  (dst0 :pointer)
  (dst1 :pointer)
  (dst2 :pointer)
  (dst3 :pointer))

(cffi:defcfun ("cvMerge" merge) :void
  (src0 :pointer)
  (src1 :pointer)
  (src2 :pointer)
  (src3 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvConvertScale" convert-scale) :void
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

(cffi:defcfun ("cvConvertScaleAbs" convert-scale-abs) :void
  (src :pointer)
  (dst :pointer)
  (scale :double)
  (shift :double))


;; ****************************************************************************************\
;;                   Arithmetic, logic and comparison operations                          *
;;  ***************************************************************************************/

(cffi:defcfun ("cvAdd" add) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvAddS" add-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvSub" sub) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvSubS" sub-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvSubRS" sub-r-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvMul" mul) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double))

(cffi:defcfun ("cvDiv" div) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double))


(cffi:defcfun ("cvAddWeighted" add-weighted) :void
  (src :pointer)
  (alpha :double)
  (src2 :pointer)
  (beta :double)
  (gamma :double)
  (dst :pointer))

(cffi:defcfun ("cvDotProduct" dot-product) :double
  (src1 :pointer)
  (src2 :pointer))


(cffi:defcfun ("cvAnd" and) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvAndS" and-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvOr" or) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvOrS" or-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvXor" xor) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvXorS" xor-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvNot" not) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("cvInRange" in-range) :void
  (src :pointer)
  (lower :pointer)
  (upper :pointer)
  (dst :pointer))

(cffi:defcfun ("cvInRangeS" in-range-s) :void
  (src :pointer)
  (lower (:struct scalar))
  (upper (:struct scalar))
  (dst :pointer))

(cffi:defcfun ("cvCmp" cmp) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (cmp-op :cmp-enum))

(cffi:defcfun ("cvCmpS" cmp-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer)
  (cmp-op :cmp-enum))

(cffi:defcfun ("cvMin" min) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvMax" max) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvMinS" min-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer))

(cffi:defcfun ("cvMaxS" max-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer))

(cffi:defcfun ("cvAbsDiff" abs-diff) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvAbsDiffS" abs-diff-s) :void
  (src :pointer)
  (dst :pointer)
  (value (:struct scalar)))

(defun abs (src dst)
  (abs-diff-s src dst (scalar-all 0.0d0)))


;; ****************************************************************************************\
;;                                Math operations                                         *
;; ***************************************************************************************/

(cffi:defcfun ("cvCartToPolar" cart-to-polar) :void
  (x :pointer)
  (y :pointer)
  (magintude :pointer)
  (angle :pointer)
  (angle-in-degrees :int))

(cffi:defcfun ("cvPolarToCart" polar-to-cart) :void
  (magnitude :pointer)
  (angle :pointer)
  (x :pointer)
  (y :pointer)
  (angle-in-degrees :int))



;; ****************************************************************************************\
;;                                Matrix operations                                       *
;  ***************************************************************************************/

(cffi:defcfun ("cvCrossProduct" cross-product) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvGEMM" gemm) :void
  (src1 :pointer)
  (src2 :pointer)
  (alpha :double)
  (src3 :pointer)
  (beta :double)
  (dst :pointer)
  (t-abc :int))

(cffi:defcfun ("cvTransform" transform) :void
  (src :pointer)
  (dst :pointer)
  (trans-mat :pointer)
  (shift-vec :pointer))

(cffi:defcfun ("cvPerspectiveTransform" perspective-transform) :void
  (src :pointer)
  (dst :pointer)
  (mat :pointer))

(cffi:defcfun ("cvTranspose" transpose) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("cvFlip" flip) :void
  (src :pointer)
  (dst :pointer)
  (flip-mode :int))

(cffi:defcfun ("cvSVD" svd) :void
  (a :pointer)
  (w :pointer)
  (u :pointer)
  (v :pointer)
  (flags :svd-enum))

(cffi:defcfun ("cvSVBkSb" svbksb) :void
  (w :pointer)
  (u :pointer)
  (v :pointer)
  (b :pointer)
  (x :pointer)
  (flags :svd-enum))

(cffi:defcfun ("cvInvert" invert) :double
  (src :pointer)
  (dst :pointer)
  (method :invert-enum))

(defun inv (src dst &optional (method :cv-lu))
  (invert src dst method))

(cffi:defcfun ("cvSolve" solve) :int
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (method :int))

(cffi:defcfun ("cvDet" det) :double
  (mat :pointer))

(cffi:defcfun ("cvTrace" trace) (:struct scalar)
  (mat :pointer))

(cffi:defcfun ("cvEigenVV" eigen-vv) :void
  (mat :pointer)
  (evects :pointer)
  (evals :pointer)
  (eps :double)
  (low-index :int)
  (high-index :int))

(cffi:defcfun ("cvSetIdentity" set-identity) :void
  (mat :pointer)
  (value (:struct scalar)))


(cffi:defcfun ("cvCalcCovarMatrix" calc-covar-matrix) :void
  (vects :pointer)
  (count :int)
  (cov-mat :pointer)
  (avg :pointer)
  (flags :covar-matrix-enum))


;; ****************************************************************************************\
;;                                    Array Statistics                                    *
;; ***************************************************************************************/

(cffi:defcfun ("cvSum" sum) (:struct scalar)
  (arr :pointer))

(cffi:defcfun ("cvCountNonZero" count-non-zero) :int
  (arr :pointer))

(cffi:defcfun ("cvAvg" avg) (:struct scalar)
  (arr :pointer)
  (mask :pointer))

(cffi:defcfun ("cvAvgSdv" avg-sdv) :void
  (arr :pointer)
  (mean :pointer)
  (std-dev :pointer)
  (mask :pointer))

(cffi:defcfun ("cvMinMaxLoc" min-max-loc) :void
  (arr :pointer)
  (min-val :pointer)
  (max-val :pointer)
  (min-loc :pointer)
  (max-loc :pointer)
  (mask :pointer))

(cffi:defcfun ("cvNorm" norm) :double
  (arr1 :pointer)
  (arr2 :pointer)
  (norm-type :norm-enum)
  (mask :pointer))

(cffi:defcfun ("cvNormalize" normalize) :void
  (src1 :pointer)
  (dst :pointer)
  (a :double)
  (b :double)
  (norm-type :norm-enum)
  (mask :pointer))

(cffi:defcfun ("cvReduce" reduce) :void
  (src :pointer)
  (dst :pointer)
  (dim :int)
  (op :reduce-enum))

;; ****************************************************************************************\
;;                      Discrete Linear Transforms and Related Functions                  *
;; ***************************************************************************************/

(cffi:defcfun ("cvDFT" dft) :void
  (src :pointer)
  (dst :pointer)
  (flags :dxt-enum)
  (nonzero-rows :int))

(cffi:defcfun ("cvMulSpectrums" mul-spectrums) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (flags :dxt-enum))

(cffi:defcfun ("cvGetOptimalDFTSize" get-optimal-dft-size) :int
  (size0 :int))

(cffi:defcfun ("cvDCT" dct) :void
  (src :pointer)
  (dst :pointer)
  (flags :dxt-enum))


;;**************************************************************************************
;;                              Dynamic data structures                                 
;;**************************************************************************************

;;; CvMemStorage
(cffi:defcfun ("cvCreateMemStorage" create-mem-storage) :pointer
  (block-size :int))

(cffi:defcfun ("cvReleaseMemStorage" release-mem-storage) :void
  (ptr :pointer))

(cffi:defcfun ("cvClearMemStorage" clear-mem-storage) :void
  (mem-storage :pointer))

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

(cffi:defcfun ("cvSeqElemIdx" seq-elem-idx) :int
  (seq :pointer)
  (element :pointer)
  (block :pointer))

(cffi:defcfun ("cvStartWriteSeq" start-write-seq) :void
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



;; ****************************************************************************************\
;;                                    Data Persistence                                    *
;; ****************************************************************************************/

(cffi:defcfun ("cvLoad" load) :pointer
  (filename :string)
  (memstorage :pointer)
  (name :string)
  (real-name :string))


