(in-package :cv)

;; ****************************************************************************************\
;;          Array allocation, deallocation, initialization and access to elements         *
;; ***************************************************************************************/
(define-cfun ("cvAlloc" alloc) :pointer
  (size :sizet))

(define-cfun ("cvFree_" free) :void
  (ptr :pointer))

(define-cfun ("cvCreateImageHeader" create-image-header) :pointer
  (size (:struct size))
  (depth :int)
  (channels :int))

(define-cfun ("cvInitImageHeader" init-image-header) :pointer
  (image :pointer)
  (size (:struct size))
  (depth :int)
  (channels :int)
  (origin :int 0)
  (align :int 4))

(define-cfun ("cvCreateImage" create-image) :pointer
  (size (:struct size))
  (depth :int)
  (channels :int))

(cffi:defcfun ("cvReleaseImageHeader" %release-image-header) :void
  (ptr :pointer))

(defun release-image-header (ipl-image)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) ipl-image)
    (%release-image-header ptr)))

(cffi:defcfun ("cvReleaseImage" %release-image) :void
  (ptr :pointer))

(defun release-image (ipl-image)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) ipl-image)
    (%release-image ptr)))

(define-cfun ("cvCloneImage" clone-image) :pointer
  (ipl-image :pointer))

(define-cfun ("cvSetImageCOI" set-image-coi) :void
  (image :pointer)
  (coi :int))

(define-cfun ("cvGetImageCOI" get-image-coi) :int
  (image :pointer))

(define-cfun ("cvSetImageROI" set-image-roi) :void
  (image :pointer)
  (rect (:struct rect)))

(define-cfun ("cvResetImageROI" reset-image-roi) :void
  (image :pointer))

(define-cfun ("cvGetImageROI" get-image-roi) (:struct rect)
  (image :pointer))

(define-cfun ("cvCreateMatHeader" create-mat-header) :pointer
  (rows :int)
  (cols :int)
  (type :int))

;;#define CV_AUTOSTEP  0x7fffffff

(define-cfun ("cvInitMatHeader" init-mat-header) :pointer
  (mat :pointer)
  (rows :int)
  (cols :int)
  (type :int)
  (data :pointer (cffi:null-pointer))
  (step :int #x7fffffff))

(define-cfun ("cvCreateMat" create-mat) (:pointer (:struct mat))
  (rows :int)
  (cols :int)
  (type :int))

(cffi:defcfun ("cvReleaseMat" %release-mat) :void
  (ptr :pointer))

(defun release-mat (mat)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) mat)
    (%release-mat ptr)))

;;; cvDecRefData
;;; cvIncRefData

(define-cfun ("cvCloneMat" clone-mat) :pointer
  (mat :pointer))

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
  (delta-row :int 1))

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
  (diag :int 0))

(define-cfun ("cvScalarToRawData" scalar-to-row-data) :void
  (scalar :pointer)
  (data :pointer)
  (type :int)
  (extend-to-12 :int 0))

(define-cfun ("cvRawDataToScalar" raw-data-to-scalar) :void
  (data :pointer)
  (type :int)
  (scalar :pointer))

(define-cfun ("cvCreateMatNDHeader" create-mat-nd-header) :pointer
  (dims :int)
  (sizes :pointer)
  (type :int))

(define-cfun ("cvCreateMatND" create-mat-nd) :pointer
  (dims :int)
  (sizes :pointer)
  (type :int))

(define-cfun ("cvInitMatNDHeader" init-mat-nd-header) :pointer
  (mat-nd :pointer)
  (dims :int)
  (sizes :pointer)
  (type :int)
  (data :pointer (cffi:null-pointer)))

(defun release-mat-nd (mat)
  (release-mat mat))

(define-cfun ("cvCloneMatND" clone-mat-nd) :pointer
  (mat-nd :pointer))

(define-cfun ("cvCreateSparseMat" create-sparse-mat) :pointer
  (dims :int)
  (sizes :pointer)
  (type :int))

(cffi:defcfun ("cvReleaseSparseMat" %release-sparse-mat) :void
  (mat :pointer))

(defun release-sparse-mat (mat)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) mat)
    (%release-sparse-mat ptr)))

(define-cfun ("cvInitSparseMatIterator" init-sparse-mat-iterator) :pointer
  (sparse-mat :pointer)
  (mat-iterator :pointer))

;;; cvGetNextSparseNode
;;; cvInitNArrayIterator
;;; cvNextNArraySlice


(define-cfun ("cvGetElemType" get-elem-type) :int
  (arr :pointer))

(define-cfun ("cvGetDims" get-dims) :int
  (arr :pointer)
  (sizes :pointer (cffi:null-pointer)))

(define-cfun ("cvGetDimSize" get-dim-size) :int
  (arr :pointer)
  (index :int))

(define-cfun ("cvPtr1D" ptr-1d) :pointer
  (arr :pointer)
  (idx0 :int)
  (type :pointer (cffi:null-pointer)))

(define-cfun ("cvPtr2D" ptr-2d) :pointer
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (type :pointer (cffi:null-pointer)))

(define-cfun ("cvPtr3D" ptr-3d) :pointer
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int)
  (type :pointer (cffi:null-pointer)))

(define-cfun ("cvPtrND" ptr-nd) :pointer
  (arr :pointer)
  (idx :pointer)
  (type :pointer (cffi:null-pointer))
  (create-node :int 1)
  (precalc-hashval :pointer (cffi:null-pointer)))


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

(define-cfun ("cvGetND" get-nd) (:struct scalar)
  (arr :pointer)
  (idx :pointer))


(define-cfun ("cvGetReal1D" get-real-1d) :double
  (arr :pointer)
  (idx0 :int))

(define-cfun ("cvGetReal2D" get-real-2d) :double
  (arr :pointer)
  (idx0 :int)
  (idx1 :int))

(define-cfun ("cvGetReal3D" get-real-3d) :double
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int))

(define-cfun ("cvGetRealND" get-real-nd) :double
  (arr :pointer)
  (idx :pointer))



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

(define-cfun ("cvSetND" set-nd) :void
  (arr :pointer)
  (idx :pointer)
  (value (:struct scalar)))


(define-cfun ("cvSetReal1D" set-real-1d) :void
  (arr :pointer)
  (idx0 :int)
  (value :double))

(define-cfun ("cvSetReal2D" set-real-2d) :void
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (value :double))

(define-cfun ("cvSetReal3D" set-real-3d) :void
  (arr :pointer)
  (idx0 :int)
  (idx1 :int)
  (idx2 :int)
  (value :double))

(define-cfun ("cvSetRealND" set-real-nd) :void
  (arr :pointer)
  (idx :pointer)
  (value :double))

(define-cfun ("cvClearND" clear-nd) :void
  (arr :pointer)
  (idx :pointer))

(define-cfun ("cvGetMat" get-mat) :pointer
  (arr :pointer)
  (header :pointer)
  (coi :pointer (cffi:null-pointer))
  (allow-nd :int 0))

(define-cfun ("cvGetImage" get-image) :pointer
  (arr :pointer)
  (image-header :pointer))

(define-cfun ("cvReshapeMatND" reshape-mat-nd) :pointer
  (arr :pointer)
  (sizeof-header :int)
  (header :pointer)
  (new-cn :int)
  (new-dims :int)
  (new-sizes :pointer))

(define-cfun ("cvReshape" reshape) :pointer
  (arr :pointer)
  (header :pointer)
  (new-cn :int)
  (new-rows :int 0))

(define-cfun ("cvRepeat" repeat) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvCreateData" create-data) :void
  (arr :pointer))

(define-cfun ("cvReleaseData" release-data) :void
  (arr :pointer))

(define-cfun ("cvSetData" set-data) :void
  (arr :pointer)
  (data :pointer)
  (step :int))

(define-cfun ("cvGetRawData" get-raw-data) :void
  (arr :pointer)
  (data :pointer)
  (step :pointer (cffi:null-pointer))
  (roi-size :pointer (cffi:null-pointer)))

(define-cfun ("cvGetSize" get-size) (:struct size)
  (arr :pointer))

(define-cfun ("cvCopy" copy) :void
  (src :pointer)
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvSet" set) :void
  (src :pointer)
  (value (:struct scalar))
  (mask :pointer (cffi:null-pointer)))

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

(define-cfun ("cvMixChannels" mix-channels) :void
  (src :pointer)
  (src-ount :int)
  (dst :pointer)
  (dst-count :int)
  (from-to :pointer)
  (pair-count :int))

(define-cfun ("cvConvertScale" convert-scale) :void
  (src :pointer)
  (dst :pointer)
  (scale :double 1)
  (shift :double 0))

(defun scale (src dst &optional (scale 1.0d0) (shift 0.0d0))
  (convert-scale src dst scale shift))

(defun convert (src dst)
  (convert-scale src dst 1.0d0 0.0d0))

(defun cvt-scale (src dst &optional (scale 1.0d0) (shift 0.0d0))
  (convert-scale src dst scale shift))

(define-cfun ("cvConvertScaleAbs" convert-scale-abs) :void
  (src :pointer)
  (dst :pointer)
  (scale :double 1)
  (shift :double 0))

;;(define-cfun ("cvCheckTermCriteria" check-term-criteria) :)

;; ****************************************************************************************\
;;                   Arithmetic, logic and comparison operations                          *
;;  ***************************************************************************************/

(define-cfun ("cvAdd" add) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvAddS" add-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvSub" sub) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvSubS" sub-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvSubRS" sub-r-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvMul" mul) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double 1))

(define-cfun ("cvDiv" div) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double 1))

(define-cfun ("cvScaleAdd" scale-add) :void
  (src1 :pointer)
  (scalar (:struct scalar))
  (src2 :pointer)
  (dst :pointer))

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
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvAndS" and-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvOr" or) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvOrS" or-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvXor" xor) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvXorS" xor-s) :void
  (src :pointer)
  (value (:struct scalar))
  (dst :pointer)
  (mask :pointer (cffi:null-pointer)))

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
  (cmp-op :int))

(define-cfun ("cvCmpS" cmp-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer)
  (cmp-op :int))

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
  (angle :pointer (cffi:null-pointer))
  (angle-in-degrees :int 0))

(define-cfun ("cvPolarToCart" polar-to-cart) :void
  (magnitude :pointer)
  (angle :pointer)
  (x :pointer)
  (y :pointer)
  (angle-in-degrees :int 0))

(define-cfun ("cvPow" pow) :void
  (src :pointer)
  (dst :pointer)
  (power :double))

(define-cfun ("cvExp" exp) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvLog" log) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvFastArctan" fast-arctan) :float
  (y :float)
  (x :float))

(define-cfun ("cvCbrt" cbrt) :float
  (value :float))

(define-cfun ("cvCheckArr" check-arr) :int
  (arr :pointer)
  (flags :int 0)
  (min-val :double 0)
  (max-val :double 0))

(define-cfun ("cvRandArr" rand-arr) :void
  (rng :pointer)
  (arr :pointer)
  (dist-type :int)
  (param1 (:struct scalar))
  (param2 (:struct scalar)))

(define-cfun ("cvRandShuffle" rand-shuffle) :void
  (mat :pointer)
  (rng :pointer)
  (iter-factor :double 1.0))

(define-cfun ("cvSort" sort) :void
  (src :pointer)
  (dst :pointer (cffi:null-pointer))
  (idxmat :pointer (cffi:null-pointer))
  (flags :int 0)) 

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
  (t-abc :int 0))

(define-cfun ("cvTransform" transform) :void
  (src :pointer)
  (dst :pointer)
  (trans-mat :pointer)
  (shift-vec :pointer (cffi:null-pointer)))

(define-cfun ("cvPerspectiveTransform" perspective-transform) :void
  (src :pointer)
  (dst :pointer)
  (mat :pointer))

(define-cfun ("cvMulTransposed" mul-transposed) :void
  (src :pointer)
  (dst :pointer)
  (order :int)
  (delta :pointer (cffi:null-pointer))
  (scale :double 1.0d0))

(define-cfun ("cvTranspose" transpose) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvFlip" flip) :void
  (src :pointer)
  (dst :pointer (cffi:null-pointer))
  (flip-mode :int 0))

(define-cfun ("cvSVD" svd) :void
  (a :pointer)
  (w :pointer)
  (u :pointer (cffi:null-pointer))
  (v :pointer (cffi:null-pointer))
  (flags :int 0))

(define-cfun ("cvSVBkSb" svbksb) :void
  (w :pointer)
  (u :pointer)
  (v :pointer)
  (b :pointer)
  (x :pointer)
  (flags :int))

(define-cfun ("cvInvert" invert) :double
  (src :pointer)
  (dst :pointer)
  (method :int +lu+))

(defun inv (src dst &optional (method +lu+))
  (invert src dst method))

(define-cfun ("cvSolve" solve) :int
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (method :int +lu+))

(define-cfun ("cvDet" det) :double
  (mat :pointer))

(define-cfun ("cvTrace" trace) (:struct scalar)
  (mat :pointer))

(define-cfun ("cvEigenVV" eigen-vv) :void
  (mat :pointer)
  (evects :pointer)
  (evals :pointer)
  (eps :double 0)
  (low-index :int -1)
  (high-index :int -1))

(define-cfun ("cvSetIdentity" set-identity) :void
  (mat :pointer)
  (value (:struct scalar) (real-scalar 1.0d0)))


(define-cfun ("cvCalcCovarMatrix" calc-covar-matrix) :void
  (vects :pointer)
  (count :int)
  (cov-mat :pointer)
  (avg :pointer)
  (flags :int))


;; ****************************************************************************************\
;;                                    Array Statistics                                    *
;; ***************************************************************************************/

(define-cfun ("cvSum" sum) (:struct scalar)
  (arr :pointer))

(define-cfun ("cvCountNonZero" count-non-zero) :int
  (arr :pointer))

(define-cfun ("cvAvg" avg) (:struct scalar)
  (arr :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvAvgSdv" avg-sdv) :void
  (arr :pointer)
  (mean :pointer)
  (std-dev :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvMinMaxLoc" min-max-loc) :void
  (arr :pointer)
  (min-val :pointer)
  (max-val :pointer)
  (min-loc :pointer (cffi:null-pointer))
  (max-loc :pointer (cffi:null-pointer))
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvNorm" norm) :double
  (arr1 :pointer)
  (arr2 :pointer (cffi:null-pointer))
  (norm-type :int +l2+)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvNormalize" normalize) :void
  (src1 :pointer)
  (dst :pointer)
  (a :double 1.0d0)
  (b :double 0.0d0)
  (norm-type :int +l2+)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvReduce" reduce) :void
  (src :pointer)
  (dst :pointer)
  (dim :int -1)
  (op :int +reduce-sum+))

;; ****************************************************************************************\
;;                      Discrete Linear Transforms and Related Functions                  *
;; ***************************************************************************************/

(define-cfun ("cvDFT" dft) :void
  (src :pointer)
  (dst :pointer)
  (flags :int)
  (nonzero-rows :int 0))

(define-cfun ("cvMulSpectrums" mul-spectrums) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (flags :int))

(define-cfun ("cvGetOptimalDFTSize" get-optimal-dft-size) :int
  (size0 :int))

(define-cfun ("cvDCT" dct) :void
  (src :pointer)
  (dst :pointer)
  (flags :int))

;;**************************************************************************************
;;                              Dynamic data structures                                 
;;**************************************************************************************

;;; CvMemStorage
(define-cfun ("cvCreateMemStorage" create-mem-storage) :pointer
  (block-size :int 0))

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

(define-cfun ("cvSeqPush" seq-push) :pointer
  (seq :pointer)
  (element :pointer (cffi:null-pointer)))

(define-cfun ("cvSeqPushFront" seq-push-front) :pointer
  (seq :pointer)
  (element :pointer (cffi:null-pointer)))

(define-cfun ("cvSeqPop" seq-pop) :void
  (seq :pointer)
  (element :pointer (cffi:null-pointer)))

(define-cfun ("cvSeqPopFront" seq-pop-front) :void
  (seq :pointer)
  (element :pointer (cffi:null-pointer)))

(define-cfun ("cvSeqPushMulti" seq-push-multi) :void
  (seq :pointer)
  (elements :pointer)
  (count :int)
  (in-front :int 0))

(define-cfun ("cvSeqPopMulti" seq-pop-multi) :void
  (seq :pointer)
  (elements :pointer)
  (count :int)
  (in-front :int 0))

(define-cfun ("cvSeqInsert" seq-insert) :pointer
  (seq :pointer)
  (before-index :int)
  (element :pointer (cffi:null-pointer)))

(define-cfun ("cvSeqRemove" seq-remove) :void
  (seq :pointer)
  (index :int))

(define-cfun ("cvClearSeq" clear-seq) :void
  (seq :pointer))

(define-cfun ("cvGetSeqElem" get-seq-elem) :pointer
  (seq :pointer)
  (index :int))

(define-cfun ("cvSeqElemIdx" seq-elem-idx) :int
  (seq :pointer)
  (element :pointer)
  (block :pointer (cffi:null-pointer)))

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
  (reverse :int 0))

(define-cfun ("cvGetSeqReaderPos" get-seq-reader-pos) :int
  (seq-reader :pointer))

(define-cfun ("cvSetSeqReaderPos" set-seq-reader-pos) :void
  (seq-reader :pointer)
  (index :int)
  (is-relative :int 0))

(define-cfun ("cvSeqSlice" seq-slice) :pointer
  (seq :pointer)
  (slice (:struct slice))
  (storage :pointer (cffi:null-pointer))
  (copy-data :int 0))

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
  (memstorage :pointer (cffi:null-pointer))
  (name :string (cffi:null-pointer))
  (real-name :string (cffi:null-pointer)))

