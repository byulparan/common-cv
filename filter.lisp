(in-package #:cv)

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

(cffi:defcfun ("cvAddWeighted" add-weighted) :void
  (src :pointer)
  (alpha :double)
  (src2 :pointer)
  (beta :double)
  (gamma :double)
  (dst :pointer))

(cffi:defcfun ("cvSetZero" set-zero) :void
  (arr :pointer))

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

(cffi:defcfun ("cvAvg" avg) (:struct scalar)
  (arr :pointer)
  (mask :pointer))

(cffi:defcfun ("cvAvgSdv" avg-sdv) :void
  (arr :pointer)
  (mean :pointer)
  (std-dev :pointer)
  (mask :pointer))

(cffi:defcfun ("cvCalcCovarMatrix" calc-covar-matrix) :void
  (vects :pointer)
  (count :int)
  (cov-mat :pointer)
  (avg :pointer)
  (flags :covar-matrix-enum))

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

;;; convert scale
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

(cffi:defcfun ("cvCopy" copy) :void
  (src :pointer)
  (dst :pointer)
  (mask :pointer))

(cffi:defcfun ("cvCountNonZero" count-non-zero) :int
  (arr :pointer))

(cffi:defcfun ("cvCrossProduct" cross-product) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))


;;; convert color
(cffi:defcfun ("cvCvtColor" cvt-color) :void
  (src :pointer)
  (dst :pointer)
  (code :cvt-color-enum))

(cffi:defcfun ("cvDet" det) :double
  (mat :pointer))

(cffi:defcfun ("cvDiv" div) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double))

(cffi:defcfun ("cvDotProduct" dot-product) :double
  (src1 :pointer)
  (src2 :pointer))

(cffi:defcfun ("cvEigenVV" eigen-vv) :void
  (mat :pointer)
  (evects :pointer)
  (evals :pointer)
  (eps :double)
  (low-index :int)
  (high-index :int))

(cffi:defcfun ("cvFlip" flip) :void
  (src :pointer)
  (dst :pointer)
  (flip-mode :int))

(cffi:defcfun ("cvGEMM" gemm) :void
  (src1 :pointer)
  (src2 :pointer)
  (alpha :double)
  (src3 :pointer)
  (beta :double)
  (dst :pointer)
  (t-abc :int))

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

(cffi:defcfun ("cvGetRows" get-rows) :pointer
  (arr :pointer)
  (submat :pointer)
  (start-row :int)
  (end-row :int)
  (delta-row :int))

(defun get-row (arr submat row)
  (get-rows arr submat row (+ row 1) 1))

(cffi:defcfun ("cvGetSize" get-size) (:struct size)
  (arr :pointer))

(cffi:defcfun ("cvGetSubRect" get-sub-rect) :pointer
  (arr :pointer)
  (submat :pointer)
  (rect (:struct rect)))

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

(cffi:defcfun ("cvInvert" invert) :double
  (src :pointer)
  (dst :pointer)
  (method :invert-enum))

(defun inv (src dst &optional (method :cv-lu))
  (invert src dst method))

(cffi:defcfun ("cvMahalanobis" mahalanobis) :double
  (vec1 :pointer)
  (vec2 :pointer)
  (mat :pointer))

(cffi:defcfun ("cvMax" max) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvMaxS" max-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer))

(cffi:defcfun ("cvMerge" merge) :void
  (src0 :pointer)
  (src1 :pointer)
  (src2 :pointer)
  (src3 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvMin" min) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer))

(cffi:defcfun ("cvMinS" min-s) :void
  (src :pointer)
  (value :double)
  (dst :pointer))

(cffi:defcfun ("cvMinMaxLoc" min-max-loc) :void
  (arr :pointer)
  (min-val :pointer)
  (max-val :pointer)
  (min-loc :pointer)
  (max-loc :pointer)
  (mask :pointer))

(cffi:defcfun ("cvMul" mul) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (scale :double))

(cffi:defcfun ("cvNot" not) :void
  (src :pointer)
  (dst :pointer))

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

(cffi:defcfun ("cvReduce" reduce) :void
  (src :pointer)
  (dst :pointer)
  (dim :int)
  (op :reduce-enum))

(cffi:defcfun ("cvRepeat" repeat) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("cvSet" set) :void
  (src :pointer)
  (value (:struct scalar))
  (mask :pointer))

(cffi:defcfun ("cvSolve" solve) :int
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (method :int))

(cffi:defcfun ("cvSplit" split) :void
  (src :pointer)
  (dst0 :pointer)
  (dst1 :pointer)
  (dst2 :pointer)
  (dst3 :pointer))

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

(cffi:defcfun ("cvTranspose" transpose) :void
  (src :pointer)
  (dst :pointer))

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

(cffi:defcfun ("cvTrace" trace) (:struct scalar)
  (mat :pointer))

(cffi:defcfun ("cvSetIdentity" set-identity) :void
  (mat :pointer)
  (value (:struct scalar)))

(cffi:defcfun ("cvSum" sum) (:struct scalar)
  (arr :pointer))

;;; smoothing
(cffi:defcfun ("cvSmooth" smooth) :void
  (src :pointer)
  (dst :pointer)
  (smooth-type :smooth-method-enum)
  (size1 :int)
  (size2 :int)
  (size3 :double)
  (size4 :double))

;;;
(cffi:defcfun ("cvErode" erode) :void
  (src :pointer)
  (dst :pointer)
  (ipl-conv-kernel :pointer)
  (iterations :int))

(cffi:defcfun ("cvDilate" dilate) :void
  (src :pointer)
  (dst :pointer)
  (ipl-conv-kernel :pointer)
  (iterations :int))

(cffi:defcfun ("cvMorphologyEx" morphology-ex) :void
  (src :pointer)
  (dst :pointer)
  (temp :pointer)
  (ipl-conv-kernel :pointer)
  (operation :morphology-enum)
  (iterations :int))

(cffi:defcfun ("cvFloodFill" floor-fill) :void
  (image :pointer)
  (seed-point (:struct point))
  (new-val (:struct scalar))
  (lo-diff (:struct scalar))
  (up-diff (:struct scalar))
  (comp :pointer)
  (flags :int)
  (mask :pointer))

(cffi:defcfun ("cvResize" resize) :void
  (src :pointer)
  (dst :pointer)
  (iterpolation :resize-enum))

(cffi:defcfun ("cvPyrDown" pyr-down) :void
  (src :pointer)
  (dst :pointer)
  (filter :pyramid-enum))

(cffi:defcfun ("cvPyrUp" pyr-up) :void
  (src :pointer)
  (dst :pointer)
  (filter :pyramid-enum))

;; (cffi:defcfun ("cvPyrSegmentation" pyr-segmantation) :void
;;   (src :pointer)
;;   (dst :pointer)
;;   (storage :pointer)
;;   (comp :pointer)
;;   (level :int)
;;   (threshold1 :double)
;;   (threshold2 :double))

(cffi:defcfun ("cvThreshold" threshold) :void
  (src :pointer)
  (dst :pointer)
  (threshold :double)
  (max-value :double)
  (threshold-type :threshold-enum))

(cffi:defcfun ("cvAdaptiveThreshold" adaptive-threshold) :void
  (src :pointer)
  (dst :pointer)
  (max-val :double)
  (ataptive-method :adaptive-method-enum)
  (threshold-type :threshold-enum)
  (block-size :int)
  (param1 :double))

(cffi:defcfun ("cvFilter2D" filter-2d) :void
  (src :pointer)
  (dst :pointer)
  (kernel :pointer)
  (anchor (:struct point)))

(cffi:defcfun ("cvCopyMakeBorder" copy-make-border) :void
  (src :pointer)
  (dst :pointer)
  (offset (:struct point))
  (border-type :make-border-enum)
  (value (:struct scalar)))

(cffi:defcfun ("cvSobel" sobel) :void
  (src :pointer)
  (dst :pointer)
  (x-order :int)
  (y-order :int)
  (aperture-size :int))

(cffi:defcfun ("cvLaplace" laplace) :void
  (src :pointer)
  (dst :pointer)
  (aperture-size :int))

(cffi:defcfun ("cvCanny" canny) :void
  (image :pointer)
  (edges :pointer)
  (threshold1 :double)
  (threshold2 :double)
  (aperture-size :int))

(cffi:defcfun ("cvRemap" remap) :void
  (src :pointer)
  (dst :pointer)
  (map-x :pointer)
  (map-y :pointer)
  (flags :int)
  (fillval (:struct scalar)))

(cffi:defcfun ("cvWrapAffine" wrap-affine) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer)
  (flags :int)
  (fillval (:struct scalar)))

(cffi:defcfun ("cvGetQuadrangleSubPix" get-quadrangle-sub-pix) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(cffi:defcfun ("cvGetAffineTransform" get-affine-transform) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(cffi:defcfun ("cv2DRotationMatrix" 2d-rotation-matrix) :pointer
  (center (:struct point2d-32f))
  (angle :double)
  (scale :double)
  (map-matrix :pointer))

(cffi:defcfun ("cvTransform" transform) :void
  (src :pointer)
  (dst :pointer)
  (trans-mat :pointer)
  (shift-vec :pointer))

(cffi:defcfun ("cvWrapPerspective" wrap-perspective) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer)
  (flags :int)
  (fillval (:struct scalar)))

(cffi:defcfun ("cvGetPerspectiveTransform" get-perspective-transform) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(cffi:defcfun ("cvPerspectiveTransform" perspective-transform) :void
  (src :pointer)
  (dst :pointer)
  (mat :pointer))

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

(cffi:defcfun ("cvLogPolar" log-polar) :void
  (src :pointer)
  (dst :pointer)
  (center (:struct point2d-32f))
  (m :double)
  (flags :int))

(cffi:defcfun ("cvDFT" dft) :void
  (src :pointer)
  (dst :pointer)
  (flags :dxt-enum)
  (nonzero-rows :int))

(cffi:defcfun ("cvGetOptimalDFTSize" get-optimal-dft-size) :int
  (size0 :int))

(cffi:defcfun ("cvMulSpectrums" mul-spectrums) :void
  (src1 :pointer)
  (src2 :pointer)
  (dst :pointer)
  (flags :dxt-enum))

(cffi:defcfun ("cvDCT" dct) :void
  (src :pointer)
  (dst :pointer)
  (flags :dxt-enum))

(cffi:defcfun ("cvIntegral" integral) :void
  (image :pointer)
  (sum :pointer)
  (sqsum :pointer)
  (tilted-sum :pointer))

(cffi:defcfun ("cvDistTransform" dist-transform) :void
  (src :pointer)
  (dst :pointer)
  (distance-type :dist-enum)
  (mask-size :int)
  (mask :pointer)
  (labels :pointer))

(cffi:defcfun ("cvEqualizeHist" equalize-hist) :void
  (src :pointer)
  (dst :pointer))


