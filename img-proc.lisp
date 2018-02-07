(in-package #:cv)

(define-cfun ("cvAcc" acc) :void
  (image :pointer)
  (sum :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvSquareAcc" squre-acc) :void
  (image :pointer)
  (sqsum :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvMultiplyAcc" multiply-acc) :void
  (image1 :pointer)
  (image2 :pointer)
  (acc :pointer)
  (mask :pointer (cffi:null-pointer)))

(define-cfun ("cvRunningAvg" running-avg) :void
  (image :pointer)
  (acc :pointer)
  (alpha :double)
  (mask :pointer (cffi:null-pointer)))


;; ****************************************************************************************\
;;                                    Image Processing                                    *
;; ***************************************************************************************/

(define-cfun ("cvCopyMakeBorder" copy-make-border) :void
  (src :pointer)
  (dst :pointer)
  (offset (:struct point))
  (border-type :int)
  (value (:struct scalar) (scalar-all 0.0d0)))

(define-cfun ("cvSmooth" smooth) :void
  (src :pointer)
  (dst :pointer)
  (smooth-type :int +gaussian+)
  (size1 :int 3)
  (size2 :int 0)
  (sigma1 :double 0)
  (sigma2 :double 0))

(define-cfun ("cvFilter2D" filter-2d) :void
  (src :pointer)
  (dst :pointer)
  (kernel :pointer)
  (anchor (:struct point) (point -1 -1)))

(define-cfun ("cvIntegral" integral) :void
  (image :pointer)
  (sum :pointer)
  (sqsum :pointer (cffi:null-pointer))
  (tilted-sum :pointer (cffi:null-pointer)))

(define-cfun ("cvPyrDown" pyr-down) :void
  (src :pointer)
  (dst :pointer)
  (filter :int +gaussian-5x5+))

(define-cfun ("cvPyrUp" pyr-up) :void
  (src :pointer)
  (dst :pointer)
  (filter :int +gaussian-5x5+))

(define-cfun ("cvCreatePyramid" create-pyramid) :pointer
  (img :pointer)
  (extra-layers :int)
  (rate :double)
  (layer-sizes :pointer (cffi:null-pointer))
  (bufarr :pointer (cffi:null-pointer))
  (calc :int 1)
  (filter :int cv:+gaussian-5x5+))

(define-cfun ("cvReleasePyramid" release-pyramid) :void
  (pyramid :pointer)
  (extra-layers :int))

(define-cfun ("cvPyrMeanShiftFiltering" pyr-mean-shift-filtering) :void
  (src :pointer)
  (dst :pointer)
  (sp :double)
  (sr :double)
  (max-level :int 1)
  (termcrit (:struct term-criteria) (term-criteria (+ cv:+termcrit-iter+
						      cv:+termcrit-eps+) 
						   5 1)))


(define-cfun ("cvWatershed" watershed) :void
  (image :pointer)
  (markers :pointer))

(define-cfun ("cvSobel" sobel) :void
  (src :pointer)
  (dst :pointer)
  (x-order :int)
  (y-order :int)
  (aperture-size :int 3))

(define-cfun ("cvLaplace" laplace) :void
  (src :pointer)
  (dst :pointer)
  (aperture-size :int 3))

(define-cfun ("cvCvtColor" cvt-color) :void
  (src :pointer)
  (dst :pointer)
  (code :int))

(define-cfun ("cvResize" resize) :void
  (src :pointer)
  (dst :pointer)
  (iterpolation :int +inter-linear+))

(define-cfun ("cvWarpAffine" warp-affine) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer)
  (flags :int (+ +inter-linear+ +warp-fill-outliers+))
  (fillval (:struct scalar) (scalar-all 0)))

(cffi:defcfun ("cvGetAffineTransform" %get-affine-transform) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(defun get-affine-transform (srcs dsts map-matrix)
  (cffi:with-foreign-objects ((src-ptr '(:struct point2d-32f) (length srcs))
			      (dst-ptr '(:struct point2d-32f) (length dsts)))
    (dotimes (i (length srcs))
      (setf (cffi:mem-aref src-ptr '(:struct point2d-32f) i) (nth i srcs)
	    (cffi:mem-aref dst-ptr '(:struct point2d-32f) i) (nth i dsts)))
    (%get-affine-transform src-ptr dst-ptr map-matrix)))

(define-cfun ("cv2DRotationMatrix" 2d-rotation-matrix) :pointer
  (center (:struct point2d-32f))
  (angle :double)
  (scale :double)
  (map-matrix :pointer))

(define-cfun ("cvWarpPerspective" warp-perspective) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer)
  (flags :int (+ +inter-linear+ +warp-fill-outliers+))
  (fillval (:struct scalar) (cv:scalar-all 0)))

(cffi:defcfun ("cvGetPerspectiveTransform" %get-perspective-transform) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(defun get-perspective-transform (srcs dsts map-matrix)
  (cffi:with-foreign-objects ((src-ptr '(:struct point2d-32f) (length srcs))
			      (dst-ptr '(:struct point2d-32f) (length dsts)))
    (dotimes (i (length srcs))
      (setf (cffi:mem-aref src-ptr '(:struct point2d-32f) i) (nth i srcs)
	    (cffi:mem-aref dst-ptr '(:struct point2d-32f) i) (nth i dsts)))
    (%get-perspective-transform src-ptr dst-ptr map-matrix)))


(define-cfun ("cvRemap" remap) :void
  (src :pointer)
  (dst :pointer)
  (map-x :pointer)
  (map-y :pointer)
  (flags :int (+ +inter-linear+ +warp-fill-outliers+))
  (fillval (:struct scalar) (scalar-all 0)))

(define-cfun ("cvConvertMaps" convert-maps) :void
  (mapx :pointer)
  (mapy :pointer)
  (mapxy :pointer)
  (mapalpha :pointer))

(define-cfun ("cvLogPolar" log-polar) :void
  (src :pointer)
  (dst :pointer)
  (center (:struct point2d-32f))
  (m :double)
  (flags :int (+ +inter-linear+ +warp-fill-outliers+)))

(define-cfun ("cvLinearPolar" linear-polar) :void
  (src :pointer)
  (dst :pointer)
  (center (:struct point2d-32f))
  (max-radius :double)
  (flags :int (+ cv:+inter-linear+
		 cv:+warp-fill-outliers+)))

(define-cfun ("cvUndistort2" undistort2) :void
  (src :pointer)
  (dst :pointer)
  (camera-matrix :pointer)
  (distortion-coeffs :pointer)
  (new-camera-matrix :pointer (cffi:null-pointer)))

(define-cfun ("cvInitUndistortMap" init-undistort-map) :void
  (camera-matrix :pointer)
  (distortion-coeffs :pointer)
  (mapx :pointer)
  (mapy :pointer))

(define-cfun ("cvInitUndistortRectifyMap" init-undistort-rectify-map) :void
  (camera-matrix :pointer)
  (dist-coeffs :pointer)
  (r :pointer)
  (new-camera-matrix :pointer)
  (mapx :pointer)
  (mapy :pointer))

(define-cfun ("cvUndistortPoints" undistort-points) :void
  (src :pointer)
  (dst :pointer)
  (camera-matrix :pointer)
  (dist-coeffs :pointer)
  (r :pointer (cffi:null-pointer))
  (p :pointer (cffi:null-pointer)))




(define-cfun ("cvCreateStructuringElementEx" create-structuring-element-ex)
    (:pointer (:struct ipl-conv-kernel))
  (cols :int)
  (rows :int)
  (anchor-x :int)
  (anchor-y :int)
  (shape :int)
  (values :pointer (cffi:null-pointer)))

(cffi:defcfun ("cvReleaseStructuringElement" %release-structuring-element) :void
  (ptr :pointer))

(defun release-structuring-element (ipl-conv-kernel)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) ipl-conv-kernel)
    (%release-structuring-element ptr)))

(define-cfun ("cvErode" erode) :void
  (src :pointer)
  (dst :pointer)
  (element :pointer (cffi:null-pointer))
  (iterations :int 1))

(define-cfun ("cvDilate" dilate) :void
  (src :pointer)
  (dst :pointer)
  (element :pointer (cffi:null-pointer))
  (iterations :int 1))

(define-cfun ("cvMorphologyEx" morphology-ex) :void
  (src :pointer)
  (dst :pointer)
  (temp :pointer)
  (element :pointer)
  (operation :int)
  (iterations :int 1))

(define-cfun ("cvMoments" moments) :void
  (arr :pointer)
  (moments :pointer)
  (binary :int 0))

(define-cfun ("cvGetSpatialMoment" get-spatial-moment) :double
  (moments :pointer)
  (x-order :int)
  (y-order :int))

(define-cfun ("cvGetCentralMoment" get-central-moment) :double
  (moment :pointer)
  (x-order :int)
  (y-order :int))

(define-cfun ("cvGetNormalizedCentralMoment" get-normalized-central-moment) :double
  (moment :pointer)
  (x-order :int)
  (y-order :int))

(define-cfun ("cvGetHuMoments" get-hu-moments) :void
  (moments :pointer)
  (hu-moments :pointer))

;; *********************************** data sampling **************************************/
(define-cfun ("cvSampleLine" sample-line) :int
  (image :pointer)
  (pt1 (:struct point))
  (pt2 (:struct point))
  (buffer :pointer)
  (connectivity :int 8))

(define-cfun ("cvGetRectSubPix" get-rect-sub-pix) :void
  (src :pointer)
  (dst :pointer)
  (center (:struct point2d-32f)))


(define-cfun ("cvGetQuadrangleSubPix" get-quadrangle-sub-pix) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(define-cfun ("cvMatchTemplate" match-template) :void
  (image :pointer)
  (templ :pointer)
  (result :pointer)
  (method :int))

(define-cfun ("cvCalcEMD2" calc-emd2) :float
  (signature1 :pointer)
  (signature2 :pointer)
  (distance-type :int)
  (distance-func :pointer (cffi:null-pointer))
  (cost-matrix :pointer (cffi:null-pointer) )
  (flow :pointer (cffi:null-pointer))
  (lower-bound :pointer (cffi:null-pointer))
  (user-data :pointer (cffi:null-pointer)))

;; ****************************************************************************************\
;;                              Contours retrieving                                       *
;; ****************************************************************************************/

(cffi:defcfun ("cvFindContours" %find-contours) :int
  (image :pointer)
  (storage :pointer)
  (first-contour :pointer)
  (header-size :int 128)
  (mode :int +retr-list+)
  (method :int +chain-approx-simple+)
  (offset (:struct point) (point 0 0)))

(defun find-contours (image storage
		      &optional
			(header-size 128)
			(mode +retr-list+)
			(method +chain-approx-simple+)
			(offset (cv:point 0 0)))
  (cffi:with-foreign-object (find-contour :pointer)
    (%find-contours image storage find-contour header-size mode method offset)
    (cffi:mem-ref find-contour :pointer)))

(define-cfun ("cvStartFindContours" start-find-contours) :pointer
  (image :pointer)
  (storage :pointer)
  (header-size :int 128)
  (mode :int +retr-list+)
  (method :int +chain-approx-simple+)
  (offset (:struct point) (point 0 0)))

(cffi:defcfun ("cvFindNextContour" %find-next-contour) :pointer
  (scanner :pointer))

(defun find-next-contour (scanner)
  (let* ((contour (%find-next-contour scanner)))
    (if (cffi:null-pointer-p contour) nil
      contour)))

(define-cfun ("cvSubstituteContour" substitute-contour) :void
  (scanner :pointer)
  (new-contour :pointer))

(define-cfun ("cvEndFindContours" end-find-contours) :pointer
  (scanner :pointer))

(define-cfun ("cvApproxChains" approx-chains) :pointer
  (seq :pointer)
  (storage :pointer)
  (method :int +chain-approx-simple+)
  (parameter :double 0)
  (minimal-perimeter :int 0)
  (recursive :int 0))

(define-cfun ("cvStartReadChainPoints" start-read-chain-points) :pointer
  (chain :pointer))

(define-cfun ("cvReadChainPoint" read-chain-point) (:struct point)
  (reader :pointer))


;; ****************************************************************************************\
;;                            Contour Processing and Shape Analysis                       *
;; ****************************************************************************************/

(define-cfun ("cvApproxPoly" approx-poly) :pointer
  (src-seq :pointer)
  (header-size :int)
  (storage :pointer)
  (method :int)
  (eps :double)
  (recursive :int 0))

;; #define CV_WHOLE_SEQ_END_INDEX 0x3fffffff
;; #define CV_WHOLE_SEQ  cvSlice(0, CV_WHOLE_SEQ_END_INDEX)
(define-cfun ("cvArcLength" arc-length) :double
  (curve :pointer)
  (slice (:struct slice) (slice 0 #x3fffffff))
  (is-closed :int -1))

(defun contour-perimeter (contour)
  (arc-length contour (slice 0 #x3fffffff) 1))


(define-cfun ("cvBoudingRect" bounding-rect) (:struct rect)
  (points :pointer)
  (update :int 0))

(define-cfun ("cvContourArea" contour-area) :double
  (contour :pointer)
  (slice (:struct slice) (slice 0 #x3fffffff))
  (oriented :int 0))

(define-cfun ("cvMinAreaRect2" min-area-rect2) (:struct box2d)
  (points :pointer)
  (storage :pointer (cffi:null-pointer)))

(define-cfun ("cvMinEnclosingCircle" min-enclosing-circle) :int
  (points :pointer)
  (center :pointer)
  (radius :pointer))

(define-cfun ("cvMatchShapes" match-shapes) :double
  (object1 :pointer)
  (object2 :pointer)
  (method :int)
  (parameter :double 0))

(define-cfun ("cvConvexHull2" convex-hull) :pointer
  (input :pointer)
  (hull-storage :pointer (cffi:null-pointer))
  (orientation :int +clockwise+)
  (return-points :int 0))

(define-cfun ("cvCheckContourConvexity" check-contour-convexity) :int
  (contour :pointer))

(define-cfun ("cvConvexityDefects" convexity-defects) :pointer
  (contour :pointer)
  (convexhull :pointer)
  (storage :pointer (cffi:null-pointer)))

(define-cfun ("cvFitEllipse2" fit-ellipse2) (:struct box2d)
  (points :pointer))

(define-cfun ("cvMaxRect" max-rect) (:struct rect)
  (rect1 :pointer)
  (rect2 :pointer))

(define-cfun ("cvBoxPoints" box-points) :void
  (box (:struct box2d))
  (pt (:struct point2d-32f) :count 4))

(define-cfun ("cvPointSeqFromMat" point-seq-from-mat) :pointer
  (seq-kind :int)
  (mat :pointer)
  (contour-header :pointer)
  (block :pointer))

(define-cfun ("cvPointPolygonTest" point-polygon-test) :double
  (contour :pointer)
  (pt (:struct point2d-32f))
  (measure-dist :int))


;; ****************************************************************************************\
;;                                  Histogram functions                                   *
;; ****************************************************************************************/

(cffi:defcfun ("cvCreateHist" %create-hist) :pointer
  (dims :int)
  (sizes :pointer)
  (type :int)
  (ranges :pointer (cffi:null-pointer))
  (uniform :int 1))

(defun create-hist (dims sizes type &optional ranges (uniform 1))
  (let* ((ref nil))
    (cffi:with-foreign-objects ((size-array :int (length sizes)))
      (dotimes (i (length sizes))
	(setf (cffi:mem-aref size-array :int i) (nth i sizes)))
      (cffi:with-foreign-object (ranges-pointer :pointer (length ranges))
	(dotimes (i (length ranges))
	  (let ((elements (cffi:foreign-alloc :float :count (length (nth i ranges)))))
	    (dotimes (k (length (nth i ranges)))
	      (setf (cffi:mem-aref elements :float k) (coerce (nth k (nth i ranges)) 'single-float)))
	    (setf (cffi:mem-aref ranges-pointer :pointer i) elements)))
	(setf ref (%create-hist (floor dims) size-array type (if ranges ranges-pointer (cffi:null-pointer)) (floor uniform)))
	(dotimes (i (length ranges))
	  (cffi-sys:foreign-free (cffi:mem-aref ranges-pointer :pointer i)))))
    ref))

;;; @need ranges to list ------------------------------------------------------
(cffi:defcfun ("cvSetHistBinRanges" %set-hist-bin-ranges) :void
  (hist :pointer)
  (ranges :pointer)
  (uniform :int))

(cffi:defcfun ("cvMakeHistHeaderForArray" make-hist-header-for-array) :pointer
  (dims :int)
  (sizes :int)
  (hist :pointer)
  (data :pointer)
  (ranges :pointer)
  (uniform :int))
;;; ---------------------------------------------------------------------------


(cffi:defcfun ("cvReleaseHist" %release-hist) :void
  (ptr :pointer))

(defun release-hist (histogram)
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-ref ptr :pointer) histogram)
    (%release-hist ptr)))

(define-cfun ("cvClearHist" clear-hist) :void
  (hist :pointer))

(cffi:defcfun ("cvGetMinMaxHistValue" %get-min-max-hist-value) :void
  (hist :pointer)
  (min-value :pointer)
  (max-value :pointer)
  (min-idx :pointer (cffi:null-pointer))
  (max-idx :pointer (cffi:null-pointer)))

(defun get-min-max-hist-value (hist)
  (cffi:with-foreign-objects ((min-value :float)
			      (max-value :float)
			      (min-idx :int)
			      (max-idx :int))
    (%get-min-max-hist-value hist min-value max-value min-idx max-idx)
    (values (cffi:mem-ref min-value :float)
	    (cffi:mem-ref max-value :float)
	    (cffi:mem-ref min-idx :int)
	    (cffi:mem-ref max-idx :int))))

(define-cfun ("cvNormalizeHist" normalize-hist) :void
  (hist :pointer)
  (factor :double))

(define-cfun ("cvThreshHist" thresh-hist) :void
  (hist :pointer)
  (threshold :double))

(define-cfun ("cvCompareHist" compare-hist) :void
  (hist1 :pointer)
  (hist2 :pointer)
  (method :int))

(define-cfun ("cvCopyHist" copy-hist) :void
  (src :pointer)
  (dst :pointer))

(define-cfun ("cvCalcBayesianProb" calc-bayesian-prob) :void
  (src :pointer)
  (number :int)
  (dst :pointer))

;;; @renaming 
(define-cfun ("cvCalcArrHist" calc-hist) :void
  (images :pointer)
  (hist :pointer)
  (accumulate :int 0)
  (mask :pointer (cffi:null-pointer)))

;;; @renaming 
(define-cfun ("cvCalcArrBackProject" calc-back-project) :void
  (images :pointer)
  (back-project :pointer)
  (hist :pointer))

(define-cfun ("cvCalcArrBackProjectPatch" calc-back-project-patch) :void
  (images :pointer)
  (dst :pointer)
  (range (:struct size))
  (hist :pointer)
  (method :int)
  (factor :double))

(define-cfun ("cvCalcProbDensity" calc-prob-density) :void
  (hist1 :pointer)
  (hist2 :pointer)
  (dst-hist :pointer)
  (scale :double 255))

(define-cfun ("cvEqualizeHist" equalize-hist) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("cvDistTransform" %dist-transform) :void
  (src :pointer)
  (dst :pointer)
  (distance-type :int +dist-l2+)
  (mask-size :int 3)
  (mask :pointer (cffi:null-pointer))
  (labels :pointer (cffi:null-pointer))
  (label-type :int +dist-label-ccomp+))

(defun dist-transform (src dst &optional
				 (distance-type +dist-l2+)
				 (mask-size 3)
				 mask
				 labels
				 (labels-type cv:+dist-label-ccomp+))
  (cffi:with-foreign-object (mask-ptr :float (length mask))
    (dotimes (i (length mask))
      (setf (cffi:mem-aref mask-ptr :float i) (coerce (nth i mask) 'single-float)))
    (%dist-transform src dst distance-type mask-size
		     (if mask mask-ptr (cffi:null-pointer))
		     (if labels labels (cffi:null-pointer))
		     labels-type)))

(define-cfun ("cvThreshold" threshold) :void
  (src :pointer)
  (dst :pointer)
  (threshold :double)
  (max-value :double)
  (threshold-type :int))

(define-cfun ("cvAdaptiveThreshold" adaptive-threshold) :void
  (src :pointer)
  (dst :pointer)
  (max-val :double)
  (ataptive-method :int +adaptive-thresh-mean-c+)
  (threshold-type :int +thresh-binary+)
  (block-size :int 3)
  (param1 :double 5))

(define-cfun ("cvFloodFill" floor-fill) :void
  (image :pointer)
  (seed-point (:struct point))
  (new-val (:struct scalar))
  (lo-diff (:struct scalar) (scalar-all 0.0d0))
  (up-diff (:struct scalar) (scalar-all 0.0d0))
  (comp :pointer (cffi:null-pointer))
  (flags :int 4)
  (mask :pointer (cffi:null-pointer)))


;; ***************************************************************************************\
;;                                  Feature detection                                     *
;; ****************************************************************************************/

(define-cfun ("cvCanny" canny) :void
  (image :pointer)
  (edges :pointer)
  (threshold1 :double)
  (threshold2 :double)
  (aperture-size :int 3))

(define-cfun ("cvPreCornerDetect" pre-corner-detect) :void
  (image :pointer)
  (corners :pointer)
  (aperture-size :int 3))

(define-cfun ("cvCornerEigenValsAndVecs" corner-eigen-vals-and-vecs) :void
  (image :pointer)
  (eigenvv :pointer)
  (block-size :int)
  (aperture-size :int 3))

(define-cfun ("cvCornerMinEigenVal" corenr-min-eigen-val) :void
  (image :pointer)
  (eigenval :pointer)
  (block-size :int)
  (aperture-size :int 3))

(define-cfun ("cvCornerHarris" corner-harris) :void
  (image :pointer)
  (harris-response :pointer)
  (block-size :int)
  (aperture-size :int 3)
  (k :double .04))

(define-cfun ("cvFindCornerSubPix" find-corner-sub-pix) :void
  (image :pointer)
  (corners :pointer)
  (count :int)
  (win (:struct size))
  (zero-zone (:struct size))
  (criteria (:struct term-criteria)))

(define-cfun ("cvGoodFeaturesToTrack" good-features-to-track) :void
  (image :pointer)
  (eig-image :pointer)
  (temp-image :pointer)
  (corners :pointer)
  (corner-count :pointer)
  (quality-level :double)
  (min-distance :double)
  (mask :pointer (cffi:null-pointer))
  (block-size :int 3)
  (user-harris :int 0)
  (k :double .04))

(define-cfun ("cvHoughLines2" hough-lines2) :pointer
  (image :pointer)
  (line-storage :pointer)
  (method :int)
  (rho :double)
  (theta :double)
  (threshold :int)
  (param1 :double 0.0)
  (param2 :double 0.0)
  (min-theta :double 0.0)
  (max-theta :double  pi))

(define-cfun ("cvHoughCircles" hough-circles) :pointer
  (image :pointer)
  (circle-storage :pointer)
  (method :int)
  (dp :double)
  (min-dist :double)
  (param1 :double 100.0)
  (param2 :double 100.0)
  (min-radius :int 0)
  (max-radius :int 0))

(define-cfun ("cvFitLine" fit-line) :void
  (points :pointer)
  (dist-type :int)
  (param :double)
  (reps :double)
  (aeps :double)
  (line :pointer))

;; ****************************************************************************************\
;;                                     Drawing                                            *
;; ****************************************************************************************/

(define-cfun ("cvLine" line) :void
  (img :pointer)
  (pt1 (:struct point))
  (pt2 (:struct point))
  (color (:struct scalar))
  (thickness :int 1)
  (line-type :int 8)
  (shift :int 0))

(define-cfun ("cvRectangle" rectangle) :void
  (img :pointer)
  (pt1 (:struct point))
  (pt2 (:struct point))
  (color (:struct scalar))
  (thickness :int 1)
  (line-type :int 8)
  (shift :int 0))

(define-cfun ("cvRectangleR" rectangle-r) :void
  (img :pointer)
  (r (:struct rect))
  (color (:struct scalar))
  (thickness :int 1)
  (line-type :int 8)
  (shift :int 0))


(define-cfun ("cvCircle" circle) :void
  (img :pointer)
  (center (:struct point))
  (radius :int)
  (color (:struct scalar))
  (thickness :int 1)
  (line-type :int 8)
  (shift :int 0))

(define-cfun ("cvEllipse" ellipse) :void
  (img :pointer)
  (center (:struct point))
  (axes (:struct size))
  (angle :double)
  (start-angle :double)
  (end-angle :double)
  (color (:struct scalar))
  (thickness :int 1)
  (line-type :int 8)
  (shift :int 0))

(define-cfun ("cvEllipseBox" ellipse-box) :void
  (img :pointer)
  (box (:struct box2d))
  (color (:struct scalar))
  (thickness :int 1)
  (line-type :int 8)
  (shift :int 0))

(define-cfun ("cvFillConvexPoly" fill-convex-poly) :void
  (img :pointer)
  (pts :pointer)
  (npts :int)
  (color (:struct scalar))
  (line-type :int)
  (shift :int))

(define-cfun ("cvFillPoly" fill-poly) :void
  (img :pointer)
  (ptr :pointer)
  (npts :pointer)
  (contours :int)
  (color (:struct scalar))
  (line-type :int 8)
  (shift :int 0))

(define-cfun ("cvPolyLine" poly-line) :void
  (img :pointer)
  (ptr :pointer)
  (npts :pointer)
  (contours :int)
  (is-closed :int)
  (color (:struct scalar))
  (thickness :int 1)
  (line-type :int 8)
  (shift :int 0))

(define-cfun ("cvClipLine" clip-line) :int
  (image-size (:struct size))
  (pt1 :pointer)
  (pt2 :pointer))

;;; cvInitLineIterator
(cffi:defcstruct font
  (name-font :string)
  (color (:struct scalar))
  (font-face :int)
  (ascii :pointer)
  (greek :pointer)
  (cyrillic :pointer)
  (hscale :float)
  (vscale :float)
  (shear :float)
  (thickness :int)
  (dx :float)
  (line-type :int))

(define-cfun ("cvInitFont" init-font) :void
  (font :pointer)
  (font-face :int)
  (hscale :double)
  (vscale :double)
  (shear :double 0.0)
  (thickness :int 1)
  (line-type :int 8))

(define-cfun ("cvPutText" put-text) :void
  (img :pointer)
  (text :string)
  (org (:struct point))
  (font :pointer)
  (color (:struct scalar)))

(define-cfun ("cvGetTextSize" get-text-size) :void
  (text-string :string)
  (font :pointer)
  (text-size :pointer)
  (baseline :pointer))

(define-cfun ("cvColorToScalar" color-to-scalar) (:struct scalar)
  (packed-color :double)
  (arrtype :int))

(define-cfun ("cvEllipse2Poly" ellipse-2-poly) :int
  (center (:struct point))
  (axes (:struct size))
  (angle :int)
  (arc-start :int)
  (arc-end :int)
  (pts :pointer)
  (delta :int))


(define-cfun ("cvDrawContours" draw-contours) :void
  (image :pointer)
  (contour :pointer)
  (external-color (:struct scalar))
  (hole-color (:struct scalar))
  (max-level :int)
  (thickness :int 1)
  (line-type :int 8)
  (offset (:struct point) (point 0 0)))

