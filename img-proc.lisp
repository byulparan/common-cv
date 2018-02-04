(in-package #:cv)

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
  (fillval (:struct scalar)))

(define-cfun ("cvLogPolar" log-polar) :void
  (src :pointer)
  (dst :pointer)
  (center (:struct point2d-32f))
  (m :double)
  (flags :int (+ +inter-linear+ +warp-fill-outliers+)))

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


(define-cfun ("cvSubstitudeContour" substitude-contour) :void
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

(define-cfun ("cvPutText" put-text) :void
  (img :pointer)
  (text :string)
  (org (:struct point))
  (font :pointer)
  (color (:struct scalar)))


(define-cfun ("cvDrawContours" draw-contours) :void
  (image :pointer)
  (contour :pointer)
  (external-color (:struct scalar))
  (hole-color (:struct scalar))
  (max-level :int)
  (thickness :int 1)
  (line-type :int 8)
  (offset (:struct point) (point 0 0)))

