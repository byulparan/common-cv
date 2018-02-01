(in-package #:cv)

;; ****************************************************************************************\
;;                                    Image Processing                                    *
;; ***************************************************************************************/

(cffi:defcfun ("cvCopyMakeBorder" copy-make-border) :void
  (src :pointer)
  (dst :pointer)
  (offset (:struct point))
  (border-type :make-border-enum)
  (value (:struct scalar)))

(cffi:defcfun ("cvSmooth" smooth) :void
  (src :pointer)
  (dst :pointer)
  (smooth-type :smooth-method-enum)
  (size1 :int)
  (size2 :int)
  (sigma1 :double)
  (sigma2 :double))

(cffi:defcfun ("cvFilter2D" filter-2d) :void
  (src :pointer)
  (dst :pointer)
  (kernel :pointer)
  (anchor (:struct point)))

(cffi:defcfun ("cvIntegral" integral) :void
  (image :pointer)
  (sum :pointer)
  (sqsum :pointer)
  (tilted-sum :pointer))

(cffi:defcfun ("cvPyrDown" pyr-down) :void
  (src :pointer)
  (dst :pointer)
  (filter :pyramid-enum))

(cffi:defcfun ("cvPyrUp" pyr-up) :void
  (src :pointer)
  (dst :pointer)
  (filter :pyramid-enum))

(cffi:defcfun ("cvWatershed" watershed) :void
  (image :pointer)
  (markers :pointer))

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

(cffi:defcfun ("cvCvtColor" cvt-color) :void
  (src :pointer)
  (dst :pointer)
  (code :cvt-color-enum))

(cffi:defcfun ("cvResize" resize) :void
  (src :pointer)
  (dst :pointer)
  (iterpolation :resize-enum))

(cffi:defcfun ("cvWrapAffine" wrap-affine) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer)
  (flags :int)
  (fillval (:struct scalar)))

(cffi:defcfun ("cvGetAffineTransform" get-affine-transform) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(cffi:defcfun ("cv2DRotationMatrix" 2d-rotation-matrix) :pointer
  (center (:struct point2d-32f))
  (angle :double)
  (scale :double)
  (map-matrix :pointer))

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

(cffi:defcfun ("cvRemap" remap) :void
  (src :pointer)
  (dst :pointer)
  (map-x :pointer)
  (map-y :pointer)
  (flags :int)
  (fillval (:struct scalar)))

(cffi:defcfun ("cvLogPolar" log-polar) :void
  (src :pointer)
  (dst :pointer)
  (center (:struct point2d-32f))
  (m :double)
  (flags :int))

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

(cffi:defcfun ("cvMoments" moments) :void
  (moments :pointer)
  (binary :int))

(cffi:defcfun ("cvGetSpatialMoment" get-spatial-moment) :double
  (moments :pointer)
  (x-order :int)
  (y-order :int))

(cffi:defcfun ("cvGetCentralMoment" get-central-moment) :double
  (moment :pointer)
  (x-order :int)
  (y-order :int))

(cffi:defcfun ("cvGetNormalizedCentralMoment" get-normalized-central-moment) :double
  (moment :pointer)
  (x-order :int)
  (y-order :int))

(cffi:defcfun ("cvGetQuadrangleSubPix" get-quadrangle-sub-pix) :void
  (src :pointer)
  (dst :pointer)
  (map-matrix :pointer))

(cffi:defcfun ("cvMatchTemplate" match-template) :void
  (image :pointer)
  (templ :pointer)
  (result :pointer)
  (method :match-enum))

(cffi:defcfun ("cvCalcEMD2" calc-emd2) :float
  (signature1 :pointer)
  (signature2 :pointer)
  (distance-type :dist-enum)
  (distance-func :pointer)
  (cost-matrix :pointer)
  (flow :pointer)
  (lower-bound :pointer)
  (user-data :pointer))

;; ****************************************************************************************\
;;                              Contours retrieving                                       *
;; ****************************************************************************************/

(cffi:defcfun ("cvFindContours" find-contours) :int
  (image :pointer)
  (storage :pointer)
  (first-contour :pointer)
  (header-size :int)
  (mode :contours-mode-enum)
  (method :contours-memthod-enum)
  (offset (:struct point)))

(cffi:defcfun ("cvStartFindContours" start-find-contours) :pointer
  (image :pointer)
  (storage :pointer)
  (header-size :int)
  (mode :contours-mode-enum)
  (method :contours-memthod-enum)
  (offset (:struct point)))

(cffi:defcfun ("cvFindNextContour" find-next-contour) :pointer
  (scanner :pointer))

(cffi:defcfun ("cvSubstitudeContour" substitude-contour) :void
  (scanner :pointer)
  (new-contour :pointer))

(cffi:defcfun ("cvEndFindContours" end-find-contours) :pointer
  (scanner :pointer))

(cffi:defcfun ("cvApproxChains" approx-chains) :pointer
  (seq :pointer)
  (storage :pointer)
  (method :contours-memthod-enum)
  (parameter :double)
  (minimal-perimeter :int)
  (recursive :int))

(cffi:defcfun ("cvStartReadChainPoints" start-read-chain-points) :pointer
  (chain :pointer))

(cffi:defcfun ("cvReadChainPoint" read-chain-point) (:struct point)
  (reader :pointer))


;; ****************************************************************************************\
;;                            Contour Processing and Shape Analysis                       *
;; ****************************************************************************************/

(cffi:defcfun ("cvApproxPoly" approx-poly) :pointer
  (src-seq :pointer)
  (header-size :int)
  (storage :pointer)
  (method :int)
  (eps :double)
  (recursive :int))


(cffi:defcfun ("cvArcLength" arc-length) :double
  (curve :pointer)
  (slice (:struct slice))
  (is-closed :int))

(defun contour-perimeter (contour)
  (arc-length contour (slice 0 #x3fffffff) 1))


(cffi:defcfun ("cvBoudingRect" bounding-rect) (:struct rect)
  (points :pointer)
  (update :int))

(cffi:defcfun ("cvContourArea" contour-area) :double
  (contour :pointer)
  (slice (:struct slice))
  (oriented :int))

(cffi:defcfun ("cvMinAreaRect2" min-area-rect2) (:struct box2d)
  (points :pointer)
  (storage :pointer))

(cffi:defcfun ("cvMinEnclosingCircle" min-enclosing-circle) :int
  (points :pointer)
  (center :pointer)
  (radius :pointer))

(cffi:defcfun ("cvMatchShapes" match-shapes) :double
  (object1 :pointer)
  (object2 :pointer)
  (method :contours-match-enum)
  (parameter :double))

(cffi:defcfun ("cvConvexHull2" convex-hull) :pointer
  (input :pointer)
  (hull-storage :pointer)
  (orientation :int)
  (return-points :int))

(cffi:defcfun ("cvCheckContourConvexity" check-contour-convexity) :int
  (contour :pointer))

(cffi:defcfun ("cvConvexityDefects" convexity-defects) :pointer
  (contour :pointer)
  (convexhull :pointer)
  (storage :pointer))

(cffi:defcfun ("cvFitEllipse2" fit-ellipse2) (:struct box2d)
  (points :pointer))

(cffi:defcfun ("cvMaxRect" max-rect) (:struct rect)
  (rect1 :pointer)
  (rect2 :pointer))

(cffi:defcfun ("cvBoxPoints" box-points) :void
  (box (:struct box2d))
  (pt (:struct point2d-32f) :count 4))

(cffi:defcfun ("cvPointPolygonTest" point-polygon-test) :double
  (contour :pointer)
  (pt (:struct point2d-32f))
  (measure-dist :int))


;; ****************************************************************************************\
;;                                  Histogram functions                                   *
;; ****************************************************************************************/

(cffi:defcfun ("cvCreateHist" create-hist) :pointer
  (dims :int)
  (sizes :pointer)
  (type :hist-enum)
  (ranges :pointer)
  (uniform :int))

;; (defun create-hist* (dims sizes type &optional ranges (uniform 1))
;;   (let* ((ref nil))
;;     (cffi:with-foreign-objects ((native-size :int (length sizes)))
;;       (dotimes (i (length sies))
;; 	(setf (cffi:mem-aref native-size :int i) (nth i sizes)))
;;       (if ranges (cffi:with-foreign-object (ranges-pointer :pointer (length ranges))
;; 		   (dotimes (i (length ranges))
;; 		     (let ((elements (cffi:foreign-alloc :float :count (length (nth i ranges)))))
;; 		       (dotimes (k (length (nth i ranges)))
;; 			 (setf (cffi:mem-aref elements :float k) (coerce (nth k (nth i ranges)) 'single-float)))
;; 		       (setf (cffi:mem-aref ranges-pointer :pointer i) elements)))
;; 		   (setf ref (cffi:foreign-funcall "cvCreateHist"
;; 						   :int (floor dims)
;; 						   :pointer native-size
;; 						   :int (cffi:foreign-enum-value :hist-enum type)
;; 						   :pointer ranges-pointer
;; 						   :int (floor uniform)
;; 						   :pointer))
;; 		   (dotimes (i (length ranges))
;; 		     (cffi-sys:foreign-free (cffi:mem-aref ranges-pointer :pointer i))))
;; 	(setf ref (cffi:foreign-funcall "cvCreateHist"
;; 					:int (floor dims)
;; 					:pointer native-size
;; 					:int (cffi:foreign-enum-value :hist-enum type)
;; 					:pointer (cffi:null-pointer)
;; 					:int (floor uniform)
;; 					:pointer))))
;;     ref))


(cffi:defcfun ("cvReleaseHist" release-hist) :void
  (ptr :pointer))

(cffi:defcfun ("cvClearHist" clear-hist) :void
  (hist :pointer))

(cffi:defcfun ("cvGetMinMaxHistValue" get-min-max-hist-value) :void
  (hist :pointer)
  (min-value :pointer)
  (max-value :pointer)
  (min-idx :pointer)
  (max-idx :pointer))

(cffi:defcfun ("cvNormalizeHist" normalize-hist) :void
  (hist :pointer)
  (factor :double))

(cffi:defcfun ("cvThreshHist" thresh-hist) :void
  (hist :pointer)
  (threshold :double))

(cffi:defcfun ("cvCompareHist" compare-hist) :void
  (hist1 :pointer)
  (hist2 :pointer)
  (method :hist-compare-enum))

(cffi:defcfun ("cvCopyHist" copy-hist) :void
  (src :pointer)
  (dst :pointer))

;;; @renaming 
(cffi:defcfun ("cvCalcArrHist" calc-hist) :void
  (images :pointer)
  (hist :pointer)
  (accumulate :int)
  (mask :pointer))

;;; @renaming 
(cffi:defcfun ("cvCalcArrBackProject" calc-back-project) :void
  (images :pointer)
  (back-project :pointer)
  (hist :pointer))

(cffi:defcfun ("cvCalcArrBackProjectPatch" calc-back-project-patch) :void
  (images :pointer)
  (dst :pointer)
  (range (:struct size))
  (hist :pointer)
  (method :hist-compare-enum)
  (factor :double))

(cffi:defcfun ("cvEqualizeHist" equalize-hist) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("cvDistTransform" dist-transform) :void
  (src :pointer)
  (dst :pointer)
  (distance-type :dist-enum)
  (mask-size :int)
  (mask :pointer)
  (labels :pointer))

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

(cffi:defcfun ("cvFloodFill" floor-fill) :void
  (image :pointer)
  (seed-point (:struct point))
  (new-val (:struct scalar))
  (lo-diff (:struct scalar))
  (up-diff (:struct scalar))
  (comp :pointer)
  (flags :int)
  (mask :pointer))


;; ***************************************************************************************\
;;                                  Feature detection                                     *
;; ****************************************************************************************/

(cffi:defcfun ("cvCanny" canny) :void
  (image :pointer)
  (edges :pointer)
  (threshold1 :double)
  (threshold2 :double)
  (aperture-size :int))

;; ****************************************************************************************\
;;                                     Drawing                                            *
;; ****************************************************************************************/

(cffi:defcfun ("cvLine" line) :void
  (img :pointer)
  (pt1 (:struct point))
  (pt2 (:struct point))
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvRectangle" rectangle) :void
  (img :pointer)
  (pt1 (:struct point))
  (pt2 (:struct point))
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvCircle" circle) :void
  (img :pointer)
  (center (:struct point))
  (radius :int)
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvEllipse" ellipse) :void
  (img :pointer)
  (center (:struct point))
  (axes (:struct size))
  (angle :double)
  (start-angle :double)
  (end-angle :double)
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvEllipseBox" ellipse-box) :void
  (img :pointer)
  (box (:struct box2d))
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvFillConvexPoly" fill-convex-poly) :void
  (img :pointer)
  (pts :pointer)
  (npts :int)
  (color (:struct scalar))
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvFillPoly" fill-poly) :void
  (img :pointer)
  (ptr :pointer)
  (npts :pointer)
  (contours :int)
  (color (:struct scalar))
  (line-type :int)
  (shift :int))

(cffi:defcfun ("cvPolyLine" poly-line) :void
  (img :pointer)
  (ptr :pointer)
  (npts :pointer)
  (contours :int)
  (is-closed :int)
  (color (:struct scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))

(cffi:defcstruct font
  (name-font :string)
  (color (:struct scalar))
  (font-face :font-face-enum)
  (ascii :pointer)
  (greek :pointer)
  (cyrillic :pointer)
  (hscale :float)
  (vscale :float)
  (shear :float)
  (thickness :int)
  (dx :float)
  (line-type :int))

(cffi:defcfun ("cvPutText" put-text) :void
  (img :pointer)
  (text :string)
  (org (:struct point))
  (font :pointer)
  (color (:struct scalar)))


(cffi:defcfun ("cvDrawContours" draw-contours) :void
  (image :pointer)
  (contour :pointer)
  (external-color (:struct scalar))
  (hole-color (:struct scalar))
  (max-level :int)
  (thickness :int)
  (line-type :int)
  (offset (:struct point)))




