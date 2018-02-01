(in-package #:cv)

;;; histogram
(cffi:defcfun ("cvNormalizeHist" normalize-hist) :void
  (hist :pointer)
  (factor :double))

(cffi:defcfun ("cvThreshHist" thresh-hist) :void
  (hist :pointer)
  (threshold :double))

(cffi:defcfun ("cvCopyHist" copy-hist) :void
  (src :pointer)
  (dst :pointer))

(cffi:defcfun ("cvGetMinMaxHistValue" get-min-max-hist-value) :void
  (hist :pointer)
  (min-value :pointer)
  (max-value :pointer)
  (min-idx :pointer)
  (max-idx :pointer))

;;; @renaming 
(cffi:defcfun ("cvCalcArrHist" calc-hist) :void
  (images :pointer)
  (hist :pointer)
  (accumulate :int)
  (mask :pointer))

(cffi:defcfun ("cvCompareHist" compare-hist) :void
  (hist1 :pointer)
  (hist2 :pointer)
  (method :hist-compare-enum))

(cffi:defcfun ("cvCalcEMD2" calc-emd2) :float
  (signature1 :pointer)
  (signature2 :pointer)
  (distance-type :dist-enum)
  (distance-func :pointer)
  (cost-matrix :pointer)
  (flow :pointer)
  (lower-bound :pointer)
  (user-data :pointer))

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

(cffi:defcfun ("cvMatchTemplate" match-template) :void
  (image :pointer)
  (templ :pointer)
  (result :pointer)
  (method :match-enum))

;;; Contours library
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

(cffi:defcfun ("cvDrawContours" draw-contours) :void
  (image :pointer)
  (contour :pointer)
  (external-color (:struct scalar))
  (hole-color (:struct scalar))
  (max-level :int)
  (thickness :int)
  (line-type :int)
  (offset (:struct point)))

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

(cffi:defcfun ("cvContourArea" contour-area) :double
  (contour :pointer)
  (slice (:struct slice))
  (oriented :int))

(cffi:defcfun ("cvBoudingRect" bounding-rect) (:struct rect)
  (points :pointer)
  (update :int))

(cffi:defcfun ("cvMinAreaRect2" min-area-rect2) (:struct box2d)
  (points :pointer)
  (storage :pointer))

(cffi:defcfun ("cvMinEnclosingCircle" min-enclosing-circle) :int
  (points :pointer)
  (center :pointer)
  (radius :pointer))

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

(cffi:defcfun ("cvWatershed" watershed) :void
  (image :pointer)
  (markers :pointer))
