
(in-package #:cv)

;;; histogram
(define-method query-hist-value-1d ((hist cv-histogram) idx0)
  (cffi:foreign-funcall "cv_QueryHistValue_1D" :pointer (ref hist)
					      :int (floor idx0)
					      :double))

(define-method query-hist-value-2d ((hist cv-histogram) idx0 idx1)
  (cffi:foreign-funcall "cv_QueryHistValue_2D" :pointer (ref hist)
					      :int (floor idx0)
					      :int (floor idx1)
					      :double))

(define-method query-hist-value-3d ((hist cv-histogram) idx0 idx1 idx2)
  (cffi:foreign-funcall "cv_QueryHistValue_3D" :pointer (ref hist)
					      :int (floor idx0)
					      :int (floor idx1)
					      :int (floor idx2)
					      :double))

(define-method query-hist-value-nd ((hist cv-histogram) (idxn list))
  (cffi:with-foreign-object (native-idxn :int (length idxn))
    (dotimes (i (length idxn))
      (setf (cffi:mem-aref native-idxn :int i) (floor (nth i idxn))))
    (cffi:foreign-funcall "cv_QueryHistValue_nD" :pointer (ref hist)
						:pointer native-idxn
						:double)))


;; (define-method get-hist-value-1d ((hist cv-histogram) idx0)
;;   (cffi:foreign-funcall "cvGetHistValue_1D" :pointer (ref hist)
;; 					      :int (floor idx0)
;; 					      :double))

;; (define-method get-hist-value-2d ((hist cv-histogram) idx0 idx1)
;;   (cffi:foreign-funcall "cvGetHistValue_2D" :pointer (ref hist)
;; 					      :int (floor idx0)
;; 					      :int (floor idx1)
;; 					      :double))

;; (define-method get-hist-value-3d ((hist cv-histogram) idx0 idx1 idx2)
;;   (cffi:foreign-funcall "cvGetHistValue_3D" :pointer (ref hist)
;; 					      :int (floor idx0)
;; 					      :int (floor idx1)
;; 					      :int (floor idx2)
;; 					      :double))

;; (define-method get-hist-value-nd ((hist cv-histogram) (idxn list))
;;   (cffi:with-foreign-object (native-idxn :int (length idxn))
;;     (dotimes (i (length idxn))
;;       (setf (cffi:mem-aref native-idxn :int i) (floor (nth i idxn))))
;;     (cffi:foreign-funcall "cvGetHistValue_nD" :pointer (ref hist)
;; 						:pointer native-idxn
;; 						:double)))

(define-method normalize-hist ((hist cv-histogram) factor)
  (cffi:foreign-funcall "cvNormalizeHist" :pointer (ref hist)
					  :double (* 1.0d0 factor)))

(define-method thresh-hist ((hist cv-histogram) threshold)
  (cffi:foreign-funcall "cvThreshHist" :pointer (ref hist)
				       :double (* 1.0d0 threshold)))

(define-method copy-hist ((src cv-histogram) (dst cv-histogram))
  (cffi:foreign-funcall "cvCopyHist" :pointer (ref src)
				     :pointer (ref dst)))

(define-method get-min-max-hist-value ((hist cv-histogram) min-value max-value
				       &optional min-idx max-idx)
  (let (min-v max-v min-i max-i)
    (when min-value (setf min-v (cffi:foreign-alloc :float)))
    (when max-value (setf max-v (cffi:foreign-alloc :float)))
    (when min-idx (setf min-i (cffi:foreign-alloc :int :count (dims hist))))
    (when max-idx (setf max-i (cffi:foreign-alloc :int :count (dims hist))))
    (cffi:foreign-funcall "cvGetMinMaxHistValue" :pointer (ref hist)
						 :pointer (if min-value min-v
							      (cffi-sys:null-pointer))
						 :pointer (if max-value max-v
							      (cffi-sys:null-pointer))
						 :pointer (if min-idx min-i
							      (cffi-sys:null-pointer))
						 :pointer (if max-idx max-i
							      (cffi-sys:null-pointer)))
    (prog1
	(list
	 (when min-value  (cffi:mem-ref min-v :float))
	 (when max-value  (cffi:mem-ref max-v :float))
	 (when min-idx  (let ((val (loop for i from 0 below (dims hist)
					 collect (cffi:mem-aref min-i :int i))))
			  (if (= 1 (length val)) (car val) val)))
	 (when max-idx (let ((val (loop for i from 0 below (dims hist)
					collect (cffi:mem-aref max-i :int i))))
			 (if (= 1 (length val)) (car val) val))))
      (when min-value (cffi:foreign-free min-v))
      (when max-value (cffi:foreign-free max-v))
      (when min-idx (cffi:foreign-free min-i))
      (when max-idx (cffi-sys:foreign-free max-i)))))

(define-method calc-hist ((image list) (hist cv-histogram) &optional (accumulate 0)
			  mask)
  (cffi:with-foreign-object (native-image :pointer (length image))
    (dotimes (i (length image))
      (setf (cffi:mem-aref native-image :pointer i) (ref (nth i image))))
    (cffi:foreign-funcall "cv_CalcHist" :pointer native-image
					:pointer (ref hist)
					:int (floor accumulate)
					:pointer (if mask (ref mask) (cffi-sys:null-pointer)))))

(define-method compare-hist ((hist1 cv-histogram) (hist2 cv-histogram) (method symbol))
  (cffi:foreign-funcall "cvCompareHist" :pointer (ref hist1)
					:pointer (ref hist2)
					:int (cffi:foreign-enum-value :hist-compare-enum method)))



(define-method calc-emd-2 ((signature1 cv-arr) (signature2 cv-arr) (distance-type symbol)
			   &optional distance-func cost-matrix flow lower-bound userdata)
  (cffi:foreign-funcall "cvCalcEMD2" :pointer (ref signature1)
				     :pointer (ref signature2)
				     :int (cffi:foreign-enum-value :dist-enum distance-type)
				     :pointer (if distance-func distance-func (cffi-sys:null-pointer))
				     :pointer (if cost-matrix (ref cost-matrix) (cffi-sys:null-pointer))
				     :pointer (if flow (ref flow) (cffi-sys:null-pointer))
				     :pointer (if lower-bound lower-bound (cffi-sys:null-pointer))
				     :pointer (if userdata userdata (cffi-sys:null-pointer))
				     :float))

(define-method calc-back-project ((image list) (back-project cv-arr) (hist cv-histogram))
  (cffi:with-foreign-object (native-image :pointer (length image))
    (dotimes (i (length image))
      (setf (cffi:mem-aref native-image :pointer i) (ref (nth i image))))
    (cffi:foreign-funcall "cv_CalcBackProject" :pointer native-image
					      :pointer (ref back-project)
					      :pointer (ref hist))))

(define-method calc-back-project-patch ((image list) (dst cv-arr) (patch-size cv-size)
					(hist cv-histogram) (method symbol) factor)
  (with-cv-size (native-size patch-size)
    (cffi:with-foreign-object (native-image :pointer (length image))
      (dotimes (i (length image))
	(setf (cffi:mem-aref native-image :pointer i) (ref (nth i image))))
      (cffi:foreign-funcall "cv_CalcBackProjectPatch" :pointer native-image
						      :pointer (ref dst)
						      :pointer native-size
						      :pointer (ref hist)
						      :int (cffi:foreign-enum-value :hist-compare-enum method)
						      :float (* 1.0 factor)))))

(define-method match-template ((image cv-arr) (templ cv-arr) (result cv-arr) (method symbol))
  (cffi:foreign-funcall "cvMatchTemplate" :pointer (ref image)
					  :pointer (ref templ)
					  :pointer (ref result)
					  :int (cffi:foreign-enum-value :match-enum method)))





;;; Contours Library

(define-method find-contours ((image cv-arr) (storage cv-mem-storage)
			      &optional (header-size 128) (mode :cv-retr-list) (method :cv-chain-approx-simple)
			      (offset (point 0 0)))
  (cffi:with-foreign-object (seq-ptr :pointer)
    (with-cv-point (native-offset offset)
      (cffi:foreign-funcall "cv_FindContours" :pointer (ref image)
					      :pointer (ref storage)
					      :pointer seq-ptr
					      :int header-size
					      :int (cffi:foreign-enum-value :contours-mode-enum mode)
					      :int (cffi:foreign-enum-value :contours-memthod-enum method)
					      :pointer native-offset
					      :int))
    (if (eql method :cv-chain-code)
	(make-instance 'cv-chain :ref (cffi:mem-ref seq-ptr :pointer))
	(make-instance 'cv-seq :ref (cffi:mem-ref seq-ptr :pointer)))))


(define-method start-find-contours ((image cv-arr) (storage cv-mem-storage) &optional (header-size 128)
				    (mode :cv-retr-list) (method :cv-chain-approx-simple) (offset (point 0 0)))
  (let ((result (with-cv-point (native-offset offset)
		  (cffi:foreign-funcall "cv_StartFindContours" :pointer (ref image)
							       :pointer (ref storage)
							       :int header-size
							       :int (cffi:foreign-enum-value :contours-mode-enum mode)
							       :int (cffi:foreign-enum-value :contours-memthod-enum method)
							       :pointer native-offset
							       :pointer))))
    (make-instance 'cv-contour-scanner :ref result)))

(define-method find-next-contour ((scanner cv-contour-scanner))
  (let ((result (cffi:foreign-funcall "cvFindNextContour" :pointer (ref scanner)
							   :pointer)))
    (make-instance 'cv-seq :ref result)))

(define-method substitute-contour ((scanner cv-contour-scanner) (new-contour cv-seq))
  (cffi:foreign-funcall "cvSubstituteContour" :pointer (ref scanner)
					       :pointer (ref new-contour)))

;; ;; (define-method end-find-contour ((scanner cv-contour-scanner))
;; ;;   (let ((result (cffi:foreign-funcall "cv_EndFindContour" :pointer (ref scanner)
;; ;; 							  :pointer)))
;; ;;     (make-instance 'cv-seq :ref result)))

(define-method approx-chains ((src-seq cv-seq) (storage cv-mem-storage) &optional (method :cv-chain-approx-simple)
			      (parameter 0.0) (minimal-perimeter 0) (recursive 0))
  (let ((result (cffi:foreign-funcall "cvApproxChains" :pointer (ref src-seq) :pointer (ref storage)
						       :int (cffi:foreign-enum-value :contours-memthod-enum method)
						       :double (* 1.0d0 parameter)
						       :int (floor minimal-perimeter)
						       :int (floor recursive))))
    (make-instance 'cv-chain :ref result)))

(define-method start-read-chain-points ((chain cv-chain))
  (let ((result (cffi:foreign-funcall "cv_StartReadChainPoints" :pointer (ref chain)
								:pointer)))
    (make-instance 'cv-chain-pt-reader :ref result)))

(define-method read-chain-point ((reader cv-chain-pt-reader))
  (cffi:with-foreign-object (native-ptr '(:struct CvPoint))
    (let ((ret (point 0 0)))
      (cffi:foreign-funcall "cv_ReadChainPoint" :pointer (ref reader)
						:pointer native-ptr)
      (cffi:with-foreign-slots ((x y) native-ptr (:struct CvPoint))
	(setf (x ret) x
	      (y ret) y))
      ret)))

(define-method draw-contours ((image cv-arr) (contour cv-seq) (external-color cv-scalar)
			      (hole-color cv-scalar) max-level &optional (thickness 1)
			      (line-type 8) (offset (point 0 0)))
  (with-cv-scalar (native-external-color external-color)
    (with-cv-scalar (native-hole-color hole-color)
      (with-cv-point (native-offset offset)
	(cffi:foreign-funcall "cv_DrawContours" :pointer (ref image)
						:pointer (ref contour)
						:pointer native-external-color
						:pointer native-hole-color
						:int (floor max-level)
						:int (floor thickness)
						:int (floor line-type)
						:pointer native-offset)))))


(define-method approx-poly (src-seq header-size (storage cv-mem-storage) method parameter &optional (recursive 0))
  (declare (ignore method))
  (make-instance 'cv-seq :ref
		 (cffi:foreign-funcall "cvApproxPoly" :pointer (ref src-seq) :int (floor header-size) :pointer (ref storage)
						      :int 0 :double (* 1.0d0 parameter) :int (floor recursive) :pointer)))

(define-method find-dominant-points ((contour cv-seq) (storage cv-mem-storage)
				     &optional (method 1) (parameter1 0)
				     (parameter2 0) (parameter3 0) (parameter4 0))
  (make-instance 'cv-seq :ref
		 (cffi:foreign-funcall "cvFindDominantPoints" :pointer (ref contour)
							      :pointer (ref storage)
							      :int method
							      :double (* 1.0d0 parameter1)
							      :double (* 1.0d0 parameter2)
							      :double (* 1.0d0 parameter3)
							      :double (* 1.0d0 parameter4)
							      :pointer)))




(defvar +cv-whole-seq+ (slice 0 #x3fffffff))
(define-method arc-length (curve &optional (slice +cv-whole-seq+) (is-closed -1))
  (cffi:with-foreign-object (native-slice '(:struct CvSlice))
    (cffi:with-foreign-slots ((start-index end-index) native-slice (:struct CvSlice))
      (setf start-index (start-index slice)
	    end-index (end-index slice)))
    (cffi:foreign-funcall "cv_ArcLength" :pointer (ref curve) :pointer native-slice :int (floor is-closed)
			  :double)))

(define-method contour-perimeter (contour)
  (arc-length contour +cv-whole-seq+ 1))

(define-method contour-area (contour &optional (slice +cv-whole-seq+))
  (cffi:with-foreign-object (native-slice '(:struct CvSlice))
    (cffi:with-foreign-slots ((start-index end-index) native-slice (:struct CvSlice))
      (setf start-index (start-index slice)
	    end-index (end-index slice)))
    (cffi:foreign-funcall "cv_ContourArea" :pointer (ref contour) :pointer native-slice :double)))


(define-method bounding-rect (points &optional (update 0))
  (cffi:with-foreign-object (rect '(:struct CvRect))
    (cffi:foreign-funcall "cv_BoundingRect" :pointer (ref points) :int update :pointer rect)
    (cffi:with-foreign-slots ((x y width height) rect (:struct CvRect))
      (rect x y width height))))

(define-method min-area-rect2 (points &optional storage)
  (cffi:with-foreign-object (box '(:struct cvbox2d))
    (cffi:foreign-funcall "cv_MinAreaRect2" :pointer (ref points)
					    :pointer (if storage (ref storage) (cffi-sys:null-pointer))
					    :pointer box)
    (cffi:with-foreign-slots ((center size angle) box (:struct cvbox2d))
      (cffi:with-foreign-slots ((x y) center (:struct cvpoint))
	(cffi:with-foreign-slots ((width height) size (:struct cvsize))
	  (box-2d x y width height angle))))))

(define-method min-enclosing-circle (points)
  (cffi:with-foreign-object (center '(:struct cvpoint2d32f))
    (cffi:with-foreign-object (radius :float)
      (cffi:foreign-funcall "cvMinEnclosingCircle" :pointer (ref points)
						   :pointer center
						   :pointer radius)
      (cffi:with-foreign-slots ((x y) center (:struct cvpoint2d32f))
	(list (point-2d-32f x y) (cffi:mem-ref radius :float))))))

(define-method fit-ellipse2 (points)
  (cffi:with-foreign-object (native-box '(:struct cvbox2d))
    (cffi:foreign-funcall "cv_FitEllipse2" :pointer (ref points)
					   :pointer native-box)
    (cffi:with-foreign-slots ((center size angle) native-box (:struct cvbox2d))
      (cffi:with-foreign-slots ((x y) center (:struct cvpoint))
	(cffi:with-foreign-slots ((width height) size (:struct cvsize))
	  (box-2d x y width height angle))))))

(define-method max-rect ((rect1 cv-rect) (rect2 cv-rect))
  (cffi:with-foreign-objects ((dst '(:struct cvrect))
			      (rect-1 '(:struct cvrect))
			      (rect-2 '(:struct cvrect)))
    (cffi:with-foreign-slots ((x y width height) rect-1 (:struct cvrect))
      (setf x (x rect1)
	    y (y rect1)))
    (cffi:with-foreign-slots ((x y width height) rect-2 (:struct cvrect))
      (setf x (x rect2)
	    y (y rect2)))
    (cffi:foreign-funcall "cv_MaxRect" :pointer rect-1 :pointer rect-2 :pointer dst)
    (cffi:with-foreign-slots ((x y width height) dst (:struct cvrect))
      (rect x y width height))))

(define-method box-points ((box cv-box-2d) (pt list))
  (cffi:with-foreign-objects ((native-box '(:struct cvbox2d))
			      (native-pt '(:struct cvpoint2d32f) (length pt)))
    (cffi:with-foreign-slots ((center size angle) native-box (:struct cvbox2d))
      (cffi:with-foreign-slots ((x y) center (:struct cvpoint))
	(setf x (x (box-center box))
	      y (y (box-center box))))
      (cffi:with-foreign-slots ((width height) size (:struct cvsize))
	(setf width (width (box-size box))
	      height (height (box-size box))))
      (setf angle (box-angle box)))
    (dotimes (i (length pt))
      (cffi:with-foreign-slots ((x y) (cffi:mem-aptr native-pt '(:struct cvpoint2d32f) i) (:struct cvpoint2d32f))
	(setf x (x (nth i pt))
	      y (y (nth i pt)))))
    (cffi:foreign-funcall "cv_BoxPoints" :pointer native-box :pointer native-pt)))

(define-method point-polygon-test (contour (pt cv-point-2d-32f) measure-dist)
  (cffi:with-foreign-object (native-pt '(:struct cvpoint2d32f))
    (cffi:with-foreign-slots ((x y) native-pt (:struct cvpoint2d32f))
      (setf x (x pt)
	    y (y pt)))
    (cffi:foreign-funcall "cv_PointPolygonTest" :pointer (ref contour)
						:pointer native-pt
						:int (floor measure-dist))))

(define-method contour-moments (contour)
  (let ((mem (cffi:foreign-alloc :double :count 18)))
    (cffi:foreign-funcall "cvMoments" :pointer (ref contour)
				      :pointer mem
				      :int 0)
    (make-instance 'cv-moments :ref mem)))

(define-method get-spatial-moment ((moments cv-moments) x-order y-order)
  (cffi:foreign-funcall "cvGetSpatialMoment" :pointer (ref moments)
					     :int (floor x-order)
					     :int (floor y-order)
					     :double))

(define-method moments ((image cv-arr) &optional (binary 0))
  (let ((mem (cffi:foreign-alloc :double :count 18)))
    (cffi:foreign-funcall "cvMoments" :pointer (ref image)
				      :pointer mem
				      :int (floor binary))
    (make-instance 'cv-moments :ref mem)))

(define-method get-central-moment ((moment cv-moments) x-order y-order)
  (cffi:foreign-funcall "cvGetCentralMoment" :pointer (ref moment)
					     :int (floor x-order)
					     :int (floor y-order)
					     :double))

(define-method get-normalized-central-moment ((moment cv-moments) x-order y-order)
  (cffi:foreign-funcall "cvGetNormalizedCentralMoment" :pointer (ref moment)
						       :int (floor x-order)
						       :int (floor y-order)
						       :double))


(define-method match-shapes (object1 object2 method &optional (parameter 0))
  (cffi:foreign-funcall "cvMatchShapes" :pointer (ref object1)
					:pointer (ref object2)
					:int (cffi:foreign-enum-value :contours-match-enum method)
					:double (* 1.0d0 parameter)
					:double))

(define-method create-contour-tree ((contour cv-seq) (storage cv-mem-storage) threshold)
  (make-instance 'cv-contour-tree
		 :ref (cffi:foreign-funcall "cvCreateContourTree" :pointer (ref contour)
								  :pointer (ref storage)
								  :double (* 1.0d0 threshold)
								  :pointer)))

(define-method contour-from-contour-tree ((tree cv-contour-tree) (storage cv-mem-storage) (criteria cv-term-criteria))
  (make-instance 'cv-seq :ref
		 (cffi:foreign-funcall "cv_ContourFromContourTree" :pointer (ref tree) :pointer (ref storage) :pointer (ref criteria)
								   :pointer)))

(define-method match-contour-trees ((tree1 cv-contour-tree) (tree2 cv-contour-tree) (method symbol) threshold)
  (cffi:foreign-funcall "cvMatchContourTrees" :pointer (ref tree1)
					      :pointer (ref tree2)
					      :int (cffi:foreign-enum-value :contours-match-enum method)
					      :double (* 1.0d0 threshold)
					      :double))

(define-method convex-hull2 (input hull-storage &optional (orientation :cv-clockwise) (return-points 0))
  (let ((seq
	  (cffi:foreign-funcall "cvConvexHull2" :pointer (ref input) :pointer (ref hull-storage)
						:int (cffi:foreign-enum-value :convex-enum orientation) :int (floor return-points)
						:pointer)))
    (make-instance 'cv-seq :ref seq)))

(define-method check-contour-convexity (contour)
  (cffi:foreign-funcall "cvCheckContourConvexity" :pointer (ref contour) :int))

(define-method convexity-defects (contour convexhull &optional storage)
  (let ((seq (cffi:foreign-funcall "cvConvexityDefects" :pointer (ref contour)
							:pointer (ref convexhull)
							:pointer (if storage (ref storage) (cffi-sys:null-pointer)))))
    (make-instance 'cv-seq :ref seq)))

(define-method calc-pgh ((contour cv-seq) (hist cv-histogram))
  (cffi:foreign-funcall "cvCalcPGH" :pointer (ref contour) :pointer (ref hist)))

(define-method watershed ((image cv-arr) (markers cv-arr))
  (cffi:foreign-funcall "cvWatershed" :pointer (ref image)
				      :pointer (ref markers)))

(define-method haar-detect-object ((image cv-arr) (cascade cv-haar-classifier-cascade) (storage cv-mem-storage)
				   &optional (scale-factor 1.1) (min-seighbor 3) flags (min-size (size 0 0))
				   (max-size (size 0 0)))
  (with-cv-size (native-min-size min-size)
    (with-cv-size (native-max-size max-size)
      (make-instance 'cv-seq :ref
		     (cffi:foreign-funcall "cv_HaarDetectObjects" :pointer (ref image)
								  :pointer (ref cascade)
								  :pointer (ref storage)
								  :double (* 1.0d0 scale-factor)
								  :int (floor min-seighbor)
								  :int (apply #'logior (mapcar #!(cffi:foreign-enum-value :haar-detect-enum %1) (su:mklist flags)))
								  :pointer native-min-size
								  :pointer native-max-size
								  :pointer)))))
