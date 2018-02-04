(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :common-cv)
  (ql:quickload :trivial-main-thread)
  (ql:quickload :bordeaux-threads))

(in-package :cl-user)

(defparameter *examples-dir*
  (concatenate 'string (namestring (asdf/system:system-source-directory :common-cv)) "examples/"))

(defmacro with-gui-thread (&body body)
  "Wraps BODY in code which masks float traps.
   This is needed in SBCL on OSX because native code often
   generate :inexact traps and land you in the debugger.
   For non SBCL this wraps body in a progn."
  `(trivial-main-thread:call-in-main-thread
    (lambda () 
      #+sbcl (sb-int:with-float-traps-masked (:invalid :divide-by-zero)
	       ,@body)
      #+ccl (unwind-protect (progn
			       (ccl:set-fpu-mode :invalid nil)
			       ,@body)
	       (ccl:set-fpu-mode :invalid t))
      #-(or sbcl ccl)
      ,@body)))


(defun adaptive-threshold ()
  (with-gui-thread 
    (cv:with-named-window ("adaptive-threshold")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid)))
	    (cv:with-ipl-images ((img (cv:get-size frame) cv:+ipl-depth-8u+ 1))
	      (cv:cvt-color frame img cv:+bgr-2-gray+)
	      (cv:adaptive-threshold  img img 240 cv:+adaptive-thresh-mean-c+ cv:+thresh-binary+
				      3 5)
	      (cv:show-image "adaptive-threshold" img)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))

(defun filter-mat ()
  (let* ((filter-5x5 (mapcar (lambda (p) (/ p 256.0d0))
			     (list 100 140 116 114 111
				   112 118 12 8 2
				   6 24 36 24 6
				   2 8 12 8 2
				   1 4 6 3 1)))
	 (mat (cv:create-mat 5 5 cv:+64fc1+))
	 (data (cv:mat-data mat)))
    (dotimes (i 25)
      (setf (cffi:mem-aref data :double i) (nth i filter-5x5)))
    (with-gui-thread 
      (cv:with-named-window ("filter-mat")
	(cv:with-captured-camera (vid :width 640 :height 480)
	  (loop
	    (let* ((frame (cv:query-frame vid)))
	      (cv:with-ipl-images ((img (cv:get-size frame) cv:+ipl-depth-8u+ 3))
		(cv:filter-2d frame img mat (cv:point 3 3))
		(cv:show-image "filter-mat" img)))
	    (let ((c (cv:wait-key 33)))
	      (when (= c 27)
		(return)))))))))


;;; copy-make-border
(defun make-border ()
  (with-gui-thread
    (cv:with-named-window ("MakeBorder")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid))
		 (size (cv:get-size frame)))
	    (cv:with-ipl-images ((dst (cv:size (+ (cv:width size) (* 2 2))
					       (+ (cv:height size) (* 2 2)))
				      cv:+ipl-depth-8u+ 3))
	      (cv:copy-make-border frame dst (cv:point 2 2)
				   cv:+ipl-border-constant+
				   (cv:real-scalar 200))
	      (cv:show-image "MakeBorder" dst)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))

;;; sobel
(defun sobel ()
  (with-gui-thread
    (cv:with-named-window ("Sobel")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid))
		 (size (cv:get-size frame)))
	    (cv:with-ipl-images ((cvt size cv:+ipl-depth-8u+ 1)
				 (dst size cv:+ipl-depth-8u+ 1)
				 (tmp size cv:+ipl-depth-16s+ 1))
	      (cv:cvt-color frame cvt cv:+bgr-2-gray+)
	      (cv:sobel cvt tmp 1 0 -1)
	      (cv:convert-scale-abs tmp dst)
	      (cv:show-image "Sobel" dst)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))


;;; laplace
(defun laplace ()
  (with-gui-thread 
    (cv:with-named-window ("laplace")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid))
		 (size (cv:get-size frame)))
	    (cv:with-ipl-images ((cvt size cv:+ipl-depth-8u+ 1)
				 (dst size cv:+ipl-depth-8u+ 1)
				 (tmp size cv:+ipl-depth-16s+ 1))
	      (cv:cvt-color frame cvt cv:+bgr-2-gray+)
	      (cv:laplace cvt tmp 3)
	      (cv:convert-scale-abs tmp dst)
	      (cv:show-image "laplace" dst)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))

;;; canny
(defun canny ()
  (with-gui-thread
    (cv:with-named-window ("canny")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid))
		 (size (cv:get-size frame)))
	    (cv:with-ipl-images ((src size cv:+ipl-depth-8u+ 1)
				 (dst size cv:+ipl-depth-8u+ 1))
	      (cv:cvt-color frame src cv:+bgr-2-gray+)
	      (cv:canny src dst 10.0 50.0)
	      (cv:show-image "canny" dst)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))


;;; affine
(defun affine ()
  (with-gui-thread
    (let ((src-tri (list (cv:point2d-32f 0 0) (cv:point2d-32f 0 0) (cv:point2d-32f 0 0)))
	  (dst-tri (list (cv:point2d-32f 0 0) (cv:point2d-32f 0 0) (cv:point2d-32f 0 0)))
	  (rot-mat (cv:create-mat 2 3 cv:+32fc1+))
	  (warp-mat (cv:create-mat 2 3 cv:+32fc1+)))
      (cv:with-named-window ("affine")
	(cv:with-captured-camera (vid :width 640 :height 480)
 	  (loop
	    (let* ((src (cv:query-frame vid))
		   (size (cv:get-size src)))
	      (cv:with-ipl-images ((dst size cv:+ipl-depth-8u+ 3))
		(setf (cv:x (nth 0 src-tri)) 0.0
		      (cv:y (nth 0 src-tri)) 0.0
		      (cv:x (nth 1 src-tri)) (- (cv:width size) 1.0)
		      (cv:y (nth 1 src-tri)) 0.0
		      (cv:x (nth 2 src-tri)) 0.0
		      (cv:y (nth 2 src-tri)) (- (cv:height size) 1.0))
		(setf (cv:x (nth 0 dst-tri)) (* 0.0 (cv:width size))
		      (cv:y (nth 0 dst-tri)) (* (cv:height size) 0.33)
		      (cv:x (nth 1 dst-tri)) (* (cv:width size) 0.85)
		      (cv:y (nth 1 dst-tri)) (* (cv:height size) 0.25)
		      (cv:x (nth 2 dst-tri)) (* (cv:width size) 0.15)
		      (cv:y (nth 2 dst-tri)) (* (cv:height size) 0.7))
		(cv:get-affine-transform src-tri dst-tri warp-mat)
		(cv:warp-affine src dst warp-mat  (+ cv:+inter-linear+ cv:+warp-fill-outliers+) (cv:scalar 0 0 100))
		(cv:copy dst src)
		(cv:2d-rotation-matrix (cv:point2d-32f (* (cv:width size) .5) (* (cv:height size) .5))
				       -50.0 0.6 rot-mat)
		(cv:warp-affine src dst rot-mat)
		(cv:show-image "affine" dst))
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return))))))))))



;;; perspective
(defun perspective ()
  (with-gui-thread
    (let ((src-quad (list (cv:point2d-32f 0 0)
			  (cv:point2d-32f 0 0)
			  (cv:point2d-32f 0 0)
			  (cv:point2d-32f 0 0)))
	  (dst-quad (list (cv:point2d-32f 0 0)
			  (cv:point2d-32f 0 0)
			  (cv:point2d-32f 0 0)
			  (cv:point2d-32f 0 0)))
	  (warp-mat (cv:create-mat 3 3 cv:+32fc1+)))
      (cv:with-named-window ("pers")
	(cv:with-captured-camera (vid :width 640 :height 480)
	  (loop
	    (let* ((src (cv:query-frame vid))
		   (size (cv:get-size src)))
	      (cv:with-ipl-images ((dst size cv:+ipl-depth-8u+ 3))
		(setf (cv:x (nth 0 src-quad)) 0.0
		      (cv:y (nth 0 src-quad)) 0.0
		      (cv:x (nth 1 src-quad)) (- (cv:width size) 1.0)
		      (cv:y (nth 1 src-quad)) 0.0
		      (cv:x (nth 2 src-quad)) 0.0
		      (cv:y (nth 2 src-quad)) (- (cv:height size) 1.0)
		      (cv:x (nth 3 src-quad)) (- (cv:width size) 1.0)
		      (cv:y (nth 3 src-quad)) (- (cv:height size) 1.0))
		(setf (cv:x (nth 0 dst-quad)) (* (cv:width size) 0.05)
		      (cv:y (nth 0 dst-quad)) (* (cv:height size) 0.33)
		      (cv:x (nth 1 dst-quad)) (* (cv:width size) 0.9)
		      (cv:y (nth 1 dst-quad)) (* (cv:height size) 0.25)
		      (cv:x (nth 2 dst-quad)) (* (cv:width size) 0.2)
		      (cv:y (nth 2 dst-quad)) (* (cv:height size) 0.7)
		      (cv:x (nth 3 dst-quad)) (* (cv:width size) 0.8)
		      (cv:y (nth 3 dst-quad)) (* (cv:height size) 0.9))
		(cv:get-perspective-transform src-quad dst-quad warp-mat)
		(cv:warp-perspective src dst warp-mat )
		(cv:show-image "pers" dst))
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return))))))))))

;;; log-polar
(defun log-polar ()
  (with-gui-thread
    (cv:with-named-window ("log-polar")
      (let ((m 10.0d0))
	(cv:with-captured-camera (vid :width 640 :height 480)
	  (loop
	    (let* ((frame (cv:query-frame vid))
		   (size (cv:get-size frame)))
	      (cv:with-ipl-images ((dst size cv:+ipl-depth-8u+ 3)
				   (src size cv:+ipl-depth-8u+ 3))
		(cv:log-polar frame dst (cv:point2d-32f (/ (cv:size-width size) 4.0)
							(/ (cv:size-height size) 2.0))
			      m)
		(cv:log-polar dst src (cv:point2d-32f (/ (cv:size-width size) 4.0)
						      (/ (cv:size-height size) 2.0))
			      m (+ cv:+inter-linear+ cv:+warp-inverse-map+))
		(cv:show-image "log-polar" src)))
	    (let ((c (cv:wait-key 33)))
	      (when (= c 27)
		(return)))))))))


;;; dist-transform
(defun dist-transform ()
  (with-gui-thread
    (cv:with-named-window ("dist-transform")
       (cv:with-captured-camera (vid :width 640 :height 480)
	 (loop
	   (let* ((frame (cv:query-frame vid)))
	     (cv:with-ipl-images ((src (cv:get-size frame) cv:+ipl-depth-8u+ 1)
				  (dst (cv:get-size frame) cv:+ipl-depth-8u+ 1)
				  (final (cv:get-size frame) cv:+ipl-depth-32f+ 1))
	       (cv:cvt-color frame src cv:+bgr-2-gray+)
	       (cv:canny src dst 100.0d0 200.0d0 3)
	       (cv:threshold dst dst 1.0 255 cv:+thresh-binary-inv+)
	       (cv:dist-transform dst final cv:+dist-l2+ 3 (list 10.2 20.2))
	       (cv:show-image "dist-transform" final))
	     (let ((c (cv:wait-key 33)))
	       (when (= c 27)
		 (return)))))))))


;;; eqaulize-hist
(defun equalize-hist ()
  (with-gui-thread
    (cv:with-named-window ("equalize-hist")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid))
		 (size (cv:get-size frame)))
	    (cv:with-ipl-images ((src size cv:+ipl-depth-8u+ 1)
				 (dst size cv:+ipl-depth-8u+ 1))
	      (cv:cvt-color frame src cv:+bgr-2-gray+)
	      (cv:equalize-hist src dst)
	      (cv:show-image "equalize-hist" dst)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))


;; ;;; histogram
;; (defun histogram ()
;;   (with-gui-thread 
;;     (cv:with-named-window ("histogram")
;;       (cv:with-captured-camera (capture :width 640 :height 480)
;; 	(let* ((h-bins 30)
;; 	       (s-bins 32)
;; 	       (hist (cv:create-hist 2 (list h-bins s-bins) :cv-hist-array (list (list 0 180) (list 0 255)) 1)))
;; 	  (loop
;; 	    (let* ((frame (cv:query-frame capture))
;; 		   (size (cv:get-size frame)))
;; 	      (cv:with-ipl-images ((hsv size cv:+ipl-depth-8u+ 3)
;; 				   (h-plane size cv:+ipl-depth-8u+ 1)
;; 				   (s-plane size cv:+ipl-depth-8u+ 1)
;; 				   (v-plane size cv:+ipl-depth-8u+ 1)
;; 				   (hist-img (cv:size (* h-bins 10) (* s-bins 10)) cv:+ipl-depth-8u+ 3))
;; 		(cv:cvt-color frame hsv :cv-bgr-2-hsv)
;; 		(cv:cvt-pix-to-plane hsv h-plane s-plane v-plane nil)
;; 		(cv:calc-hist (list h-plane s-plane) hist 0)
;; 		(cv:normalize-hist hist (* 255 20))
;; 		(destructuring-bind (a max-value b c)
;; 		    (cv:get-min-max-hist-value hist nil t)
;; 		  (declare (ignore a b c))
;; 		  (cv:zero hist-img)
;; 		  (dotimes (h h-bins)
;; 		    (dotimes (s s-bins)
;; 		      (let* ((bin-val (cv:query-hist-value-2d hist h s))
;; 			     (intensity (cv:round (/ (* bin-val 255) max-value))))
;; 			(cv:rectangle hist-img
;; 				      (cv:point (* h 10) (* s 10))
;; 				      (cv:point (- (* (+ h 1) 10) 1) (- (* (+ s 1) 10) 1))
;; 				      (cv:scalar intensity intensity intensity) -1))))))
;; 	      (cv:show-image "histogram" hist-img)
;; 	      (let ((c (cv:wait-key 33)))
;; 		(when (= c 27)
;; 		  (return))))))))))



;;; match-template
(defun match-template ()
  (with-gui-thread
    (let* ((temp (cv:load-image (concatenate 'string *examples-dir* "room7_temp.png")
				cv:+load-image-color+)))
      (cv:with-named-window ("match")
	(cv:with-named-window ("cam")
	  (cv:with-captured-camera (vid :width 640 :height 480)
	    (loop
	      (let* ((frame (cv:query-frame vid))
		     (size (cv:get-size frame)))
		(cv:with-ipl-images ((src size cv:+ipl-depth-8u+ 3)
				     (result (cv:size (+ (- (cv:width size) (cv:width (cv:get-size temp))) 1)
						      (+ (- (cv:height size) (cv:height (cv:get-size temp))) 1))
					     cv:+ipl-depth-32f+ 1))
		  (cv:match-template frame  temp result cv:+tm-sqdiff-normed+)
		  (cv:normalize result result 1.0d0 0.0d0 cv:+minmax+)
		  (cv:show-image "match" result)
		  (cv:show-image "cam" frame)))
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return))))))))))



;;; find contours
(defun find-contours-1 ()
  (with-gui-thread
    (cv:with-named-window ("find-contours-1")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(let* ((mem-storage (cv:create-mem-storage)))
	  (loop
	    (let* ((frame (cv:query-frame vid)))
	      (cv:with-ipl-images ((result (cv:get-size frame) cv:+ipl-depth-8u+ 1)
				   (dst (cv:get-size frame) cv:+ipl-depth-8u+ 3))
		(cv:cvt-color frame result cv:+bgr-2-gray+)
		(cv:threshold result result 100 255 cv:+thresh-binary+)
		(let ((contours (cv:find-contours result mem-storage 128 cv:+retr-tree+)))
		  (cv:set-zero result)
		  (cv:set-zero dst)
		  (cv:cvt-color result dst cv:+gray-2-bgr+)
		  (cv:draw-contours dst contours
				    (cv:scalar 255.0d0 255.0d0 0.0d0 255.0d0) 
				    (cv:scalar 128.0d0 100.0d0 200.0d0 1000.0d0)
				    100)
		  (cv:show-image "find-contours-1" dst)
		  (cv:clear-mem-storage mem-storage))))
	    (let ((c (cv:wait-key 33)))
	      (when (= c 27)
		(cv:release-mem-storage mem-storage)
		(return)))))))))

(defun find-contours-2 ()
  (with-gui-thread
    (cv:with-named-window ("find-contours-2")
      (cv:with-captured-camera (vid :width 640 :height 480)
	(let* ((mem-storage (cv:create-mem-storage)))
	  (loop
	    (let* ((frame (cv:query-frame vid))
		   (size (cv:get-size frame)))
	      (cv:with-ipl-images ((result size cv:+ipl-depth-8u+ 1)
				   (dst size cv:+ipl-depth-8u+ 3))
		(cv:cvt-color frame result cv:+bgr-2-gray+)
		(cv:threshold result result 100.0d0 255.0d0 cv:+thresh-binary+)
		(let ((scanner (cv:start-find-contours result mem-storage 128 cv:+retr-tree+)))
		  (cv:cvt-color result dst cv:+gray-2-bgr+)
		  (loop
		    (let* ((contour (cv:find-next-contour scanner)))
		      (unless contour (return))
		      (cv:draw-contours dst contour
			    		      (cv:scalar 255.0d0 255.0d0 0.0d0 255.0d0)
					      (cv:scalar 0.0d0 0.0d0 255.0d0 100.0d0)
			    		      100)))
		  (cv:show-image "find-contours-2" dst)
		  (cv:clear-mem-storage mem-storage))))
	    (let ((c (cv:wait-key 33)))
	      (when (= c 27)
		(cv:release-mem-storage mem-storage)
		(return)))))))))


;;; find face
(defun face-detect ()
  (with-gui-thread
    (let* ((mem-storage (cv:create-mem-storage))
	   (cascade (cv:load-haar-classifier-cascade (concatenate 'string *examples-dir* "haarcascade_frontalface_default.xml")
						     (cv:size 64 128))))
      (cv:with-named-window ("Face Detect")
	(cv:with-captured-camera (vid :width 640 :height 480)
	  (loop
	    (let* ((frame (cv:query-frame vid))
		   (size (cv:get-size frame)))
	      (cv:with-ipl-images ((result size cv:+ipl-depth-8u+ 1))
		  (cv:clear-mem-storage mem-storage)
		  (let ((seq (cv:haar-detect-objects frame cascade mem-storage 1.1d0 2
						     cv:+haar-do-canny-pruning+
						     (cv:size 40 40) (cv:size 0 0))))
		    (dotimes (i (cv:get-total seq))
		      (let ((r (cv:get-seq-elem-rect seq i)))
			(cv:ellipse frame (cv:point (+ (cv:x r) (* 0.5 (cv:width r)))
						    (+ (cv:y r) (* 0.5 (cv:height r))))
				    (cv:size (* 0.5 (cv:width r))
					     (* 0.5 (cv:height r)))
				    0.0 0.0 360.0
				    (cv:scalar 255.0d0 255.0d0 0.0d0 255.0d0) 3)))
		    (cv:show-image "Face Detect" frame)
		    (let ((c (cv:wait-key 33)))
		      (when (= c 27)
			(return)))))))
	  (cv:release-mem-storage mem-storage))))))


;; ;;; cvblob
;; (ql:quickload :cvblob)

;; (defun red-object-tracking (&optional (cam -1))
;;   (with-call-gui-thread
;;     (let* ((morph-kernel (cv:create-structuring-element-ex 5 5 1 1 :cv-shape-rect)))
;;       (cv:with-named-window ("red_object_tracking")
;; 	(cv:with-captured-camera (vid cam :width 640 :height 480)
;; 	  (cv:with-cv-tracks (tracks)
;; 	    (let* ((img (cv:query-frame vid))
;; 		   (frame (cv:create-image (cv:size (cv:width img) (cv:height img)) cv:+ipl-depth-8u+ 3)))
;; 	      (loop
;; 		(let ((img (cv:query-frame vid)))
;; 		  (cv:convert-scale img frame 1 0)
;; 		  (cv:with-cv-blobs (blobs)
;; 		    (let ((width (cv:width img))
;; 			  (height (cv:height img)))
;; 		      (cv:with-ipl-image ((segmentated (cv:size width height) cv:+ipl-depth-8u+ 1)
;; 					  (label-img (cv:size width height) :ipl-depth-label 1))
;; 			(let ((frame-data (cv:image-data frame))
;; 			      (segmentated-data (cv:image-data segmentated)))
;; 			  (dotimes (j height)
;; 			    (dotimes (i width)
;; 			      (let* ((b (/ (cffi:mem-aref frame-data :unsigned-char (+ (+ (* 3 i) (* 3 j width)) 0)) 255.0))
;; 				     (g (/ (cffi:mem-aref frame-data :unsigned-char (+ (+ (* 3 i) (* 3 j width)) 1)) 255.0))
;; 				     (r (/ (cffi:mem-aref frame-data :unsigned-char (+ (+ (* 3 i) (* 3 j width)) 2)) 255.0))
;; 				     (f (* 255 (if (cl:and (> r (+ 0.2 g)) (> r (+ 0.2 b))) 1 0))))
;; 				(setf (cffi:mem-aref segmentated-data :unsigned-char (+ i (* j width))) f)))))
;; 			(cv:morphology-ex segmentated segmentated nil morph-kernel :cv-mop-open 1)
;; 			(cv:label segmentated label-img blobs)
;; 			(cv:filter-by-area blobs 500 1000000)
;; 			(cv:render-blobs label-img blobs frame frame :cv-blob-render-bounding-box)
;; 			(cv:update-tracks blobs tracks 200. 5)
;; 			(cv:render-tracks tracks frame frame '(:cv-track-render-id
;; 							       :cv-track-render-bounding-box))
;; 			(cv:show-image "red_object_tracking" frame)
;; 			(cv:release-blobs blobs)))))
;; 		(let ((c (cv:wait-key 33)))
;; 		  (when (= c 27)
;; 		    (return))))
;; 	      (cv:release-structuring-element morph-kernel)
;; 	      (cv:release-image frame))))))))
