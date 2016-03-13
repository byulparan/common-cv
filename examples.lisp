
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :common-cv)
  (ql:quickload :bordeaux-threads))

(in-package :cl-user)

(defparameter *examples-dir*
  (su:cat (su:full-pathname (asdf/system:system-source-directory :common-cv)) "examples-resources/"))

(defmacro with-masked-float-traps (&body body)
  "Wraps BODY in code which masks float traps.
   This is needed in SBCL on OSX because native code often
   generate :inexact traps and land you in the debugger.
   For non SBCL this wraps body in a progn."
  #+sbcl `(sb-int:with-float-traps-masked (:invalid)
	    ,@body)
  #-sbcl `(progn ,@body))

(defmacro with-call-gui-thread (&body body)
  #+darwin
  `(bt:interrupt-thread
   #+ccl ccl::*initial-process*
   #+sbcl (sb-thread:main-thread)
   #+ecl (find 'si:top-level (bt:all-threads) :key #'mp:process-name)
   (lambda ()
     (with-masked-float-traps
       ,@body)))
  #-darwin
  `(progn ,@body))

(defun adaptive-threshold (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("adaptive-threshold")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((img (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1))
	      (cv:cvt-color frame img :cv-bgr-2-gray)
	      (cv:adaptive-threshold img img 240 :cv-adaptive-thresh-mean-c :cv-thresh-binary)
	      (cv:show-image "adaptive-threshold" img)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))

(defun filter-mat (&optional (cam -1))
  (let* ((filter-5x5 (mapcar #!(/ %1 256.0d0) (list 100 140 116 114 111
						    112 118 12 8 2
						    6 24 36 24 6
						    2 8 12 8 2
						    1 4 6 3 1)))
	 (mat (cv:create-mat 5 5 :cv-64fc1))
	 (data (cv:data mat)))
    (dotimes (i 25)
      (setf (cffi:mem-aref data :double i) (nth i filter-5x5)))
    (with-call-gui-thread
      (cv:with-named-window ("filter-mat")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (loop
	    (let* ((frame (cv:query-frame vid)))
	      (cv:with-ipl-image ((img (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 3))
		(cv:filter-2d frame img mat (cv:point 3 3))
		(cv:show-image "filter-mat" img)))
	    (let ((c (cv:wait-key 33)))
	      (when (= c 27)
		(return)))))))))


;;; copy-make-border
(defun make-border (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("MakeBorder")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((dst (cv:size (+ (cv:width frame) (* 2 2))
					      (+ (cv:height frame) (* 2 2))) :ipl-depth-8u 3))
	      (cv:show-image "MakeBorder" (progn
					    (cv:copy-make-border frame dst (cv:point 2 2)
								 :ipl-border-constant
								 (cv:scalar 200))
					    dst))))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))

;;; sobel
(defun sobel (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("Sobel")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((cvt (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(src (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(tmp (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-16s 1))
	      (cv:cvt-color frame cvt :cv-bgr-2-gray)
	      (cv:show-image "Sobel" (progn
					 (cv:sobel cvt tmp 1 0 :cv-scharr)
					 (cv:convert-scale-abs tmp src)
					 src))))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))


;;; laplace
(defun laplace (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("laplace")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((cvt (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(src (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(tmp (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-16s 1))
	      (cv:cvt-color frame cvt :cv-bgr-2-gray)
	      (cv:show-image "laplace" (progn
					 (cv:laplace cvt tmp)
					 (cv:convert-scale-abs tmp src)
					 src))))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))



;;; canny
(defun canny (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("canny")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(loop
	  (let* ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((src (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(dst (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1))
	      (cv:cvt-color frame src :cv-bgr-2-gray)
	      (cv:canny src dst 10 50)
	      (cv:show-image "canny" dst)))
	  (let ((c (cv:wait-key 33)))
	    (when (= c 27)
	      (return))))))))


;;; affine
(defun affine (&optional (cam -1))
  (with-call-gui-thread
    (let ((src-tri (list (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0)))
	  (dst-tri (list (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0)))
	  (rot-mat (cv:create-mat 2 3 :cv-32fc1))
	  (warp-mat (cv:create-mat 2 3 :cv-32fc1))
	  (dst nil))
      (cv:with-named-window ("affine")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
 	  (loop
	    (let ((src (cv:query-frame vid)))
	      (unless dst
		(setf dst (cv:create-image (cv:size (cv:width src) (cv:height src)) :ipl-depth-8u 3))
		(setf (cv:x (nth 0 src-tri)) 0.0
		      (cv:y (nth 0 src-tri)) 0.0
		      (cv:x (nth 1 src-tri)) (- (cv:width src) 1.0)
		      (cv:y (nth 1 src-tri)) 0.0
		      (cv:x (nth 2 src-tri)) 0.0
		      (cv:y (nth 2 src-tri)) (- (cv:height src) 1.0))
		(setf (cv:x (nth 0 dst-tri)) (* 0.0 (cv:width src))
		      (cv:y (nth 0 dst-tri)) (* (cv:height src) 0.33)
		      (cv:x (nth 1 dst-tri)) (* (cv:width src) 0.85)
		      (cv:y (nth 1 dst-tri)) (* (cv:height src) 0.25)
		      (cv:x (nth 2 dst-tri)) (* (cv:width src) 0.15)
		      (cv:y (nth 2 dst-tri)) (* (cv:height src) 0.7)))
	      (cv:get-affine-transform src-tri dst-tri warp-mat)
	      (cv:warp-affine src dst warp-mat '(:cv-inter-linear :cv-warp-fill-outliers) (cv:scalar 0 0 100))
	      (cv:copy dst src)
	      (cv:2d-rotation-matrix (cv:point-2d-32f (/ (cv:width src) 2) (/ (cv:height src) 2)) -50.0 0.6 rot-mat)
	      (cv:warp-affine src dst rot-mat '(:cv-inter-linear :cv-warp-fill-outliers) (cv:scalar 100))
	      (cv:show-image "affine" dst)
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return))))))))))



;;; perspective
(defun perspective (&optional (cam -1))
  (with-call-gui-thread
    (let ((src-quad (list (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0)))
	  (dst-quad (list (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0) (cv:point-2d-32f 0 0)))
	  (warp-mat (cv:create-mat 3 3 :cv-32fc1))
	  (dst nil))
      (cv:with-named-window ("pers")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (loop
	    (let ((src (cv:query-frame vid)))
	      (unless dst
		(setf dst (cv:create-image (cv:size (cv:width src) (cv:height src)) :ipl-depth-8u 3))
		(setf (cv:x (nth 0 src-quad)) 0.0
		      (cv:y (nth 0 src-quad)) 0.0
		      (cv:x (nth 1 src-quad)) (- (cv:width src) 1.0)
		      (cv:y (nth 1 src-quad)) 0.0
		      (cv:x (nth 2 src-quad)) 0.0
		      (cv:y (nth 2 src-quad)) (- (cv:height src) 1.0)
		      (cv:x (nth 3 src-quad)) (- (cv:width src) 1.0)
		      (cv:y (nth 3 src-quad)) (- (cv:height src) 1.0))
		(setf (cv:x (nth 0 dst-quad)) (* (cv:width src) 0.05)
		      (cv:y (nth 0 dst-quad)) (* (cv:height src) 0.33)
		      (cv:x (nth 1 dst-quad)) (* (cv:width src) 0.9)
		      (cv:y (nth 1 dst-quad)) (* (cv:height src) 0.25)
		      (cv:x (nth 2 dst-quad)) (* (cv:width src) 0.2)
		      (cv:y (nth 2 dst-quad)) (* (cv:height src) 0.7)
		      (cv:x (nth 3 dst-quad)) (* (cv:width src) 0.8)
		      (cv:y (nth 3 dst-quad)) (* (cv:height src) 0.9)))
	      (cv:get-perspective-transform src-quad dst-quad warp-mat)
	      (cv:warp-perspective src dst warp-mat)
	      (cv:show-image "pers" dst)
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return))))))))))

;;; log-polar
(defun log-polar (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("log-polar")
      (let ((m 10))
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (loop
	    (let* ((src (cv:query-frame vid)))
	      (cv:with-ipl-image ((dst (cv:size (cv:width src) (cv:height src)) :ipl-depth-8u 3)
				  (src2 (cv:size (cv:width src) (cv:height src)) :ipl-depth-8u 3))
		(cv:log-polar src dst (cv:point-2d-32f (/ (cv:width src) 4.0) (/ (cv:height src) 2.0)) m)
		(cv:log-polar dst src2 (cv:point-2d-32f (/ (cv:width src) 4.0) (/ (cv:height src) 2.0)) m
			      '(:cv-inter-linear :cv-warp-inverse-map))
		(cv:show-image "log-polar" src2)))
	    (let ((c (cv:wait-key 33)))
	      (when (= c 27)
		(return)))))))))



;;; dist-transform
(defun dist-transform (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("dist-transform")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(let ((frame (cv:query-frame vid)))
	  (cv:with-ipl-image ((src (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
			      (dst (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
			      (final (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-32f 1))
	    (loop
	      (let* ((frame (cv:query-frame vid)))
		(cv:cvt-color frame src :cv-bgr-2-gray)
		(cv:canny src dst 100 200)
		(cv:threshold dst dst 1 255 :cv-thresh-binary-inv )
		(cv:dist-transform dst final :cv-dist-user 3 '(1 1.5))
		(cv:show-image "dist-transform" final))
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return))))))))))


;;; eqaulize-hist
(defun equalize-hist (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("equalize-hist")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(let ((frame (cv:query-frame vid)))
	  (cv:with-ipl-image ((src (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
			      (dst (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1))
	    (loop
	      (let* ((frame (cv:query-frame vid)))
		(cv:cvt-color frame src :cv-bgr-2-gray)
		(cv:equalize-hist src dst)
		(cv:show-image "equalize-hist" dst))
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return))))))))))


;;; histogram
(defun histogram (&optional (cam -1))
  (with-call-gui-thread
    (cv:with-named-window ("histogram")
      (cv:with-captured-camera (vid cam :width 640 :height 480)
	(let* ((h-bins 30)
	       (s-bins 32)
	       (hist (cv:create-hist 2 (list h-bins s-bins) :cv-hist-array (list (list 0 180) (list 0 255)) 1))
	       (frame (cv:query-frame vid)))
	  (cv:with-ipl-image ((hsv (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 3)
			      (h-plane (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
			      (s-plane (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
			      (v-plane (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
			      (hist-img (cv:size (* h-bins 10) (* s-bins 10)) :ipl-depth-8u 3))
	    (loop
	      (let* ((frame (cv:query-frame vid)))
		(cv:cvt-color frame hsv :cv-bgr-2-hsv)
		(cv::reset-info-ipl-image hsv)
		(cv:cvt-pix-to-plane hsv h-plane s-plane v-plane nil)
		(cv:calc-hist (list h-plane s-plane) hist 0)
		(cv:normalize-hist hist (* 255 20))
		(destructuring-bind (a max-value b c)
		      (cv:get-min-max-hist-value hist nil t)
		  (declare (ignore a b c))
		(cv:zero hist-img)
		(dotimes (h h-bins)
		  (dotimes (s s-bins)
		    (let* ((bin-val (cv:query-hist-value-2d hist h s))
			   (intensity (cv:round (/ (* bin-val 255) max-value))))
		      (cv:rectangle hist-img
				    (cv:point (* h 10) (* s 10))
				    (cv:point (- (* (+ h 1) 10) 1) (- (* (+ s 1) 10) 1))
				    (cv:scalar intensity intensity intensity) -1)))))
	      (cv:show-image "histogram" hist-img)
	      (let ((c (cv:wait-key 33)))
		(when (= c 27)
		  (return)))))))))))


;;; back-project
(defun back-project ()
  (with-call-gui-thread
    (cv:with-named-window ("calcbackproject")
      (let* ((h-bins 30)
	     (s-bins 32)
	     (hist (cv:create-hist 2 (list h-bins s-bins) :cv-hist-array (list (list 0 180) (list 0 255)) 1))
	     (src (cv:load-image (su:cat *examples-dir* "room7_temp.png") :cv-load-image-color)))
	(cv:with-ipl-image ((h-plane (cv:size (cv:width src) (cv:height src)) :ipl-depth-8u 1)
			    (s-plane (cv:size (cv:width src) (cv:height src)) :ipl-depth-8u 1)
			    (v-plane (cv:size (cv:width src) (cv:height src)) :ipl-depth-8u 1))
	  (cv:cvt-pix-to-plane src h-plane s-plane v-plane nil)
	  (cv:calc-hist (list h-plane s-plane) hist)
	  (cv:normalize-hist hist 1)
	  (let ((patch-size (cv:size (cv:width src) (cv:height src))))
	    (let ((dst (cv:load-image (su:cat *examples-dir* "room7.png") :cv-load-image-color)))
	      (cv:with-ipl-image ((H-DST (cv:size (cv:width dst) (cv:height dst)) :ipl-depth-8u 1)
				  (S-DST (cv:SIZE (cv:width dst) (cv:height dst)) :ipl-depth-8u 1)
				  (result (cv:size (+ (- (cv:width h-dst) (cv:width patch-size)) 1)
						   (+ (- (cv:height h-dst) (cv:height patch-size)) 1))
					  :ipl-depth-32f 1))
		(cv:cvt-pix-to-plane dst h-dst s-dst nil nil)
		(cv:calc-back-project-patch (list h-dst s-dst) result patch-size hist
					    :cv-comp-correl 1)
		(cv:show-image "calcbackproject" result)
		(cv:wait-key 0)))))))))



;;; match-template
(defun match-template (&optional (cam -1))
  (with-call-gui-thread
    (let* ((temp (cv:load-image (su:cat *examples-dir* "room7_temp.png") :cv-load-image-color)))
      (cv:with-named-window ("match")
	(cv:with-named-window ("cam")
	  (cv:with-captured-camera (vid cam :width 640 :height 480)
	    (let* ((frame (cv:query-frame vid))
		   (src (cv:create-image (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 3))
		   (result (cv:create-image (cv:size (+ (- (cv:width src) (cv:width temp)) 1)
						     (+ (- (cv:height src) (cv:height temp)) 1))
					    :ipl-depth-32f 1)))
	      (loop
		(let ((vid-img (cv:query-frame vid)))
		  (cv:match-template vid-img  temp result :cv-tm-sqdiff-normed)
		  (cv:normalize result result 1 0 :cv-minmax)
		  (cv:show-image "match" result)
		  (cv:show-image "cam" vid-img)
		  (let ((c (cv:wait-key 33)))
		    (when (= c 27)
		      (return))))))))))))



;;; find contours
(defun find-contours-1 (&optional (cam -1))
  (with-call-gui-thread
    (let* ((mem-storage (cv:create-mem-storage)))
      (cv:with-named-window ("find-contours-1")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (let ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((result (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1))
	      (loop
		(cv:cvt-color (cv:query-frame vid) result :cv-bgr-2-gray)
		(cv:threshold result result 100 255 :cv-thresh-binary)
		(let ((contours (cv:find-contours result mem-storage 128 :cv-retr-tree)))
		  (cv:zero result)
		  (cv:draw-contours result contours (cv:scalar-all 255)  (cv:scalar-all 128) 100)
		  (cv:show-image "find-contours-1" result)
		  (cv:clear-mem-storage mem-storage))
		(let ((c (cv:wait-key 33)))
		  (when (= c 27)
		    (return))))
	      (cv:release-mem-storage mem-storage))))))))

;;; in Linux... this function crash! I don't know why...
(defun find-contours-2 (&optional (cam -1))
  (with-call-gui-thread
    (let* ((mem-storage (cv:create-mem-storage)))
      (cv:with-named-window ("find-contours-2")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (let ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((result (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(ret (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 3))
	      (loop
		(cv:cvt-color (cv:query-frame vid) result :cv-bgr-2-gray)
		(cv:threshold result result 100 255 :cv-thresh-binary)
		(let ((contours (cv:find-contours result mem-storage 128 :cv-retr-tree)))
		  (loop while contours do
		    (cv:cvt-color result ret :cv-gray-2-bgr)
		    (cv:draw-contours ret contours (cv:scalar 0 0 255)  (cv:scalar 255 0 0) 0 2 8)
		    (setf contours (cv:next contours)))
		  (cv:show-image "find-contours-2" ret)
		  (cv:clear-mem-storage mem-storage))
		(let ((c (cv:wait-key 33)))
		  (when (= c 27)
		    (return))))
	      (cv:release-mem-storage mem-storage))))))))



;;; find face
(defun face-detect (&optional (cam -1))
  (with-call-gui-thread
    (let* ((mem-storage (cv:create-mem-storage))
	   (cascade (cv:load-haar-cascade (su:cat *examples-dir* "haarcascade_frontalface_default.xml"))))
      (cv:with-named-window ("Face Detect")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (let ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((result (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1))
	      (loop
		(let ((frame (cv:query-frame vid)))
		  (cv:clear-mem-storage mem-storage)
		  (let ((seq (cv:haar-detect-object frame cascade mem-storage 1.1 2 :cv-haar-do-canny-pruning
						    (cv:size 40 40))))
		    (dotimes (i (cv:total seq))
		      (let ((r (cv:get-seq-elem-rect seq i)))
			(cv:ellipse frame (cv:point (+ (cv:x r) (* 0.5 (cv:width r)))
						    (+ (cv:y r) (* 0.5 (cv:height r))))
				    (cv:size (* 0.5 (cv:width r)) (* 0.5 (cv:height r))) 0  0 360
				    (cv:scalar 255) 3)))
		    (cv:show-image "Face Detect" frame)))
		(let ((c (cv:wait-key 33)))
		  (when (= c 27)
		    (return))))
	      (cv:release-mem-storage mem-storage))))))))

(defun detect-lines (&optional (cam -1))
  (with-call-gui-thread
    (let* ((mem-storage (cv:create-mem-storage)))
      (cv:with-named-window ("detect-lines")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (let ((frame (cv:query-frame vid)))
	    (cv:with-ipl-image ((src (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(canned (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 1)
				(result (cv:size (cv:width frame) (cv:height frame)) :ipl-depth-8u 3))
	      (loop
		 (cv:cvt-color (cv:query-frame vid) src :cv-bgr-2-gray)
		 (cv:copy (cv:query-frame vid) result)
		 (cv:canny src canned 90 110)
		 (loop with lines = (cv:hough-lines canned mem-storage 1 (/ pi 180) 110)
		    for pt = (cv:pop-seq-point-2d-32f lines)
		    for rho = (cv:x pt) and theta = (cv:y pt)
		    for a = (cos theta) and b = (sin theta)
		    for x0 = (* a rho) and y0 = (* b rho)
		    repeat (cv:total lines) do
		      (cv:line result (cv:point (+ x0 (* 1000 (- b))) (+ y0 (* 1000 a)))
			       (cv:point (- x0 (* 1000 (- b))) (- y0 (* 1000 a)))
			       (cv:scalar 255 0 0) 1))
		 (cv:show-image "detect-lines" result)
		 (let ((c (cv:wait-key 33)))
		   (when (= c 27)
		     (return))))
	      (cv:release-mem-storage mem-storage))))))))

;;; cvblob
(ql:quickload :cvblob)

(defun red-object-tracking (&optional (cam -1))
  (with-call-gui-thread
    (let* ((morph-kernel (cv:create-structuring-element-ex 5 5 1 1 :cv-shape-rect)))
      (cv:with-named-window ("red_object_tracking")
	(cv:with-captured-camera (vid cam :width 640 :height 480)
	  (cv:with-cv-tracks (tracks)
	    (let* ((img (cv:query-frame vid))
		   (frame (cv:create-image (cv:size (cv:width img) (cv:height img)) :ipl-depth-8u 3)))
	      (loop
		(let ((img (cv:query-frame vid)))
		  (cv:convert-scale img frame 1 0)
		  (cv:with-cv-blobs (blobs)
		    (let ((width (cv:width img))
			  (height (cv:height img)))
		      (cv:with-ipl-image ((segmentated (cv:size width height) :ipl-depth-8u 1)
					  (label-img (cv:size width height) :ipl-depth-label 1))
			(let ((frame-data (cv:image-data frame))
			      (segmentated-data (cv:image-data segmentated)))
			  (dotimes (j height)
			    (dotimes (i width)
			      (let* ((b (/ (cffi:mem-aref frame-data :unsigned-char (+ (+ (* 3 i) (* 3 j width)) 0)) 255.0))
				     (g (/ (cffi:mem-aref frame-data :unsigned-char (+ (+ (* 3 i) (* 3 j width)) 1)) 255.0))
				     (r (/ (cffi:mem-aref frame-data :unsigned-char (+ (+ (* 3 i) (* 3 j width)) 2)) 255.0))
				     (f (* 255 (if (cl:and (> r (+ 0.2 g)) (> r (+ 0.2 b))) 1 0))))
				(setf (cffi:mem-aref segmentated-data :unsigned-char (+ i (* j width))) f)))))
			(cv:morphology-ex segmentated segmentated nil morph-kernel :cv-mop-open 1)
			(cv:label segmentated label-img blobs)
			(cv:filter-by-area blobs 500 1000000)
			(cv:render-blobs label-img blobs frame frame :cv-blob-render-bounding-box)
			(cv:update-tracks blobs tracks 200. 5)
			(cv:render-tracks tracks frame frame '(:cv-track-render-id
							       :cv-track-render-bounding-box))
			(cv:show-image "red_object_tracking" frame)
			(cv:release-blobs blobs)))))
		(let ((c (cv:wait-key 33)))
		  (when (= c 27)
		    (return))))
	      (cv:release-structuring-element morph-kernel)
	      (cv:release-image frame))))))))

