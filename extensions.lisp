(in-package :cv)

;;;
(defgeneric x (cv-struct))
(defgeneric y (cv-struct))
(defgeneric width (cv-struct))
(defgeneric height (cv-struct))

(defmethod x ((cv-struct point))
  (point-x cv-struct))

(defmethod (setf x) (value (cv-struct point))
  (setf (point-x cv-struct) (floor value)))

(defmethod y ((cv-struct point))
  (point-y cv-struct))

(defmethod (setf y) (value (cv-struct point))
  (setf (point-y cv-struct) (floor value)))


(defmethod x ((cv-struct point2d-32f))
  (point2d-32f-x cv-struct))

(defmethod (setf x) (value (cv-struct point2d-32f))
  (setf (point2d-32f-x cv-struct) (coerce value 'single-float)))

(defmethod y ((cv-struct point2d-32f))
  (point2d-32f-y cv-struct))

(defmethod (setf y) (value (cv-struct point2d-32f))
  (setf (point2d-32f-y cv-struct) (coerce value 'single-float)))



(defmethod x ((cv-struct point2d-64f))
  (point2d-64f-x cv-struct))

(defmethod (setf x) (value (cv-struct point2d-64f))
  (setf (point2d-64f-x cv-struct) (coerce value 'double-float)))

(defmethod y ((cv-struct point2d-64f))
  (point2d-64f-y cv-struct))

(defmethod (setf y) (value (cv-struct point2d-64f))
  (setf (point2d-64f-y cv-struct) (coerce value 'double-float)))



(defmethod width ((cv-struct size))
  (size-width cv-struct))

(defmethod (setf width) (value (cv-struct size))
  (setf (size-width cv-struct) (floor value)))

(defmethod height ((cv-struct size))
  (size-height cv-struct))

(defmethod (setf height) (value (cv-struct size))
  (setf (size-height cv-struct) (floor value)))


(defmethod width ((cv-struct size2d-32f))
  (size2d-32f-width cv-struct))

(defmethod (setf width) (value (cv-struct size2d-32f))
  (setf (size2d-32f-width cv-struct) (coerce value 'single-float)))

(defmethod height ((cv-struct size2d-32f))
  (size2d-32f-height cv-struct))

(defmethod (setf height) (value (cv-struct size2d-32f))
  (setf (size2d-32f-height cv-struct) (coerce value 'single-float)))


(defmethod x ((cv-struct rect))
  (rect-x cv-struct))

(defmethod (setf x) (value (cv-struct rect))
  (setf (rect-x cv-struct) (floor value)))

(defmethod y ((cv-struct rect))
  (rect-y cv-struct))

(defmethod (setf y) (value (cv-struct rect))
  (setf (rect-y cv-struct) (floor value)))

(defmethod width ((cv-struct rect))
  (rect-width cv-struct))

(defmethod (setf width) (value (cv-struct rect))
  (setf (rect-width cv-struct) (floor value)))

(defmethod height ((cv-struct rect))
  (rect-height cv-struct))

(defmethod (setf height) (value (cv-struct rect))
  (setf (rect-height cv-struct) (floor value)))




(defun mat-data (mat)
  (cffi:with-foreign-slots ((data) mat (:struct mat))
    data))

(defun get-total (seq)
  (cffi:with-foreign-slots ((total) seq (:struct seq))
    total))

(defun get-seq-elem-rect (seq i)
  (cffi:with-foreign-slots ((x y width height) (get-seq-elem seq i) (:struct rect))
    (rect x y width height)))

(defmacro with-named-window ((name &optional (options cv:+window-autosize+)) &body body)
  `(unwind-protect (progn
		     (cv:named-window ,name ,options)
		     ,@body)
     (cv:destroy-window ,name)
     (cv:wait-key 2)))

(defmacro with-captured-camera ((capture &key (idx 0) (width 640) (height 480)) &body body)
  `(let* ((,capture (cv:create-camera-capture ,idx)))
     (cv:set-capture-property ,capture cv:+cap-prop-frame-width+ (coerce ,width 'double-float))
     (cv:set-capture-property ,capture cv:+cap-prop-frame-height+ (coerce ,height 'double-float))
     (unwind-protect (progn ,@body)
       (cv:release-capture ,capture))))

(defmacro with-ipl-images ((&rest images) &body body)
  `(let* (,@(loop for img in images
		 collect `(,(car img) (cv:create-image ,@(cdr img)))))
     (unwind-protect (progn ,@body)
       ,@(loop for img in images
	      collect `(cv:release-image ,(car img))))))


(defmacro with-init-font ((font-var &key (font-face cv:+font-hershey-plain+)
				      (scale 1) (shear 0) (thickness 1) (line-type 16))
			  &body body)
  `(cffi:with-foreign-objects ((,font-var '(:struct font)))
     (cffi:with-foreign-slots ((font-face hscale vscale shear thickness line-type) ,font-var
			       (:struct font))
       (setf font-face ,font-face
	     hscale (coerce ,scale 'single-float)
	     vscale (coerce ,scale 'single-float)
	     shear (coerce ,shear 'single-float)
	     thickness (floor ,thickness)
	     line-type (floor ,line-type)))
     ,@body))


(defvar *trackbar-callback-table* (make-hash-table))

(cffi:defcallback trackbar-callback2 :void ((pos :int)
					    (userdata :pointer))
  (let* ((id (cffi:pointer-address userdata))
	 (handle (gethash id *trackbar-callback-table*)))
    (when handle
      (funcall handle pos))))


(let* ((id 0))
  (defun create-trackbar (trackbar-name window-name init-value count callback)
    (setf (gethash id *trackbar-callback-table*) callback)
    (cffi:with-foreign-objects ((init :int))
      (setf (cffi:mem-ref init :int) (floor init-value))
      (%create-trackbar2 trackbar-name window-name init count (cffi:callback trackbar-callback2) (cffi:make-pointer id)))
    (incf id)
    (values)))



