(in-package :cv)

;;;
(defun mat-data (mat)
  (cffi:with-foreign-slots ((data) mat (:struct mat))
    data))

(defun get-total (seq)
  (cffi:with-foreign-slots ((total) seq (:struct seq))
    total))

(defun get-seq-elem-rect (seq i)
  (cffi:with-foreign-slots ((x y width height) (get-seq-elem seq i) (:struct rect))
    (rect x y width height)))


(defmacro with-named-window ((name &optional (options :cv-window-autosize)) &body body)
  `(unwind-protect (progn
		     (cv:named-window ,name ,options)
		     ,@body)
     (cv:destroy-window ,name)
     (cv:wait-key 2)))

(defmacro with-captured-camera ((capture &key (idx 0) (width 640) (height 480)) &body body)
  `(let* ((,capture (cv:create-camera-capture ,idx)))
     (cv:set-capture-property ,capture :cv-cap-prop-frame-width (coerce ,width 'double-float))
     (cv:set-capture-property ,capture :cv-cap-prop-frame-height (coerce ,height 'double-float))
     (unwind-protect (progn ,@body)
       (cv:release-capture ,capture))))

(defmacro with-ipl-images ((&rest images) &body body)
  `(let* (,@(loop for img in images
		 collect `(,(car img) (cv:create-image ,@(cdr img)))))
     (unwind-protect (progn ,@body)
       ,@(loop for img in images
	      collect `(cv:release-image ,(car img))))))
