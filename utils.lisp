(in-package #:cv)

(defmacro define-cfun ((c-name name) return-type &body body)
  `(progn
     (defun ,name ,(loop for p in body
			 with optional = nil
			 with a = () with b = ()
			 do (if (= (length p) 2) (progn
						   (when optional (error "why not optional?"))
						   (push (car p) a))
			      (progn
				(setf optional t)
				(push (list (car p) (third p)) b)))
			 finally (return (append (reverse a) (cons '&optional (reverse b)))))
       (cffi:foreign-funcall
	,c-name
	,@(mapcan
	   #'(lambda (p)
	       (destructuring-bind (var type &rest r)
		   p
		 (declare (ignore r))
		 (list type (case type
			      (:float `(coerce ,var 'single-float))
			      (:double `(coerce ,var 'double-float))
			      (:int `(floor ,var))
			      (otherwise var)))))
	   body)
	,return-type))
     (export ',name)))


(defmacro define-export-constant (name value)
  `(progn
     (defconstant ,name ,value)
     (export ',name)))

