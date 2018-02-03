(in-package #:cv)

(defmacro define-cfun (names return-type &body body)
  `(progn
     (cffi:defcfun ,names ,return-type
       ,@body)
     (export ',(second names))))
