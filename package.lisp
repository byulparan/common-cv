(defpackage #:opencv
  (:nicknames :cv)
  (:use #:cl)
  (:shadow #:abs #:and #:max #:merge #:min #:not #:or #:pow #:exp #:log #:sort #:reduce #:set #:trace #:round #:load #:type)
  (:export ;; cv-types
   #:point
   #:point-x
   #:point-y
   #:point2d-32f
   #:point2d-64f
   #:point2d-32f-x
   #:point2d-32f-y
   #:point2d-64f-x
   #:point2d-64f-y
   #:size
   #:size-width
   #:size-height
   #:size2d-32f
   #:size2d-32f-width
   #:size2d-32f-height
   #:box2d
   #:box2d-center
   #:box2d-size
   #:box2d-angle
   #:rect
   #:rect-x
   #:rect-y
   #:rect-width
   #:rect-height
   #:scalar
   #:scalar-val
   #:scalar-all
   #:real-scalar
   #:slice
   #:slice-start-index
   #:slice-end-index
   #:term-criteria
   #:term-criteria-type
   #:term-criteria-max-iter
   #:term-criteria-epsilon
   
   #:release-image
   #:release-mat
   #:release-mat-nd
   #:release-sparse-mat
   
   #:release-mem-storage
   #:release-structuring-element
   #:release-hist
   #:release-capture
   #:create-hist
   #:get-min-max-hist-value
   #:dist-transform

   #:get-affine-transform
   #:get-perspective-transform
   #:find-contours
   #:find-next-contour
   
   #:x
   #:y
   #:width
   #:height
   #:mat-data
   #:get-total
   #:get-seq-elem-rect
   #:with-named-window
   #:with-captured-camera
   #:with-ipl-images
   #:with-init-font

   #:create-trackbar))
