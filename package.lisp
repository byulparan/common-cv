
(defpackage #:opencv
  (:nicknames :cv)
  (:use #:cl)
  (:shadow #:abs #:and #:max #:merge #:min #:not #:or #:reduce #:set #:trace #:round #:load #:type)
  (:export  #:create-camera-capture
	    #:create-file-capture
	    #:release-capture
	    #:with-named-window
	    #:with-captured-camera
	    #:with-captured-file
	    #:show-image
	    #:query-frame
	    #:wait-key
	    #:set-mouse-callback
	    #:event-decode
	    #:get-capture-property
	    #:set-capture-property
	    #:convert-image
	    #:load-image
	    #:load-mat
	    #:load-haar-cascade
	    #:point
	    #:point-2d-32f
	    #:size
	    #:rect
	    #:scalar
	    #:real-scalar
	    #:scalar-all
	    #:x
	    #:y
	    #:width
	    #:height
	    #:create-mat
	    #:data
	    #:rows
	    #:cols
	    #:type
	    #:release-mat
	    #:create-image
	    #:create-image-header
	    #:with-ipl-image
	    #:release-image
	    #:set-image-roi
	    #:reset-image-roi
	    #:with-image-roi
	    #:origin
	    #:width-step
	    #:image-data
	    #:depth
	    #:n-channels
	    #:image-size))
