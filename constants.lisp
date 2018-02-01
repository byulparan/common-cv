(in-package #:cv)

;;; HighGUI
;;; NamedWindow
(cffi:defcenum :named-window-enum
  (:cv-window-normal #x00000000)
  (:cv-window-autosize #x00000001))

;;; mouse-event
(cffi:defcenum :mouse-event-enum
  (:cv-event-mouse-move 0)
  (:cv-event-l-button-down 1)
  (:cv-event-r-button-down 2)
  (:cv-event-m-button-down 3)
  (:cv-event-l-button-up 4)
  (:cv-event-r-button-up 5)
  (:cv-event-m-button-up 6)
  (:cv-event-l-button-dbl-clk 7)
  (:cv-event-r-button-dbl-clk 8)
  (:cv-event-m-button-dbl-clk 9))

(cffi:defcenum :mouse-event-flags-enum
  (:cv-event-flag-l-button 1)
  (:cv-event-flag-r-button 2)
  (:cv-event-flag-m-button 4)
  (:cv-event-flag-ctrl-key 8)
  (:cv-event-flag-shift-key 16)
  (:cv-event-flag-alt-key 32))

(cffi:defcenum :cap-prop-enum
  (:cv-cap-prop-pos-msec 0)
  (:cv-cap-prop-pos-frame 1)
  (:cv-cap-prop-pos-avi-ratio 2)
  (:cv-cap-prop-frame-width 3)
  (:cv-cap-prop-frame-height 4)
  (:cv-cap-prop-fps 5)
  (:cv-cap-prop-fourcc 6)
  (:cv-cap-prop-frame-count 7))

(cffi:defcenum :convert-image-enum
  (:cv-cvtimg-flip 1)
  (:cv-cvtimg-swap-rb 2))


;;; for Font-Face
(cffi:defcenum :font-face-enum
  (:CV-FONT-HERSHEY-SIMPLEX         0)
  (:CV-FONT-HERSHEY-PLAIN           1)
  (:CV-FONT-HERSHEY-DUPLEX          2)
  (:CV-FONT-HERSHEY-COMPLEX         3)
  (:CV-FONT-HERSHEY-TRIPLEX         4)
  (:CV-FONT-HERSHEY-COMPLEX-SMALL   5)
  (:CV-FONT-HERSHEY-SCRIPT-SIMPLEX  6)
  (:CV-FONT-HERSHEY-SCRIPT-COMPLEX  7))


;;; CvMat
(cffi:defcenum :mat-type-enum
  ;; (:CV-8UC1 0)
  ;; (:CV-8UC2 8)
  ;; (:CV-8UC3 16)
  ;; (:CV-8UC4 24)
  ;; (:CV-8SC1 1)
  ;; (:CV-8SC2 9)
  ;; (:CV-8SC3 17)
  ;; (:CV-8SC4 25)
  ;; (:CV-16UC1 2)
  ;; (:CV-16UC2 10)
  ;; (:CV-16UC3 18) 
  ;; (:CV-16UC4 26)
  ;; (:CV-16SC1 3)
  ;; (:CV-16SC2 11)
  ;; (:CV-16SC3 19)
  ;; (:CV-16SC4 27)
  ;; (:CV-32SC1 4)
  ;; (:CV-32SC2 12)
  ;; (:CV-32SC3 20)
  ;; (:CV-32SC4 8)
  (:CV-32FC1 5)
  ;; (:CV-32FC2 13)
  ;; (:CV-32FC3 21)
  ;; (:CV-32FC4 29)
  (:CV-64FC1 6)
  ;; (:CV-64FC2 14)
  ;; (:CV-64FC3 22)
  ;; (:CV-64FC4 30)
  )

;;; IPL Image
;;; Depth for CreateImage
(cffi:defcenum :ipl-depth-enum
  (:ipl-depth-label 32)
  (:IPL-DEPTH-1U 1)
  (:IPL-DEPTH-8U 8)
  (:IPL-DEPTH-16U 16)
  (:IPL-DEPTH-32F 32)
  (:IPL-DEPTH-64F 64)
  (:IPL-DEPTH-8S -2147483640)
  (:IPL-DEPTH-16S -2147483632)
  (:IPL-DEPTH-32S -2147483616))

;;; dataOrder
(cffi:defcenum :ipl-data-order-enum
  (:IPL-DATA-ORDER-PIXEL  0)
  (:IPL-DATA-ORDER-PLANE  1))

;;; Origin
(cffi:defcenum :ipl-origin-enum
  (:IPL-ORIGIN-TL 0)
  (:IPL-ORIGIN-BL 1))

;;; align
(cffi:defcenum :ipl-align-enum
  (:IPL-ALIGN-4BYTES   4)
  (:IPL-ALIGN-8BYTES   8)
  (:IPL-ALIGN-16BYTES 16)
  (:IPL-ALIGN-32BYTES 32))

;;; Histogram
(cffi:defcenum :hist-enum
  (:cv-hist-array 0) 
  (:cv-hist-sparse 1))

(cffi:defcenum :hist-compare-enum
  (:CV-COMP-CORREL        0)
  (:CV-COMP-CHISQR        1)
  (:CV-COMP-INTERSECT     2)
  (:CV-COMP-BHATTACHARYYA 3))


;;; for cvLoadImage
(cffi:defcenum :ipl-load-image-enum
  (:CV-LOAD-IMAGE-COLOR 1)
  (:CV-LOAD-IMAGE-GRAYSCALE 0)
  (:CV-LOAD-IMAGE-UNCHANGED -1)
  (:CV-LOAD-IMAGE-ANYCOLOR 4)
  (:CV-LOAD-IMAGE-ANYDEPTH 2))


;;; CvSeq
(cffi:defcenum :cv-seq-type-enum
  (:cv-seq-kind-generic 0)
  (:cv-seq-kind-curve 4096) 
  (:cv-seq-kind-bin-tree 8192)
  (:cv-seq-kind-graph 4096)
  (:cv-seq-kind-subdiv2d 8192)
  (:cv-seq-flag-closed 16384)
  (:cv-seq-flag-simple 0)
  (:cv-seq-flag-convex 0)
  (:cv-seq-flag-hole 32768))



(cffi:defcenum :contours-mode-enum
  (:CV-RETR-EXTERNAL 0)
  (:CV-RETR-LIST 1)
  (:CV-RETR-CCOMP 2 )
  (:CV-RETR-TREE 3))

(cffi:defcenum :contours-memthod-enum
  (:CV-CHAIN-CODE 0)
  (:CV-CHAIN-APPROX-NONE 1)
  (:CV-CHAIN-APPROX-SIMPLE 2)
  (:CV-CHAIN-APPROX-TC89-L1 3)
  (:CV-CHAIN-APPROX-TC89-KCOS 4)
  (:CV-LINK-RUNS 5))







;;; filter
(cffi:defcenum :covar-matrix-enum
  (:cv-covar-scrambled 0)
  (:cv-covar-normar 1)
  (:cv-covar-use-avg 2)
  (:cv-covar-scale 4)
  (:cv-covar-rows 8)
  (:cv-covar-cols 16))

(cffi:defcenum :cmp-enum
  (:cv-cmp-eq 0)
  (:cv-cmp-gt 1)
  (:cv-cmp-ge 2)
  (:cv-cmp-lt 3)
  (:cv-cmp-le 4)
  (:cv-cmp-ne 5))

(cffi:defcenum :invert-enum 
  (:CV-LU  0)
  (:CV-SVD 1)
  (:CV-SVD-SYM 2)
  (:CV-CHOLESKY 3)
  (:CV-QR  4)
  (:CV-NORMAL 16))

(cffi:defcenum :norm-enum
  (:CV-C            1)
  (:CV-L1           2)
  (:CV-L2           4)
  (:CV-NORM-MASK    7)
  (:CV-RELATIVE     8)
  (:CV-DIFF         16)
  (:CV-MINMAX       32)
  (:CV-DIFF-C       17)
  (:CV-DIFF-L1      18)
  (:CV-DIFF-L2      20)
  (:CV-RELATIVE-C   9)
  (:CV-RELATIVE-L1  10)
  (:CV-RELATIVE-L2  12))

(cffi:defcenum :reduce-enum
  (:CV-REDUCE-SUM 0)
  (:CV-REDUCE-AVG 1)
  (:CV-REDUCE-MAX 2)
  (:CV-REDUCE-MIN 3))

(cffi:defcenum :svd-enum
  (:CV-SVD-MODIFY-A   1)
  (:CV-SVD-U-T        2)
  (:CV-SVD-V-T        4))

(cffi:defcenum :cvt-color-enum
  (:CV-BGR-2-BGRA 0)
  (:cv-rgb-2-rgba 0)
  (:CV-BGRA-2-BGR 1)
  (:cv-rgba-2-rgb 1)
  (:cv-bgr-2-rgba 2)
  (:cv-rgb-2-bgra 2)
  (:cv-rgba-2-bgr 3)
  (:cv-bgra-2-rgb 3)
  (:CV-BGR-2-RGB 4)
  (:cv-rgb-2-bgr 4)
  (:cv-bgra-2-rgba 5)
  (:cv-rgba-2-bgra 5)
  (:cv-bgr-2-gray 6)
  (:cv-rgb-2-gray 7)
  (:CV-GRAY-2-BGR 8)
  (:cv-gray-2-rgb 8)
  (:CV-GRAY-2-BGRA 9)
  (:cv-gray-2-rgba 9)
  (:CV-BGRA-2-GRAY 10)
  (:cv-rgba-2-gray 11)
  (:CV-BGR-2-BGR565 12)
  (:CV-BGR565-2-BGR 14)
  (:CV-BGRA-2-BGR565 16)
  (:CV-BGR565-2-BGRA 18)
  (:CV-GRAY-2-BGR565 20)
  (:CV-BGR565-2-GRAY 21)
  (:CV-BGR-2-BGR555 22)
  (:CV-BGR555-2-BGR 24)
  (:CV-BGRA-2-BGR555 26)
  (:CV-BGR555-2-BGRA 28)
  (:CV-GRAY-2-BGR555 30)
  (:CV-BGR555-2-GRAY 31)
  (:CV-BGR-2-XYZ 32)
  (:CV-XYZ-2-BGR 34)
  (:CV-BGR-2-YCrCb 36)
  (:CV-YCrCb-2-BGR 38)
  (:CV-BGR-2-HSV 40)
  (:CV-BGR-2-Lab 44)
  (:CV-BayerBG-2-BGR 46)
  (:CV-BayerGB-2-BGR 47)
  (:CV-BayerRG-2-BGR 48)
  (:CV-BayerGR-2-BGR 49)
  (:CV-BGR-2-Luv 50)
  (:CV-BGR-2-HLS 52)
  (:CV-HSV-2-BGR 54)
  (:CV-Lab-2-BGR 56)
  (:CV-Luv-2-BGR 58)
  (:CV-HLS-2-BGR 60))

(cffi:defcenum :smooth-method-enum
  (:CV-BLUR-NO-SCALE 0)
  (:CV-BLUR 1)
  (:CV-GAUSSIAN 2)
  (:CV-MEDIAN 3)
  (:CV-BILATERAL 4))

(cffi:defcenum :conv-kernel-shape-enum
  (:cv-shape-rect 0)
  (:cv-shape-cross 1)
  (:cv-shape-ellipse 2)
  (:cv-shape-custom 100))

(cffi:defcenum :morphology-enum
  (:cv-mop-erode 0)
  (:cv-mop-dilate 1)
  (:cv-mop-open 2)
  (:cv-mop-close 3)
  (:cv-mop-gradient 4)
  (:cv-mop-tophat 5)
  (:cv-mop-blackhat 6))

(cffi:defcenum :flood-fill-enum
  (:cv-flood-fill-fixed-range 65536)
  (:cv-flood-fill-mask-only 131072))

(cffi:defcenum :resize-enum
  (:cv-inter-nn 0)
  (:cv-inter-linear 1)
  (:cv-inter-cubic 2)
  (:cv-inter-area 3)
  (:cv-inter-lanczos4 4))

(cffi:defcenum :pyramid-enum
  (:ipl-gaussian-5x5 7))


(cffi:defcenum :threshold-enum
  (:cv-thresh-binary 0)
  (:cv-thresh-binary-inv 1)
  (:cv-thresh-trunc 2)
  (:cv-thresh-tozero 3)
  (:cv-thresh-tozero-inv 4)
  (:cv-thresh-mask 7)
  (:cv-thresh-otsu 8))

(cffi:defcenum :adaptive-method-enum
  (:cv-adaptive-thresh-mean-c 0)
  (:cv-adaptive-thresh-gaussian-c 1))

(cffi:defcenum :make-border-enum
  (:ipl-border-constant 0)
  (:ipl-border-replicate 1))

(cffi:defcenum :remap-enum
  (:cv-inter-nn 0)
  (:cv-inter-linear 1)
  (:cv-inter-cubic 2)
  (:cv-inter-area 3)
  (:cv-inter-lanczos4 4))

(cffi:defcenum :warp-affine-enum
  (:cv-warp-fill-outliers 8)
  (:cv-warp-inverse-map 16))

(cffi:defcenum :dxt-enum
  (:cv-dxt-forward 0)
  (:cv-dxt-inverse 1)
  (:cv-dxt-scale 2)
  (:cv-dxt-inv-scale 3)
  (:cv-dxt-inverse-scale 3)
  (:cv-dxt-rows 4)
  (:cv-dxt-mul-conj 8))

(cffi:defcenum :dist-enum
  (:cv-dist-user -1)
  (:cv-dist-l1 1)
  (:cv-dist-l2 2)
  (:cv-dist-c 3)
  (:cv-dist-l12 4)
  (:cv-dist-fair 5)
  (:cv-dist-welsch 6)
  (:cv-dist-huber 7))

(cffi:defcenum :match-enum
  (:CV-TM-SQDIFF        0)
  (:CV-TM-SQDIFF-NORMED 1)
  (:CV-TM-CCORR         2)
  (:CV-TM-CCORR-NORMED  3)
  (:CV-TM-CCOEFF        4)
  (:CV-TM-CCOEFF-NORMED 5))

(cffi:defcenum :contours-match-enum
  (:cv-contours-match-i1 1)
  (:cv-contours-match-i2 2)
  (:cv-contours-match-i3 3))

(cffi:defcenum :convex-enum
  (:cv-clockwise 1)
  (:cv-counter-clockwise 2))

(cffi:defcenum :haar-detect-enum
  (:cv-haar-do-canny-pruning 1)
  (:cv-haar-scale-image 2)
  (:cv-haar-find-biggest-object 4)
  (:cv-haar-do-rough-search 8))
