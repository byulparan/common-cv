
#include <highgui.h>
#include <cv.h>

void cv_FloodFill(IplImage* img, CvPoint* seedPoint, CvScalar* newVal, CvScalar* loDiff, CvScalar* upDiff,
		  CvConnectedComp* comp, int flags, CvArr* mask) {
  cvFloodFill(img, *seedPoint, *newVal, *loDiff, *upDiff, comp, flags, mask);
}

void cv_Filter2D(const CvArr* src, CvArr* dst, const CvMat* kernel, CvPoint* anchor) {
  cvFilter2D(src, dst, kernel, *anchor);
}

void cv_CopyMakeBorder(const CvArr* src, CvArr* dst, CvPoint* offset, int bordertype, CvScalar* value) {
  cvCopyMakeBorder(src, dst, *offset, bordertype, *value);
}

void cv_Remap (const CvArr* src, CvArr* dst, const CvArr* mapx, const CvArr* mapy, int flags,
	       CvScalar* fillval) {
  cvRemap(src, dst, mapx, mapy, flags, *fillval);
}

void cv_WarpAffine(const CvArr* src, CvArr* dst, const CvMat* map_matrix, int flags, CvScalar* fillval) {
  cvWarpAffine(src, dst, map_matrix, flags, *fillval);
}

CvMat* cv_2DRotationMatrix(CvPoint2D32f* center, double angle, double scale, CvMat* map_matrix) {
  return cv2DRotationMatrix(*center, angle, scale, map_matrix);
}

void cv_WarpPerspective(const CvArr* src, CvArr* dst, const CvMat* map_matrix, int flags, CvScalar* fillval) {
  cvWarpPerspective(src, dst, map_matrix, flags, *fillval);
}

void cv_LogPolar(const CvArr* src, CvArr* dst, CvPoint2D32f *center, double m, int flags) {
  cvLogPolar(src, dst, *center, m, flags);
}
