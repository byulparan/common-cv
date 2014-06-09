
#include <cv.h>
#include <highgui.h>

double cv_QueryHistValue_1D (CvHistogram* hist, int idx0) {
  return cvQueryHistValue_1D(hist, idx0);
}

double cv_QueryHistValue_2D (CvHistogram* hist, int idx0,int idx1) {
  return cvQueryHistValue_2D(hist, idx0, idx1);
}

double cv_QueryHistValue_3D (CvHistogram* hist, int idx0, int idx1, int idx2) {
  return cvQueryHistValue_3D(hist, idx0, idx1, idx2);
}

double cv_QueryHistValue_nD (CvHistogram* hist, int* idx) {
  return cvQueryHistValue_nD(hist, idx);
}

void cv_CalcHist (IplImage** image, CvHistogram* hist, int accumulate, const CvArr* mask) {
  cvCalcHist(image, hist, accumulate, mask);
}

void cv_CalcBackProject(IplImage** image, CvArr* back_project, const CvHistogram* hist) {
  cvCalcBackProject(image, back_project, hist);
}

void cv_CalcBackProjectPatch(IplImage** image, CvArr* dst, CvSize* patch_size, CvHistogram* hist,
			     int method, float factor) {
  cvCalcBackProjectPatch(image, dst, *patch_size, hist, method, factor);
}
