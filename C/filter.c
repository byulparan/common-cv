
#include <cv.h>

void cv_AbsDiffS(const CvArr* src, CvArr* dst, CvScalar* value) {
  cvAbsDiffS(src, dst, *value);
}

void cv_AddS(const CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) {
  cvAddS(src, *value, dst, mask);
}

int cv_Round (double val) {
  return cvRound(val);
}

void cv_AndS(const CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) {
  cvAndS(src, *value, dst, mask);
}

void cv_Avg (const CvArr* arr, const CvArr* mask, CvScalar* dst) {
  CvScalar scalar = cvAvg(arr, mask);
  for (int i = 0; i < 4; i++) {
    dst->val[i] = scalar.val[i];
  }
}


void cv_GetSize(const CvArr* arr, CvSize* dst) {
  CvSize size = cvGetSize(arr);
  dst->width = size.width;
  dst->height = size.height;
}

CvMat* cv_GetSubRect (const CvArr *arr, CvMat* submat, CvRect* rect) {
  return cvGetSubRect(arr, submat, *rect);
}

void cv_InRangeS(const CvArr* src, CvScalar* lower, CvScalar* upper, CvArr* dst) {
  cvInRangeS(src, *lower, *upper, dst);
}

void cv_OrS(const CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) {
  cvOrS(src, *value, dst, mask);
}

void cv_CvtPixToPlane (const CvArr* src, CvArr* dst0, CvArr* dst1, CvArr* dst2, CvArr* dst3) {
  cvCvtPixToPlane(src, dst0,dst1,dst2,dst3);
}

void cv_Set(CvArr* arr, CvScalar* value, const CvArr* mask) {
  cvSet(arr, *value, mask);
}

void cv_SubS(CvArr* src, CvScalar* value, CvArr* dst, CvArr* mask) {
  cvSubS(src, *value, dst, mask);
}

void cv_SubRS(CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) {
  cvSubRS(src, *value, dst, mask);
}

void cv_XorS(CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) {
  cvXorS(src, *value, dst, mask);
}

void cv_SetIdentity (CvArr* mat, CvScalar* scalar) {
  cvSetIdentity(mat, *scalar);
}

void cv_Sum(CvArr* arr, CvScalar* scalar) {
  CvScalar result = cvSum(arr);
  for(int i = 0; i < 4; i++) {
    scalar->val[i] = result.val[i];
  }
}

void cv_Trace(const CvArr* mat, CvScalar* scalar) {
  CvScalar result = cvTrace(mat);
  for(int i = 0; i < 4; i++) {
    scalar->val[i] = result.val[i];
  }
}
