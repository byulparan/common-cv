
#include <cv.h>
#include <highgui.h>

CvSeq* cv_SeqSlice (const CvSeq* seq, CvSlice* slice, CvMemStorage* storage, int copy_data) {
  return cvSeqSlice(seq, *slice, storage, copy_data);
}

void cv_SeqRemoveSlice(CvSeq* seq, CvSlice* slice) {
  cvSeqRemoveSlice(seq, *slice);
}

int cv_FindContours(IplImage* img, CvMemStorage* storage, CvSeq** firstContour,
		    int headerSize, int mode, int method, CvPoint* offset) {
  return  cvFindContours(img, storage, firstContour, headerSize, mode, method, *offset);
}

CvContourScanner cv_StartFindContours(CvArr* image, CvMemStorage* storage, int headerSize,
				       int mode, int method, CvPoint* offset) {
  return cvStartFindContours(image, storage, headerSize, mode, method, *offset);
}


CvChainPtReader* cv_StartReadChainPoints(CvChain* chain) {
  CvChainPtReader* reader = (CvChainPtReader*) malloc (sizeof (CvChainPtReader));
  cvStartReadChainPoints(chain, reader);
  return reader;
}

void cv_ReadChainPoint(CvChainPtReader* reader, CvPoint* point) {
  CvPoint result = cvReadChainPoint(reader);
  point->x = result.x;
  point->y = result.y;
}

void cv_DrawContours(CvArr* img, CvSeq* contour, CvScalar* external, CvScalar* hole,
		     int max_level, int thickness, int line_type, CvPoint* offset) {
  cvDrawContours(img, contour, *external, *hole, max_level, thickness, line_type, *offset);
}

double cv_ArcLength(const void* curve, CvSlice* slice, int is_closed) {
  return cvArcLength(curve, *slice, is_closed);
}

double cv_ContourArea(const CvSeq* contour, CvSlice* slice) {
  return cvContourArea(contour, *slice, 0);
}

void cv_BoundingRect(CvArr* points, int update, CvRect* rect) {
  CvRect ret = cvBoundingRect(points, update);
  rect->x = ret.x;
  rect->y = ret.y;
  rect->width = ret.width;
  rect->height = ret.height;
}

void cv_MinAreaRect2(const CvArr* points, CvMemStorage* storage, CvBox2D* box) {
  CvBox2D ret = cvMinAreaRect2(points, storage);
  box->center.x = ret.center.x;
  box->center.y = ret.center.y;
  box->size.width = ret.size.width;
  box->size.height = ret.size.height;
  box->angle = ret.angle;
}

void cv_fitEllipse2(const CvArr* points, CvBox2D* box) {
  CvBox2D ret = cvFitEllipse2(points);
  box->center.x = ret.center.x;
  box->center.y = ret.center.y;
  box->size.width = ret.size.width;
  box->size.height = ret.size.height;
  box->angle = ret.angle;
}


void cv_MaxRect(const CvRect* rect1, const CvRect* rect2, CvRect* rect) {
  CvRect ret = cvMaxRect(rect1, rect2);
  rect->x = ret.x;
  rect->y = ret.y;
  rect->width = ret.width;
  rect->height = ret.height;
}

void cv_BoxPoints(CvBox2D* box, CvPoint2D32f* pt) {
  cvBoxPoints(*box, pt);
}

double cv_PointPolygonTest(const CvArr* contour, CvPoint2D32f* pt, int measure_dist) {
  return cvPointPolygonTest(contour, *pt, measure_dist);
}

CvSeq* cv_HaarDetectObjects(const CvArr* image, CvHaarClassifierCascade* cascade, CvMemStorage* storage,
			    double scale_factor, int min_neighbors, int flags, CvSize* minSize, CvSize* maxSize) {
  return cvHaarDetectObjects(image, cascade, storage, scale_factor, min_neighbors, flags, *minSize, *maxSize);
}
