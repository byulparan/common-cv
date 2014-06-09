
#include "cvblobs_wrap.h"


CvBlobs* make_cv_blobs () {
  CvBlobs *blobs = new CvBlobs;
  return blobs;
}

void destroy_cv_blobs(CvBlobs *blobs) {
  delete blobs;
}

unsigned int cv_Label(IplImage const *img, IplImage* imgOut, CvBlobs* blobs) {
  return cvLabel(img, imgOut, *blobs);
}

void cv_FilterLabels(IplImage *imgIn, IplImage* imgOut, const CvBlobs* blobs) {
  cvFilterLabels(imgIn, imgOut, *blobs);
}

CvLabel cv_GetLabel(IplImage const *img, unsigned int x, unsigned int y) {
  return cvGetLabel(img, x, y);
}

void cv_ReleaseBlobs(CvBlobs* blobs) {
  cvReleaseBlobs(*blobs);
}

CvLabel cv_LargestBlob(const CvBlobs *blobs) {
  return cvLargestBlob(*blobs);
}

void cv_FilterByArea(CvBlobs *blobs, unsigned int minArea, unsigned int maxArea) {
  cvFilterByArea(*blobs, minArea, maxArea);
}

void cv_FilterByLabel(CvBlobs *blobs, CvLabel label) {
  cvFilterByLabel(*blobs, label);
}




void cv_Centroid(CvBlob* blob, CvPoint2D64f *point) {
  CvPoint2D64f result = cvCentroid(blob);
  point->x = result.x;
  point->y = result.y;
}

void cv_RenderBlob (const IplImage *imgLabel, CvBlob *blob, IplImage *imgSource,
			   IplImage *imgDest, unsigned short mode, CvScalar* color, double alpha) {
  cvRenderBlob(imgLabel, blob, imgSource, imgDest, mode,*color, alpha);
}

void cv_RenderBlobs(const IplImage *imgLabel, CvBlobs *blobs, IplImage *imgSource,
		    IplImage *imgDest, unsigned short mode, double alpha) {
  cvRenderBlobs(imgLabel, *blobs, imgSource, imgDest, mode, alpha);
}




void cv_SetImageROItoBlob(IplImage *img, CvBlob const *blob) {
  cvSetImageROItoBlob(img, blob);
}

void cv_BlobMeanColor(CvBlob const *blob, IplImage const *imgLabel, IplImage const *img,CvScalar* scalar) {
  CvScalar result = cvBlobMeanColor(blob, imgLabel, img);
  for(int i = 0; i < 4; i++) {
    scalar->val[i] = result.val[i];
  }
}

double cv_DotProductPoints(CvPoint const *a, CvPoint const *b, CvPoint const* c) {
  return cvDotProductPoints(*a, *b, *c);
}

double cv_CrossProductPoints(CvPoint const *a, CvPoint const *b, CvPoint const *c) {
  return cvCrossProductPoints(*a, *b,*c);
}

double cv_DistancePointPoint(CvPoint const *a, CvPoint const *b) {
  return cvDistancePointPoint(*a, *b);
}

double cv_DistanceLinePoint(CvPoint const *a, CvPoint const *b, CvPoint const *c, int isSegment) {
  return cvDistanceLinePoint(*a, *b, *c, (isSegment == 1 ? true : false));
}

CvBlob* do_cv_blobs_first(CvBlobs *blobs) {
  CvBlobs original = *blobs;
  it = original.begin();
  it_end = original.end();
  if (it == it_end) {
    return NULL;
  }
  else {
    return (*it).second;
  }
}

CvBlob* do_cv_blobs_next() {
  it++;
  if (it == it_end) {
    return NULL;
  }
  else {
    return (*it).second;
  }
}





CvTracks* make_cv_tracks () {
  CvTracks *tracks = new CvTracks;
  return tracks;
}

void destroy_cv_tracks(CvTracks* tracks) {
  delete tracks;
}

void cv_ReleaseTracks(CvTracks* tracks) {
  cvReleaseTracks(*tracks);
}


void cv_UpdateTracks(CvBlobs const *b, CvTracks* t, const double thDistance, const unsigned int thInactive, const unsigned int thActive) {
  cvUpdateTracks(*b, *t, thDistance, thInactive, thActive);
}

void cv_RenderTracks(CvTracks const *tracks, IplImage* imgSrc, IplImage* imgDst, unsigned short mode, CvFont *font) {
  cvRenderTracks(*tracks, imgSrc, imgDst, mode, font);
}
