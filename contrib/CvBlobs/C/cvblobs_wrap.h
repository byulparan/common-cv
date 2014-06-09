

#include <cvblob.h>
#include <cv.h>

using namespace std;
using namespace cvb;

extern "C" {
  
  CvBlobs* make_cv_blobs ();
  void destroy_cv_blobs(CvBlobs* blobs);
  unsigned int cv_Label(IplImage const *img, IplImage* imgOut, CvBlobs* blobs);
  void cv_FilterLabels(IplImage *imgIn, IplImage* imgOut, const CvBlobs* blobs);

  CvLabel cv_GetLabel(IplImage const *img, unsigned int x, unsigned int y);

  void cv_ReleaseBlobs(CvBlobs* blobs);
  CvLabel cv_LargestBlob(const CvBlobs *blobs);

  void cv_FilterByArea(CvBlobs *blobs, unsigned int minArea, unsigned int maxArea);

  void cv_FilterByLabel(CvBlobs *blobs, CvLabel label);

  void cv_Centroid(CvBlob* blob, CvPoint2D64f *point);

  
  void cv_RenderBlob (const IplImage *imgLabel, CvBlob *blob, IplImage *imgSource,
		      IplImage *imgDest, unsigned short mode, CvScalar* color,  double alpha);

  void cv_RenderBlobs(const IplImage *imgLabel, CvBlobs *blobs, IplImage *imgSource,
		      IplImage *imgDest, unsigned short mode, double alpha);

  void cv_SetImageROItoBlob(IplImage *img, CvBlob const *blob);

  void cv_BlobMeanColor(CvBlob const *blob, IplImage const *imgLabel, IplImage const *img,CvScalar* scalar);

  double cv_DotProductPoints(CvPoint const *a, CvPoint const *b, CvPoint const* c);

  double cv_CrossProductPoints(CvPoint const *a, CvPoint const *b, CvPoint const *c);

  double cv_DistancePointPoint(CvPoint const *a, CvPoint const *b);
  
  double cv_DistanceLinePoint(CvPoint const *a, CvPoint const *b, CvPoint const *c, int isSegment);

  static CvBlobs::iterator it;
  static CvBlobs::iterator it_end;

  CvBlob* do_cv_blobs_first(CvBlobs *blobs);

  CvBlob* do_cv_blobs_next();

  CvTracks* make_cv_tracks();
  void destroy_cv_tracks(CvTracks* tracks);
  void cv_ReleaseTracks(CvTracks* tracks);
  void cv_UpdateTracks(CvBlobs const *b, CvTracks *t, const double thDistance, const unsigned int thInactive, const unsigned int thActive);
  void cv_RenderTracks(CvTracks const *tracks, IplImage* imgSrc, IplImage* imgDst, unsigned short mode, CvFont* font);
}
