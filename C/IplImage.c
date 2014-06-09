
#include <highgui.h>
#include <cv.h>

int get_mat_step (CvMat* mat) {
  return mat->step;
}

int get_mat_rows (CvMat* mat) {
  return cvGetSize(mat).width;
}

int get_mat_cols (CvMat* mat) {
  return cvGetSize(mat).height;
}

uchar* get_mat_data (CvMat* mat) {
  return (mat->data).ptr;
}

void set_mat_data (CvMat* mat, uchar* data) {
  mat->data.ptr = data;
}


IplImage* cv_CreateImage(CvSize* size, int depth, int channels) {
  return cvCreateImage(*size, depth, channels);
}

IplImage* cv_CreateImageHeader(CvSize* size, int depth, int channels) {
  return cvCreateImageHeader(*size, depth, channels);
}

void cv_SetImageROI(IplImage* image, CvRect* rect) {
  cvSetImageROI(image, *rect);
}

int get_nSize (IplImage* image) {
  return image->nSize;
}

int get_ID (IplImage* image) {
  return image->ID;
}

int get_nChannels (IplImage* image) {
  return image->nChannels;
}

int get_alphaChannel(IplImage* image) {
  return image->alphaChannel;
}

int get_depth(IplImage* image) {
  return image->depth;
}

int get_dataOrder(IplImage* image) {
  return image->dataOrder;
}

int get_origin(IplImage* image) {
  return image->origin;
}

int get_align(IplImage* image) {
  return image->align;
}

int get_width(IplImage* image) {
  return image->width;
}

int get_height(IplImage* image) {
  return image->height;
}

int get_imageSize(IplImage* image) {
  return image->imageSize;
}

char* get_imageData(IplImage* image) {
  return image->imageData;
}

int get_widthStep(IplImage* image) {
  return image->widthStep;
}

void set_origin (IplImage* image, int origin) {
  image->origin = origin;
}

void set_widthStep (IplImage* image, int widthStep) {
  image->widthStep = widthStep;
}

void set_imageData (IplImage* image, char* imageData) {
  image->imageData = imageData;
}

void set_depth(IplImage* image, int depth) {
  image->depth = depth;
}

void set_nChannels (IplImage* image, int channels) {
  image->nChannels = channels;
}

void set_imageSize (IplImage* image, int size) {
  image->imageSize = size;
}

void set_width (IplImage* image, int width) {
  image->width = width;
}

void set_height (IplImage* image, int height) {
  image->height = height;
}

void cv_Get1D(const CvArr* arr, int idx0, CvScalar* scalar) {
  CvScalar result = cvGet1D(arr, idx0);
  for (int i = 0; i < 4; i++) {
    scalar->val[i] = result.val[i];
  }
}

void cv_Get2D(const CvArr* arr, int idx0,int idx1, CvScalar* scalar) {
  CvScalar result = cvGet2D(arr, idx0, idx1);
  for (int i = 0; i < 4; i++) {
    scalar->val[i] = result.val[i];
  }
}

void cv_Get3D(const CvArr* arr, int idx0,int idx1,int idx2, CvScalar* scalar) {
  CvScalar result = cvGet3D(arr, idx0, idx1,idx2);
  for (int i = 0; i < 4; i++) {
    scalar->val[i] = result.val[i];
  }
}

void cv_GetND(const CvArr* arr, int* idx, CvScalar* scalar) {
  CvScalar result = cvGetND(arr, idx);
  for (int i = 0; i < 4; i++) {
    scalar->val[i] = result.val[i];
  }
}

void cv_Set1D(CvArr* arr, int idx0, CvScalar* value) {
  cvSet1D(arr, idx0, *value);
}

void cv_Set2D(CvArr* arr, int idx0,int idx1, CvScalar* value) {
  cvSet2D(arr, idx0,idx1, *value);
}

void cv_Set3D(CvArr* arr, int idx0,int idx1,int idx2, CvScalar* value) {
  cvSet3D(arr, idx0,idx1,idx2, *value);
}

void cv_SetND(CvArr* arr, int* idx, CvScalar* value) {
  cvSetND(arr, idx, *value);
}

double cv_mGet(const CvMat* mat, int row, int col) {
  return cvmGet(mat, row, col);
}

void cv_mSet(CvMat* mat, int row, int col, double value) {
  cvmSet(mat, row, col, value);
}




/*CvSeq  */
int get_seq_total (CvSeq* seq) {
  return seq->total;
}

CvSeq* get_next (CvSeq* seq) {
  return seq->h_next;
}

CvSeqWriter* cv_StartWriteSeq (int seq_flags, int header_size, int elem_size,
			       CvMemStorage* storage) {
  CvSeqWriter* writer = (CvSeqWriter*) malloc(sizeof(CvSeqWriter));
  cvStartWriteSeq(seq_flags, header_size, elem_size, storage, writer);
  return writer;
}

void cv_write_seq_elem_point (CvPoint* point, CvSeqWriter* writer) {
  CV_WRITE_SEQ_ELEM(*point, *writer);
}

void cv_write_seq_elem_rect (CvRect* rect, CvSeqWriter* writer) {
  CV_WRITE_SEQ_ELEM(*rect, *writer);
}

CvSeqReader* cv_StartReadSeq(const CvSeq* seq, int reverse) {
  CvSeqReader* reader = (CvSeqReader*) malloc(sizeof(CvSeqReader));
  cvStartReadSeq(seq, reader, reverse);
  return reader;
}

void cv_read_seq_elem_point(CvPoint* point, CvSeqReader* reader) {
  memcpy(point, (*reader).ptr, sizeof(CvPoint));
  CV_NEXT_SEQ_ELEM(sizeof(CvPoint), *reader);
}

void cv_read_seq_elem_rect(CvRect* rect, CvSeqReader* reader) {
  memcpy(rect, (*reader).ptr, sizeof(CvRect));
  CV_NEXT_SEQ_ELEM(sizeof(CvRect), *reader);
}

/* double get_connected_comp_area (CvConnectedComp* comp) { */
/*   return comp->area; */
/* } */


/* void get_connected_comp_value (CvConnectedComp *comp, CvScalar *value) { */
/*   CvScalar scalar = comp->value; */
/*   for (int i = 0; i < 4; i++) { */
/*     value->val[i] = scalar.val[i]; */
/*   } */
/* } */

/* void get_connected_comp_rect (CvConnectedComp *comp, CvRect* r) { */
/*   CvRect rect = comp->rect; */
/*   r->x = rect.x; */
/*   r->y = rect.y; */
/*   r->width = rect.width; */
/*   r->height = rect.height; */
/* } */

/* CvSeq* get_connected_comp_contour (CvConnectedComp *comp) { */
/*   return comp->contour; */
/* } */

