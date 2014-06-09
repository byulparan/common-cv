
#include <highgui.h>

void cv_Line (CvArr* array, CvPoint* pt1, CvPoint* pt2, CvScalar* color, int thickness, int lineType,int shift) {
  cvLine(array, *pt1, *pt2, *color, thickness, lineType, shift);
}

void cv_Rectangle(CvArr* array, CvPoint* pt1, CvPoint* pt2, CvScalar* color, int thickness, int lineType, int shift) {
  cvRectangle(array, *pt1, *pt2, *color, thickness,lineType, shift);
}

void cv_Circle(CvArr* array, CvPoint* center, int radius, CvScalar* color, int thickness, int lineType, int shift) {
  cvCircle(array, *center, radius, *color, thickness, lineType, shift);
}

void cv_Ellipse(CvArr* img, CvPoint* center, CvSize* axes, double angle, double start_angle, double end_angle, CvScalar* color, int thickness, int line_type, int shift) {
  cvEllipse(img, *center, *axes, angle, start_angle, end_angle, *color, thickness, line_type,shift);
}

void cv_EllipseBox(CvArr* img, CvBox2D* box, CvScalar* color, int thickness, int line_type,int shift) {
  cvEllipseBox(img, *box, *color, thickness, line_type, shift);
}


void cv_FillPoly(CvArr* img, CvPoint** pts, int* npts, int contours, CvScalar* color, int line_type, int shift) {
  cvFillPoly(img, pts, npts, contours, *color, line_type,shift);
}

void cv_fillConvexPoly(CvArr* img, CvPoint* pts, int npts, CvScalar* color, int line_type,int shift) {
  cvFillConvexPoly(img, pts, npts, *color, line_type,shift);
}

void cv_PolyLine(CvArr* img, CvPoint** pts, int* npts, int contours, int is_closed, CvScalar* color, int thickness, int line_type,int shift) {
  cvPolyLine(img, pts, npts, contours, is_closed, *color, thickness, line_type,shift);
}


void cv_PutText(CvArr* img, const char* text, CvPoint* origin, CvScalar* color, int font_face, double hscale, double vscale,
		double shear, int thickness, int line_type) {
  
  CvFont font;
  cvInitFont(&font, font_face, hscale, vscale, shear, thickness, line_type);
  cvPutText(img, text, *origin, &font, *color);
}
		
