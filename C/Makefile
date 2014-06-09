TARGET=libopencv_wrap
OBJ= IplImage.o filter.o draw.o imageproc.o hist.o contours.o
CFLAGS=`pkg-config --cflags opencv` -std=c99 -fPIC
LIBS=`pkg-config --libs opencv`

all : $(TARGET)

$(TARGET) : $(OBJ)
	cc -shared -o $(TARGET) $(OBJ) $(LIBS)

clean :
	rm $(TARGET) $(OBJ)
