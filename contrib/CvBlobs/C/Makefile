
CXXFLAGS=`pkg-config --cflags opencv cvblob` -fPIC
CXXLDFLAGS=`pkg-config --libs opencv cvblob`
TARGET=libcvblob_wrap
OBJS=cvblobs_wrap.o

all : $(TARGET)

$(TARGET) : $(OBJS)
	c++ -shared -o $(TARGET) $(OBJS) $(CXXLDFLAGS)

clean :
	rm $(TARGET) $(OBJS)
