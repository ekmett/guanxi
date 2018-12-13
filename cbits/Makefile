CXX := g++
CXXFLAGS=-std=c++11 -Wall -g

SRCS := $(shell find . -name "*.cpp")
OBJS := $(patsubst %.cpp, %.o, $(SRCS))

all: queens dlx_capi.o

clean:
	rm -f queens $(OBJS)

dist-clean: clean
	rm -f .depend

.PHONY: all clean dist-clean depend

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

include .depend

queens: queens.o dlx.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o queens $^ $(LDLIBS)

%.o: %.cpp
	$(CC) $(CXXFLAGS) -c $<
