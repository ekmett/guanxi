CXX := g++
CXXFLAGS=-std=c++11 -Wall -g

SRCS := $(shell find . -name "*.cpp")
OBJS := $(patsubst %.cpp, %.o, $(SRCS))

all: dxz_queens dlx_queens dlx_capi.o

clean:
	rm -f dlx_queens dxz_queens $(OBJS)

dist-clean: clean
	rm -f .depend

.PHONY: all clean dist-clean depend

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^>>./.depend;

include .depend

dlx_queens: dlx_queens.o dlx.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o dlx_queens $^ $(LDLIBS)

dxz_queens: dxz_queens.o dxz.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o dxz_queens $^ $(LDLIBS)

%.o: %.cpp
	$(CC) $(CXXFLAGS) -c $<
