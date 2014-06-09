CC = clang
OPTS = -O3
IROPTS= -S -emit-llvm
TARGET = test.c


all:
		$(CC)  $(IROPTS) $(TARGET) -S -o llvmIR.ll
		$(CC)  $(OPTS) $(TARGET) -o llvmIR.o

clean:
		rm *.o *.ll
