UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
	JHOME := $($(dirname $(readlink $(which javac)))/javahome)
	IPATH := linux
	ONAME := libEFastCircuit.so
endif
ifeq ($(UNAME), Darwin)
	JHOME := $(shell $(shell dirname $(shell readlink $(shell which javac)))/java_home)
	IPATH := darwin
	ONAME := libEFastCircuit.dylib
endif

all:
	gcc -shared -fPIC -Wall -O3 -I/usr/include/ -I$(JHOME)/include/ -I$(JHOME)/include/$(IPATH) HomNAND_EFastCircuit.c -o $(ONAME)



