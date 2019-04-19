UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
	JHOME := $(shell dirname $(shell dirname $(shell readlink -f $(shell which javac))))
	IPATH := linux
	ONAME := libEFastCircuit.so
endif
ifeq ($(UNAME), Darwin)
	JHOME := $(shell $(shell dirname $(shell readlink $(shell which javac)))/java_home)
	IPATH := darwin
	ONAME := libEFastCircuit.dylib
endif

all:
	javah -cp ../target/scala-2.11/classes/ HomNAND.EFastCircuit
	gcc -shared -fPIC -Wall -O3 -I/usr/include/ -I$(JHOME)/include/ -I$(JHOME)/include/$(IPATH) HomNAND_EFastCircuit.c -o $(ONAME)



