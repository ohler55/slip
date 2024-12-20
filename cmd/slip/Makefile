BIN = $(GOPATH)/bin
APPBIN = $(shell basename `pwd`)

LDFLAG = '-s -w \
-X main.version=$(shell git describe --tags --always --dirty) \
-X github.com/ohler55/slip/pkg/cl.version=$(shell git describe --tags --always --dirty)'

all: build

clean:
	rm -f slip

build:
	go build -ldflags $(LDFLAG) -a -installsuffix nocgo -o slip *go

.PHONY: all clean build
