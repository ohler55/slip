BIN = $(GOPATH)/bin
APPBIN = $(shell basename `pwd`)

LDFLAG = '-s -w \
-X main.version=$(shell git describe --tags --always --dirty) \
-X main.built=$(shell date -Iseconds)'

all: build

clean:
	rm slip

build:
	go build -ldflags $(LDFLAG) -a -installsuffix nocgo -o slip *go

.PHONY: all clean build
