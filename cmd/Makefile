
all: build

build:
	make -C slip
	make -C slipr

clean:
	make -C slip clean
	make -C slipr clean

lint:
	golangci-lint run

.PHONY: all lint clean build
