
all: build

build:
	make -C cmd

clean:
	make -C cmd clean

lint:
	golangci-lint run

test: lint
	make -C test

.PHONY: all lint clean build test
